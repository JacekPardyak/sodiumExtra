#' Clone a Git repository
#'
#' Clone a remote Git repository into the current working directory.
#' If the repository already exists, its path is returned without
#' cloning it again.
#'
#' @param repository_url A single character string containing the URL
#'   of the Git repository.
#'
#' @return An absolute path to the local repository, invisibly.
#'
#' @details
#' If a directory with the repository name already exists but is not
#' a Git repository, the function raises an error.
#' The working directory is not changed.
#'
#' @seealso [system2()], [utils::packageDescription()]
#'
#' @export
clone_vault <- function(repository_url) {
  if (!is.character(repository_url) ||
      length(repository_url) != 1L ||
      is.na(repository_url) ||
      !nzchar(trimws(repository_url))) {
    stop(
      "'repository_url'must be a single, non-empty string of characters.",
      call. = FALSE
    )
  }
  
  repository_url <- trimws(repository_url)
  repository_url <- sub("/+$", "", repository_url)
  
  repo_name <- basename(repository_url)
  repo_name <- sub("\\.git$", "", repo_name)
  
  if (!nzchar(repo_name) || repo_name %in% c(".", "..")) {
    stop(
      "The repository name cannot be determined from the URL.",
      call. = FALSE
    )
  }
  
  if (!dir.exists(repo_name)) {
    .run_git(c("clone", repository_url))
    
    if (!dir.exists(file.path(repo_name, ".git")) &&
        !file.exists(file.path(repo_name, ".git"))) {
      stop(
        "Cloning failed or the Git repository was not created.",
        call. = FALSE
      )
    }
    
  } else if (!dir.exists(file.path(repo_name, ".git")) &&
             !file.exists(file.path(repo_name, ".git"))) {
    stop(
      paste0(
        "Folder '", repo_name,
        "' exists, but it is not a Git repository.",
        "Change its name or check the working directory."
      ),
      call. = FALSE
    )
    
  } else {
    message("The repository already exists.")
  }
  
  invisible(normalizePath(repo_name))
  # First, go to the repository!
  setwd(repo_name)
}
