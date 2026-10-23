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



#' Synchronize a local Git repository with GitHub
#'
#' Configure Git identity, stage local changes, create a commit if
#' necessary, and push changes to the remote GitHub repository.
#'
#' @param user_repo Character string containing the remote repository URL.
#' @param user_token Character string containing a GitHub access token.
#' @param user_name Git author name.
#' @param user_email Git author email.
#'
#' @return Invisibly returns TRUE if synchronization succeeds.
#'
#' @details
#' The function must be called from within a local Git repository.
#' It temporarily configures Git authentication using a temporary
#' askpass script and restores the original environment variables
#' after execution.
#'
#' @export
sync_vault <- function(user_repo, user_token, user_name, user_email) {
  
  # Validate arguments
  args <- list(
    user_repo = user_repo,
    user_token = user_token,
    user_name = user_name,
    user_email = user_email
  )
  
  valid <- vapply(
    args,
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(trimws(x))
    },
    logical(1)
  )
  
  if (!all(valid)) {
    stop(
      paste(
        "Arguments must be non-empty character strings:",
        paste(names(args)[!valid], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Preserve the original environment
  env_names <- c(
    "GITHUB_PAT",
    "GIT_ASKPASS",
    "GIT_TERMINAL_PROMPT"
  )
  
  old_env <- Sys.getenv(env_names, unset = NA_character_)
  
  # Create temporary askpass script
  askpass <- tempfile("git-askpass-")
  
  on.exit({
    # Restore environment variables
    for (i in seq_along(env_names)) {
      if (is.na(old_env[i])) {
        Sys.unsetenv(env_names[i])
      } else {
        do.call(
          Sys.setenv,
          stats::setNames(list(old_env[i]), env_names[i])
        )
      }
    }
    
    # Remove temporary script
    unlink(askpass)
  }, add = TRUE)
  
  writeLines(
    c(
      "#!/bin/sh",
      'case "$1" in',
      '  *Username*) printf "%s\\n" "x-access-token" ;;',
      '  *Password*) printf "%s\\n" "$GITHUB_PAT" ;;',
      '  *) exit 1 ;;',
      'esac'
    ),
    askpass
  )
  
# if (Sys.chmod(askpass, "0700") != 0L) {
#    stop(
#      "Failed to set permissions for the askpass file.",
#      call. = FALSE
#    )
#  }
  
  # Configure temporary authentication environment
  Sys.setenv(
    GITHUB_PAT = user_token,
    GIT_ASKPASS = askpass,
    GIT_TERMINAL_PROMPT = "0"
  )
  
  # Verify that the current directory is a Git repository
  .run_git(c("rev-parse", "--show-toplevel"))
  
  # Configure Git identity and remote
  .run_git(c("config", "user.name", user_name))
  .run_git(c("config", "user.email", user_email))
  .run_git(c("remote", "set-url", "origin", user_repo))
  
  # Stage all changes, including deletions
  .run_git(c("add", "-A"))
  
  # Check whether there are staged changes
  staged_status <- .run_git(
    c("diff", "--cached", "--quiet"),
    fail = FALSE
  )
  
  if (staged_status == 1L) {
    
    .run_git(c(
      "commit",
      "-m",
      "Synchronize files from Google Colab"
    ))
    
  } else if (staged_status == 0L) {
    
    message("No changes to approve.")
    
  } else {
    
    stop(
      "Failed to check changes prepared for commit.",
      call. = FALSE
    )
  }
  
  # Push changes to GitHub
  .run_git(c("push"))
  
  message("SUCCESS: synchronization with GitHub completed.")
  
  invisible(TRUE)
}
