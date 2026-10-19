
#' Synchronize a Git repository with GitHub
#'
#' Adds /private/ to .gitignore, configures the local Git author,
#' pulls remote changes, commits local changes and pushes them.
#'
#' @param root Path to the Git repository.
#' @param user.name Git author name.
#' @param user.email Git author email.
#' @param commit_message Commit message.
#' @return Invisibly returns TRUE if synchronization succeeds.
#' @export
sync_github <- function(
    root = ".",
    user.name,
    user.email,
    commit_message = "Github synchronization"
) {
  # Validate arguments
  if (!is.character(user.name) || length(user.name) != 1L ||
      is.na(user.name) || !nzchar(user.name)) {
    stop("user.name must be a non-empty string.")
  }
  
  if (!is.character(user.email) || length(user.email) != 1L ||
      is.na(user.email) || !nzchar(user.email)) {
    stop("user.email must be a non-empty string.")
  }
  
  if (!is.character(commit_message) ||
      length(commit_message) != 1L ||
      is.na(commit_message) || !nzchar(commit_message)) {
    stop("commit_message must be a non-empty string.")
  }
  
  root <- normalizePath(root, mustWork = TRUE)
  
  # Helper for executing Git commands safely
  git <- function(args) {
    result <- processx::run(
      command = "git",
      args = args,
      wd = root,
      error_on_status = FALSE,
      echo = FALSE
    )
    
    if (result$status != 0L) {
      details <- trimws(result$stderr)
      if (!nzchar(details)) {
        details <- trimws(result$stdout)
      }
      
      stop(
        "Git command failed: git ",
        paste(args, collapse = " "),
        if (nzchar(details)) paste0("\n", details),
        call. = FALSE
      )
    }
    
    result
  }
  
  # Verify that root is a Git repository
  git(c("rev-parse", "--show-toplevel"))
  
  # Add /private/ to .gitignore if necessary
  gitignore_path <- file.path(root, ".gitignore")
  line_to_add <- "/private/"
  
  if (!file.exists(gitignore_path)) {
    file.create(gitignore_path)
  }
  
  lines <- readLines(gitignore_path, warn = FALSE)
  
  if (!line_to_add %in% lines) {
    # Preserve existing contents and ensure a newline
    if (length(lines) > 0L) {
      cat(paste0(paste(lines, collapse = "\n"), "\n"),
          file = gitignore_path)
    }
    
    cat(line_to_add, "\n", file = gitignore_path, append = TRUE)
    message("Entry /private/ added to .gitignore")
  } else {
    message("Entry /private/ already exists in .gitignore")
  }
  
  # Refuse to continue if private files are already tracked
  tracked <- git(c("ls-files", "--", "private/"))$stdout
  
  if (nzchar(trimws(tracked))) {
    stop(
      "Git is already tracking files under private/. ",
      "Review them and remove them from the index with ",
      "'git rm -r --cached -- private/' before synchronizing.",
      call. = FALSE
    )
  }
  
  # Configure author for this repository only
  git(c("config", "--local", "user.name", user.name))
  git(c("config", "--local", "user.email", user.email))
  
  # Display configured remotes
  remotes <- git(c("remote", "-v"))$stdout
  cat(remotes)
  
  if (!nzchar(trimws(remotes))) {
    stop("No Git remote is configured.", call. = FALSE)
  }
  
  # Pull only if fast-forward is possible
  git(c("pull", "--ff-only"))
  
  # Stage changes; ignored files remain excluded
  git(c("add", "-A"))
  
  # Check whether there is anything to commit
  staged <- processx::run(
    "git",
    c("diff", "--cached", "--quiet"),
    wd = root,
    error_on_status = FALSE,
    echo = FALSE
  )
  
  if (staged$status == 0L) {
    message("No changes to commit.")
  } else if (staged$status == 1L) {
    git(c("commit", "-m", commit_message))
  } else {
    stop("Could not determine whether staged changes exist.")
  }
  
  # Push changes to the configured upstream
  git(c("push"))
  
  message("GitHub synchronization completed.")
  invisible(TRUE)
}
