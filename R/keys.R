
#' Create public and private keys for a user
#'
#' @param user_id Identifier of the user.
#' @param root Project directory used for storing keys.
#' @return Invisibly returns paths to the generated key files.
#' @export
create_user_keys <- function(user_id, root = ".") {
  stopifnot(
    is.character(user_id),
    length(user_id) == 1L,
    !is.na(user_id),
    nzchar(user_id),
    !grepl("[/\\\\]", user_id),
    user_id != ".",
    user_id != ".."
  )
  
  private_dir <- file.path(root, "private", "keys")
  public_dir <- file.path(root, "public", "keys")
  
  dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)
  
  private_path <- file.path(
    private_dir, paste0(user_id, "_key.bin")
  )
  public_path <- file.path(
    public_dir, paste0(user_id, "_pubkey.bin")
  )
  
  if (file.exists(private_path) || file.exists(public_path)) {
    stop("Keys already exist. Refusing to overwrite them.")
  }
  
  key <- sodium::keygen()
  pubkey <- sodium::pubkey(key)
  
  writeBin(key, private_path)
  writeBin(pubkey, public_path)
  
  invisible(list(
    private_key = private_path,
    public_key = public_path
  ))
}
