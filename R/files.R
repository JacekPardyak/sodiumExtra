#' Encrypt a file
#'
#' @param input_file_path Path to the original file.
#' @param sender_id Sender identifier.
#' @param recipient_id Recipient identifier.
#' @param file_id Unique file identifier.
#' @param root Project directory.
#' @return Path to the encrypted file, invisibly.
#' @export
encrypt_file <- function(input_file_path, sender_id,
                         recipient_id, file_id, root = ".") {
  .validate_id(sender_id, "sender_id")
  .validate_id(recipient_id, "recipient_id")
  .validate_id(file_id, "file_id")
  
  if (!file.exists(input_file_path) ||
      dir.exists(input_file_path)) {
    stop("Input file does not exist or is not a regular file.")
  }
  
  sender_key <- .read_raw(file.path(
    root, "private", "keys",
    paste0(sender_id, "_key.bin")
  ))
  
  recipient_pubkey <- .read_raw(file.path(
    root, "public", "keys",
    paste0(recipient_id, "_pubkey.bin")
  ))
  
  file_data <- .read_raw(input_file_path)
  
  ciphertext <- sodium::auth_encrypt(
    file_data, sender_key, recipient_pubkey
  )
  
  encrypted_path <- file.path(
    root, "public", "messages",
    paste0(file_id, ".bin")
  )
  
  nonce_path <- file.path(
    root, "public", "messages",
    paste0(file_id, "_nonce.bin")
  )
  
  nonce <- attr(ciphertext, "nonce")
  
  if (is.null(nonce)) {
    stop("Encryption did not return a nonce.")
  }
  
  .write_raw(as.raw(ciphertext), encrypted_path)
  .write_raw(as.raw(nonce), nonce_path)
  
  invisible(encrypted_path)
}


#' Decrypt a file
#'
#' @param file_id Identifier of the encrypted file.
#' @param sender_id Sender identifier.
#' @param recipient_id Recipient identifier.
#' @param output_file_path Destination for the decrypted file.
#' @param root Project directory.
#' @return Destination path, invisibly.
#' @export
decrypt_file <- function(file_id, sender_id, recipient_id,
                         output_file_path, root = ".") {
  .validate_id(file_id, "file_id")
  .validate_id(sender_id, "sender_id")
  .validate_id(recipient_id, "recipient_id")
  
  sender_pubkey <- .read_raw(file.path(
    root, "public", "keys",
    paste0(sender_id, "_pubkey.bin")
  ))
  
  recipient_key <- .read_raw(file.path(
    root, "private", "keys",
    paste0(recipient_id, "_key.bin")
  ))
  
  encrypted_path <- file.path(
    root, "public", "messages",
    paste0(file_id, ".bin")
  )
  
  nonce_path <- file.path(
    root, "public", "messages",
    paste0(file_id, "_nonce.bin")
  )
  
  ciphertext <- .read_raw(encrypted_path)
  nonce <- .read_raw(nonce_path)
  
  decrypted <- sodium::auth_decrypt(
    ciphertext, recipient_key, sender_pubkey, nonce
  )
  
  .write_raw(decrypted, output_file_path)
  
  invisible(output_file_path)
}
