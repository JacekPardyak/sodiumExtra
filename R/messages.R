
#' Encrypt a text message
#'
#' @param message Text to encrypt.
#' @param sender_id Sender identifier.
#' @param recipient_id Recipient identifier.
#' @param message_id Unique message identifier.
#' @param root Project directory.
#' @return Path to the encrypted message file, invisibly.
#' @export
encrypt_message <- function(message, sender_id, recipient_id,
                            message_id, root = ".") {
  .validate_id(sender_id, "sender_id")
  .validate_id(recipient_id, "recipient_id")
  .validate_id(message_id, "message_id")
  
  if (!is.character(message) || length(message) != 1L ||
      is.na(message)) {
    stop("message must be one non-missing character string.")
  }
  
  sender_key <- .read_raw(file.path(
    root, "private", "keys",
    paste0(sender_id, "_key.bin")
  ))
  
  recipient_pubkey <- .read_raw(file.path(
    root, "public", "keys",
    paste0(recipient_id, "_pubkey.bin")
  ))
  
  msg <- charToRaw(enc2utf8(message))
  
  ciphertext <- sodium::auth_encrypt(
    msg, sender_key, recipient_pubkey
  )
  
  message_path <- file.path(
    root, "public", "messages",
    paste0(message_id, ".bin")
  )
  
  nonce_path <- file.path(
    root, "public", "messages",
    paste0(message_id, "_nonce.bin")
  )
  
  nonce <- attr(ciphertext, "nonce")
  
  if (is.null(nonce)) {
    stop("Encryption did not return a nonce.")
  }
  
  .write_raw(as.raw(ciphertext), message_path)
  .write_raw(as.raw(nonce), nonce_path)
  
  invisible(message_path)
}


#' Decrypt a text message
#'
#' @param message_id Message identifier.
#' @param sender_id Sender identifier.
#' @param recipient_id Recipient identifier.
#' @param root Project directory.
#' @return Decrypted UTF-8 text.
#' @export
decrypt_message <- function(message_id, sender_id,
                            recipient_id, root = ".") {
  .validate_id(message_id, "message_id")
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
  
  message_path <- file.path(
    root, "public", "messages",
    paste0(message_id, ".bin")
  )
  
  nonce_path <- file.path(
    root, "public", "messages",
    paste0(message_id, "_nonce.bin")
  )
  
  ciphertext <- .read_raw(message_path)
  nonce <- .read_raw(nonce_path)
  
  decrypted <- sodium::auth_decrypt(
    ciphertext, recipient_key, sender_pubkey, nonce
  )
  
  rawToChar(decrypted)
}
