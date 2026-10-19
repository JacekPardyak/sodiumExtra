devtools::create("sodiumExtra")
usethis::use_mit_license()

devtools::document()
devtools::test()

# Sprawdzenie kodu pakietu
devtools::check()

# Zbudowanie archiwum pakietu
devtools::build()

# Instalacja lokalna
devtools::install()

# Załadowanie biblioteki
library(sodiumExtra)

create_user_keys("vito")
create_user_keys("bob")

encrypt_message(
  message = "Ala ma kota\nzażółć gęślą jaźń",
  sender_id = "vito",
  recipient_id = "bob",
  message_id = "msg_001"
)

decrypt_message(
  message_id = "msg_001",
  sender_id = "vito",
  recipient_id = "bob"
)

sodiumExtra::sync_github(
  root = ".",
  user.name = "John Doe",
  user.email = "john.doe@mail.com",
  commit_message = "Github synchronization"
)

library(sodium)
#library(sodiumExtra)

# Tab 0 - Initialization
dir.create("public/messages", recursive = TRUE, showWarnings = FALSE)
dir.create("public/keys", recursive = TRUE, showWarnings = FALSE)
dir.create("private/messages", recursive = TRUE, showWarnings = FALSE)
dir.create("private/keys", recursive = TRUE, showWarnings = FALSE)



## Tab 1 - Key generation
user_id <- "vito"
user_key <- sodium::keygen()
user_pubkey <- sodium::pubkey(user_key)
writeBin(user_key, glue::glue("private/keys/{user_id}_key.bin"))
writeBin(user_pubkey, glue::glue("public/keys/{user_id}_pubkey.bin"))

## Tab 2 - Message encryption
sender_id <- "vito"
recipient_id <- "bob"
message <- "Ala ma kota \n zażółć gęślą jaźń"
message_id <- "msg_001"
sender_key_path <- glue::glue("private/keys/{sender_id}_key.bin")
recipient_pubkey_path <- glue::glue("public/keys/{recipient_id}_pubkey.bin")
sender_key <- readBin(sender_key_path, what = "raw", n = file.info(sender_key_path)$size)
recipient_pubkey <- readBin(recipient_pubkey_path, what = "raw", n = file.info(recipient_pubkey_path)$size)
msg <- charToRaw(enc2utf8(message))
ciphertext <- sodium::auth_encrypt(msg, sender_key, recipient_pubkey)
message_path <- glue::glue("public/messages/{message_id}.bin")
writeBin(as.raw(ciphertext), message_path)
nonce <- attr(ciphertext, "nonce")
nonce_path <- glue::glue("public/messages/{message_id}_nonce.bin")
writeBin(as.raw(nonce), nonce_path)

## Tab 3 - Message decryption
sender_id <- "vito"
recipient_id <- "bob"
message_id <- "msg_001"

sender_pubkey_path <- glue::glue("public/keys/{sender_id}_pubkey.bin")
recipient_key_path <- glue::glue("private/keys/{recipient_id}_key.bin")
message_path <- glue::glue("public/messages/{message_id}.bin")
nonce_path <- glue::glue("public/messages/{message_id}_nonce.bin")
recipient_key <- readBin(recipient_key_path, what = "raw", n = file.info(recipient_key_path)$size)
sender_pubkey <- readBin(sender_pubkey_path, what = "raw", n = file.info(sender_pubkey_path)$size)
ciphertext <- readBin(message_path, what = "raw", n = file.info(message_path)$size)
nonce <- readBin(nonce_path, what = "raw", n = file.info(nonce_path)$size)
decrypted <- sodium::auth_decrypt(ciphertext, recipient_key, sender_pubkey, nonce)
rawToChar(decrypted)

# Tab 4 - File encryption

sender_id <- "vito"
recipient_id <- "bob"

input_file_path <- "private/messages/Untitled.html"
file_id <- "file_001"
sender_key_path <- glue::glue("private/keys/{sender_id}_key.bin")
recipient_pubkey_path <- glue::glue("public/keys/{recipient_id}_pubkey.bin")
sender_key <- readBin(sender_key_path, what = "raw", n = file.info(sender_key_path)$size)
recipient_pubkey <- readBin(recipient_pubkey_path, what = "raw", n = file.info(recipient_pubkey_path)$size)
file_data <- readBin(input_file_path, what = "raw", n = file.info(input_file_path)$size)
ciphertext <- sodium::auth_encrypt(file_data, sender_key, recipient_pubkey)
encrypted_file_path <- glue::glue("public/messages/{file_id}.bin")
writeBin(as.raw(ciphertext), encrypted_file_path)
nonce <- attr(ciphertext, "nonce")
nonce_path <- glue::glue("public/messages/{file_id}_nonce.bin")
writeBin(as.raw(nonce), nonce_path)


# Tab 5 - File decryption

sender_id <- "vito"
recipient_id <- "bob"

file_id <- "file_001"
output_file_path <- "private/messages/dokument.html"
sender_pubkey_path <- glue::glue("public/keys/{sender_id}_pubkey.bin")
recipient_key_path <- glue::glue("private/keys/{recipient_id}_key.bin")
encrypted_file_path <- glue::glue("public/messages/{file_id}.bin")
nonce_path <- glue::glue("public/messages/{file_id}_nonce.bin")
recipient_key <- readBin(recipient_key_path, what = "raw", n = file.info(recipient_key_path)$size)
sender_pubkey <- readBin(sender_pubkey_path, what = "raw", n = file.info(sender_pubkey_path)$size)
ciphertext <- readBin(encrypted_file_path, what = "raw", n = file.info(encrypted_file_path)$size)
nonce <- readBin(nonce_path, what = "raw", n = file.info(nonce_path)$size)
decrypted <- sodium::auth_decrypt(ciphertext, recipient_key, sender_pubkey, nonce)
writeBin(decrypted, output_file_path)

## Tab 6 - Github synchronization
user.name <- "John Doe"
user.email <- "john.doe@mail.com"
  
system("git remote -v")
system(glue::glue('git config --global user.name "{user.name}"'))
system(glue::glue('git config --global user.email "{user.email}"'))
system("git pull")
system("git add .")
system('git commit -m "Github synchronization"')
system("git push")









library(sodium)
#library(sodiumExtra)

# Tab 0 - Initialization
dir.create("public/messages", recursive = TRUE, showWarnings = FALSE)
dir.create("public/keys", recursive = TRUE, showWarnings = FALSE)
dir.create("private/messages", recursive = TRUE, showWarnings = FALSE)
dir.create("private/keys", recursive = TRUE, showWarnings = FALSE)

gitignore_path <- ".gitignore"
line_to_add <- "/private/"
if (!file.exists(gitignore_path)) {
  file.create(gitignore_path)
}
lines <- readLines(gitignore_path, warn = FALSE)

if (!line_to_add %in% lines) {
  write(line_to_add, file = gitignore_path, append = TRUE)
  message("Dodano /private/ do .gitignore")
} else {
  message("Wpis /private/ już istnieje w .gitignore")
}

## Tab 1 - Key generation
user_id <- "bob"
user_key <- sodium::keygen()
user_pubkey <- sodium::pubkey(user_key)
writeBin(user_key, glue::glue("private/keys/{user_id}_key.bin"))
writeBin(user_pubkey, glue::glue("public/keys/{user_id}_pubkey.bin"))

## Tab 2 - Message encryption
sender_id <- "vito"
recipient_id <- "bob"
message <- "Ala ma kota \n zażółć gęślą jaźń"
message_id <- "msg_001"
sender_key_path <- glue::glue("private/keys/{sender_id}_key.bin")
recipient_pubkey_path <- glue::glue("public/keys/{recipient_id}_pubkey.bin")
sender_key <- readBin(sender_key_path, what = "raw", n = file.info(sender_key_path)$size)
recipient_pubkey <- readBin(recipient_pubkey_path, what = "raw", n = file.info(recipient_pubkey_path)$size)
msg <- charToRaw(enc2utf8(message))
ciphertext <- sodium::auth_encrypt(msg, sender_key, recipient_pubkey)
message_path <- glue::glue("public/messages/{message_id}.bin")
writeBin(as.raw(ciphertext), message_path)
nonce <- attr(ciphertext, "nonce")
nonce_path <- glue::glue("public/messages/{message_id}_nonce.bin")
writeBin(as.raw(nonce), nonce_path)

## Tab 3 - Message decryption
sender_id <- "vito"
recipient_id <- "bob"
message_id <- "msg_001"

sender_pubkey_path <- glue::glue("public/keys/{sender_id}_pubkey.bin")
recipient_key_path <- glue::glue("private/keys/{recipient_id}_key.bin")
message_path <- glue::glue("public/messages/{message_id}.bin")
nonce_path <- glue::glue("public/messages/{message_id}_nonce.bin")
recipient_key <- readBin(recipient_key_path, what = "raw", n = file.info(recipient_key_path)$size)
sender_pubkey <- readBin(sender_pubkey_path, what = "raw", n = file.info(sender_pubkey_path)$size)
ciphertext <- readBin(message_path, what = "raw", n = file.info(message_path)$size)
nonce <- readBin(nonce_path, what = "raw", n = file.info(nonce_path)$size)
decrypted <- sodium::auth_decrypt(ciphertext, recipient_key, sender_pubkey, nonce)
rawToChar(decrypted)

# Tab 4 - File encryption

sender_id <- "vito"
recipient_id <- "bob"

input_file_path <- "private/messages/Untitled.html"
file_id <- "file_001"
sender_key_path <- glue::glue("private/keys/{sender_id}_key.bin")
recipient_pubkey_path <- glue::glue("public/keys/{recipient_id}_pubkey.bin")
sender_key <- readBin(sender_key_path, what = "raw", n = file.info(sender_key_path)$size)
recipient_pubkey <- readBin(recipient_pubkey_path, what = "raw", n = file.info(recipient_pubkey_path)$size)
file_data <- readBin(input_file_path, what = "raw", n = file.info(input_file_path)$size)
ciphertext <- sodium::auth_encrypt(file_data, sender_key, recipient_pubkey)
encrypted_file_path <- glue::glue("public/messages/{file_id}.bin")
writeBin(as.raw(ciphertext), encrypted_file_path)
nonce <- attr(ciphertext, "nonce")
nonce_path <- glue::glue("public/messages/{file_id}_nonce.bin")
writeBin(as.raw(nonce), nonce_path)


# Tab 5 - File decryption

sender_id <- "vito"
recipient_id <- "bob"

file_id <- "file_001"
output_file_path <- "private/messages/dokument.html"
sender_pubkey_path <- glue::glue("public/keys/{sender_id}_pubkey.bin")
recipient_key_path <- glue::glue("private/keys/{recipient_id}_key.bin")
encrypted_file_path <- glue::glue("public/messages/{file_id}.bin")
nonce_path <- glue::glue("public/messages/{file_id}_nonce.bin")
recipient_key <- readBin(recipient_key_path, what = "raw", n = file.info(recipient_key_path)$size)
sender_pubkey <- readBin(sender_pubkey_path, what = "raw", n = file.info(sender_pubkey_path)$size)
ciphertext <- readBin(encrypted_file_path, what = "raw", n = file.info(encrypted_file_path)$size)
nonce <- readBin(nonce_path, what = "raw", n = file.info(nonce_path)$size)
decrypted <- sodium::auth_decrypt(ciphertext, recipient_key, sender_pubkey, nonce)
writeBin(decrypted, output_file_path)

## Tab 6 - Github synchronization
gitignore_path <- ".gitignore"
line_to_add <- "/private/"
if (!file.exists(gitignore_path)) {
  file.create(gitignore_path)
}
lines <- readLines(gitignore_path, warn = FALSE)

if (!line_to_add %in% lines) {
  write(line_to_add, file = gitignore_path, append = TRUE)
  message("Entry /private/ added to .gitignore")
} else {
  message("Entry /private/ already exists in .gitignore")
}

user.name <- "John Doe"
user.email <- "john.doe@mail.com"
  
system("git remote -v")
system(glue::glue('git config --global user.name "{user.name}"'))
system(glue::glue('git config --global user.email "{user.email}"'))
system("git pull")
system("git add .")
system('git commit -m "Github synchronization"')
system("git push")
