
test_that("messages and files can be encrypted and decrypted", {
  root <- tempfile("sodiumExtra-")
  dir.create(root)
  
  on.exit(unlink(root, recursive = TRUE))
  
  create_user_keys("vito", root = root)
  create_user_keys("bob", root = root)
  
  message_text <- "Ala ma kota\nzażółć gęślą jaźń"
  
  encrypt_message(
    message_text, "vito", "bob", "msg_001", root
  )
  
  result <- decrypt_message(
    "msg_001", "vito", "bob", root
  )
  
  expect_identical(result, message_text)
  
  input_path <- file.path(root, "input.html")
  output_path <- file.path(root, "output.html")
  
  writeBin(charToRaw("<h1>Zażółć</h1>"), input_path)
  
  encrypt_file(
    input_path, "vito", "bob", "file_001", root
  )
  
  decrypt_file(
    "file_001", "vito", "bob", output_path, root
  )
  
  expect_identical(
    readBin(input_path, "raw", n = file.info(input_path)$size),
    readBin(output_path, "raw", n = file.info(output_path)$size)
  )
})
