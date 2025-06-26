# Fungsi Caesar Cipher: Pergeseran tetap 3
caesar_cipher <- function(plaintext) {
  alphabet <- letters
  result <- ""

  for (i in 1:nchar(plaintext)) {
    char <- substr(plaintext, i, i)
    if (char %in% alphabet) {
      pos <- match(char, alphabet)
      new_pos <- (pos + 3 - 1) %% 26 + 1
      result <- paste0(result, alphabet[new_pos])
    } else {
      result <- paste0(result, char)
    }
  }

  return(result)
}

# Jalankan Caesar Cipher langsung pada kata "daffa"
plaintext <- "daffa"
ciphertext <- caesar_cipher(tolower(plaintext))

cat("Kalimat asli      :", plaintext, "\n")
cat("Hasil Caesar Cipher:", ciphertext, "\n")
