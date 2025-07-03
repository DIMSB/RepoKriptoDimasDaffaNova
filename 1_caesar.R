# ============================
# ===== CAESAR CIPHER =======
# ============================

# Fungsi untuk mengenkripsi plaintext menggunakan Caesar Cipher
caesar_encrypt <- function(plaintext, shift = 3) {
  alphabet <- letters
  result <- ""

  for (i in 1:nchar(plaintext)) {
    char <- substr(plaintext, i, i)
    if (char %in% alphabet) {
      pos <- match(char, alphabet)
      new_pos <- (pos + shift - 1) %% 26 + 1
      result <- paste0(result, alphabet[new_pos])
    } else {
      result <- paste0(result, char)
    }
  }

  return(result)
}

# Fungsi untuk mendekripsi ciphertext menggunakan Caesar Cipher
caesar_decrypt <- function(ciphertext, shift = 3) {
  alphabet <- letters
  result <- ""

  for (i in 1:nchar(ciphertext)) {
    char <- substr(ciphertext, i, i)
    if (char %in% alphabet) {
      pos <- match(char, alphabet)
      new_pos <- (pos - shift - 1) %% 26 + 1
      result <- paste0(result, alphabet[new_pos])
    } else {
      result <- paste0(result, char)
    }
  }

  return(result)
}

# Fungsi brute-force attack
caesar_attack <- function(ciphertext) {
  cat("Brute-force attack Caesar Cipher:\n\n")
  for (s in 1:25) {
    decrypted <- caesar_decrypt(ciphertext, shift = s)
    cat(sprintf("Shift %2d: %s\n", s, decrypted))
  }
}

# ============================
# ======== DEMO TEST =========
# ============================

# Variabel yang ditentukan langsung
plaintext <- "halo dunia ini uji coba cipher caesar"
shift_val <- 5

# Enkripsi
cat("Plaintext       :", plaintext, "\n")
encrypted_text <- caesar_encrypt(tolower(plaintext), shift_val)
cat("Hasil Enkripsi  :", encrypted_text, "\n")

# Dekripsi
decrypted_text <- caesar_decrypt(encrypted_text, shift_val)
cat("Hasil Dekripsi  :", decrypted_text, "\n")

# Brute-force attack
cat("\n===== BRUTE-FORCE ATTACK =====\n")
caesar_attack(encrypted_text)