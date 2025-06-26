# ============================
# ===== CAESAR CIPHER =======
# ============================

# Fungsi Enkripsi Caesar (default shift 3)
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

# Fungsi Dekripsi Caesar
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


# Fungsi Attack Caesar (brute-force)
caesar_attack <- function(ciphertext) {
  cat("Percobaan semua kemungkinan shift Caesar Cipher:\n\n")
  for (s in 1:25) {
    decrypted <- caesar_decrypt(ciphertext, shift = s)
    cat(sprintf("Shift %2d: %s\n", s, decrypted))
  }
}

# ============================
# ========= MENU UTAMA ======
# ============================

repeat {
cat("\n====== MENU CAESAR CIPHER ======\n",
    "1. Enkripsi\n",
    "2. Dekripsi\n",
    "3. Attack\n",
    "4. Keluar\n", sep = "")

  pilihan <- as.integer(readline(prompt = "Pilih opsi (1-4): "))

  if (pilihan == 1) {
    plaintext <- tolower(readline(prompt = "Masukkan plaintext: "))
    shift <- as.integer(readline(prompt = "Masukkan shift (default 3): "))
    if (is.na(shift)) shift <- 3
    ciphertext <- caesar_encrypt(plaintext, shift)
    cat("Hasil Enkripsi :", ciphertext, "\n")

  } else if (pilihan == 2) {
    ciphertext <- tolower(readline(prompt = "Masukkan ciphertext: "))
    shift <- as.integer(readline(prompt = "Masukkan shift (default 3): "))
    if (is.na(shift)) shift <- 3
    plaintext <- caesar_decrypt(ciphertext, shift)
    cat("Hasil Dekripsi :", plaintext, "\n")

  } else if (pilihan == 3) {
    ciphertext <- tolower(readline(prompt = "Masukkan ciphertext yang akan di-attack: "))
    caesar_attack(ciphertext)

  } else if (pilihan == 4) {
    cat("Program selesai.\n")
    break

  } else {
    cat("Pilihan tidak valid. Silakan coba lagi.\n")
  }
}
