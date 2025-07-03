# === Fungsi Enkripsi Satu Kali ===
encrypt_columnar <- function(text, key) {
  key_order <- order(strsplit(key, "")[[1]])
  ncol <- nchar(key)
  chars <- strsplit(gsub(" ", "", toupper(text)), "")[[1]]
  nrow <- ceiling(length(chars) / ncol)
  matrix_text <- matrix(c(chars, rep("X", nrow * ncol - length(chars))),
                        nrow = nrow, ncol = ncol, byrow = TRUE)
  ciphertext <- ""
  for (i in key_order) {
    ciphertext <- paste0(ciphertext, paste0(matrix_text[, i], collapse = ""))
  }
  return(ciphertext)
}

# === Fungsi Dekripsi Satu Kali ===
decrypt_columnar <- function(ciphertext, key) {
  key_order <- order(strsplit(key, "")[[1]])
  ncol <- nchar(key)
  nrow <- ceiling(nchar(ciphertext) / ncol)
  matrix_text <- matrix("", nrow = nrow, ncol = ncol)

  k <- 1
  for (i in key_order) {
    for (j in 1:nrow) {
      matrix_text[j, i] <- substr(ciphertext, k, k)
      k <- k + 1
    }
  }

  plaintext <- paste0(matrix_text, collapse = "")
  return(plaintext)
}

# === Fungsi Enkripsi Double Columnar ===
encrypt_double_columnar <- function(plaintext, key1, key2) {
  step1 <- encrypt_columnar(plaintext, key1)
  step2 <- encrypt_columnar(step1, key2)
  return(step2)
}

# === Fungsi Dekripsi Double Columnar ===
decrypt_double_columnar <- function(ciphertext, key1, key2) {
  step1 <- decrypt_columnar(ciphertext, key2)
  step2 <- decrypt_columnar(step1, key1)
  return(step2)
}

# === Fungsi Attack Double Columnar (Brute-force pendek) ===
attack_double_columnar <- function(ciphertext, max_key_len = 3) {
  cat("\n=== Double Columnar Cipher Attack ===\n")
  alphabet <- letters
  count <- 0
  for (len1 in 2:max_key_len) {
    key_combos1 <- combn(alphabet, len1, simplify = FALSE)
    for (key1 in key_combos1[1:3]) {
      for (len2 in 2:max_key_len) {
        key_combos2 <- combn(alphabet, len2, simplify = FALSE)
        for (key2 in key_combos2[1:3]) {
          k1 <- paste0(key1, collapse = "")
          k2 <- paste0(key2, collapse = "")
          guess <- decrypt_double_columnar(ciphertext, k1, k2)
          cat("Key1:", k1, "Key2:", k2, "| Tebakan Plaintext:", guess, "\n")
          count <- count + 1
          if (count >= 10) {
            lanjut <- readline("Tampilkan 10 hasil lagi? (y/n): ")
            if (tolower(lanjut) != "y") return()
            count <- 0
          }
        }
      }
    }
  }
}

# === PROGRAM UTAMA DENGAN MENU INTERAKTIF ===
repeat {
  cat("\n====== MENU (ALGORITMA) DOUBLE COLUMNAR TRANSPOSITION ======\n",
      "1. Enkripsi\n",
      "2. Dekripsi\n",
      "3. Attack (Brute-force sederhana)\n",
      "4. Keluar\n", sep = "")
  mode <- readline("Pilih mode (1/2/3/4): ")

  if (mode == "1") {
    plaintext <- readline("Masukkan plaintext: ")
    key1 <- readline("Masukkan key pertama: ")
    key2 <- readline("Masukkan key kedua: ")
    hasil <- encrypt_double_columnar(plaintext, key1, key2)
    cat("\nHasil Enkripsi:\n")
    cat("Plaintext :", plaintext, "\n")
    cat("Key1      :", key1, "\n")
    cat("Key2      :", key2, "\n")
    cat("Cipher    :", hasil, "\n")

  } else if (mode == "2") {
    cipher_text <- readline("Masukkan ciphertext: ")
    key1 <- readline("Masukkan key pertama: ")
    key2 <- readline("Masukkan key kedua: ")
    hasil <- decrypt_double_columnar(cipher_text, key1, key2)
    cat("\nHasil Dekripsi:\n")
    cat("Cipher    :", cipher_text, "\n")
    cat("Key1      :", key1, "\n")
    cat("Key2      :", key2, "\n")
    cat("Plaintext :", hasil, "\n")

  } else if (mode == "3") {
    cipher_text <- readline("Masukkan ciphertext: ")
    attack_double_columnar(cipher_text)

  } else if (mode == "4") {
    cat("Program selesai.\n")
    break

  } else {
    cat("Pilihan tidak valid. Silakan pilih 1, 2, 3, atau 4.\n")
  }
}


