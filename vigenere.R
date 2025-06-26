# ============================
# ==== FUNGSI PEMBANTU ======
# ============================

char_to_num <- function(chars) {
  sapply(chars, function(char) {
    as.integer(utf8ToInt(char)) - utf8ToInt("A")
  })
}

num_to_char <- function(nums) {
  sapply(as.integer(nums), function(num) {
    intToUtf8((num %% 26) + utf8ToInt("A"))
  })
}

repeat_key <- function(key, length_needed) {
  rep(strsplit(key, "")[[1]], length.out = length_needed)
}

# ============================
# ====== FUNGSI ENKRIPSI =====
# ============================

vigenere_encrypt <- function(plaintext, key) {
  plaintext_clean <- casefold(gsub("[^A-Za-z]", "", plaintext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  pt_chars <- strsplit(plaintext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(pt_chars))

  pt_nums <- char_to_num(pt_chars)
  key_nums <- char_to_num(key_chars)

  cipher_nums <- (pt_nums + key_nums) %% 26
  cipher_chars <- num_to_char(cipher_nums)

  paste(cipher_chars, collapse = "")
}

# ============================
# ====== FUNGSI DEKRIPSI =====
# ============================

vigenere_decrypt <- function(ciphertext, key) {
  ciphertext_clean <- casefold(gsub("[^A-Za-z]", "", ciphertext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  ct_chars <- strsplit(ciphertext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(ct_chars))

  ct_nums <- char_to_num(ct_chars)
  key_nums <- char_to_num(key_chars)

  plain_nums <- (ct_nums - key_nums + 26) %% 26
  plain_chars <- num_to_char(plain_nums)

  paste(plain_chars, collapse = "")
}

# ============================
# ======== FUNGSI ATTACK =====
# ============================

vigenere_attack <- function(ciphertext) {
  cat("Attack pada Vigenère cipher memerlukan teknik kriptanalisis seperti Kasiski atau Friedman.\n")
  cat("Fitur attack belum tersedia otomatis di versi ini.\n")
  cat("Ciphertext yang ingin dianalisis: ", ciphertext, "\n")
}

# ============================
# ======== PROGRAM UTAMA =====
# ============================

repeat {
cat("\n====== MENU VIGENÈRE CIPHER ======\n",
    "1. Enkripsi\n",
    "2. Dekripsi\n",
    "3. Attack\n",
    "4. Keluar\n", sep = "")

  pilihan <- menu(c("Enkripsi", "Dekripsi", "Attack", "Keluar"), title = "Pilih operasi:")

  if (pilihan == 1) {
    plaintext <- readline(prompt = "Masukkan plaintext: ")
    key <- readline(prompt = "Masukkan key: ")
    hasil <- vigenere_encrypt(plaintext, key)
    cat("Hasil Enkripsi: ", hasil, "\n")

  } else if (pilihan == 2) {
    ciphertext <- readline(prompt = "Masukkan ciphertext: ")
    key <- readline(prompt = "Masukkan key: ")
    hasil <- vigenere_decrypt(ciphertext, key)
    cat("Hasil Dekripsi: ", hasil, "\n")

  } else if (pilihan == 3) {
    ciphertext <- readline(prompt = "Masukkan ciphertext yang akan dianalisis: ")
    vigenere_attack(ciphertext)

  } else if (pilihan == 4) {
    cat("Program selesai.\n")
    break

  } else {
    cat("Pilihan tidak valid. Coba lagi.\n")
  }
}
