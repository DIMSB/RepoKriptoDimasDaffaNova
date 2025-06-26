# ============================
# ==== FUNGSI PEMBANTU =======
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
vigenere_encrypt <- function(plaintext, key) {
  plaintext_clean <- casefold(gsub("[^A-Za-z]", "", plaintext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  if (!grepl("[A-Z]", plaintext_clean) || !grepl("[A-Z]", key_clean)) {
    stop("Plaintext dan key tidak boleh kosong dan harus mengandung huruf A-Z.")
  }

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
vigenere_decrypt <- function(ciphertext, key) {
  ciphertext_clean <- casefold(gsub("[^A-Za-z]", "", ciphertext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  if (!grepl("[A-Z]", ciphertext_clean) || !grepl("[A-Z]", key_clean)) {
    stop("Ciphertext dan key tidak boleh kosong dan harus mengandung huruf A-Z.")
  }

  ct_chars <- strsplit(ciphertext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(ct_chars))

  ct_nums <- char_to_num(ct_chars)
  key_nums <- char_to_num(key_chars)

  plain_nums <- (ct_nums - key_nums + 26) %% 26
  plain_chars <- num_to_char(plain_nums)

  paste(plain_chars, collapse = "")
}

# ============================
# ===== PROGRAM UTAMA ========
# ============================

# Tanpa input user
plaintext <- "HELLO"
key <- "WORLD"

cat("Plaintext: ", plaintext, "\n")
cat("Key      : ", key, "\n")

ciphertext <- vigenere_encrypt(plaintext, key)
cat("Hasil Enkripsi : ", ciphertext, "\n")

decrypted <- vigenere_decrypt(ciphertext, key)
cat("Hasil Dekripsi : ", decrypted, "\n")
