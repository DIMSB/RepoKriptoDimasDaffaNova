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
  plaintext_clean <- toupper(gsub("[^A-Za-z]", "", plaintext))
  key_clean <- toupper(gsub("[^A-Za-z]", "", key))

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
  ciphertext_clean <- toupper(gsub("[^A-Za-z]", "", ciphertext))
  key_clean <- toupper(gsub("[^A-Za-z]", "", key))

  ct_chars <- strsplit(ciphertext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(ct_chars))

  ct_nums <- char_to_num(ct_chars)
  key_nums <- char_to_num(key_chars)

  plain_nums <- (ct_nums - key_nums + 26) %% 26
  plain_chars <- num_to_char(plain_nums)

  paste(plain_chars, collapse = "")
}

# ============================
# ========== DEMO ============
# ============================

plaintext <- "uji coba vigenere cipher"
key <- "rahasia"

cat("Plaintext :", plaintext, "\n")
cat("Key       :", key, "\n")

# Enkripsi
ciphertext <- vigenere_encrypt(plaintext, key)
cat("Ciphertext:", ciphertext, "\n")

# Dekripsi
decrypted <- vigenere_decrypt(ciphertext, key)
cat("Dekripsi  :", decrypted, "\n")

# Simulasi attack (placeholder)
cat("\n===== SIMULASI ATTACK =====\n")
cat("Attack otomatis belum tersedia.\n")
cat("Ciphertext yang tersedia: ", ciphertext, "\n")
