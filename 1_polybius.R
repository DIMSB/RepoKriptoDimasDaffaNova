# ===============================
# === POLYBIUS CIPHER (R) ======
# ===============================

# === Generate Key Square 5x5 (menggabungkan I dan J) ===
generate_key_square <- function(keyword) {
  alphabet <- "abcdefghiklmnopqrstuvwxyz"
  keyword <- tolower(gsub("[^a-z]", "", keyword))
  keyword <- gsub("j", "i", keyword)
  keyword_unique <- paste(unique(strsplit(keyword, "")[[1]]), collapse = "")
  remaining_letters <- gsub(paste0("[", keyword_unique, "]"), "", alphabet)
  full_key <- paste0(keyword_unique, remaining_letters)
  matrix(strsplit(full_key, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)
}

# === Temukan posisi huruf dalam key square ===
find_position <- function(letter, key_square) {
  if (letter == "j") letter <- "i"
  pos <- which(key_square == letter, arr.ind = TRUE)
  return(c(pos[1], pos[2]))
}

# === Fungsi Enkripsi Polybius ===
polybius_encrypt <- function(plaintext, keyword) {
  plaintext_clean <- tolower(gsub("[^a-z]", "", plaintext))
  plaintext_clean <- gsub("j", "i", plaintext_clean)
  key_square <- generate_key_square(keyword)

  cipher <- c()
  for (char in strsplit(plaintext_clean, "")[[1]]) {
    pos <- find_position(char, key_square)
    cipher <- c(cipher, paste0(pos[1], pos[2]))
  }

  return(paste(cipher, collapse = ""))
}

# === Fungsi Dekripsi Polybius ===
polybius_decrypt <- function(cipher_text, keyword) {
  key_square <- generate_key_square(keyword)
  cipher_pairs <- unlist(strsplit(cipher_text, ""))

  if (length(cipher_pairs) %% 2 != 0) {
    stop("Panjang cipher tidak valid, harus genap.")
  }

  plaintext <- c()
  for (i in seq(1, length(cipher_pairs), by = 2)) {
    row <- as.numeric(cipher_pairs[i])
    col <- as.numeric(cipher_pairs[i + 1])
    letter <- key_square[row, col]
    plaintext <- c(plaintext, letter)
  }

  return(paste(plaintext, collapse = ""))
}

# === Fungsi Attack Manual (menggunakan key square default) ===
polybius_attack <- function(cipher_text) {
  cat("=== Polybius Manual Attack ===\n")
  cat("Cipher Text:", cipher_text, "\n")
  key_square <- matrix(strsplit("abcdefghiklmnopqrstuvwxyz", "")[[1]], nrow = 5, byrow = TRUE)
  print(key_square)

  cipher_pairs <- unlist(strsplit(cipher_text, ""))

  if (length(cipher_pairs) %% 2 != 0) {
    stop("Panjang cipher tidak valid, harus genap.")
  }

  plaintext <- c()
  for (i in seq(1, length(cipher_pairs), by = 2)) {
    row <- as.numeric(cipher_pairs[i])
    col <- as.numeric(cipher_pairs[i + 1])
    plaintext <- c(plaintext, key_square[row, col])
  }

  cat("Tebakan hasil attack:", paste(plaintext, collapse = ""), "\n")
}

# ===============================
# ========== DEMO ===============
# ===============================

plaintext <- "uji coba polybius cipher"
keyword <- "kriptografi"

cat("Plaintext :", plaintext, "\n")
cat("Keyword   :", keyword, "\n")

# Enkripsi
cipher_text <- polybius_encrypt(plaintext, keyword)
cat("Cipher    :", cipher_text, "\n")

# Dekripsi
decrypted_text <- polybius_decrypt(cipher_text, keyword)
cat("Deksripsi :", decrypted_text, "\n")

# Simulasi attack
cat("\n===== SIMULASI ATTACK =====\n")
polybius_attack(cipher_text)
