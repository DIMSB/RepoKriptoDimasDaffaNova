# ============================
# ===== BIFID CIPHER R  =====
# ============================

# Buat Polybius Square 5x5 tanpa huruf J (I dan J digabung)
generate_polybius_square <- function() {
  huruf <- c("A", "B", "C", "D", "E",
             "F", "G", "H", "I", "K",
             "L", "M", "N", "O", "P",
             "Q", "R", "S", "T", "U",
             "V", "W", "X", "Y", "Z")
  matrix(huruf, nrow = 5, byrow = TRUE)
}

# Fungsi cari posisi huruf dalam tabel
get_positions <- function(char, square) {
  loc <- which(square == char, arr.ind = TRUE)
  c(row = loc[1], col = loc[2])
}

# Enkripsi Bifid Cipher
bifid_encrypt <- function(plaintext) {
  square <- generate_polybius_square()
  plaintext <- toupper(gsub("J", "I", plaintext))  # Ganti J dengan I
  plaintext <- gsub("[^A-Z]", "", plaintext)

  rows <- c()
  cols <- c()

  for (char in strsplit(plaintext, "")[[1]]) {
    pos <- get_positions(char, square)
    rows <- c(rows, pos["row"])
    cols <- c(cols, pos["col"])
  }

  combined <- c(rows, cols)
  split_pairs <- matrix(combined, nrow = 2)

  encrypted <- ""
  for (i in 1:ncol(split_pairs)) {
    row <- split_pairs[1, i]
    col <- split_pairs[2, i]
    encrypted <- paste0(encrypted, square[row, col])
  }

  return(encrypted)
}

# Dekripsi Bifid Cipher
bifid_decrypt <- function(ciphertext) {
  square <- generate_polybius_square()
  ciphertext <- toupper(gsub("J", "I", ciphertext))  # Ganti J dengan I
  ciphertext <- gsub("[^A-Z]", "", ciphertext)

  coords <- c()
  for (char in strsplit(ciphertext, "")[[1]]) {
    pos <- get_positions(char, square)
    coords <- c(coords, pos["row"], pos["col"])
  }

  half <- length(coords) / 2
  rows <- coords[1:half]
  cols <- coords[(half + 1):length(coords)]

  decrypted <- ""
  for (i in 1:length(rows)) {
    decrypted <- paste0(decrypted, square[rows[i], cols[i]])
  }

  return(decrypted)
}

# ============================
# ==== CONTOH PENGGUNAAN =====
# ============================

plaintext <- "HELLO"
encrypted <- bifid_encrypt(plaintext)
decrypted <- bifid_decrypt(encrypted)

cat("Plaintext   :", plaintext, "\n")
cat("Encrypted   :", encrypted, "\n")
cat("Decrypted   :", decrypted, "\n")
