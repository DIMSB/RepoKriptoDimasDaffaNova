# =============================
# ===== NIHILIST CIPHER =======
# =============================

# Buat Polybius Square 5x5 (menggabungkan i dan j)
create_polybius_square <- function() {
  alphabet <- c("a", "b", "c", "d", "e",
                "f", "g", "h", "i", "k",
                "l", "m", "n", "o", "p",
                "q", "r", "s", "t", "u",
                "v", "w", "x", "y", "z")
  matrix(alphabet, nrow = 5, ncol = 5, byrow = TRUE)
}

# Konversi huruf ke koordinat Polybius (2 digit)
polybius_encode <- function(text, square) {
  coords <- c()
  text <- gsub("j", "i", tolower(text))
  for (char in strsplit(text, "")[[1]]) {
    found <- which(square == char, arr.ind = TRUE)
    if (length(found) > 0) {
      row <- found[1]
      col <- found[2]
      coords <- c(coords, paste0(row, col))
    }
  }
  return(coords)
}

# Konversi koordinat Polybius (2 digit) ke huruf
polybius_decode <- function(coords, square) {
  chars <- c()
  for (pair in coords) {
    row <- as.numeric(substr(pair, 1, 1))
    col <- as.numeric(substr(pair, 2, 2))
    chars <- c(chars, square[row, col])
  }
  return(paste0(chars, collapse = ""))
}

# Enkripsi Nihilist Cipher
nihilist_encrypt <- function(plaintext, keyword) {
  square <- create_polybius_square()
  plaintext_coords <- polybius_encode(plaintext, square)
  key_coords <- polybius_encode(keyword, square)
  
  # Ubah ke bentuk angka
  cipher_nums <- as.numeric(plaintext_coords) + 
                 rep(as.numeric(key_coords), length.out = length(plaintext_coords))
  
  return(cipher_nums)
}

# Dekripsi Nihilist Cipher
nihilist_decrypt <- function(cipher_nums, keyword) {
  square <- create_polybius_square()
  key_coords <- polybius_encode(keyword, square)
  
  plaintext_coords <- cipher_nums - rep(as.numeric(key_coords), length.out = length(cipher_nums))
  plaintext_coords <- sprintf("%02d", plaintext_coords)  # pastikan dua digit

  plaintext <- polybius_decode(plaintext_coords, square)
  return(plaintext)
}

# =============================
# ========== DEMO =============
# =============================

plaintext <- "halo dunia"
keyword <- "kunci"

cat("Plaintext       :", plaintext, "\n")
cat("Keyword         :", keyword, "\n")

# Enkripsi
cipher_output <- nihilist_encrypt(plaintext, keyword)
cat("Hasil Enkripsi  :", paste(cipher_output, collapse = " "), "\n")

# Dekripsi
decrypted_text <- nihilist_decrypt(cipher_output, keyword)
cat("Hasil Dekripsi  :", decrypted_text, "\n")
