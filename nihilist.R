# Membuat key square standar (alphabet tanpa 'j')
create_polybius_square <- function() {
  alphabet <- "abcdefghiklmnopqrstuvwxyz"  # i dan j digabung
  matrix(strsplit(alphabet, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)
}

# Fungsi untuk mengenkripsi teks menggunakan Polybius Cipher
polybius_encrypt <- function(plaintext) {
  square <- create_polybius_square()
  plaintext_clean <- tolower(gsub("[^a-z]", "", plaintext))  # hilangkan non-huruf
  plaintext_clean <- gsub("j", "i", plaintext_clean)  # ganti j dengan i

  ciphertext <- c()

  for (char in strsplit(plaintext_clean, "")[[1]]) {
    pos <- which(square == char, arr.ind = TRUE)
    ciphertext <- c(ciphertext, paste0(pos[1], pos[2]))  # format: baris kolom
  }

  list(original = plaintext, cleaned = plaintext_clean, encrypted = paste(ciphertext, collapse = " "))
}

# Jalankan enkripsi langsung untuk kata "daffa"
result <- polybius_encrypt("daffa")
cat("Kalimat asli     :", result$original, "\n")
cat("Hasil Cipher     :", result$encrypted, "\n")
