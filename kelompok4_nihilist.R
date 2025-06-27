# ============================
# ===== NIHILIST CIPHER =====
# ============================

# Fungsi membuat Polybius Square standar (tanpa huruf 'j')
create_polybius_square <- function() {
  alphabet <- "abcdefghiklmnopqrstuvwxyz"                  # Alfabet tanpa 'j'
  matrix(strsplit(alphabet, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)  # Matriks 5x5
}

# Fungsi mengubah teks menjadi angka berdasarkan posisi di Polybius Square
text_to_numeric <- function(text, square) {
  text <- tolower(gsub("[^a-z]", "", text))  # Hapus karakter selain huruf
  text <- gsub("j", "i", text)               # Gabungkan 'j' ke 'i'
  numeric <- c()
  for (char in strsplit(text, "")[[1]]) {
    pos <- which(square == char, arr.ind = TRUE)          # Temukan posisi baris dan kolom
    numeric <- c(numeric, as.integer(paste0(pos[1], pos[2])))  # Gabung jadi angka 2 digit
  }
  return(numeric)
}

# Fungsi mengubah angka kembali ke huruf (berdasarkan posisi di Polybius Square)
numeric_to_text <- function(numbers, square) {
  chars <- c()
  for (num in numbers) {
    num <- as.integer(num)        # Konversi ke integer
    row <- num %/% 10             # Angka pertama sebagai baris
    col <- num %% 10              # Angka kedua sebagai kolom
    chars <- c(chars, square[row, col])  # Ambil huruf dari koordinat tersebut
  }
  return(paste0(chars, collapse = ""))    # Gabungkan jadi string
}

# Fungsi enkripsi Nihilist Cipher
nihilist_encrypt <- function(plaintext, keytext) {
  square <- create_polybius_square()                    # Buat Polybius Square
  pt_nums <- text_to_numeric(plaintext, square)         # Konversi plaintext ke angka
  key_nums <- text_to_numeric(keytext, square)          # Konversi keytext ke angka
  key_nums <- rep(key_nums, length.out = length(pt_nums))  # Ulangi key agar sepanjang plaintext
  cipher_nums <- pt_nums + key_nums                     # Penjumlahan angka-angka
  return(paste(cipher_nums, collapse = " "))            # Gabungkan hasil menjadi string angka
}

# Fungsi dekripsi Nihilist Cipher
nihilist_decrypt <- function(ciphertext, keytext) {
  square <- create_polybius_square()                    # Buat Polybius Square
  cipher_nums <- as.integer(strsplit(ciphertext, " ")[[1]])  # Pecah ciphertext ke angka
  key_nums <- text_to_numeric(keytext, square)               # Konversi keytext ke angka
  key_nums <- rep(key_nums, length.out = length(cipher_nums)) # Ulangi key
  pt_nums <- cipher_nums - key_nums                         # Kurangi untuk mendapatkan plaintext
  return(numeric_to_text(pt_nums, square))                  # Konversi kembali ke huruf
}

# Fungsi attack (sementara hanya placeholder)
nihilist_attack <- function(ciphertext) {
  cat("Attack belum diimplementasikan.\n")
}

# ============================
# ========= MENU UTAMA =======
# ============================

repeat {
  # Tampilkan menu pilihan
  cat("\n====== MENU (ALGORITMA) NIHILIST CIPHER ======\n",
      "1. Enkripsi\n",
      "2. Dekripsi\n",
      "3. Attack\n",
      "4. Keluar\n", sep = "")

  # Ambil pilihan dari user
  choice <- readline("Pilih menu (1/2/3/4): ")

  if (choice == "1") {
    # Mode enkripsi
    pt <- readline("Masukkan plaintext: ")            # Input plaintext
    key <- readline("Masukkan keytext: ")             # Input keytext
    encrypted <- nihilist_encrypt(pt, key)            # Enkripsi
    cat("Hasil Enkripsi:", encrypted, "\n")

  } else if (choice == "2") {
    # Mode dekripsi
    ct <- readline("Masukkan ciphertext (misal: 45 54 62): ")  # Input ciphertext berupa angka
    key <- readline("Masukkan keytext: ")                      # Input keytext
    decrypted <- nihilist_decrypt(ct, key)                     # Dekripsi
    cat("Hasil Dekripsi:", decrypted, "\n")

  } else if (choice == "3") {
    # Mode attack (placeholder)
    ct <- readline("Masukkan ciphertext untuk diserang: ")
    nihilist_attack(ct)

  } else if (choice == "4") {
    # Keluar program
    cat("Keluar dari program.\n")
    break

  } else {
    # Jika input tidak valid
    cat("Pilihan tidak valid. Silakan pilih 1-4.\n")
  }
}
    