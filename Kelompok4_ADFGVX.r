# install.packages("combinat") # Mungkin perlu dijalankan sekali jika belum terpasang
library(combinat)

# === Fungsi Helper: Membuat Polybius Square 6x6 ===
# Square ini berisi A-Z dan 0-9, dikacaukan berdasarkan sebuah kunci.
generate_polybius_square <- function(key) {
  # Set karakter dasar (26 huruf + 10 angka)
  chars_set <- c(LETTERS, 0:9)
  
  # Buat urutan unik dari karakter kunci + sisa karakter
  key_chars <- unique(strsplit(toupper(key), "")[[1]])
  unique_chars <- unique(c(key_chars, chars_set))
  
  # Buat matriks 6x6
  square <- matrix(unique_chars, nrow = 6, ncol = 6, byrow = TRUE)
  rownames(square) <- c('A', 'D', 'F', 'G', 'V', 'X')
  colnames(square) <- c('A', 'D', 'F', 'G', 'V', 'X')
  return(square)
}

# === Fungsi Enkripsi ADFGVX ===
encrypt_adfgvx <- function(plaintext, sub_key, trans_key) {
  # 1. TAHAP SUBSTITUSI
  square <- generate_polybius_square(sub_key)
  plaintext_clean <- toupper(gsub("[^A-Za-z0-9]", "", plaintext))
  
  substituted_text <- ""
  for (char in strsplit(plaintext_clean, "")[[1]]) {
    # Temukan koordinat karakter di dalam square
    coords <- which(square == char, arr.ind = TRUE)
    if (length(coords) > 0) {
      row_name <- rownames(square)[coords[1]]
      col_name <- colnames(square)[coords[2]]
      substituted_text <- paste0(substituted_text, row_name, col_name)
    }
  }
  
  # 2. TAHAP TRANSPOSISI (Columnar Transposition)
  key_order <- order(strsplit(toupper(trans_key), "")[[1]])
  ncol <- nchar(trans_key)
  chars <- strsplit(substituted_text, "")[[1]]
  
  # Jika teks substitusi tidak pas di grid, tambahkan 'X' (karakter netral)
  if (length(chars) %% ncol != 0) {
      padding_needed <- ncol - (length(chars) %% ncol)
      chars <- c(chars, rep("X", padding_needed))
  }
  
  nrow <- length(chars) / ncol
  matrix_text <- matrix(chars, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  ciphertext <- ""
  for (i in key_order) {
    ciphertext <- paste0(ciphertext, paste0(matrix_text[, i], collapse = ""))
  }
  return(ciphertext)
}

# === Fungsi Dekripsi ADFGVX ===
decrypt_adfgvx <- function(ciphertext, sub_key, trans_key) {
  # 1. TAHAP REVERSE TRANSPOSISI
  key_order <- order(strsplit(toupper(trans_key), "")[[1]])
  inv_key_order <- order(key_order) # Urutan untuk mengembalikan kolom
  
  ncol <- nchar(trans_key)
  nchar_cipher <- nchar(ciphertext)
  
  # Hitung panjang setiap kolom
  base_len <- floor(nchar_cipher / ncol)
  rem <- nchar_cipher %% ncol
  col_lengths <- rep(base_len, ncol)
  if (rem > 0) {
    # Kolom yang lebih panjang sesuai dengan urutan kunci transposisi
    long_cols <- key_order[1:rem]
    col_lengths[long_cols] <- col_lengths[long_cols] + 1
  }

  # Buat matriks kosong untuk diisi kembali
  nrow_max <- ceiling(nchar_cipher / ncol)
  matrix_text <- matrix("", nrow = nrow_max, ncol = ncol)
  
  k <- 1
  for (i in key_order) {
    len <- col_lengths[i]
    if (len > 0) {
        col_data <- substr(ciphertext, k, k + len - 1)
        matrix_text[1:len, i] <- strsplit(col_data, "")[[1]]
        k <- k + len
    }
  }

  # Baca matriks secara horizontal untuk mendapatkan teks substitusi
  substituted_text <- paste0(t(matrix_text), collapse = "")
  # Hapus padding 'X' yang mungkin ditambahkan di akhir
  substituted_text <- gsub("X*$", "", substituted_text)


  # 2. TAHAP REVERSE SUBSTITUSI
  square <- generate_polybius_square(sub_key)
  plaintext <- ""
  i <- 1
  while (i <= nchar(substituted_text)) {
    row_char <- substr(substituted_text, i, i)
    col_char <- substr(substituted_text, i + 1, i + 1)
    
    if (row_char %in% rownames(square) && col_char %in% colnames(square)) {
      original_char <- square[row_char, col_char]
      plaintext <- paste0(plaintext, original_char)
    }
    i <- i + 2
  }
  return(plaintext)
}

# === Fungsi Attack ADFGVX (Dictionary Attack Sederhana) ===
# Brute-force murni tidak praktis, jadi kita gunakan daftar kata (dictionary)
attack_adfgvx <- function(ciphertext) {
  cat("\n=== ADFGVX Cipher Attack (Dictionary) ===\n")
  # Daftar kata sederhana sebagai kunci potensial
  wordlist <- c("KUNCI", "RAHASIA", "JERMAN", "PERANG", "CIPHER", "AMAN", "TUGAS")
  count <- 0
  
  for (k1 in wordlist) {
    for (k2 in wordlist) {
      # Hindari kunci yang sama persis
      if (k1 == k2) next
      
      guess <- decrypt_adfgvx(ciphertext, k1, k2)
      # Tampilkan tebakan jika hasilnya hanya berisi karakter alfanumerik
      if (grepl("^[A-Z0-9]+$", guess) && nchar(guess) > 2) {
        cat("Sub Key:", k1, "| Trans Key:", k2, "| Tebakan Plaintext:", guess, "\n")
        count <- count + 1
        if (count %% 5 == 0) {
          lanjut <- readline("Tampilkan 5 hasil lagi? (y/n): ")
          if (tolower(lanjut) != "y") return()
        }
      }
    }
  }
  cat("Pencarian selesai.\n")
}

# === PROGRAM UTAMA DENGAN MENU INTERAKTIF ===
repeat {
  cat("\n====== MENU ALGORITMA ADFGVX CIPHER ======\n",
      "1. Enkripsi\n",
      "2. Dekripsi\n",
      "3. Attack (Dictionary sederhana)\n",
      "4. Keluar\n", sep = "")
  mode <- readline("Pilih mode (1/2/3/4): ")

  if (mode == "1") {
    plaintext <- readline("Masukkan plaintext: ")
    sub_key <- readline("Masukkan substitution key: ")
    trans_key <- readline("Masukkan transposition key: ")
    hasil <- encrypt_adfgvx(plaintext, sub_key, trans_key)
    cat("\n=== Hasil Enkripsi ===\n")
    cat("Plaintext          :", plaintext, "\n")
    cat("Substitution Key   :", sub_key, "\n")
    cat("Transposition Key  :", trans_key, "\n")
    cat("Ciphertext         :", hasil, "\n")

  } else if (mode == "2") {
    cipher_text <- readline("Masukkan ciphertext: ")
    sub_key <- readline("Masukkan substitution key: ")
    trans_key <- readline("Masukkan transposition key: ")
    hasil <- decrypt_adfgvx(cipher_text, sub_key, trans_key)
    cat("\n=== Hasil Dekripsi ===\n")
    cat("Ciphertext         :", cipher_text, "\n")
    cat("Substitution Key   :", sub_key, "\n")
    cat("Transposition Key  :", trans_key, "\n")
    cat("Plaintext          :", hasil, "\n")

  } else if (mode == "3") {
    cipher_text <- readline("Masukkan ciphertext untuk di-attack: ")
    attack_adfgvx(cipher_text)

  } else if (mode == "4") {
    cat("Program selesai.\n")
    break

  } else {
    cat("Pilihan tidak valid. Silakan pilih 1, 2, 3, atau 4.\n")
  }
}

