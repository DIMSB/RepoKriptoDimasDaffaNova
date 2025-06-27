# === Generate Key Square 5x5 (menggabungkan I dan J) ===
generate_key_square <- function(keyword) {
  alphabet <- "abcdefghiklmnopqrstuvwxyz"  # Alfabet tanpa huruf J
  keyword <- tolower(gsub("[^a-z]", "", keyword))  # Hanya huruf kecil
  keyword <- gsub("j", "i", keyword)  # Gabungkan J ke I
  keyword_unique <- paste(unique(strsplit(keyword, "")[[1]]), collapse = "")  # Ambil huruf unik dari keyword
  remaining_letters <- gsub(paste0("[", keyword_unique, "]"), "", alphabet)  # Huruf sisa dari alfabet
  full_key <- paste0(keyword_unique, remaining_letters)  # Gabungkan keyword dan huruf sisa
  matrix(strsplit(full_key, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)  # Buat matriks 5x5
}

# === Temukan posisi huruf dalam key square ===
find_position <- function(letter, key_square) {
  if (letter == "j") letter <- "i"  # Samakan J menjadi I
  pos <- which(key_square == letter, arr.ind = TRUE)  # Cari posisi huruf dalam matriks
  return(c(pos[1], pos[2]))  # Kembalikan baris dan kolom
}

# === Fungsi Enkripsi Polybius ===
polybius_encrypt <- function(plaintext, keyword) {
  plaintext_clean <- tolower(gsub("[^a-z]", "", plaintext))  # Hilangkan selain huruf, ubah ke huruf kecil
  plaintext_clean <- gsub("j", "i", plaintext_clean)  # Gabungkan J ke I
  key_square <- generate_key_square(keyword)  # Buat key square dari keyword

  cipher <- c()  # Inisialisasi hasil cipher
  for (char in strsplit(plaintext_clean, "")[[1]]) {  # Loop tiap huruf
    pos <- find_position(char, key_square)  # Cari posisi di key square
    cipher <- c(cipher, paste0(pos[1], pos[2]))  # Gabungkan baris dan kolom jadi angka 2 digit
  }

  return(paste(cipher, collapse = ""))  # Gabungkan semua hasil jadi string
}

# === Fungsi Dekripsi Polybius ===
polybius_decrypt <- function(cipher_text, keyword) {
  key_square <- generate_key_square(keyword)  # Buat key square dari keyword
  cipher_pairs <- unlist(strsplit(cipher_text, ""))  # Pecah cipher jadi karakter per digit

  if (length(cipher_pairs) %% 2 != 0) {  # Pastikan jumlah digit genap
    stop("Panjang cipher tidak valid, harus genap.")
  }

  plaintext <- c()  # Inisialisasi hasil dekripsi
  for (i in seq(1, length(cipher_pairs), by = 2)) {  # Loop per dua digit
    row <- as.numeric(cipher_pairs[i])  # Ambil baris
    col <- as.numeric(cipher_pairs[i + 1])  # Ambil kolom
    letter <- key_square[row, col]  # Ambil huruf dari posisi tersebut
    plaintext <- c(plaintext, letter)  # Tambahkan ke hasil
  }

  return(paste(plaintext, collapse = ""))  # Gabungkan huruf jadi string
}

# === Fungsi Attack Manual (menggunakan key square default) ===
polybius_attack <- function(cipher_text) {
  cat("=== Polybius Manual Attack ===\n")
  cat("Cipher Text:", cipher_text, "\n")
  key_square <- matrix(strsplit("abcdefghiklmnopqrstuvwxyz", "")[[1]], nrow = 5, byrow = TRUE)  # Gunakan default square
  print(key_square)  # Tampilkan matriks

  cipher_pairs <- unlist(strsplit(cipher_text, ""))  # Pecah cipher jadi array digit

  if (length(cipher_pairs) %% 2 != 0) {
    stop("Panjang cipher tidak valid, harus genap.")
  }

  plaintext <- c()  # Inisialisasi hasil
  for (i in seq(1, length(cipher_pairs), by = 2)) {  # Loop tiap dua digit
    row <- as.numeric(cipher_pairs[i])
    col <- as.numeric(cipher_pairs[i + 1])
    plaintext <- c(plaintext, key_square[row, col])  # Ambil huruf dari posisi tersebut
  }

  cat("Tebakan hasil attack:", paste(plaintext, collapse = ""), "\n")  # Tampilkan hasil tebakannya
}

# === PROGRAM UTAMA DENGAN MENU INTERAKTIF ===
repeat {
  # Tampilkan menu utama
  cat("\n====== MENU (ALGORITMA) POLYBIUS CIPHER ======\n",
      "1. Enkripsi\n",
      "2. Dekripsi\n",
      "3. Attack (Analisis manual)\n",
      "4. Keluar\n", sep = "")
  mode <- readline("Pilih mode (1/2/3/4): ")  # Ambil pilihan user

  if (mode == "1") {
    plaintext <- readline("Masukkan plaintext: ")  # Input plaintext
    keyword <- readline("Masukkan keyword: ")  # Input keyword
    hasil <- polybius_encrypt(plaintext, keyword)  # Enkripsi teks
    cat("\nHasil Enkripsi:\n")
    cat("Plaintext :", plaintext, "\n")
    cat("Keyword   :", keyword, "\n")
    cat("Cipher    :", hasil, "\n")

  } else if (mode == "2") {
    cipher_text <- readline("Masukkan cipher (tanpa spasi, contoh: 3211242411): ")  # Input cipher
    keyword <- readline("Masukkan keyword: ")  # Input keyword
    hasil <- polybius_decrypt(cipher_text, keyword)  # Dekripsi cipher
    cat("\nHasil Dekripsi:\n")
    cat("Cipher    :", cipher_text, "\n")
    cat("Keyword   :", keyword, "\n")
    cat("Plaintext :", hasil, "\n")

  } else if (mode == "3") {
    cipher_text <- readline("Masukkan cipher (tanpa spasi): ")  # Input cipher untuk analisis
    polybius_attack(cipher_text)  # Jalankan attack manual

  } else if (mode == "4") {
    cat("Program selesai.\n")  # Keluar dari program
    break

  } else {
    cat("Pilihan tidak valid. Silakan pilih 1, 2, 3, atau 4.\n")  # Validasi input
  }
}
