# ============================
# ===== CAESAR CIPHER =======
# ============================

# Fungsi untuk mengenkripsi plaintext menggunakan Caesar Cipher
caesar_encrypt <- function(plaintext, shift = 3) {
  alphabet <- letters            # Daftar huruf alfabet a-z
  result <- ""                   # Variabel untuk menyimpan hasil enkripsi

  for (i in 1:nchar(plaintext)) {               # Perulangan untuk setiap karakter dalam plaintext
    char <- substr(plaintext, i, i)             # Ambil karakter ke-i dari plaintext
    if (char %in% alphabet) {                   # Jika karakter adalah huruf alfabet
      pos <- match(char, alphabet)              # Cari posisi karakter dalam alfabet (1–26)
      new_pos <- (pos + shift - 1) %% 26 + 1    # Geser posisi huruf dengan rumus Caesar Cipher
      result <- paste0(result, alphabet[new_pos]) # Tambahkan huruf terenkripsi ke hasil
    } else {
      result <- paste0(result, char)            # Jika bukan huruf (misal spasi), langsung tambahkan
    }
  }

  return(result)    # Kembalikan hasil enkripsi
}

# Fungsi untuk mendekripsi ciphertext menggunakan Caesar Cipher
caesar_decrypt <- function(ciphertext, shift = 3) {
  alphabet <- letters            # Daftar huruf alfabet a-z
  result <- ""                   # Variabel untuk menyimpan hasil dekripsi

  for (i in 1:nchar(ciphertext)) {              # Perulangan untuk setiap karakter dalam ciphertext
    char <- substr(ciphertext, i, i)            # Ambil karakter ke-i dari ciphertext
    if (char %in% alphabet) {                   # Jika karakter adalah huruf alfabet
      pos <- match(char, alphabet)              # Cari posisi karakter dalam alfabet (1–26)
      new_pos <- (pos - shift - 1) %% 26 + 1    # Geser posisi huruf ke kiri (kebalikan enkripsi)
      result <- paste0(result, alphabet[new_pos]) # Tambahkan huruf terdekripsi ke hasil
    } else {
      result <- paste0(result, char)            # Jika bukan huruf, langsung tambahkan
    }
  }

  return(result)    # Kembalikan hasil dekripsi
}

# Fungsi untuk melakukan brute-force attack terhadap Caesar Cipher
caesar_attack <- function(ciphertext) {
  cat("Percobaan semua kemungkinan shift Caesar Cipher:\n\n")  # Tampilkan header
  for (s in 1:25) {                             # Coba semua kemungkinan shift dari 1 sampai 25
    decrypted <- caesar_decrypt(ciphertext, shift = s)  # Dekripsi dengan shift s
    cat(sprintf("Shift %2d: %s\n", s, decrypted))        # Tampilkan hasil dekripsi
  }
}

# ============================
# ========= MENU UTAMA ======
# ============================

# Loop utama program
repeat {
  # Tampilkan menu
  cat("\n====== MENU CAESAR CIPHER ======\n",
      "1. Enkripsi\n",
      "2. Dekripsi\n",
      "3. Attack\n",
      "4. Keluar\n", sep = "")

  # Input pilihan menu dari pengguna
  pilihan <- as.integer(readline(prompt = "Pilih opsi (1-4): "))

  # Jika memilih Enkripsi
  if (pilihan == 1) {
    plaintext <- tolower(readline(prompt = "Masukkan plaintext: "))     # Input plaintext dan ubah ke huruf kecil
    shift <- as.integer(readline(prompt = "Masukkan shift (default 3): ")) # Input shift, default 3 jika kosong
    if (is.na(shift)) shift <- 3
    ciphertext <- caesar_encrypt(plaintext, shift)     # Panggil fungsi enkripsi
    cat("Hasil Enkripsi :", ciphertext, "\n")          # Tampilkan hasil enkripsi

  # Jika memilih Dekripsi
  } else if (pilihan == 2) {
    ciphertext <- tolower(readline(prompt = "Masukkan ciphertext: "))   # Input ciphertext dan ubah ke huruf kecil
    shift <- as.integer(readline(prompt = "Masukkan shift (default 3): ")) # Input shift, default 3 jika kosong
    if (is.na(shift)) shift <- 3
    plaintext <- caesar_decrypt(ciphertext, shift)     # Panggil fungsi dekripsi
    cat("Hasil Dekripsi :", plaintext, "\n")           # Tampilkan hasil dekripsi

  # Jika memilih Attack (brute-force)
  } else if (pilihan == 3) {
    ciphertext <- tolower(readline(prompt = "Masukkan ciphertext yang akan di-attack: ")) # Input ciphertext
    caesar_attack(ciphertext)                  # Panggil fungsi brute-force attack

  # Jika memilih Keluar
  } else if (pilihan == 4) {
    cat("Program selesai.\n")                  # Tampilkan pesan keluar
    break                                      # Keluar dari repeat loop

  # Jika input salah
  } else {
    cat("Pilihan tidak valid. Silakan coba lagi.\n")  # Tampilkan pesan kesalahan
  }
}
