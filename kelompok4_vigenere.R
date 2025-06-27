# ============================
# ==== FUNGSI PEMBANTU ======
# ============================

# Mengubah karakter A-Z menjadi angka 0-25
char_to_num <- function(chars) {
  sapply(chars, function(char) {
    as.integer(utf8ToInt(char)) - utf8ToInt("A")  # Hitung posisi huruf dari A = 0 sampai Z = 25
  })
}

# Mengubah angka 0-25 menjadi huruf A-Z
num_to_char <- function(nums) {
  sapply(as.integer(nums), function(num) {
    intToUtf8((num %% 26) + utf8ToInt("A"))  # Pastikan tetap dalam rentang 0-25, lalu ubah ke huruf
  })
}

# Mengulangi key agar panjangnya sama dengan panjang teks (plaintext atau ciphertext)
repeat_key <- function(key, length_needed) {
  rep(strsplit(key, "")[[1]], length.out = length_needed)  # Ulangi karakter dalam key
}

# ============================
# ====== FUNGSI ENKRIPSI =====
# ============================

# Fungsi untuk mengenkripsi plaintext menggunakan Vigenère Cipher
vigenere_encrypt <- function(plaintext, key) {
  # Bersihkan input: hanya huruf, lalu ubah ke huruf kapital
  plaintext_clean <- casefold(gsub("[^A-Za-z]", "", plaintext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  # Ubah menjadi array karakter
  pt_chars <- strsplit(plaintext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(pt_chars))

  # Ubah karakter ke angka
  pt_nums <- char_to_num(pt_chars)
  key_nums <- char_to_num(key_chars)

  # Enkripsi dengan rumus: (plaintext + key) mod 26
  cipher_nums <- (pt_nums + key_nums) %% 26
  cipher_chars <- num_to_char(cipher_nums)

  paste(cipher_chars, collapse = "")  # Gabungkan hasil
}

# ============================
# ====== FUNGSI DEKRIPSI =====
# ============================

# Fungsi untuk mendekripsi ciphertext menggunakan Vigenère Cipher
vigenere_decrypt <- function(ciphertext, key) {
  # Bersihkan input: hanya huruf, lalu ubah ke huruf kapital
  ciphertext_clean <- casefold(gsub("[^A-Za-z]", "", ciphertext), upper = TRUE)
  key_clean <- casefold(gsub("[^A-Za-z]", "", key), upper = TRUE)

  # Ubah menjadi array karakter
  ct_chars <- strsplit(ciphertext_clean, "")[[1]]
  key_chars <- repeat_key(key_clean, length(ct_chars))

  # Ubah karakter ke angka
  ct_nums <- char_to_num(ct_chars)
  key_nums <- char_to_num(key_chars)

  # Dekripsi dengan rumus: (ciphertext - key + 26) mod 26
  plain_nums <- (ct_nums - key_nums + 26) %% 26
  plain_chars <- num_to_char(plain_nums)

  paste(plain_chars, collapse = "")  # Gabungkan hasil
}

# ============================
# ======== FUNGSI ATTACK =====
# ============================

# Fungsi simulasi attack (kriptanalisis belum tersedia)
vigenere_attack <- function(ciphertext) {
  cat("\nAttack pada Vigenère Cipher memerlukan teknik seperti Kasiski atau Friedman.\n")
  cat("Fitur ini belum tersedia secara otomatis dalam program.\n")
  cat("Ciphertext: ", ciphertext, "\n")
}

# ============================
# ======== PROGRAM UTAMA =====
# ============================

repeat {
  # Tampilkan menu interaktif dan ambil pilihan pengguna
  pilihan <- menu(c("Enkripsi", "Dekripsi", "Attack", "Keluar"),
                  title = "====== MENU VIGENÈRE CIPHER ======")

  if (pilihan == 1) {
    # Proses Enkripsi
    plaintext <- readline(prompt = "Masukkan plaintext: ")
    key <- readline(prompt = "Masukkan key: ")
    hasil <- vigenere_encrypt(plaintext, key)
    cat("Hasil Enkripsi: ", hasil, "\n")

  } else if (pilihan == 2) {
    # Proses Dekripsi
    ciphertext <- readline(prompt = "Masukkan ciphertext: ")
    key <- readline(prompt = "Masukkan key: ")
    hasil <- vigenere_decrypt(ciphertext, key)
    cat("Hasil Dekripsi: ", hasil, "\n")

  } else if (pilihan == 3) {
    # Attack (informasi saja)
    ciphertext <- readline(prompt = "Masukkan ciphertext yang akan dianalisis: ")
    vigenere_attack(ciphertext)

  } else if (pilihan == 4) {
    # Keluar dari program
    cat("Program selesai.\n")
    break

  } else {
    # Jika input tidak valid
    cat("Pilihan tidak valid. Coba lagi.\n")
  }
}
