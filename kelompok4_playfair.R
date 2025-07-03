# ====== Fungsi untuk Membuat Matriks Kunci ======
buat_matriks_kunci <- function(kunci) {
  kunci <- tolower(gsub("[^a-z]", "", kunci))
  kunci <- gsub("j", "i", kunci)
  alfabet <- setdiff(letters, "j")
  gabung <- unique(c(strsplit(kunci, "")[[1]], alfabet))
  matriks <- matrix(gabung[1:25], nrow = 5, byrow = TRUE)
  return(matriks)
}

# ====== Fungsi untuk Mencari Posisi Karakter ======
cari_posisi <- function(matriks, huruf) {
  huruf <- ifelse(huruf == "j", "i", huruf)
  pos <- which(matriks == huruf, arr.ind = TRUE)
  return(pos)
}

# ====== Fungsi Preprocessing Plaintext ======
preproses_plaintext <- function(teks) {
  teks <- tolower(gsub("[^a-z]", "", teks))
  teks <- gsub("j", "i", teks)
  pasangan <- c()
  i <- 1
  while (i <= nchar(teks)) {
    a <- substr(teks, i, i)
    b <- if (i + 1 <= nchar(teks)) substr(teks, i + 1, i + 1) else "x"
    if (a == b) {
      pasangan <- c(pasangan, paste0(a, "x"))
      i <- i + 1
    } else {
      pasangan <- c(pasangan, paste0(a, b))
      i <- i + 2
    }
  }
  if (nchar(pasangan[length(pasangan)]) == 1) {
    pasangan[length(pasangan)] <- paste0(pasangan[length(pasangan)], "x")
  }
  return(pasangan)
}

# ====== Fungsi Enkripsi ======
playfair_enkripsi <- function(plaintext, kunci) {
  matriks <- buat_matriks_kunci(kunci)
  pasangan <- preproses_plaintext(plaintext)
  hasil <- ""
  for (pair in pasangan) {
    a <- substr(pair, 1, 1)
    b <- substr(pair, 2, 2)
    pos_a <- cari_posisi(matriks, a)
    pos_b <- cari_posisi(matriks, b)
    if (pos_a[1] == pos_b[1]) {
      hasil <- paste0(hasil, matriks[pos_a[1], (pos_a[2] %% 5) + 1], matriks[pos_b[1], (pos_b[2] %% 5) + 1])
    } else if (pos_a[2] == pos_b[2]) {
      hasil <- paste0(hasil, matriks[(pos_a[1] %% 5) + 1, pos_a[2]], matriks[(pos_b[1] %% 5) + 1, pos_b[2]])
    } else {
      hasil <- paste0(hasil, matriks[pos_a[1], pos_b[2]], matriks[pos_b[1], pos_a[2]])
    }
  }
  return(hasil)
}

# ====== Fungsi Dekripsi ======
playfair_dekripsi <- function(ciphertext, kunci) {
  matriks <- buat_matriks_kunci(kunci)
  pasangan <- strsplit(ciphertext, "")[[1]]
  pasangan <- matrix(pasangan, ncol = 2, byrow = TRUE)
  hasil <- ""
  for (i in 1:nrow(pasangan)) {
    a <- pasangan[i, 1]
    b <- pasangan[i, 2]
    pos_a <- cari_posisi(matriks, a)
    pos_b <- cari_posisi(matriks, b)
    if (pos_a[1] == pos_b[1]) {
      hasil <- paste0(hasil, matriks[pos_a[1], ((pos_a[2] - 2) %% 5) + 1], matriks[pos_b[1], ((pos_b[2] - 2) %% 5) + 1])
    } else if (pos_a[2] == pos_b[2]) {
      hasil <- paste0(hasil, matriks[((pos_a[1] - 2) %% 5) + 1, pos_a[2]], matriks[((pos_b[1] - 2) %% 5) + 1, pos_b[2]])
    } else {
      hasil <- paste0(hasil, matriks[pos_a[1], pos_b[2]], matriks[pos_b[1], pos_a[2]])
    }
  }
  return(hasil)
}

# ====== Fungsi Attack (Brute-force dari Daftar Kunci) ======
playfair_attack <- function(ciphertext, daftar_kunci) {
  hasil <- list()
  for (kunci in daftar_kunci) {
    dekripsi <- playfair_dekripsi(ciphertext, kunci)
    hasil[[kunci]] <- dekripsi
  }
  return(hasil)
}

# ====== Menu Utama ======
repeat {
  cat("\n=== Playfair Cipher ===\n")
  cat("1. Enkripsi\n")
  cat("2. Dekripsi\n")
  cat("3. Brute-force Attack (dari daftar kunci)\n")
  cat("4. Keluar\n")
  pilihan <- readline(prompt = "Pilih menu: ")

  if (pilihan == "1") {
    plaintext <- readline(prompt = "Masukkan plaintext: ")
    kunci <- readline(prompt = "Masukkan kunci: ")
    ciphertext <- playfair_enkripsi(plaintext, kunci)
    cat("Ciphertext:", ciphertext, "\n")
  } else if (pilihan == "2") {
    ciphertext <- readline(prompt = "Masukkan ciphertext: ")
    kunci <- readline(prompt = "Masukkan kunci: ")
    plaintext <- playfair_dekripsi(ciphertext, kunci)
    cat("Plaintext:", plaintext, "\n")
  } else if (pilihan == "3") {
    ciphertext <- readline(prompt = "Masukkan ciphertext: ")
    daftar_input <- readline(prompt = "Masukkan daftar kunci (pisahkan dengan koma): ")
    daftar_kunci <- strsplit(daftar_input, ",")[[1]]
    daftar_kunci <- trimws(daftar_kunci)
    hasil <- playfair_attack(ciphertext, daftar_kunci)
    cat("Hasil brute-force:\n")
    for (kunci in names(hasil)) {
      cat("Kunci:", kunci, "| Hasil:", hasil[[kunci]], "\n")
    }
  } else if (pilihan == "4") {
    cat("Keluar dari program.\n")
    break
  } else {
    cat("Pilihan tidak valid. Silakan coba lagi.\n")
  }
}
