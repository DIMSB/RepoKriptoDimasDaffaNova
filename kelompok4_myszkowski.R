# =============================
# Myszkowski Transposition Cipher - R
# =============================

# Fungsi bantu: mapping karakter kunci ke peringkat (mengizinkan duplikat)
generate_key_rank <- function(key) {
  key_chars <- unlist(strsplit(key, ""))
  sorted_chars <- sort(unique(key_chars))
  rank <- sapply(key_chars, function(k) which(sorted_chars == k))
  return(rank)
}

# =============================
# Fungsi Enkripsi
# =============================
myszkowski_encrypt <- function(plaintext, key) {
  plaintext <- gsub(" ", "", plaintext)
  key_rank <- generate_key_rank(key)
  key_len <- length(key_rank)

  chars <- unlist(strsplit(plaintext, ""))
  rows <- ceiling(length(chars) / key_len)
  total_len <- rows * key_len
  chars <- c(chars, rep("X", total_len - length(chars)))
  matrix_text <- matrix(chars, nrow = rows, byrow = TRUE)

  ciphertext <- ""
  for (i in sort(unique(key_rank))) {
    cols <- which(key_rank == i)
    if (length(cols) == 1) {
      ciphertext <- paste0(ciphertext, paste(matrix_text[, cols], collapse = ""))
    } else {
      for (r in 1:rows) {
        for (c in cols) {
          ciphertext <- paste0(ciphertext, matrix_text[r, c])
        }
      }
    }
  }
  return(ciphertext)
}

# =============================
# Fungsi Dekripsi
# =============================
myszkowski_decrypt <- function(ciphertext, key) {
  key_rank <- generate_key_rank(key)
  key_len <- length(key_rank)
  total_len <- nchar(ciphertext)
  rows <- ceiling(total_len / key_len)
  matrix_text <- matrix("", nrow = rows, ncol = key_len)

  idx <- 1
  for (i in sort(unique(key_rank))) {
    cols <- which(key_rank == i)
    if (length(cols) == 1) {
      for (r in 1:rows) {
        matrix_text[r, cols] <- substr(ciphertext, idx, idx)
        idx <- idx + 1
      }
    } else {
      for (r in 1:rows) {
        for (c in cols) {
          matrix_text[r, c] <- substr(ciphertext, idx, idx)
          idx <- idx + 1
        }
      }
    }
  }

  plaintext <- paste0(as.vector(t(matrix_text)), collapse = "")
  return(plaintext)
}

# =============================
# Fungsi Brute-force Attack
# =============================
myszkowski_attack <- function(ciphertext, key_length, max_output = 10) {
  if (!requireNamespace("gtools", quietly = TRUE)) {
    install.packages("gtools")
  }
  library(gtools)

  cat("Melakukan brute-force terhadap kunci sepanjang", key_length, "...\n")

  huruf <- letters[1:6]  # dibatasi agar kombinasi tidak terlalu banyak
  perms <- permutations(n = length(huruf), r = key_length, v = huruf, repeats.allowed = TRUE)

  hasil_ditemukan <- 0

  for (i in 1:nrow(perms)) {
    key_try <- paste(perms[i, ], collapse = "")
    plain_try <- tryCatch(
      {
        myszkowski_decrypt(ciphertext, key_try)
      },
      error = function(e) { "" }
    )

    cat(sprintf("Coba %d - Key: %s → %s\n", i, key_try, plain_try))

    if (grepl("[a-zA-Z]{4,}", plain_try)) {
      hasil_ditemukan <- hasil_ditemukan + 1
      cat("\n=== Kemungkinan ditemukan ===\n")
      cat("Kunci      :", key_try, "\n")
      cat("Plaintext  :", plain_try, "\n\n")
    }

    if (hasil_ditemukan >= max_output) {
      cat("Batas hasil tercapai.\n")
      break
    }
  }

  if (hasil_ditemukan == 0) {
    cat("Tidak ada hasil cocok ditemukan.\n")
  }
}

# =============================
# MENU UTAMA
# =============================
repeat {
  cat("\n=== Myszkowski Transposition Cipher ===\n")
  cat("1. Enkripsi\n")
  cat("2. Dekripsi\n")
  cat("3. Brute-force Attack (dengan panjang kunci)\n")
  cat("4. Keluar\n")
  pilihan <- as.integer(readline("Pilih menu: "))

  if (pilihan == 1) {
    pt <- readline("Masukkan plaintext: ")
    key <- readline("Masukkan kunci (huruf saja, bisa ada duplikat): ")
    ct <- myszkowski_encrypt(pt, key)
    cat("\nCiphertext:\n", ct, "\n")

  } else if (pilihan == 2) {
    ct <- readline("Masukkan ciphertext: ")
    key <- readline("Masukkan kunci yang digunakan: ")
    pt <- myszkowski_decrypt(ct, key)
    cat("\nHasil dekripsi:\n", pt, "\n")

  } else if (pilihan == 3) {
    ct <- readline("Masukkan ciphertext: ")
    kl <- as.integer(readline("Masukkan panjang kunci yang ingin dicoba: "))
    myszkowski_attack(ct, kl)

  } else if (pilihan == 4) {
    cat("Program selesai.\n")
    break

  } else {
    cat("Pilihan tidak valid. Coba lagi.\n")
  }
}
