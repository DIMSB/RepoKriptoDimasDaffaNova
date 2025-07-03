# Load library
if (!require(gtools)) install.packages("gtools")
library(gtools)

# ===== Fungsi bantu urutan kolom dari kunci =====
generate_key_order <- function(key) {
  key_split <- unlist(strsplit(key, ""))
  key_df <- data.frame(index = seq_along(key_split), char = key_split, stringsAsFactors = FALSE)
  key_df <- key_df[order(key_df$char, key_df$index), ]
  return(key_df$index)
}

# ===== Fungsi Enkripsi =====
encrypt_columnar <- function(plain_text, key) {
  plain_text <- gsub(" ", "", plain_text)
  key_len <- nchar(key)
  key_order <- generate_key_order(key)
  
  chars <- unlist(strsplit(plain_text, ""))
  rows <- ceiling(length(chars) / key_len)
  padded_len <- rows * key_len
  chars <- c(chars, rep("X", padded_len - length(chars)))
  matrix_text <- matrix(chars, nrow = rows, byrow = TRUE)
  
  cipher <- ""
  for (i in key_order) {
    cipher <- paste0(cipher, paste0(matrix_text[, i], collapse = ""))
  }
  return(cipher)
}

# ===== Fungsi Dekripsi =====
decrypt_columnar <- function(cipher_text, key) {
  key_len <- nchar(key)
  key_order <- generate_key_order(key)
  rows <- ceiling(nchar(cipher_text) / key_len)
  total_len <- nchar(cipher_text)
  
  short_cols <- key_len - (total_len %% key_len)
  col_lengths <- rep(rows, key_len)
  if (short_cols != key_len) {
    col_lengths[tail(order(key_order), short_cols)] <- rows - 1
  }
  
  cols <- list()
  idx <- 1
  for (i in 1:key_len) {
    len <- col_lengths[which(key_order == i)]
    cols[[i]] <- substr(cipher_text, idx, idx + len - 1)
    idx <- idx + len
  }
  
  decrypted <- ""
  for (r in 1:rows) {
    for (i in 1:key_len) {
      col_index <- which(key_order == i)
      if (r <= nchar(cols[[col_index]])) {
        decrypted <- paste0(decrypted, substr(cols[[col_index]], r, r))
      }
    }
  }
  return(decrypted)
}

# ===== Fungsi Brute-force Attack =====
attack_columnar <- function(cipher_text, key_length, tampilkan_semua = TRUE) {
  huruf_unik <- unique(unlist(strsplit(cipher_text, "")))
  huruf_unik <- huruf_unik[huruf_unik %in% letters]
  
  if (length(huruf_unik) < key_length) {
    huruf_unik <- letters[1:(key_length + 2)]
  }
  
  keys <- permutations(n = length(huruf_unik), r = key_length, v = huruf_unik, repeats.allowed = FALSE)
  total <- nrow(keys)
  cat("Menyerang dengan", total, "kemungkinan kunci...\n")
  
  hasil_ditemukan <- FALSE
  for (i in 1:total) {
    key_try <- paste(keys[i, ], collapse = "")
    tryCatch({
      pt <- decrypt_columnar(cipher_text, key_try)
      
      if (tampilkan_semua) {
        cat("Key:", key_try, "->", pt, "\n")
      } else {
        if (grepl("the|and|this|that", pt, ignore.case = TRUE)) {
          cat("\nKemungkinan ditemukan:\n")
          cat("Key:", key_try, "\nPlaintext:", pt, "\n")
          hasil_ditemukan <- TRUE
          break
        }
      }
    }, error = function(e) {})
    
    if (i %% 100 == 0) flush.console()
  }
  
  if (!tampilkan_semua && !hasil_ditemukan) {
    cat("Tidak ada kandidat yang cocok ditemukan.\n")
  }
}

# ===== Program Utama =====
repeat {
  cat("\n=== Columnar Transposition Cipher ===\n")
  cat("1. Enkripsi\n")
  cat("2. Dekripsi\n")
  cat("3. Serangan (Brute-force)\n")
  cat("4. Keluar\n")
  pilihan <- readline("Pilih menu: ")
  
  if (pilihan == "1") {
    pesan <- readline("Masukkan plaintext: ")
    kunci <- readline("Masukkan kunci (huruf unik): ")
    if (length(unique(unlist(strsplit(kunci, "")))) != nchar(kunci)) {
      cat("Kunci harus terdiri dari huruf unik tanpa duplikat.\n")
    } else {
      hasil <- encrypt_columnar(pesan, kunci)
      cat("Hasil Enkripsi:\n", hasil, "\n")
    }
    
  } else if (pilihan == "2") {
    cipher <- readline("Masukkan ciphertext: ")
    kunci <- readline("Masukkan kunci (huruf unik): ")
    if (length(unique(unlist(strsplit(kunci, "")))) != nchar(kunci)) {
      cat("Kunci harus terdiri dari huruf unik tanpa duplikat.\n")
    } else {
      hasil <- decrypt_columnar(cipher, kunci)
      cat("Hasil Dekripsi:\n", hasil, "\n")
    }
    
  } else if (pilihan == "3") {
    cipher <- readline("Masukkan ciphertext: ")
    len <- as.integer(readline("Masukkan panjang kunci yang ingin dicoba: "))
    tampilkan <- readline("Tampilkan semua kemungkinan? (y/n): ")
    tampilkan_semua <- tolower(tampilkan) == "y"
    attack_columnar(cipher, len, tampilkan_semua)
    
  } else if (pilihan == "4") {
    cat("Keluar dari program.\n")
    break
    
  } else {
    cat("Pilihan tidak valid. Coba lagi.\n")
  }
}
