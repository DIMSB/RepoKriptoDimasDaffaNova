# === PAKET YANG DIGUNAKAN ===
library(httr)
library(openssl)
library(base64enc)
library(digest)

# === KONFIGURASI ===
mode <- "auto" # bisa: "enkripsi", "dekripsi", atau "auto"

file_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/dummy.txt"
enc_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/last_encrypted.txt"
hash_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/last_hash.txt"
log_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/log.txt"
restore_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/recovered_dummy.txt"

telegram_token <- "7435310919:AAG4YW44IIq3UeGJGdtoeK1LUeGzJZ9BQyI"
chat_id <- "1192588526"
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# === KUNCI AES (HARUS 16 BYTE) ===
kunci_aes <- charToRaw("1234567890ABCDEF")

# === Fungsi Telegram ===
kirim_telegram <- function(pesan) {
  url <- paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage")
  POST(url, body = list(chat_id = chat_id, text = pesan))
}

# === Fungsi Enkripsi dan Dekripsi AES-GCM ===
encrypt_file <- function(teks, key_raw) {
  iv <- rand_bytes(12)
  cipher <- aes_gcm_encrypt(charToRaw(teks), key = key_raw, iv = iv)
  c(iv, cipher)
}

decrypt_file <- function(full_raw, key_raw) {
  if (length(full_raw) <= 12) stop("Ciphertext tidak valid.")
  iv <- full_raw[1:12]
  cipher <- full_raw[-(1:12)]
  aes_gcm_decrypt(cipher, key = key_raw, iv = iv)
}

# === MODE OTOMATIS ===
if (mode == "auto") {
  mode <- if (!file.exists(restore_path)) "dekripsi" else "enkripsi"
  message("🔁 Mode otomatis memilih: ", toupper(mode))
}

# === MODE ENKRIPSI ===
if (mode == "enkripsi") {
  tryCatch(
    {
      if (!file.exists(file_path)) stop("❌ File dummy.txt tidak ditemukan.")

      isi_file <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      isi_hash <- digest(isi_file, algo = "sha256")

      perubahan_terjadi <- TRUE
      if (file.exists(hash_path)) {
        hash_lama <- readLines(hash_path, warn = FALSE)
        if (identical(isi_hash, hash_lama)) perubahan_terjadi <- FALSE
      }

      if (perubahan_terjadi) {
        isi_encrypted_raw <- encrypt_file(isi_file, kunci_aes)
        isi_encrypted_b64 <- base64_encode(isi_encrypted_raw)

        writeLines(isi_hash, hash_path) # simpan hash
        writeLines(isi_encrypted_b64, enc_path, useBytes = TRUE)

        isi_decrypted_raw <- decrypt_file(base64_decode(isi_encrypted_b64), kunci_aes)
        isi_asli <- paste(rawToChar(isi_decrypted_raw, multiple = TRUE), collapse = "")
        writeLines(isi_asli, restore_path, useBytes = TRUE)

        write(paste(timestamp, "- PERUBAHAN TERDETEKSI & DIPULIHKAN"), file = log_path, append = TRUE)
        kirim_telegram(paste0("⚠️ PERINGATAN\nFile berubah dan dipulihkan ke: ", basename(restore_path), " [", timestamp, "]"))
        message("✅ Perubahan terdeteksi dan dipulihkan.")
      } else {
        write(paste(timestamp, "- Tidak ada perubahan"), file = log_path, append = TRUE)
        kirim_telegram(paste0("✅ Tidak ada perubahan pada: ", timestamp))
        message("🔄 Tidak ada perubahan pada isi file.")
      }
    },
    error = function(e) {
      message("❌ ERROR (Enkripsi): ", e$message)
    }
  )
}

# === MODE DEKRIPSI ===
if (mode == "dekripsi") {
  tryCatch(
    {
      if (!file.exists(enc_path)) stop("❌ File terenkripsi tidak ditemukan.")

      isi_encrypted_b64 <- readLines(enc_path, warn = FALSE)
      if (length(isi_encrypted_b64) == 0) stop("⚠️ File terenkripsi kosong.")

      isi_encrypted_raw <- base64_decode(paste(isi_encrypted_b64, collapse = ""))
      isi_decrypted_raw <- decrypt_file(isi_encrypted_raw, kunci_aes)

      isi_asli <- paste(rawToChar(isi_decrypted_raw, multiple = TRUE), collapse = "")
      writeLines(isi_asli, restore_path, useBytes = TRUE)

      message("✅ File berhasil didekripsi ke: ", restore_path)
      kirim_telegram(paste0("🔓 File dipulihkan ke: ", basename(restore_path), " [", timestamp, "]"))
    },
    error = function(e) {
      message("❌ ERROR (Dekripsi): ", e$message)
    }
  )
}
