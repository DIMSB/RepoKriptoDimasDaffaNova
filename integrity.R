# === PAKET YANG DIGUNAKAN ===
library(httr)
library(openssl)
library(base64enc)
library(digest)
library(jsonlite)

# === KONFIGURASI ===
mode <- "auto"  # bisa: "enkripsi", "dekripsi", atau "auto"

file_path     <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/dummy.json"
enc_path      <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/last_encrypted.txt"
hash_path     <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/last_hash.txt"
log_path      <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/log.txt"
restore_path  <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChekcker/recovered_dummy.json"

telegram_token <- "7435310919:AAG4YW44IIq3UeGJGdtoeK1LUeGzJZ9BQyI"
chat_id <- "-4973331538"
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# === KUNCI AES (HARUS 16 BYTE) ===
kunci_aes <- charToRaw("1234567890ABCDEF")

# === FUNGSI TELEGRAM ===
kirim_telegram <- function(pesan) {
  url <- paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage")
  POST(url, body = list(chat_id = chat_id, text = pesan, parse_mode = "Markdown"))
}

# === ENKRIPSI / DEKRIPSI AES-GCM ===
encrypt_file <- function(teks, key_raw) {
  iv <- rand_bytes(12)  # IV acak
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
  if (!file.exists(file_path)) stop("❌ File JSON tidak ditemukan.")

  isi_file <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
  hash_sekarang <- digest(isi_file, algo = "sha256")

  if (!file.exists(hash_path)) {
    mode <- "enkripsi"
  } else {
    hash_lama <- readLines(hash_path, warn = FALSE)
    mode <- if (!identical(hash_sekarang, hash_lama)) "enkripsi" else "dekripsi"
  }

  message("🔁 Mode otomatis memilih: ", toupper(mode))
}

# === MODE ENKRIPSI ===
if (mode == "enkripsi") {
  tryCatch({
    if (!file.exists(file_path)) stop("❌ File JSON tidak ditemukan.")

    isi_file <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
    jsonlite::fromJSON(isi_file)  # Validasi awal JSON
    isi_hash <- digest(isi_file, algo = "sha256")

    # Log hash lama jika ada
    if (file.exists(hash_path)) {
      hash_lama <- readLines(hash_path, warn = FALSE)
      write(paste(timestamp, "- HASH lama:", hash_lama), file = log_path, append = TRUE)
    }

    # Enkripsi (tetap dilakukan walau tidak berubah)
    isi_encrypted_raw <- encrypt_file(isi_file, kunci_aes)
    isi_encrypted_b64 <- base64_encode(isi_encrypted_raw)

    # Simpan hash dan hasil enkripsi
    writeLines(isi_hash, hash_path)
    writeLines(isi_encrypted_b64, enc_path, useBytes = TRUE)

    # Dekripsi untuk validasi
    isi_decrypted_raw <- decrypt_file(base64_decode(isi_encrypted_b64), kunci_aes)
    isi_asli <- paste(rawToChar(isi_decrypted_raw, multiple = TRUE), collapse = "")
    jsonlite::fromJSON(isi_asli)  # Validasi ulang JSON
    writeLines(isi_asli, restore_path, useBytes = TRUE)

    # Logging
    write(paste(timestamp, "- JSON berhasil dipulihkan & dienkripsi."), file = log_path, append = TRUE)
    write(paste(timestamp, "- HASH baru:", isi_hash), file = log_path, append = TRUE)
    write(paste(timestamp, "- HASH base64 terenkripsi:", digest(isi_encrypted_b64, algo = "sha256")), file = log_path, append = TRUE)

    # Telegram
    kirim_telegram(paste0("⚠️ *PERINGATAN*\nFile `", basename(file_path), "` *dienkripsi ulang* & dipulihkan ke: `", basename(restore_path), "`\n🕒 ", timestamp))
    message("✅ JSON berhasil dienkripsi dan dipulihkan.")
  }, error = function(e) {
    message("❌ ERROR (Enkripsi): ", e$message)
  })
}

# === MODE DEKRIPSI ===
if (mode == "dekripsi") {
  tryCatch({
    if (!file.exists(enc_path)) stop("❌ File terenkripsi tidak ditemukan.")
    isi_encrypted_b64 <- readLines(enc_path, warn = FALSE)
    if (length(isi_encrypted_b64) == 0) stop("⚠️ File terenkripsi kosong.")

    isi_encrypted_raw <- base64_decode(paste(isi_encrypted_b64, collapse = ""))
    isi_decrypted_raw <- decrypt_file(isi_encrypted_raw, kunci_aes)
    isi_asli <- paste(rawToChar(isi_decrypted_raw, multiple = TRUE), collapse = "")

    jsonlite::fromJSON(isi_asli)  # validasi JSON
    writeLines(isi_asli, restore_path, useBytes = TRUE)

    write(paste(timestamp, "- DEKRIPSI berhasil & file dipulihkan."), file = log_path, append = TRUE)
    kirim_telegram(paste0("🔓 JSON berhasil *didekripsi* ke: `", basename(restore_path), "`\n🕒 ", timestamp))
    message("✅ File JSON berhasil didekripsi ke: ", restore_path)
  }, error = function(e) {
    message("❌ ERROR (Dekripsi): ", e$message)
  })
}

# Akhiri program
readline(prompt = "⏹ Tekan [Enter] untuk keluar...")
