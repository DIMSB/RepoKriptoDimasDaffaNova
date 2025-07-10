# === PAKET YANG DIGUNAKAN ===
library(httr)         # Untuk komunikasi HTTP (mengirim pesan ke Telegram)
library(base64enc)    # Untuk encoding dan decoding base64
library(digest)       # Untuk menghitung hash SHA256
library(jsonlite)     # Untuk validasi dan parsing JSON

# === KONFIGURASI ===
mode <- "enkripsi"  # Mode utama program: "enkripsi", "dekripsi", atau "auto"

# Path file yang digunakan
file_path    <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChecker/dummy.json"        # File JSON asli
enc_path     <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChecker/last_encrypted.txt" # File hasil enkripsi base64
hash_path    <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChecker/last_hash.txt"      # File untuk menyimpan hash terakhir
log_path     <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChecker/log.txt"            # File log proses
restore_path <- "C:/Users/DAFFA PRASETIO/Documents/IntegrityChecker/recovered_dummy.json" # File hasil dekripsi

# Konfigurasi Telegram
telegram_token <- "7435310919:AAG4YW44IIq3UeGJGdtoeK1LUeGzJZ9BQyI"  # Token bot Telegram
chat_id <- "-4973331538"                                            # ID chat tujuan
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")                # Waktu saat ini

# === KUNCI 3DES (HARUS 24 BYTE) ===
kunci_3des <- charToRaw("1234567890ABCDEFGHIJKLMNOP")  # Konversi string menjadi raw vector untuk kunci 3DES

# === FUNGSI UNTUK MENGIRIM PESAN TELEGRAM ===
kirim_telegram <- function(pesan) {
  url <- paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage")  # URL API Telegram
  POST(url, body = list(chat_id = chat_id, text = pesan, parse_mode = "Markdown"))  # Kirim POST request
}

# === FUNGSI ENKRIPSI DENGAN OPENSSL (3DES-CBC) ===
encrypt_file <- function(input_text, key_raw, tmp_plain = "plain.txt", tmp_enc = "encrypted.bin") {
  writeLines(input_text, tmp_plain, useBytes = TRUE)  # Tulis isi file ke file sementara
  iv <- as.raw(sample(0:255, 8, replace = TRUE))      # Buat IV acak 8 byte
  key_hex <- paste(sprintf("%02X", as.integer(key_raw)), collapse = "")  # Ubah key menjadi string hex
  iv_hex  <- paste(sprintf("%02X", as.integer(iv)), collapse = "")       # Ubah IV menjadi string hex
  cmd <- paste0("openssl enc -des-ede3-cbc -K ", key_hex, " -iv ", iv_hex,
                " -in ", shQuote(tmp_plain), " -out ", shQuote(tmp_enc), " -nosalt")  # Perintah OpenSSL
  status <- system(cmd, intern = FALSE)             # Jalankan perintah
  if (status != 0) stop("Gagal mengenkripsi via OpenSSL.")  # Jika gagal, hentikan program
  cipher <- readBin(tmp_enc, what = "raw", n = file.info(tmp_enc)$size)  # Baca hasil enkripsi
  c(iv, cipher)  # Gabungkan IV dan cipher sebagai output
}

# === FUNGSI DEKRIPSI DENGAN OPENSSL (3DES-CBC) ===
decrypt_file <- function(full_raw, key_raw, tmp_dec = "decrypted.txt", tmp_enc = "encrypted.bin") {
  if (length(full_raw) <= 8) stop("Ciphertext tidak valid.")  # Minimal panjang ciphertext adalah 8 byte (IV)
  iv     <- full_raw[1:8]             # Ambil IV
  cipher <- full_raw[-(1:8)]          # Ambil isi cipher
  writeBin(cipher, tmp_enc)           # Tulis cipher ke file sementara
  key_hex <- paste(sprintf("%02X", as.integer(key_raw)), collapse = "")  # Key dalam hex
  iv_hex  <- paste(sprintf("%02X", as.integer(iv)), collapse = "")       # IV dalam hex
  cmd <- paste0("openssl enc -d -des-ede3-cbc -K ", key_hex, " -iv ", iv_hex,
                " -in ", shQuote(tmp_enc), " -out ", shQuote(tmp_dec), " -nosalt")  # Perintah dekripsi
  status <- system(cmd, intern = FALSE)
  if (status != 0) stop("Gagal mendekripsi via OpenSSL.")
  paste(readLines(tmp_dec, warn = FALSE), collapse = "\n")  # Kembalikan isi dekripsi
}

# === MODE OTOMATIS UNTUK MEMILIH ENKRIPSI / DEKRIPSI ===
if (mode == "auto") {
  if (!file.exists(file_path)) stop("❌ File JSON tidak ditemukan.")  # Validasi file
  isi_file <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
  hash_sekarang <- digest(isi_file, algo = "sha256")  # Hitung hash saat ini

  if (!file.exists(hash_path)) {
    mode <- "enkripsi"  # Jika hash sebelumnya tidak ada, lakukan enkripsi
  } else {
    hash_lama <- readLines(hash_path, warn = FALSE)
    mode <- if (!identical(hash_sekarang, hash_lama)) "enkripsi" else "dekripsi"  # Bandingkan hash
  }
  message("🔁 Mode otomatis memilih: ", toupper(mode))  # Info mode yang dipilih
}

# === PROSES ENKRIPSI ===
if (mode == "enkripsi") {
  tryCatch({
    if (!file.exists(file_path)) stop("❌ File JSON tidak ditemukan.")
    isi_file <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
    jsonlite::fromJSON(isi_file)  # Validasi format JSON
    isi_hash <- digest(isi_file, algo = "sha256")  # Hash isi file

    if (file.exists(hash_path)) {
      hash_lama <- readLines(hash_path, warn = FALSE)
      write(paste(timestamp, "- HASH lama:", hash_lama), file = log_path, append = TRUE)
    }

    isi_encrypted_raw <- encrypt_file(isi_file, kunci_3des)           # Enkripsi file
    isi_encrypted_b64 <- base64enc::base64encode(isi_encrypted_raw)  # Encode base64 hasil enkripsi

    writeLines(isi_hash, hash_path)                                   # Simpan hash baru
    writeLines(isi_encrypted_b64, enc_path, useBytes = TRUE)          # Simpan file terenkripsi

    # Validasi hasil dekripsi
    isi_decrypted_raw <- decrypt_file(base64enc::base64decode(isi_encrypted_b64), kunci_3des)
    jsonlite::fromJSON(isi_decrypted_raw)                             # Pastikan bisa dibaca sebagai JSON
    writeLines(isi_decrypted_raw, restore_path, useBytes = TRUE)      # Simpan hasil dekripsi

    # Log ke file
    write(paste(timestamp, "- JSON berhasil dipulihkan & dienkripsi."), file = log_path, append = TRUE)
    write(paste(timestamp, "- HASH baru:", isi_hash), file = log_path, append = TRUE)
    write(paste(timestamp, "- HASH base64 terenkripsi:", digest(isi_encrypted_b64, algo = "sha256")), file = log_path, append = TRUE)

    # Kirim notifikasi ke Telegram
    kirim_telegram(paste0("⚠️ *PERINGATAN*\nFile `", basename(file_path), "` *dienkripsi ulang* dan dipulihkan ke `", basename(restore_path), "`\n🕒 ", timestamp))
    message("✅ JSON berhasil dienkripsi dan dipulihkan.")
  }, error = function(e) {
    message("❌ ERROR (Enkripsi): ", e$message)  # Tangani error saat enkripsi
  })
}

# === PROSES DEKRIPSI ===
if (mode == "dekripsi") {
  tryCatch({
    if (!file.exists(enc_path)) stop("❌ File terenkripsi tidak ditemukan.")  # Validasi keberadaan file terenkripsi
    isi_encrypted_b64 <- readLines(enc_path, warn = FALSE)                   # Baca isi terenkripsi
    if (length(isi_encrypted_b64) == 0) stop("⚠️ File terenkripsi kosong.")

    isi_encrypted_raw <- base64enc::base64decode(paste(isi_encrypted_b64, collapse = ""))  # Decode base64
    isi_asli <- decrypt_file(isi_encrypted_raw, kunci_3des)                                # Dekripsi
    jsonlite::fromJSON(isi_asli)                                                           # Validasi JSON

    writeLines(isi_asli, restore_path, useBytes = TRUE)         # Simpan hasil dekripsi
    write(paste(timestamp, "- DEKRIPSI berhasil & file dipulihkan."), file = log_path, append = TRUE)
    kirim_telegram(paste0("🔓 JSON berhasil *didekripsi* ke `", basename(restore_path), "`\n🕒 ", timestamp))
    message("✅ File JSON berhasil didekripsi ke: ", restore_path)
  }, error = function(e) {
    message("❌ ERROR (Dekripsi): ", e$message)  # Tangani error dekripsi
  })
}

# === HAPUS FILE SEMENTARA (BERSIH-BERSIH) ===
file.remove("plain.txt", "decrypted.txt", "cipher.bin", "encrypted.bin", "decryp.bin")

# === SELESAI ===
readline(prompt = "⏹ Tekan [Enter] untuk keluar...")
