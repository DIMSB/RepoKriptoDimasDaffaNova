# Fungsi membuat Polybius Square standar (tanpa huruf 'j')
create_polybius_square <- function() {
alphabet <- "abcdefghiklmnopqrstuvwxyz"
matrix(strsplit(alphabet, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)
}
# Fungsi konversi huruf ke angka (berbasis Polybius)
text_to_numeric <- function(text, square) {
text <- tolower(gsub("[^a-z]", "", text))
text <- gsub("j", "i", text)
numeric <- c()
for (char in strsplit(text, "")[[1]]) {
pos <- which(square == char, arr.ind = TRUE)
numeric <- c(numeric, as.integer(paste0(pos[1], pos[2])))
}
return(numeric)
}
# Fungsi konversi angka ke huruf (berbasis Polybius)
numeric_to_text <- function(numbers, square) {
chars <- c()
for (num in numbers) {
num <- as.integer(num)
row <- num %/% 10
col <- num %% 10
chars <- c(chars, square[row, col])
}
return(paste0(chars, collapse = ""))
}
# Fungsi Enkripsi Nihilist
nihilist_encrypt <- function(plaintext, keytext) {
square <- create_polybius_square()
pt_nums <- text_to_numeric(plaintext, square)
key_nums <- text_to_numeric(keytext, square)
key_nums <- rep(key_nums, length.out = length(pt_nums))
cipher_nums <- pt_nums + key_nums
return(paste(cipher_nums, collapse = " "))
}
# Fungsi Dekripsi Nihilist
nihilist_decrypt <- function(ciphertext, keytext) {
square <- create_polybius_square()
cipher_nums <- as.integer(strsplit(ciphertext, " ")[[1]])
key_nums <- text_to_numeric(keytext, square)
key_nums <- rep(key_nums, length.out = length(cipher_nums))
pt_nums <- cipher_nums - key_nums
return(numeric_to_text(pt_nums, square))
}
# Fungsi Attack (placeholder)
nihilist_attack <- function(ciphertext) {
cat("Attack belum diimplementasikan.\n")
}
# Menu Utama
repeat {
cat("\n====== MENU (ALGORITMA) NIHILIST CIPHER ======\n",
"1. Enkripsi\n",
"2. Dekripsi\n",
"3. Attack\n",
"4. Keluar\n", sep = "")
choice <- readline("Pilih menu (1/2/3/4): ")
if (choice == "1") {
pt <- readline("Masukkan plaintext: ")
key <- readline("Masukkan keytext: ")
encrypted <- nihilist_encrypt(pt, key)
cat("Hasil Enkripsi:", encrypted, "\n")
} else if (choice == "2") {
ct <- readline("Masukkan ciphertext (misal: 45 54 62): ")
key <- readline("Masukkan keytext: ")
decrypted <- nihilist_decrypt(ct, key)
cat("Hasil Dekripsi:", decrypted, "\n")
} else if (choice == "3") {
ct <- readline("Masukkan ciphertext untuk diserang: ")
nihilist_attack(ct)
} else if (choice == "4") {
cat("Keluar dari program.\n")
break
} else {
cat("Pilihan tidak valid. Silakan pilih 1-4.\n")
}
}