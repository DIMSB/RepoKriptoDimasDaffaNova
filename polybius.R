# Buat key square 5x5 (menggabungkan I dan J)
generate_key_square <- function(keyword) {
  alphabet <- "abcdefghiklmnopqrstuvwxyz"  # i & j disatukan
  keyword <- tolower(gsub("[^a-z]", "", keyword))
  keyword <- gsub("j", "i", keyword)
  keyword_unique <- paste(unique(strsplit(keyword, "")[[1]]), collapse = "")
  remaining_letters <- gsub(paste0("[", keyword_unique, "]"), "", alphabet)
  full_key <- paste0(keyword_unique, remaining_letters)
  matrix(strsplit(full_key, "")[[1]], nrow = 5, ncol = 5, byrow = TRUE)
}

# Temukan posisi huruf pada key square
find_position <- function(letter, key_square) {
  if (letter == "j") letter <- "i"
  pos <- which(key_square == letter, arr.ind = TRUE)
  return(c(pos[1], pos[2]))  
}

# Enkripsi plaintext
nihilist_encrypt <- function(plaintext, keyword) {
  plaintext_clean <- tolower(gsub("[^a-z]", "", plaintext))
  plaintext_clean <- gsub("j", "i", plaintext_clean)
  key_square <- generate_key_square(keyword)

  cipher <- c()
  for (char in strsplit(plaintext_clean, "")[[1]]) {
    pos <- find_position(char, key_square)
    cipher <- c(cipher, paste0(pos[1], pos[2]))  
  }

  list(
    original = plaintext,
    cleaned = plaintext_clean,
    keyword = keyword,
    cipher = paste(cipher, collapse = " ")
  )
}

# Jalankan langsung untuk plaintext "daffa" dan keyword "sandi"
result <- nihilist_encrypt("daffa", "sandi")
cat("Kalimat asli      :", result$original, "\n")
cat("Keyword           :", result$keyword, "\n")
cat("Hasil Cipher      :", result$cipher, "\n")
