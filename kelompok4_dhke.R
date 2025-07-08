# ===============================
# Kelompok4_DHKE.r
# Implementasi Diffie-Hellman Key Exchange + Attack
# ===============================

# Cek apakah bilangan prima
is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  if (n == 2 || n == 3) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  batas <- floor(sqrt(n))
  if (batas < 3) return(TRUE)
  for (i in 3:batas) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

# Modular exponentiation (cepat & aman)
mod_exp <- function(base, exp, mod) {
  result <- 1
  base <- base %% mod
  while (exp > 0) {
    if (exp %% 2 == 1) {
      result <- (result * base) %% mod
    }
    base <- (base * base) %% mod
    exp <- exp %/% 2
  }
  return(result)
}

# Minta input integer dan pastikan valid
get_integer_input <- function(prompt) {
  repeat {
    input <- readline(prompt)
    if (grepl("^[0-9]+$", input)) {
      return(as.integer(input))
    }
    cat("❌ Input tidak valid. Masukkan bilangan bulat positif.\n")
  }
}

# ===============================
# Fungsi: Key Exchange
# ===============================
key_exchange <- function() {
  cat("=== Diffie-Hellman Key Exchange ===\n")
  
  repeat {
    p <- get_integer_input("Masukkan bilangan prima (p): ")
    if (is_prime(p)) break
    cat("❌ Bilangan yang dimasukkan bukan prima. Coba lagi.\n")
  }
  
  g <- get_integer_input("Masukkan generator (g): ")
  a <- get_integer_input("Masukkan private key Alice (a): ")
  b <- get_integer_input("Masukkan private key Bob (b): ")
  
  A <- mod_exp(g, a, p)  # Public key Alice
  B <- mod_exp(g, b, p)  # Public key Bob
  
  shared_Alice <- mod_exp(B, a, p)
  shared_Bob <- mod_exp(A, b, p)
  
  cat("Public Key Alice (A):", A, "\n")
  cat("Public Key Bob (B):", B, "\n")
  cat("Shared Key (Alice):", shared_Alice, "\n")
  cat("Shared Key (Bob):", shared_Bob, "\n")
  
  if (shared_Alice == shared_Bob) {
    cat("✅ Shared key cocok\n")
  } else {
    cat("❌ Shared key TIDAK cocok\n")
  }
}

# ===============================
# Fungsi: Attack (brute-force cari private key)
# ===============================
attack <- function() {
  cat("=== Sniffing Attack: Cari private key dari public key ===\n")
  
  g <- get_integer_input("Masukkan generator (g): ")
  
  repeat {
    p <- get_integer_input("Masukkan bilangan prima (p): ")
    if (is_prime(p)) break
    cat("❌ Bilangan yang dimasukkan bukan prima. Coba lagi.\n")
  }
  
  public_key_target <- get_integer_input("Masukkan public key target (A atau B): ")
  
  found <- FALSE
  for (x in 1:(p - 1)) {
    hasil <- mod_exp(g, x, p)
    if (hasil == public_key_target) {
      cat("🔓 Ditemukan! Private key target kemungkinan:", x, "\n")
      found <- TRUE
      
      lawan <- readline("Masukkan public key lawan (jika diketahui, tekan enter jika tidak): ")
      if (lawan != "" && grepl("^[0-9]+$", lawan)) {
        lawan <- as.integer(lawan)
        shared_key <- mod_exp(lawan, x, p)
        cat("🕵️‍♀️ Shared key berhasil dihitung oleh sniffer:", shared_key, "\n")
      } else {
        cat("ℹ️ Public key lawan tidak diberikan atau tidak valid. Shared key tidak dihitung.\n")
      }
      break
    }
  }
  
  if (!found) {
    cat("❌ Private key tidak ditemukan (kemungkinan terlalu besar atau bukan hasil g^x mod p)\n")
  }
}

# ===============================
# Program Utama: Menu
# ===============================
repeat {
  cat("\n=== MENU ===\n")
  cat("1. Key Exchange\n")
  cat("2. Brute-force Attack (Sniffer)\n")
  cat("3. Keluar\n")
  choice <- readline("Pilih menu: ")
  
  if (choice == "1") {
    key_exchange()
  } else if (choice == "2") {
    attack()
  } else if (choice == "3") {
    cat("👋 Program selesai.\n")
    break
  } else {
    cat("❌ Pilihan tidak valid.\n")
  }
}
