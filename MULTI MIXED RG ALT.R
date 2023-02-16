simulasi3 <- function () {
  cat("==============================\n")
  cat("TUGAS SIMULASI MEET-3\n")
  cat("==============================\n")

  #Input berapa seed yang ingin dimasukkan
  repeat {
    ns <- as.numeric(readline("Input banyak seed : "))
    if(ns < 3) {
        cat("\nSilahkan input banyak seed minimal 3!\n")
    } else {
        break
    }
  }

  #Buat variabel vektor untuk menyimpan nilai seed.
  seed <- rep(0, ns)

  #Input nilai seed
  for(i in 1:ns){
    seed[i] <- as.numeric(readline(paste0("Input nilai seed [", i,"] : ")))
  }

  #Input nilai untuk koefisien model
  a <- as.numeric(readline("Input a : "))
  m <- as.numeric(readline("Input m : "))

  #Input jumlah angka random yang ingin digenerate
  n <- as.numeric(readline("Input nilai n (banyaknya bilangan acak yang dibangkitkan) : "))
  cat("Mixed Congruential RNG memiliki nilai c ≠ 0\nMultiplicative Congruential RNG memiliki nilai c = 0\n")

  #Buat variabel vektor c untuk menyimpan nilai dengan fungsi untuk menghitung model mixed
  c <- rep(0, ns)
  for(i in 1:ns) {
    c[i] <- as.numeric(readline(paste0("Input nilai c untuk seed ", seed[i], " : ")))
  }

  #Buat variabel vektor untuk menyimpan nilai random
  hasil <- rep(0, n)
  for(i in 1:ns) {
    x <- rep(0, n+1)
    random_generated <- rep(0, n+1)
    x[1] <- seed[i]
    if(c[i] != 0) {
        cat("Seed", x[1], ": Membangkitkan dengan Model Mixed Congruential RNG\n")
    } else {
        cat("Seed", x[1], ": Membangkitkan dengan Model Multiplicative RNG\n")
    }
    for(j in 2:(n+1)) {
      x[j] <- (a * x[j-1] + c[i]) %% m
      random_generated[j] <- x[j] / m
    }
    hasil <- cbind(hasil, x[-1], random_generated[-1])
  }
  #Menghapus nilai pada kolom awal (kolom berisi nilai 0)
  hasil <- hasil[,-1]

  r_seq <- 1
  for(i in 1:(2*ns)) {
    if(i %% 2 != 0) {
      colnames(hasil)[i] <- paste0("x[0,", r_seq , "]")
    } else {
      colnames(hasil)[i] <- paste0("u[", r_seq , "]")
      r_seq <- r_seq + 1
    }
  }
  print(hasil)
}

simulasi3()