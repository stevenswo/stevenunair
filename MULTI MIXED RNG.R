# Script created by Steven Soewignjo
# NIM 082011833060
# Statistika, Universitas Airlangga

psrand <- function(x0, a, m, n, c) {
    #x0 nilai inisial seed
    #a perkalian
    #m pembagi
    #n Jumlah bilangan yang ingin di generate
    #c mixed, jika 0 maka multiplikatif, jika selain nol dan bil positif maka mixed
    ns <- length(x0)
    print(ns)
    hasil <- rep(0, n)
    for(i in 1:ns) {
        x <- rep(0, n+1)
        random_generated <- rep(0, n+1)
        x[1] <- x0[i]
        if(c[i] != 0) {
            cat("Seed", x[1], ": Generating with Mixed Congruential RNG\n")
        } else {
            cat("Seed", x[1], ": Generating with Multiplicative RNG\n")
        }
        for(j in 2:(n+1)) {
            x[j] <- (a * x[j-1] + c[i]) %% m
            random_generated[j] <- x[j] / m
        }
        hasil <- cbind(hasil, x[-1], random_generated[-1])
    }
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

psrand(c(5, 2, 1), 3, 150, 10, c(0, 1, 2))

ux <- function() {
    cat(">> Generate Random Number with Pseudorandom Method\n")
    cat("> Program scripted by Steven Soewignjo (082011833060)\n")
    cat("> Mata Kuliah Simulasi Kelas S2\n")
    n_seed <- as.integer(readline("Berapakah seed yang ingin di-input?\n>> "))
    x0 <- rep(0, n_seed)
    c <- rep(0, n_seed)
    cat("Keterangan nilai Mixed Model:\nc = 0 -> Model Multiplikatif\nc ≠ 0 -> Model Mixed\n")
    for(i in 1:n_seed) {
        x0[i] <- as.numeric(readline(paste0("Masukkan seed ke-", i, "\n>> ")))
        c[i] <- as.numeric(readline(paste0("Masukkan nilai Mixed Model ke-", i, "\n>> ")))
    }
    a <- as.numeric(readline("Masukkan koefisien perkalian\n>> "))
    m <- as.numeric(readline("Masukkan koefisien pembagi\n>> "))
    n <- as.numeric(readline("Berapa angka random yang ingin di generate?\n>> "))
    psrand(x0, a, m, n, c)
}

ux()
