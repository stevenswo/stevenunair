gen_normal <- function(n) {
    u <- runif(n)
    random_generated <- qnorm(u, 0, 1)
    hasil <- cbind(u, random_generated)
    colnames(hasil) <- c("U_i", "X_i")
    hist(random_generated)
    print(hasil)
}

ux <- function() {
    cat("[Program Generate Random N(0,1) Data Diskret]\n")
    cat("Dibuat oleh Steven Soewignjo (082011833060)\n")
    n <- as.integer(readline("Masukkan jumlah bilangan random yang ingin digenerate\n>> "))
    gen_normal(n)
}

ux()
