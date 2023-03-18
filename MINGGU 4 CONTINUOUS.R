gen_normal <- function(n) {
    u <- runif(n)
    random_generated <- qnorm(u, 0, 1)
    hasil <- cbind(u, random_generated)
    colnames(hasil) <- c("U_i", "X_i")
    hist(random_generated)
    print(hasil)
    return(random_generated)
}

gen_chsq_from_normal01 <- function(z) {
    hasil <- z^2
    hist(hasil)
    print(hasil)
}

ux <- function() {
    cat("[Program Generate Random N(0,1) Data Kontinu]\n")
    cat("Dibuat oleh Steven Soewignjo (082011833060)\n")
    n <- as.integer(readline("Masukkan jumlah bilangan random yang ingin digenerate\n>> "))
    z <- gen_normal(n)
    opt1 <- readline(cat("Transformasi ke Chi-Square(1) (Y/n)\n>> "))
    if(opt1 == 'Y') {
        gen_chsq_from_normal01(z)
        cat("Program completed.")
    } else if(opt1 == 'n') {
        cat("Program completed.")
    }
}

ux()

transformasi2 <- function(n_data, m, n) {
    u <- rchisq(n_data, m)
    v <- rchisq(n_data, n)
    
    f <- (u/m)/(v/n)
    hist(f)
    # F ~ F(m, n)
}



transformasi2(30, 4, 2)