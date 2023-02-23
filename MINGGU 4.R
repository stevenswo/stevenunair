
gen_discrete <- function(n) {
    p <- c(0.2, 0.15, 0.25, 0.4)
    x <- seq(length(p))
    cdf <- rep(0, length(p))
    u <- runif(n)
    rand_generated <- rep(0, length(u))

    for(i in 1:length(p)) {
        if(i == 1) {
            cdf[i] <- p[i]
        } else {
            cdf[i] <- p[i] + cdf[i-1]
        }
    }

    for(i in 1:length(rand_generated)) {
        for(j in 1:length(cdf)) {
            if(u[i] < cdf[j]) {
                rand_generated[i] <- x[j]
                break
            }
        }
    }
    hasil <- cbind(u, rand_generated)
    colnames(hasil) <- c("U_i", "Xi")
    print(hasil)
}

ux <- function() {
    cat("[Program Generate Random Data Diskret]\n")
    cat("Dibuat oleh Steven Soewignjo (082011833060)\n")
    n <- as.integer(readline("Masukkan jumlah bilangan random yang ingin digenerate\n>> "))
    gen_discrete(n)
}
ux()
