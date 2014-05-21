add2 <- function (x,y) {
    x + y
}

above <- function (x, n=10) {
    use <- x > n
    x[use]
}

columnmean <- function(x, removeNA = TRUE) {
    nc <- ncol(x)
    means <- numeric(nc)
    for (i in 1:nc) {
        means[i] <- mean(x[, i], na.rm = removeNA)
    }
    means
}

make.power <- function(n) {
    #make.power is a constructing function as it creates another function
    pow <- function(x) { 
        x^n
    }
    pow
}
