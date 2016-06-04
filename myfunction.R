myfunction <- function(x){
	y <- rnorm(100)
	mean(y)
}

second <- function(x){
	x + rnorm(length(x))
}

cube <- function(x, n) {
        x^3
}