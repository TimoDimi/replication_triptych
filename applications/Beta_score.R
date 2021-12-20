

Beta_score <- function(y, x, alpha=1, beta=1, c_length=1000){
  c_vec <- seq(0,1,length.out=c_length)

  n <- length(y)
  S <- rep(NA,n)
  for (i in 1:n){
    xi <- x[i]
    yi <- y[i]
    if (yi == 1) {
      if (xi == 1) {
        S[i] <- 0
      } else{
        c_vec_1 <- c_vec[c_vec >= xi]
        S[i] <- (1-xi) * mean( c_vec_1^(alpha-1) * (1-c_vec_1)^beta )
      }
    } else {
      if (xi == 0) {
        S[i] <- 0
      } else {
        c_vec_0 <- c_vec[c_vec < xi]
        S[i] <- xi * mean( c_vec_0^(alpha) * (1-c_vec_0)^(beta-1) )
      }
    }
  }

  S

  # S<- ifelse((y == 1),
  #            apply(c_vec, 1, function(c){-c^(alpha-1) * (1-c)^beta,
  #                   -c^(alpha) * (1-c)^(beta-1))
  # S <- mean(S_prime)
}
