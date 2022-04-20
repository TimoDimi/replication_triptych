
S_Rm1_c <- function(x_sorted, S, c_values=seq(0,1,length.out=101)) {

  Fx <- ecdf(x_sorted)
  s_Rm1_c <- rep(NA,(length(c_values)))

  for (c_index in 1:length(c_values)) {
    c_val <- c_values[c_index]

    # Stochastic interpolation for discrete distributions!
    index_lower <- which(Fx(x_sorted) <= c_val) %>% tail(1)
    index_upper <- which(Fx(x_sorted) > c_val) %>% head(1)
    # Check what we do beyond the smallest and largest data points! Not sure if that is the correct way to go!

    if (length(index_lower)==0){
      # Case that c_val "corresponds" to a value smaller than min(x)
      x_lower <- 0 # This only holds for probabilistic classifiers
      x_upper <- x_sorted[index_upper]
    } else if (length(index_upper)==0){
      # Case that c_val "corresponds" to a value larger than max(x)
      x_lower <- x_sorted[index_lower]
      x_upper <- 1 # This only holds for probabilistic classifiers
    } else {
      x_lower <- x_sorted[index_lower]
      x_upper <- x_sorted[index_upper]
    }

    c_lower <- Fx(x_lower)
    c_upper <- Fx(x_upper)

    # Avoid some special cases for the proportion
    if (c_upper==c_lower){
      prop_lower <- 0
    } else if (c_val <= c_lower){
      prop_lower <- 1
    } else{
      prop_lower <- (c_upper-c_val)/(c_upper-c_lower)
    }

    s_Rm1_c[c_index] <- prop_lower*S(x_lower) + (1-prop_lower)*S(x_upper)
  }

  return(s_Rm1_c)
}
