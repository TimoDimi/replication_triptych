

#' Calculate score decompositions
#'
#' @param obj An object of class 'triptych'.
#' @param ... further arguments to be passed to or from methods.
#' @param score Specify scoring rule which should be decomposed.
#' @param arrange_decomp Binary whether to arrange the decomposition by DSC.
#'
#' @return
#' @export
#'
#' @examples
summary.triptych <- function(obj, ..., score = "brier", arrange_decomp=TRUE) {
  summary(obj$RelDiag, score=score) %>%
    tibble::as_tibble() %>%
    {if(arrange_decomp) dplyr::arrange(.,mean_score, discrimination) else . } %>%
    print()
}



#' Brier score
#'
#' @param y
#' @param x
#'
#' @return
#' @export
#'
#' @examples
brier_score <- function(y, x) {
  (x - y)^2
}



#' Logarithmic score
#'
#' @param y
#' @param x
#'
#' @return
#' @export
#'
#' @examples
log_score <- function(y, x) {
  ifelse(((y==0 & x==0) | (y==1 & x==1)),
         0,
         -y*log(x) - (1-y) * log(1-x))
}





#' Beta scores
#'
#' @param y
#' @param x
#' @param alpha
#' @param beta
#' @param c_length
#'
#' @return
#' @export
#'
#' @examples
Beta_score <- function(y, x, alpha=1, beta=1, c_length=1000){
  c_vec <- seq(0,1,length.out=c_length)

  n <- length(y)
  # If x is a constant!
  n_x <- length(x)
  if(n_x==1 & n>1) {x <- rep(x,n)}

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
}

