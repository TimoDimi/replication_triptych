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
summary.triptych <- function(object, ..., score = "brier", arrange_decomp = TRUE) {
  res <- summary(object$RelDiag, score = score)
  class(res)[1] <- "summary.triptych"
  if (!arrange_decomp) return(res)
  dplyr::arrange(res, mean_score, discrimination)
}



is_binary <- function(y) {
  pmin(abs(y), abs(1 - y)) < sqrt(.Machine$double.eps)
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
  stopifnot(is_binary(y))
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
  stopifnot(is_binary(y))
  ifelse(y > 0.5, -log(x), -log(1 - x))
}

#' Zero-one score
#' @param y
#' @param x
#'
#' @return
#' @export
#'
#' @examples
MR_score <- function(y, x) {
  stopifnot(is_binary(y))
  case_when(
    x < 0.5 & y > 0.5 ~ 1.0,
    x > 0.5 & y < 0.5 ~ 1.0,
    x == 0.5          ~ 0.5,
    TRUE              ~ 0.0
  )
}

#' Beta score
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
Beta_score <- function(y, x, alpha = 1, beta = 1) {
  stopifnot(is_binary(y))
  stopifnot(isTRUE(alpha > 0))
  stopifnot(isTRUE(beta > 0))
  ifelse(
    y > 0.5,
    pbeta(x, alpha, beta + 1, lower.tail = FALSE) * pbeta(alpha, beta + 1),
    pbeta(x, alpha + 1, beta) * beta(alpha + 1, beta)
  )
}
