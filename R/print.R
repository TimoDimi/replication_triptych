

#' Print method
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.triptych <- function(obj, ...) {
  print(autoplot(obj, ...))
  paste("Nothing printed yet")
  # print(summary(obj, ...))
  invisible(obj)
}
