

#' T Distribution
#'
#' @param t t
#' @param df degrees of freedom
#' @param tail 'one' or 'two'
#' @return value
#' @export
#' @importFrom stats pt
#' @examples
#' dist_t(1.4, 2, "one")
#' dist_t(1.4, 2, "two")
dist_t <- function(t, df, tail) {

  if (tail == "one") {
    pt(-abs(t), df)
  } else if(tail == "two") {
    2 * pt(-abs(t), df)
  } else {
    stop("tail must be 'one' or 'two'")
  }

}
