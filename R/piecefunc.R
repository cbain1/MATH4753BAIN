#' Piecewise Function
#'
#' @param x
#' @param coef
#'
#' @return piecewise regression
#' @export
#'
#' @examples
piecefunc <- function(x,coef){
	coef[1]+coef[2]*(x)+coef[3]*(x-18)*(x-18>0)
}
