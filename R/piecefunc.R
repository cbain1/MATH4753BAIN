#' Piecewise Function
#'
#' @param x a value for x
#' @param coef the coefficients of the regression items
#'
#' @return piecewise regression
#' @export
#'
#' @examples
piecefunc <- function(x,coef){
	coef[1]+coef[2]*(x)+coef[3]*(x-18)*(x-18>0)
}
