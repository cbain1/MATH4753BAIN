#' @title myci
#'
#' @param x a vector of the sample values
#'
#' @return the 95% confidence interval
#' @export
#'
myci <- function(x){
	if(length(x)>=30){
		z <- qnorm(1-.05/2)
		ci <- c()
		ci[1] <- mean(x)-z*(sd(x)/sqrt(length(x)))
		ci[2] <-  mean(x)+z*(sd(x)/sqrt(length(x)))
	}
	else {
		t <- qt(1-.05/2, length(x)-1)
		ci <- c()
		ci[1] <- mean(x)-t*(sd(x)/sqrt(length(x)))
		ci[2] <-  mean(x)+t*(sd(x)/sqrt(length(x)))
	}
	return(ci)
}
