#' @title Log Likelihood Function For 2 Experiments
#'
#' @param theta the parameter to be estimated
#' @param size1 size of the first experiment
#' @param suc1 number of successes in the first experiment
#' #' @param size2 size of the second experiment
#' @param suc2 number of successes in the second experiment
#'
#'
logbin2=function(theta, size1, suc1, size2, suc2){
	log(dbinom(suc1,prob=theta,size=size1)) + log(dbinom(suc2,prob=theta,size=size2))
}
