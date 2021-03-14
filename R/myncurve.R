#' My Normal Curve
#'
#' @param mu the mean of the distribution
#' @param sigma the standard deviation of the distribution
#' @param a the point for which you wish to calculate the probability below, i.e. P(Y<=a)
#'
#' @return the probability less than or equal to a, and a plot with the area shaded
#' @export
#'
#' @examples
myncurve = function(mu, sigma,a){
	#plot the curve
	curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
	#get x values
	xcurve=seq(mu-3*sigma, a,length=1000)

	# Y values corresponding t0 the x values
	ycurve=dnorm(xcurve,mean=mu,sd=sigma)

	# Fill in the polygon with the given vertices
	polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Purple")
	#Area
	prob=pnorm(a,mean = mu, sd= sigma)
	prob=round(prob,4)
	prob
}
