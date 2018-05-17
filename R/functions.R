#' Generate truncated normal random variables
#'
#' This function generates truncated normal random variables
#'
#' @param n number of random variables
#' @param lo lower truncation value. If none, just put -Inf
#' @param hi upper truncation value. If none, just put Inf
#' @param mu the mean of the untruncated normal distribution
#' @param sig the standard deviation of the untruncated normal distribution
#' 
#' @return A vector of numbers
#' @export
#' 
#' @examples
#' mean1=3; sd1=2; lo1=0; hi1=Inf
#' z=tnorm(n=10000,lo=lo1,hi=hi1,mu=mean1,sig=sd1)
#' tmp=density(z,from=lo1)
#' plot(tmp$x,tmp$y,type='l')
#' x=seq(from=lo1,to=max(z),length.out=1000)
#' y=dnorm(x,mean=mean1,sd=sd1)/(1-pnorm(lo1,mean=mean1,sd=sd1))
#' lines(x,y,col='red')
tnorm <- function(n,lo,hi,mu,sig){   #generates truncated normal variates based on cumulative normal distribution
  #normal truncated lo and hi
  
  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo,length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi,length(mu))
  
  q1 <- pnorm(lo,mu,sig) #cumulative distribution
  q2 <- pnorm(hi,mu,sig) #cumulative distribution
  
  z <- runif(n,q1,q2)
  z <- qnorm(z,mu,sig)
  z[z == -Inf]  <- lo[z == -Inf]
  z[z == Inf]   <- hi[z == Inf]
  z
}