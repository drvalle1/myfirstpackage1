% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{tnorm}
\alias{tnorm}
\title{Generate truncated normal random variables}
\usage{
tnorm(n, lo, hi, mu, sig)
}
\arguments{
\item{n}{number of random variables}

\item{lo}{lower truncation value. If none, just put -Inf}

\item{hi}{upper truncation value. If none, just put Inf}

\item{mu}{the mean of the untruncated normal distribution}

\item{sig}{the standard deviation of the untruncated normal distribution}
}
\value{
A vector of numbers
}
\description{
This function generates truncated normal random variables
}
\examples{
mean1=3; sd1=2; lo1=0; hi1=Inf
z=tnorm(n=10000,lo=lo1,hi=hi1,mu=mean1,sig=sd1)
tmp=density(z,from=lo1)
plot(tmp$x,tmp$y,type='l')
x=seq(from=lo1,to=max(z),length.out=1000)
y=dnorm(x,mean=mean1,sd=sd1)/(1-pnorm(lo1,mean=mean1,sd=sd1))
lines(x,y,col='red')
}
