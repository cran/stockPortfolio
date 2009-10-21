\name{optimalPort}
\alias{optimalPort}
\alias{print.optimalPortfolio}
\alias{summary.optimalPortfolio}
\alias{plot.optimalPortfolio}
\alias{points.optimalPortfolio}
\title{Estimate the optimal portfolio}
\description{
\code{optimalPort} estimates the optimal portfolio based on a stock model and data set.
}
\usage{
optimalPort(model, Rf = NULL, shortSell = NULL, eps = 10^(-4))
}
\arguments{
  \item{model}{ An object of class \code{"stockModel"}.}
  \item{Rf}{ An optional argument to update the risk free rate.}
  \item{shortSell}{ An optional argument to update short-selling.}
  \item{eps}{ An error term to be used in evaluating whether the risk-free rate is acceptable. This argument should not be adjusted except by advanced users.}
}
\value{
  \code{optimalPort} outputs an object of class \code{"optimalPortfolio"}, which is a list of
  \item{model }{ An object of class \code{"stockModel"}.}
  \item{X }{ The allocation of the optimal portfolio.}
  \item{R }{ The estimated return associated with allocation \code{X}.}
  \item{risk }{ The estimated risk associated with allocation \code{X}.}
}
\references{
Markowitz, Harry. "Portfolio Selection Efficient Diversification of Investments." New York: John Wiley and Sons, 1959.

Elton, Edwin, J., Gruber, Martin, J., Padberg, Manfred, W. "Simple
Criteria for Optimal Portfolio Selection," Journal of Finance, XI, No. 5
(Dec. 1976), pp. 1341-1357.

Elton, Edwin, J., Gruber, Martin, J., Padberg, Manfred, W. "Simple Rules
for Optimal Portfolio Selection:  The Multi Group Case," Journal of
Financial and Quantitative Analysis, XII, No. 3 (Sept. 1977), pp. 329-345.

Elton, Edwin, J., Gruber, Martin, J., Padberg, Manfred, W. "Simple
Criteria for Optimal Portfolio Selection:  Tracing Out the Efficient
Frontier," Journal of Finance, XIII, No. 1 (March 1978), pp. 296-302.
}
\author{David Diez and Nicolas Christou}
\seealso{\code{\link{getReturns}}, \code{\link{stockModel}}, \code{\link{testPort}}}
\examples{
#===> obtain data <===#
data(stock99)
data(stock94Info)
mgm <- stockModel(stock99, drop=25, model='MGM', industry=stock94Info$industry)

#===> build optimal portfolios <===#
opMgm1 <- optimalPort(mgm)
opMgm2 <- optimalPort(mgm, Rf=0.004)
print(opMgm1)
summary(opMgm1)

#===> plot the optimal porfolios <===#
par(mfrow=c(1,2))
# these plots provide a "head coloring" of
# the allocation by optimalPort
plot(opMgm1)
plot(opMgm2)

#===> additional plotting 1 <===#
par(mfrow=c(1,1))
plot(opMgm1, addNames=TRUE)

#===> additional plotting 2 <===#
plot(opMgm1, optPortOnly=TRUE, colOP=2, pchOP=2)
points(opMgm2, colOP=2, pchOP=4)
}
