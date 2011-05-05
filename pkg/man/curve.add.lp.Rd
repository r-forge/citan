\name{curve.add.lp}
\alias{curve.add.lp}
\title{Draw the l_p-curve of given size}
\usage{curve.add.lp(ab, p=Inf, n=101, ...)}
\description{The \eqn{l_p}-curve appears in the definition of the \eqn{l_p}-index
(see Gagolewski, Grzegorzewski, 2009) and the \code{\link{index.lp}} function.}
\seealso{\code{\link{index.lp}}, \code{\link{plot.citfun}}, \code{\link{plot.default}}}
\references{Gagolewski M., Grzegorzewski P., A geometric approach to the construction of scientific impact indices, Scientometrics, 81(3), 2009a, 617-634.\cr}
\arguments{\item{ab}{size of the \eqn{l_p}-curve; positive numeric vector of length 2.}
\item{p}{index order, \eqn{p \in [1,\infty]}{p in [1,\infty]}; defaults \eqn{\infty} (\code{Inf}).}
\item{n}{integer; the maximal number of values at which to evaluate the underlying function.}
\item{...}{additional graphical parameters.}}
\examples{john_s <- c(11,5,4,4,3,2,2,2,2,2,1,1,1,0,0,0,0);
plot.citfun(john_s, main="Smith, John");
curve.add.lp(index.lp(john_s), col="green");
curve.add.lp(index.lp(john_s,1), 1, col="blue");
curve.add.lp(index.lp(john_s,2), 2, col="red");}
