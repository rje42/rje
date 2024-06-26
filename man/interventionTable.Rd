% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interventionMatrix.R, R/interventionTable.R
\name{interventionMatrix}
\alias{interventionMatrix}
\alias{interventionTable}
\alias{intervention.table}
\title{Calculate interventional distributions.}
\usage{
interventionMatrix(x, variables, condition, dim = NULL, incols = FALSE)

interventionTable(x, variables, condition)
}
\arguments{
\item{x}{An array of probabilities.}

\item{variables}{The margin for the intervention.}

\item{condition}{The dimensions to be conditioned upon.}

\item{dim}{Integer vector containing dimensions of variables.  Assumed all
binary if not specified.}

\item{incols}{Logical specifying whether not the distributions are stored as
the columns in the matrix; assumed to be rows by default.}
}
\value{
A numerical array of the same dimension as \eqn{x}.
}
\description{
Calculate interventional distributions from a probability table or matrix of
multivariate probability distributions.
}
\details{
This just divides the joint distribution \eqn{p(x)} by \eqn{p(v | c)}, where
\eqn{v} is \code{variables} and \eqn{c} is \code{condition}.

Under certain causal assumptions this is the interventional distribution
\eqn{p(x \,|\, do(v))} (i.e. if the direct causes of \eqn{v} are precisely
\eqn{c}.)

\code{intervention.table()} is identical to \code{interventionTable()}.
}
\section{Functions}{
\itemize{
\item \code{interventionMatrix()}: Interventions in matrix of distributions

}}
\examples{

set.seed(413)
# matrix of distributions
p = rdirichlet(10, rep(1,16))
interventionMatrix(p, 3, 2)

# take one in an array
ap = array(p[1,], rep(2,4))
interventionTable(ap, 3, 2)

}
\references{
Pearl, J., \emph{Causality}, 2nd Edition. Cambridge University
Press, 2009.
}
\seealso{
\code{\link{conditionTable}}, \code{\link{marginTable}}
}
\author{
Robin Evans
}
\keyword{arith}
