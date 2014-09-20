\name{subsetOrder}
\alias{subsetOrder}
\title{
Compare sets for inclusion.
}
\description{
A wrapper for \code{is.subset} which returns set inclusions.
}
\usage{
subsetOrder(x, y)
}

\arguments{
  \item{x}{
A vector.
}
  \item{y}{
A vector of the same type as \code{x}.
}
}
\details{
If \code{x} is a subset of \code{y}, returns \code{-1}, for the reverse
  returns \code{1}.  If sets are equal or incomparable, it returns \code{0}.
}
\value{
A single integer, 0, -1 or 1.
}
\author{
Robin Evans
}

\seealso{
\code{\link{is.subset}}.
}
\examples{
subsetOrder(2:4, 1:4)
subsetOrder(2:4, 3:5)
}
\keyword{arith}
