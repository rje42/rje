\name{is.wholenumber}
\alias{is.wholenumber}
\title{
Determine whether number is integral or not.
}
\description{
Checks whether a numeric value is integral, up to machine or other specified prescision.
}
\usage{
is.wholenumber(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{
 numeric vector to be tested.
}
  \item{tol}{
The desired precision.
}
}
\value{
A logical vector of the same length as \code{x}, containing the results of the
test.
}
\author{
Robin Evans
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
x = c(0.5, 1, 2L, 1e-20)
is.wholenumber(x)
}
\keyword{arith}

