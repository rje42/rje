\name{subsetMatrix}
\alias{subsetMatrix}
\title{
Matrix of Subset Indicators
}
\description{
Produces a matrix whose rows indicate what subsets of a set are included in
which other subsets.
}
\usage{
subsetMatrix(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
 integer containing the number of elements in the set.
}
}
\details{
This function returns a matrix, with each row and column corresponding to a 
subset of a hypothetical set of size \code{n}, ordered lexographically.  The
entry in row \code{i}, column \code{j} corresponds to whether or not the
subset associated with \code{i} is a superset of that associated with \code{j}.

A 1 or -1 indicates that \code{i} is a superset of \code{j}, with the sign 
referring to the number of fewer elements in \code{j}.  0 indicates that
\code{i} is not a superset of \code{j}.
}
\value{
An integer matrix of dimension 2^n by 2^n.
}
\author{
Robin Evans
}
\note{
The inverse of the output matrix is just \code{abs(subsetMatrix(n))}.
}

\seealso{
\code{\link{combinations}}, \code{\link{powerSet}}.
}
\examples{
subsetMatrix(3)
}
\keyword{arith}
