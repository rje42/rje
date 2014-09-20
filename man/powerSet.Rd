\name{powerSet}
\alias{powerSet}
\title{
Power Set
}
\description{
Produces the power set of a vector.
}
\usage{
powerSet(x, rev = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 vector of elements (the set).
}
  \item{rev}{
 logical indicating whether to reverse the order of subsets.
}
}
\details{
Creates a list containing every subset of the elements of the vector \code{x}.
}
\value{
A list of vectors of the same type as \code{x}.

With \code{rev = FALSE} (the defualt) the list is ordered such that all 
subsets containing the last element of \code{x} come after those which do not, 
and so on.
}
\author{
Robin Evans
}

\seealso{
\code{\link{powerSetMat}}.
}
\examples{
powerSet(1:3)
powerSet(letters[3:5], rev=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}

