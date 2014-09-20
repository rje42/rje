\name{is.subset}
\alias{is.subset}
\alias{\%subof\%}

\title{
Check subset inclusion
}
\description{
Determines whether one vector contains all the elements of another.
}
\usage{
is.subset(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 vector.
}
  \item{y}{
 vector.
}
}
\details{
Determines whether or not every element of \code{x} is also found in \code{y}.
Returns \code{TRUE} if so, and \code{FALSE} if not.
}
\value{
A logical of length 1.
}
\author{
Robin Evans
}

\seealso{
\code{\link{setmatch}}.
}
\examples{
is.subset(1:2, 1:3)
is.subset(1:2, 2:3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
