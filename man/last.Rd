\name{last}
\alias{last}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Last element of a vector or list
}
\description{
Returns the last element of a list or vector.
}
\usage{
last(x)
}
\arguments{
  \item{x}{
a list or vector.
}
}
\details{
Designed to be faster than using \code{tail()} or \code{rev()}, and cleaner than writing \code{x[length(x)]}.
}
\value{
An object of the same type as \code{x} of length 1 (or empty if \code{x} is empty).
}
\author{
Robin Evans
}

\seealso{
\code{\link[utils]{tail}}, \code{\link[base]{rev}}.
}
\examples{
last(1:10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

