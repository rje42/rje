% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dirichlet.R
\name{Dirichlet}
\alias{Dirichlet}
\alias{ddirichlet}
\alias{rdirichlet}
\title{The Dirichlet Distribution}
\usage{
ddirichlet(x, alpha, log = FALSE, tol = 1e-10) 
rdirichlet(n, alpha)
}
\arguments{
\item{x}{vector (or matrix) of points in sample space.}

\item{alpha}{vector of Dirichlet hyper parameters.}

\item{log}{logical; if TRUE, natural logarithm of density is returned.}

\item{tol}{tolerance of vectors not summing to 1 and negative values.}

\item{n}{number of random variables to be generated.}
}
\value{
\code{rdirichlet} returns a matrix, each row of which is an
independent draw % from a Dirichlet distribution with parameter vector
\code{alpha}.

\code{ddirichlet} returns a vector, each entry being the density of the %
corresponding row of \code{x}.  If \code{x} is a vector, then the output %
will have length 1.
}
\description{
Density function and random generation for Dirichlet distribution with %
parameter vector \code{alpha}.
}
\details{
If \code{x} is a matrix, each row is taken to be a different point whose %
density is to be evaluated.  If the number of columns in (or length of, in
the % case of a vector) \code{x} is one less than the length of
\code{alpha}, the % remaining column (or entry) is assumed to make the
vector sum to 1.

The \emph{k}-dimensional Dirichlet distribution has density
\deqn{\frac{\Gamma\left(\sum_i \alpha_i\right)}{\prod_i \Gamma(\alpha_i)}
\prod_{i=1}^k x_i^{\alpha_i-1}}{Gamma (alpha_1 + ... +
alpha_k)*(Gamma(alpha_1)*...*Gamma(alpha_k))^{-1}*x_1^{alpha_1-1}*...*x_k^{alpha_k-1}}
assuming that \eqn{x_i > 0}{x_i > 0} and \eqn{\sum_i x_i = 1}{x_1 + ... +
x_k = 1}, and zero otherwise.

If the sum of row entries in \code{x} differs from 1 by more than
\code{tol}, % or any entry takes a value less than \code{-tol}, the density
is assumed to be % zero.
}
\examples{

x = rdirichlet(10, c(1,2,3))
x

# Find densities at random points.
ddirichlet(x, c(1,2,3))
# Last column to be inferred.
ddirichlet(x[,c(1,2)], c(1,2,3))
ddirichlet(x, matrix(c(1,2,3), 10, 3, byrow=TRUE))

}
\references{
\url{https://en.wikipedia.org/wiki/Dirichlet_distribution}
}
\author{
Robin Evans
}
\keyword{distribution}
