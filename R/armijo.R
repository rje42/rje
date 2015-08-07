#' Generic functions to aid finding local minima given search direction
#' 
#' Allows use of an Armijo rule or coarse line search as part of minimisation
#' (or maximisation) of a differentiable function of multiple arguments (via
#' gradient descent or similar).  Repeated application of one of these rules
#' should (hopefully) lead to a local minimum.
#' 
#' \code{coarseLine} performs a stepwise search and tries to find the integer
#' \eqn{k} minimising \eqn{f(x_k)} where \deqn{x_k = x + \beta^k dx.} Note
#' \eqn{k} may be negative.
#' 
#' \code{armijo} implements an Armijo rule for moving, which is to say that
#' \deqn{f(x_k) - f(x) < - \sigma \beta^k dx \cdot \nabla_x f.}{f(x_k) - f(x) <
#' - \sigma \beta^k dx . grad.} This has better convergence guarantees than a
#' simple line search, but may be slower in practice.  See Bertsekas (1999) for
#' theory underlying the Armijo rule.
#' 
#' Each of these rules should be applied repeatedly to achieve convergence (see
#' example below).
#' 
#' @aliases armijo coarseLine
#' @param fun a function whose first argument is a numeric vector
#' @param x a starting value to be passed to \code{fun}
#' @param dx numeric vector containing feasible direction for search; defaults
#' to \code{-grad} for ordinary gradient descent
#' @param beta numeric value (greater than 1) giving factor by which to adjust
#' step size
#' @param sigma numeric value (less than 1) giving steepness criterion for move
#' @param grad numeric gradient of \code{f} at \code{x} (will be estimated if
#' not provided)
#' @param maximise logical: if set to \code{TRUE} search is for a maximum
#' rather than a minimum.
#' @param searchup logical: if set to \code{TRUE} method will try to find
#' largest move satisfying Armijo criterion, rather than just accepting the
#' first it sees
#' @param adj.start an initial adjustment factor for the step size.
#' @param \dots other arguments to be passed to \code{fun}
#' @return A list comprising \item{best}{the value of the function at the final
#' point of evaluation} \item{adj}{the constant in the step, i.e.
#' \eqn{\beta^n}} \item{move}{the final move; i.e. \eqn{\beta^n dx}}
#' \item{code}{an integer indicating the result of the function; 0 = returned
#' OK, 1 = very small move suggested, may be at minimum already, 2 = failed to
#' find minimum: function evaluated to \code{NA} or was always larger than
#' \eqn{f(x)} (direction might be infeasible), 3 = failed to find minimum:
#' stepsize became too small or large without satisfying rule.}
#' @author Robin Evans
#' @references Bertsekas, D.P. \emph{Nonlinear programming}, 2nd Edition.
#' Athena, 1999.
#' @keywords optimize
#' @examples
#' 
#' 
#' # minimisation of simple function of three variables
#' x = c(0,-2,4)
#' f = function(x) ((x[1]-3)^2 + x[2]*sin(x[2]) + exp(x[3]) - x[3])
#' 
#' tol = .Machine$double.eps
#' mv = 1
#' 
#' while (mv > tol) {
#'   # or replace with coarseLine()
#'   out = armijo(f, x, sigma=0.1)
#'   x = out$x
#'   mv = sum(out$move^2)
#' }
#' 
#' # correct solution is c(3,0,0)
#' x
#' 
#' 
#' @export armijo
armijo <-
function (fun, x, dx, beta = 3, sigma = 0.5, grad, maximise = FALSE, 
    searchup = TRUE, adj.start = 1, ...) 
{
    sign = 1 - 2 * maximise
    if (beta <= 1) 
        stop("adjustment factor beta must be larger than 1")
    adj = adj.start
    tol = .Machine$double.eps
    cur = sign * fun(x, ...)
    nok = TRUE
    neg = TRUE
    if (missing(grad)) {
        grad = numeric(length(x))
        for (i in seq_along(x)) {
            x2 = x
            x2[i] = x[i] + 1e-08
            grad[i] = 1e+08 * (sign * fun(x2, ...) - cur)
        }
    }
    else grad = sign * grad
    if (missing(dx)) 
        dx = -grad
    s = -sigma * sum(dx * grad)
    moddx = sqrt(sum(dx^2))
    if (s < 0) {
        return(list(x = x, adj = 0, move = dx * 0, best = sign * 
            cur, code = 2))
    }
    while (nok) {
        try = sign * fun(x + dx * adj, ...)
        if (!is.na(try) && (cur - try) > adj * s) {
            nok = FALSE
        }
        else {
            adj = adj/beta
            searchup = FALSE
        }
        if (neg && !is.na(try) && (cur - try) > -adj * s) 
            neg = FALSE
        if (adj * moddx < tol) 
            return(list(x = x, adj = adj, move = dx * adj, best = sign * 
                cur, code = 2 * neg))
    }
    if (searchup) {
        ok = TRUE
        while (ok) {
            adj = adj * beta
            try2 = sign * fun(x + dx * adj, ...)
            if (is.na(try2)) {
                adj = adj/beta
                break
            }
            if (try - try2 < .Machine$double.eps) {
                adj = adj/beta
                break
            }
            else {
                try = try2
            }
            if (adj > 1/tol) {
                if (sqrt(sum(adj * dx^2)) < 10 * tol) 
                  return(list(x = x + dx * adj, adj = adj, move = dx * 
                    adj, best = sign * try, code = 3))
                else break
            }
        }
    }
    return(list(x = x + dx * adj, adj = adj, move = dx * adj, 
        best = sign * try, code = 1))
}

#' @describeIn armijo
#' @export coarseLine
coarseLine <-
  function (fun, x, dx, beta = 3, maximise = FALSE, ...) 
  {
    sign = 1 - 2 * maximise
    if (missing(dx)) {
      dx = numeric(length(x))
      for (i in seq_along(x)) {
        x2 = x
        x2[i] = x[i] + 1e-08
        dx[i] = -sign * 1e+08 * (fun(x2, ...) - fun(x, ...))
      }
    }
    if (beta <= 1) 
      stop("adjustment factor beta must be larger than 1")
    adj = 1
    tol = .Machine$double.eps
    best = sign * fun(x, ...)
    nok = TRUE
    while (nok) {
      best = sign * fun(x + adj * dx, ...)
      if (!is.na(best)) 
        nok = FALSE
      else adj = adj/beta
      if (adj < tol) 
        stop("Failed to find valid move")
    }
    try = sign * fun(x + dx * adj * beta, ...)
    if (is.na(try) || try >= best) 
      down = TRUE
    else down = FALSE
    if (down) 
      fact = 1/beta
    else fact = beta
    nok = TRUE
    while (nok) {
      try = sign * fun(x + dx * adj * fact, ...)
      if (is.na(try) || try >= best) {
        nok = FALSE
        if (adj < tol || 1/adj < tol) 
          return(list(x = x + dx * adj, adj = adj, move = dx * 
                        adj, best = sign * best, code = 2))
      }
      else {
        adj = adj * fact
        best = try
        if (adj < tol || 1/adj < tol) 
          return(list(x = x + dx * adj, adj = adj/fact, 
                      move = dx * adj/fact, best = sign * best, code = 3))
      }
    }
    return(list(x = x + adj * dx, adj = adj, move = dx * adj, 
                best = sign * best, code = 1))
  }

