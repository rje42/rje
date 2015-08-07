#' Find conditional probability table
#' 
#' Given a numeric array or matrix (of probabilities), calculates margins of
#' some dimensions conditional on particular values of others.
#' 
#' \code{conditionTable} calculates the marginal distribution over the
#' dimensions in \code{variables} for each specified value of the dimensions in
#' \code{condition}.  Single or multiple values of each dimension in
#' \code{condition} may be specified in \code{condition.value}; in the case of
#' multiple values, \code{condition.value} must be a list.
#' 
#' The sum over the dimensions in \code{variables} is normalized to 1 for each
#' value of \code{condition}.
#' 
#' \code{conditionTable2} is just a wrapper which returns the conditional
#' distribution as an array of the same dimensions and ordering as the original
#' \code{x}.  Values are repeated as necessary.
#' 
#' \code{conditionMatrix} takes a matrix whose rows (or columns if \code{incols
#' = TRUE}) each represent a separate multivariate probability distribution and
#' finds the relevant conditional distribution in each case.  These are then
#' returned in the same format.  The order of the variables under
#' \code{conditionMatrix} is always as in the original distribution, unlike for
#' \code{conditionTable} above.
#' 
#' The probabilities are assumed in reverse lexicographic order, as
#' in a flattened R array: i.e. the first value changes fastest: (1,1,1),
#' (2,1,1), (1,2,1), ..., (2,2,2).
#' 
#' \code{condition.table} and \code{condition.table2} are identical to
#' \code{conditionTable} and \code{conditionTable2}.
#' 
#' @aliases conditionTable conditionTable2 condition.table condition.table2
#' conditionMatrix
#' @param x A numeric array.
#' @param variables An integer vector containing the margins of interest from
#' \code{x}.
#' @param margin An integer vector containing the margins of interest from
#' \code{x}.
#' @param condition An integer vector containing the dimensions of \code{x} to
#' condition on.
#' @param condition.value An integer vector or list of the same length as
#' \code{condition}, containing the values to condition with.  If \code{NULL},
#' then the full conditional distribution is returned.
#' @param undef if conditional probability is undefined, what should the value
#' be given as
#' @param order logical - if \code{TRUE} conditioned variables come last, if 
#' \code{FALSE} variables are in original order.
#' @param dim Integer vector containing dimensions of variables.  Assumed all
#' binary if not specified.
#' @param incols Logical specifying whether not the distributions are stored as
#' the columns in the matrix; assumed to be rows by default.
#' @return \code{conditionTable} returns an array whose first
#' \code{length(variables)} corresponds to the dimensions in \code{variables},
#' and the remainder (if any) to dimensions in \code{condition} with a
#' corresponding entry in \code{condition.value} of length > 1.
#' 
#' \code{conditionTable2} always returns an array of the same dimensions as
#' \code{x}, with the variables in the same order.
#' @author Mathias Drton, Robin Evans
#' @seealso \code{\link{marginTable}}, \code{\link[base]{margin.table}},
#' \code{\link{interventionTable}}
#' @keywords array
#' @examples
#' 
#' x = array(1:16, rep(2,4))
#' x = x/sum(x) # probability distribution on 4 binary variables x1, x2, x3, x4.
#' 
#' # distribution of x2, x3 given x1 = 1 and x4=2.
#' conditionTable(x, c(2,3), c(1,4), c(1,2))
#' # x2, x3 given x1 = 1,2 and x4 = 2.
#' conditionTable(x, c(2,3), c(1,4), list(1:2,2))
#' 
#' # complete conditional of x2, x3 given x1, x4
#' conditionTable(x, c(2,3), c(1,4))
#' 
#' # conditionTable2 leaves dimensions unchanged
#' tmp = conditionTable2(x, c(2,3), c(1,4))
#' aperm(tmp, c(2,3,1,4))
#' 
#' ####
#' set.seed(2314)
#' # set of 10 2x2x2 probability distributions
#' x = rdirichlet(10, rep(1,8))
#' 
#' conditionMatrix(x, 3, 1)
#' conditionMatrix(x, 3, 1, 2)
#' 
#' @export conditionTable
conditionTable <-
function (x, variables, condition = NULL, condition.value = NULL, undef = NaN, order = TRUE) 
{
    if (!is.null(condition.value) && length(condition) != length(condition.value)) 
        stop("condition and condition.value must have same length")
    if (length(intersect(variables, condition)) > 0) 
        stop("margin and condition must be disjoint")
    k = length(variables)
    if (order) marg = marginTable(x, c(variables, condition), order=TRUE)
    else marg = marginTable(x, sort.int(c(variables, condition)), order=FALSE)
    if (length(condition) == 0) 
        return(marg/sum(marg))
    if (order) {
      # variables <- seq_len(k)
      condition <- k + seq_along(condition)
    }
    else {
      condition <- match(sort.int(condition), sort.int(c(variables, condition)))
    }
    cond <- propTable(marg, condition)
    if (is.null(condition.value)) {
        out = cond
    }
    else if (is.list(condition.value)) {
        out = subtable(cond, condition, condition.value)
    }
    else {
        out = subtable(cond, condition, condition.value)
    }
    if (!is.nan(undef[1])) out[is.nan(out)] = undef[1]
    return(out)
}

#' @export condition.table
condition.table <- conditionTable
