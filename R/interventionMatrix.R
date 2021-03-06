#' @describeIn interventionTable Interventions in matrix of distributions
#' @export interventionMatrix
interventionMatrix <-
function (x, variables, condition, dim = NULL, incols = FALSE) 
{
    d = 2 - incols
    if (length(variables) == 0) 
        return(x)
    if (is.null(dim)) 
        dim = rep(2, log2(dim(x)[d]))
    tmp = conditionMatrix(x, variables, condition, dim = dim, 
        incols = incols, undef=.5)
    vars = sort(c(variables, condition))
    patt = patternRepeat0(vars, dim)
    if (incols) 
        tmp = tmp[patt, ]
    else tmp = tmp[, patt]
    return(x/c(tmp))
}
