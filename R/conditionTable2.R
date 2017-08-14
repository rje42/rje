#' @describeIn conditionTable Conditioning whilst preserving all dimensions
#' @export conditionTable2
conditionTable2 <-
function (x, variables, condition, undef=NaN) 
{
    out = conditionTable(x, variables, condition, undef=undef)
    tmp = patternRepeat0(c(variables, condition), dim(x), keep.order = TRUE)
    out = array(out[tmp], dim(x))
    dimnames(out) = dimnames(x)
    out
}

#' @export condition.table2
condition.table2 <- conditionTable2
