#' @export conditionTable2
conditionTable2 <-
function (x, variables, condition) 
{
    out = conditionTable(x, variables, condition)
    tmp = patternRepeat0(c(variables, condition), dim(x), keep.order = TRUE)
    return(array(out[tmp], dim(x)))
}

#' @export condition.table2
condition.table2 <- conditionTable2
