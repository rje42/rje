patternRepeat <-
function (x, which, n, careful = TRUE, keep.order = FALSE) 
{
    if (careful) {
        if (length(which) == 0) {
            if (length(x) != 1) 
                stop("x not of correct length")
            else return(rep.int(x, prod(n)))
        }
        if (prod(n[which]) != length(x)) 
            stop("x not of correct length")
    }
    else if (length(which) == 0) 
        return(rep.int(x, prod(n)))
    idx = patternRepeat0(which, n, careful, keep.order)
    return(x[idx])
}
