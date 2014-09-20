#' @export patternRepeat0
patternRepeat0 <-
function (which, n, careful = TRUE, keep.order = FALSE) 
{
    out_size = as.integer(prod(n))
    if (length(which) == 0) 
        return(rep.int(1, out_size))
    out = integer(out_size)
    if (out_size == 0) 
        return(out)
    else if (out_size < 0) 
        stop("Dimensions must be positive")
    which = as.integer(which - 1L)
    if (!keep.order) 
        out[1] = 1L
    if (careful) {
        if (any(which >= length(n))) 
            stop("Indices in 'which' do not match length of dimensions in 'n'")
        if (any(which < 0)) 
            stop("Indices in 'which' must be positive integers")
        if (any(duplicated(which))) {
            which = unique.default(which)
            warning("Duplicated indices ignored")
        }
    }
    out = .C("patternRepeat", which, length(which), as.integer(n), 
        length(n), out)[[5]]
    out
}
