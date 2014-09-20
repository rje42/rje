#' Print Percentage of Activity Completed to stdout
#' 
#' Prints percentage (or alternatively just a count) of loop or similar process
#' which has been completed to the standard output.
#' 
#' \code{printPercentage} will use \code{cat} to print the proportion of loops
#' which have been completed (i.e. \code{i/n}) to the standard output.  In
#' doing so it will erase the previous such percentage, except when \code{i =
#' first}.  A new line is added when \code{i = last}, assuming that the loop is
#' finished.
#' 
#' @aliases printPercentage printCount
#' @param i the number of iterations completed.
#' @param n total number of iterations.
#' @param dp number of decimal places to display.
#' @param first number of the first iteration for which this percentage was
#' displayed
#' @param last number of the final iteration for which this percentage will be
#' displayed
#' @param prev number of the previous iteration for which this percentage was
#' displayed
#' @return \code{NULL}
#' @section Warning: This will fail to work nicely if other information is
#' printed to the standard output % during the process.
#' @author Robin Evans
#' @keywords iteration IO print
#' @examples
#' 
#' x = numeric(100)
#' 
#' for (i in 1:100) {
#'   x[i] = mean(rnorm(1e5))
#'   printPercentage(i,100)
#' }
#' 
#' 
#' i = 0
#' repeat {
#'   i = i+1
#'   if (runif(1) > 0.99) {
#'     break
#'   }
#'   printCount(i)
#' }
#' print("\n")
#' 
#' @export printPercentage
printPercentage <-
function (i, n, dp = 0, first = 1, last = n, prev = i - 1) 
{
    out = ""
    disp = round(100 * i/n, dp)
    if (prev >= first) 
        prev.disp = round(100 * prev/n, dp)
    else prev.disp = ""
    if (disp > prev.disp) {
        nc = nchar(prev.disp)
        if (i != first) {
            out = paste(out, paste(rep("\b", nc + 1), collapse = ""), 
                sep = "")
        }
        out = paste(out, disp, "%", sep = "")
    }
    if (i == last) 
        out = paste(out, "\n", sep = "")
    cat(out)
    return(NULL)
}
