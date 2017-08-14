#' Cube Helix colour palette
#' 
#' Cube Helix is a colour scheme designed to be appropriate for screen display
#' of intensity images.  The scheme is intended to be monotonically increasing
#' in brightness when displayed in greyscale.  This might also provide improved
#' visualisation for colour blindness sufferers.
#' 
#' The function evaluates a helix which moves through the RGB "cube", beginning
#' at black (0,0,0) and finishing at white (1,1,1).  Evenly spaced points on
#' this helix in the cube are returned as RGB colours.  This provides a colour
#' palette in which intensity increases monotonically, which makes for good
#' transfer to greyscale displays or printouts. This also may have advantages
#' for colour blindeness sufferers.  See references for further details.
#' 
#' @param n integer giving the number of colours in the scale
#' @param start numeric: start gives the initial angle (in radians) of the
#' helix
#' @param r numeric: number of rotations of the helix over the scale; can be
#' negative
#' @param hue numeric controling the saturation of colour: 0 gives pure
#' greyscale, defaults to 1
#' @param gamma numeric which can be used to emphasise lower or higher
#' intensity values, defaults to 1
#' @return Vector of RGB colours (strings) of length \code{n}.
#' @author Dave Green
#' 
#' Robin Evans
#' @seealso \code{\link[grDevices]{rainbow}} (for other colour palettes).
#' @references Green, D. A., 2011, A colour scheme for the display of
#' astronomical intensity images. \emph{Bulletin of the Astronomical Society of
#' India}, 39, 289.  \url{http://adsabs.harvard.edu/abs/2011BASI...39..289G}
#' 
#' See Dave Green's page at \url{http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/} for
#' other details.
#' @keywords color
#' @examples
#' 
#' cubeHelix(21)
#' 
#' \dontrun{
#' cols = cubeHelix(101)
#' 
#' plot.new()
#' plot.window(xlim=c(0,1), ylim=c(0,1))
#' axis(side=1)
#' for (i in 1:101) {
#'   rect((i-1)/101,0,(i+0.1)/101,1, col=cols[i], lwd=0)
#' }
#' }
#' 
#' \dontrun{
#' require(grDevices)
#' # comparison with other palettes
#' n = 101
#' cols = cubeHelix(n)
#' heat = heat.colors(n)
#' rain = rainbow(n)
#' terr = terrain.colors(n)
#' 
#' plot.new()
#' plot.window(xlim=c(-0.5,1), ylim=c(0,4))
#' axis(side=1, at=c(0,1))
#' axis(side=2, at=1:4-0.5, labels=1:4, pos=0)
#' for (i in 1:n) {
#'   rect((i-1)/n,3,(i+0.1)/n,3.9, col=cols[i], lwd=0)
#'   rect((i-1)/n,2,(i+0.1)/n,2.9, col=heat[i], lwd=0)
#'   rect((i-1)/n,1,(i+0.1)/n,1.9, col=rain[i], lwd=0)
#'   rect((i-1)/n,0,(i+0.1)/n,0.9, col=terr[i], lwd=0)
#' }
#' legend(-0.6,4,legend=c("4. cube helix", "3. heat", "2. rainbow", "1. terrain"), box.lwd=0)
#' }
#' 
#' @importFrom grDevices rgb
#' 
#' @export cubeHelix
cubeHelix <-
function (n, start = 0.5, r = -1.5, hue = 1, gamma = 1) 
{
    M = matrix(c(-0.14861, -0.29227, 1.97294, 1.78277, -0.90649, 
        0), ncol = 2)
    lambda = seq(0, 1, length.out = n)
    l = rep(lambda^gamma, each = 3)
    phi = 2 * pi * (start/3 + r * lambda)
    t = rbind(cos(phi), sin(phi))
    out = l + hue * l * (1 - l)/2 * (M %*% t)
    out = pmin(pmax(out, 0), 1)
    out = apply(out, 2, function(x) rgb(x[1], x[2], x[3]))
    return(out)
}
