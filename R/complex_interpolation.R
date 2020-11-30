#' Complex Interpolation
#' 
#' Enable `approx` to interpolate complex numbers
#'
#' @param x Numeric vector of the x-coordinates to be interpolated
#' @param y Complex vector of the y-coordinates to be interpolated
#' @param xout Optional set of numeric values specifying where interpolation is to take place
#' @param ... Other parameters to be sent to `approx`
#'
#' @examples
#' complex_approx(1:11, -5:5 + -5:5 * -1i, n = 5)
#' 
#' @export
complex_approx <- function(x, y, xout, ...) {
	
	if(is.null(x) | is.null(y))
		stop("x and y vectors cannot be null")
	
	# differing vector lengths handled in `approx`
	
	
	a <- stats::approx(x, Re(y), xout = xout, ...) %>% as_tibble
	b <- stats::approx(x, Im(y), xout = xout, ...) %>% as_tibble
	
	
	inner_join(a, b, by = "x") %>%
		mutate(y = .data$y.x + .data$y.y*1i,
					 .keep = "unused")
}