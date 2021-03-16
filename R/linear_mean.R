#' Linear Mean
#' 
#' Converts a numeric vector from decibel format to linear format, calculates the mean value, and converts back to decibel format.
#'
#' @param x A vector of numeric values
#' @param scalar Scalar to be used in `as_decibel` and `as_linear` functions for conversion
#' @param ... Addional methods passed to the `mean` function
#'
#' @examples
#' linear_mean(-88:-90)
#' @export
linear_mean <- function(x, scalar = 10, ...) {
	
	x <- rftk::as_linear(x, scalar = scalar)
	x <- mean(x, ...)
	x <- rftk::as_decibel(x, scalar = scalar)
	
	x
}