
#' Reactance Conversion
#' 
#' Converts between capacitance, inductance, and reactance
#'
#' @param c Capacitance (F)
#' @param l Inductance (H)
#' @param x Reactance (jOhm)
#' @param freq Frequency (Hz)
#'
#' @return Converted data in SI units.
#' @export
#'
#' @examples
#' c_to_x(1:10 * 1e-12, 2e9)
#' l_to_x(1:10 * 1e-9, 2e9)
#' x_to_c(-1:-10, 2e9) * 1e12
#' x_to_l(10:20, 2e9) * 1e9
#' 
#' @name reactance_conversion
c_to_x <- function(c, freq){ -1 / (2 * pi * freq * c)}

#' @rdname reactance_conversion
#' @export
l_to_x <- function(l, freq) 2 * pi * freq * l

#' @rdname reactance_conversion
#' @export
x_to_c <- function(x, freq) -1 / (2 * pi * freq * x)

#' @rdname reactance_conversion
#' @export
x_to_l <- function(x, freq)	x / (2 * pi * freq)
