#' Coerce Electrical Network Parameters Inputs
#' 
#' Checks and converts the input parameters to the electrical network parameter conversion functions
#'
#' @param params Input network parameters.  Must be one of:
#' \describe{\item{2x2 Matrix}{\tabular{rr}{ S11 \tab S12 \cr S21 \tab S22}}
#'   \item{Vector of length 4}{S11 S21 S12 S22}
#'   }
#' @param z0 Network parameter characteristic impedance.  Must be one of:
#' \describe{
#'   \item{`NA`, `NULL`}{Defaults to 50ohm input and output}
#'   \item{Single numeric}{Same input and output characteristic impedance}
#'   \item{Vector of length 2}{Input and output impedance, respectively}}
#' @returns Input coerced into a 2x2 matrix (params) or a vector of length 2 (z0) to be used in the conversions
#' @examples 
#' rftk:::coerce_params(c(0.1, 0.5, 0.5, 0.1))
#' rftk:::coerce_params(matrix(c(0.1, 0.5, 0.5, 0.1), ncol = 2))
#' 
#' rftk:::coerce_z0(50)
#' rftk:::coerce_z0(c(50,100))
#' @keywords internal
#' @name param_coercion
coerce_params <- function(params) {
	if (is.matrix(params)) {
		if (all(dim(params) == c(2, 2))) {
			return(params)
		}
	}
	else if (length(params) == 4) {
		return(matrix(params, nrow = 2))
	}
	
	stop("Input parameters in incorrect format")
}

#' @rdname param_coercion
coerce_z0 <- function(z0) {
	if (any(is.na(z0)) | is.null(z0)) {
		c(50, 50)
	} else if (length(z0) == 1) {
		c(z0, z0)
	} else if (length(z0) == 2) {
		c(z0)
	} else {
		stop("z0 in incorrect format")
	}
}


#' Electrical Network Transformation
#'
#' Converts electrical networks parameter matrices into equivalent types.
#'
#' @param abcd ABCD-Parameters
#' @param h h-Parameters
#' @param s S-Parameters
#' @param t t-Parameters
#' @param y Y-Parameters
#' @param z Z-Parameters
#' @param z0 Characteristic Impedance
#'
#' @return Converted electrical parameters in a 2x2 matrix format
#' @export
#'
#' @section Parameter format:
#' \describe{
#'   \item{2x2 Matrix}{\tabular{rr}{
#'   S11 \tab S12\cr
#'   S21 \tab S22
#'   }}
#'   \item{Vector of length 4}{
#'   S11 S21 S12 S22
#'   }}
#' @examples
#' s_to_abcd(c(0.1, 0.5, 0.5, 0.1))
#' s_to_abcd(matrix(c(0.1, 0.5, 0.5, 0.1), ncol = 2))
#' s_to_abcd(matrix(c(0.1, 0.5, 0.5, 0.1), ncol = 2), z0 = c(50,100))
#' @name network_conversion
#' @export
abcd_to_h <- function(abcd, z0 = c(50, 50)) {
	abcd <- coerce_params(abcd)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	a <- abcd[1, 1]
	c <- abcd[2, 1]
	b <- abcd[1, 2]
	d <- abcd[2, 2]
	
	denom <- d
	
	h11 <- b
	h12 <- a * d - b * c
	h21 <- -1
	h22 <- c
	
	matrix(c(h11, h21, h12, h22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
abcd_to_s <- function(abcd, z0 = c(50, 50)) {
	abcd <- coerce_params(abcd)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	a <- abcd[1, 1]
	c <- abcd[2, 1]
	b <- abcd[1, 2]
	d <- abcd[2, 2]
	
	denom <- a * z02 + b + c * z01 * z02 + d * z01
	
	s11 <- a * z02 + b - c * Conj(z01) * z02 - d * Conj(z01)
	s12 <- 2 * (a * d - b * c) * sqrt(Re(z01) * Re(z02))
	s21 <- 2 * sqrt(Re(z01) * Re(z02))
	s22 <- -a * Conj(z02) + b - c * z01 * Conj(z02) + d * z01
	
	matrix(c(s11, s21, s12, s22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
abcd_to_t <- function(abcd, z0 = c(50, 50)) {
	abcd <- coerce_params(abcd)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	a <- abcd[1, 1]
	c <- abcd[2, 1]
	b <- abcd[1, 2]
	d <- abcd[2, 2]
	
	denom <- 2 * sqrt(Re(z01) * Re(z02))
	
	t11 <- a * z02 + b + c * z01 * z02 + d * z01
	t12 <- a * Conj(z02) - b + c * z01 * Conj(z02) - d * z01
	t21 <- a * z02 + b - c * Conj(z01) * z02 - d * Conj(z01)
	t22 <- a * Conj(z02) - b - c * Conj(z01) * Conj(z02) + d * z01
	
	matrix(c(t11, t21, t12, t22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
abcd_to_y <- function(abcd, z0 = c(50, 50)) {
	abcd <- coerce_params(abcd)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	a <- abcd[1, 1]
	c <- abcd[2, 1]
	b <- abcd[1, 2]
	d <- abcd[2, 2]
	
	denom <- b
	
	y11 <- d
	y12 <- b * c - a * d
	y21 <- -1
	y22 <- a
	
	matrix(c(y11, y21, y12, y22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
abcd_to_z <- function(abcd, z0 = c(50, 50)) {
	abcd <- coerce_params(abcd)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	a <- abcd[1, 1]
	c <- abcd[2, 1]
	b <- abcd[1, 2]
	d <- abcd[2, 2]
	
	denom <- c
	
	z11 <- a
	z12 <- a * d - b * c
	z21 <- 1
	z22 <- d
	
	matrix(c(z11, z21, z12, z22), ncol = 2) / denom
}

#' @rdname network_conversion
#' @export
h_to_abcd <- function(h, z0 = c(50, 50)) {
	h <- coerce_params(h)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	h11 <- h[1, 1]
	h21 <- h[2, 1]
	h12 <- h[1, 2]
	h22 <- h[2, 2]
	
	denom <- h21
	
	a <- h12 * h21 - h11 * h22
	b <- -h11
	c <- -h22
	d <- -1
	
	matrix(c(a, c, b, d), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
h_to_s <- function(h, z0 = c(50, 50)) {
	h <- coerce_params(h)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	h11 <- h[1, 1]
	h21 <- h[2, 1]
	h12 <- h[1, 2]
	h22 <- h[2, 2]
	
	denom <- (z01 + h11) * (1 + h22 * z02) - h12 * h21 * z02
	
	s11 <- (h11 - Conj(z01)) * (1 + h22 * z02) - h12 * h21 * z02
	s12 <- 2 * h12 * sqrt(Re(z01) * Re(z02))
	s21 <- -2 * h21 * sqrt(Re(z01) * Re(z02))
	s22 <- (h11 + z01) * (1 - h22 * Conj(z02)) + h12 * h21 * Conj(z02)
	
	matrix(c(s11, s21, s12, s22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
h_to_t <- function(h, z0 = c(50, 50)) {
	h <- coerce_params(h)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	h11 <- h[1, 1]
	h21 <- h[2, 1]
	h12 <- h[1, 2]
	h22 <- h[2, 2]
	
	denom <- 2 * h21 * sqrt(Re(z01) * Re(z02))
	
	t11 <- (-h11 - z01) * (1 + h22 * z02) + h12 * h21 * z02
	t12 <- (h11 + z01) * (1 - h22 * Conj(z02)) + h12 * h21 * Conj(z02)
	t21 <- (Conj(z01) - h11) * (1 + h22 * z02) + h12 * h21 * z02
	t22 <- (h11 - Conj(z01)) * (1 - h22 * Conj(z02)) + h12 * h21 * Conj(z02)
	
	matrix(c(t11, t21, t12, t22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
h_to_y <- function(h, z0 = c(50, 50)) {
	h <- coerce_params(h)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	h11 <- h[1, 1]
	h21 <- h[2, 1]
	h12 <- h[1, 2]
	h22 <- h[2, 2]
	
	denom <- h11
	
	y11 <- 1
	y12 <- -h12
	y21 <- h21
	y22 <- h11 * h22 - h12 * h21
	
	matrix(c(y11, y21, y12, y22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
h_to_z <- function(h, z0 = c(50, 50)) {
	h <- coerce_params(h)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	h11 <- h[1, 1]
	h21 <- h[2, 1]
	h12 <- h[1, 2]
	h22 <- h[2, 2]
	
	denom <- h22
	
	z11 <- h11 * h22 - h12 * h21
	z12 <- h12
	z21 <- -h21
	z22 <- 1
	
	matrix(c(z11, z21, z12, z22), ncol = 2) / denom
}

#' @rdname network_conversion
#' @export
s_to_abcd <- function(s, z0 = c(50, 50)) {
	s <- coerce_params(s)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	s11 <- s[1, 1]
	s21 <- s[2, 1]
	s12 <- s[1, 2]
	s22 <- s[2, 2]
	
	denom <- 2 * s21 * sqrt(Re(z01) * Re(z02))
	
	a <- (Conj(z01) + s11 * z01) * (1 - s22) + s12 * s21 * z01
	b <-
		(Conj(z01) + s11 * z01) * (Conj(z02) + s22 * z02) - s12 * s21 * z01 * z02
	c <- (1 - s11) * (1 - s22) - s12 * s21
	d <- (1 - s11) * (Conj(z02) + s22 * z02) + s12 * s21 * z02
	
	matrix(c(a, c, b, d), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
s_to_h <- function(s, z0 = c(50, 50)) {
	s <- coerce_params(s)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	s11 <- s[1, 1]
	s21 <- s[2, 1]
	s12 <- s[1, 2]
	s22 <- s[2, 2]
	
	denom <- (1 - s11) * (Conj(z02) + s22 * z02) + s12 * s21 * z02
	
	h11 <- (Conj(z01) + s11 * z01) * (Conj(z02) + s22 * z02) - s12 * s21 *
		z01 * z02
	h12 <- 2 * s12 * sqrt(Re(z01) * Re(z02))
	h21 <- -2 * s21 * sqrt(Re(z01) * Re(z02))
	h22 <- (1 - s11) * (1 - s22) - s12 * s21
	
	matrix(c(h11, h21, h12, h22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
s_to_t <- function(s, z0 = c(50, 50)) {
	s <- coerce_params(s)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	s11 <- s[1, 1]
	s21 <- s[2, 1]
	s12 <- s[1, 2]
	s22 <- s[2, 2]
	
	denom <- s21
	
	t11 <- 1
	t12 <- -s22
	t21 <- s11
	t22 <- s12 * s21 - s11 * s22
	
	matrix(c(t11, t21, t12, t22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
s_to_y <- function(s, z0 = c(50, 50)) {
	s <- coerce_params(s)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	s11 <- s[1, 1]
	s21 <- s[2, 1]
	s12 <- s[1, 2]
	s22 <- s[2, 2]
	
	denom <- (Conj(z01) + s11 * z01) * (Conj(z02) + s22 * z02) - s12 * s21 * z01 * z02
	
	y11 <- (1 - s11) * (Conj(z02) + s22 * z02) + s12 * s21 * z02
	y12 <- -2 * s12 * sqrt(Re(z01) * Re(z02))
	y21 <- -2 * s21 * sqrt(Re(z01) * Re(z02))
	y22 <- (1 - s22) * (Conj(z01) + s11 * z01) + s12 * s21 * z01
	
	matrix(c(y11, y21, y12, y22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
s_to_z <- function(s, z0 = c(50, 50)) {
	s <- coerce_params(s)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	s11 <- s[1, 1]
	s21 <- s[2, 1]
	s12 <- s[1, 2]
	s22 <- s[2, 2]
	
	denom <- (1 - s11) * (1 - s22) - s12 * s21
	
	z11 <- (Conj(z01) + s11 * z01) * (1 - s22) + s12 * s21 * z01
	z12 <- 2 * s12 * sqrt(Re(z01) * Re(z02))
	z21 <- 2 * s21 * sqrt(Re(z01) * Re(z02))
	z22 <- (Conj(z02) + s22 * z02) * (1 - s11) + s12 * s21 * z02
	
	matrix(c(z11, z21, z12, z22), ncol = 2) / denom
}

#' @rdname network_conversion
#' @export
t_to_abcd <- function(t, z0 = c(50, 50)) {
	t <- coerce_params(t)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	t11 <- t[1, 1]
	t21 <- t[2, 1]
	t12 <- t[1, 2]
	t22 <- t[2, 2]
	
	denom <- 2 * sqrt(Re(z01) * Re(z02))
	
	a <- Conj(z01) * (t11 + t12) + z01 * (t21 + t22)
	b <- Conj(z02) * (t11 * Conj(z01) + t21 * z01) - z02 * (t12 * Conj(z01) + t22 * z01)
	c <- t11 + t12 - t21 - t22
	d <- Conj(z02) * (t11 - t21) - z02 * (t12 - t22)
	
	matrix(c(a, c, b, d), ncol = 2) / denom
}
# #' @rdname network_conversion
# #' @export
# t_to_h <- function(t, z0 = c(50, 50)) {
#   t <- coerce_params(t)
#   z0 <- coerce_z0(z0)
# 
#   z01 <- z0[1]
#   z02 <- z0[2]
# 
#   t11 <- t[1, 1]
#   t21 <- t[2, 1]
#   t12 <- t[1, 2]
#   t22 <- t[2, 2]
# 
#   denom <- Conj(z02) * (t11 - t21) - z02 * (t12 + t22)
# 
#   h11 <- Conj(z02) * (t11 * Conj(z01) + t21 * z01) - z02 * (t12 * Conj(z01) + t22 * z01)
#   h12 <- 2 * sqrt(Re(z01) * Re(z02)) * (t11 * t22 - t12 * t21)
#   h21 <- -2 * sqrt(Re(z01) * Re(z02))
#   h22 <- t11 + t12 - t21 - t22
# 
#   matrix(c(h11, h21, h12, h22), ncol = 2) / denom
# }
#' @rdname network_conversion
#' @export
t_to_s <- function(t, z0 = c(50, 50)) {
	t <- coerce_params(t)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	t11 <- t[1, 1]
	t21 <- t[2, 1]
	t12 <- t[1, 2]
	t22 <- t[2, 2]
	
	denom <- t11
	
	s11 <- t21
	s12 <- t11 * t22 - t12 * t21
	s21 <- 1
	s22 <- -t12
	
	matrix(c(s11, s21, s12, s22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
t_to_y <- function(t, z0 = c(50, 50)) {
	t <- coerce_params(t)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	t11 <- t[1, 1]
	t21 <- t[2, 1]
	t12 <- t[1, 2]
	t22 <- t[2, 2]
	
	denom <- t11 * Conj(z01) * Conj(z02) - t12 * Conj(z01) * z02 + t21 * z01 * Conj(z02) - t22 * z01 * z02
	
	y11 <- Conj(z02) * (t11 - t21) - z02 * (t12 - t22)
	y12 <- -2 * sqrt(Re(z01) * Re(z02)) * (t11 * t22 - t12 * t21)
	y21 <- -2 * sqrt(Re(z01) * Re(z02))
	y22 <- Conj(z01) * (t11 + t12) + z01 * (t21 + t22)
	
	matrix(c(y11, y21, y12, y22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
t_to_z <- function(t, z0 = c(50, 50)) {
	t <- coerce_params(t)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	t11 <- t[1, 1]
	t21 <- t[2, 1]
	t12 <- t[1, 2]
	t22 <- t[2, 2]
	
	denom <- t11 + t12 - t21 - t22
	
	z11 <- Conj(z01) * (t11 + t12) + z01 * (t21 + t22)
	z12 <- 2 * sqrt(Re(z01) * Re(z02)) * (t11 * t22 - t12 * t21)
	z21 <- 2 * sqrt(Re(z01) * Re(z02))
	z22 <- Conj(z02) * (t11 - t21) - z02 * (t12 - t22)
	
	matrix(c(z11, z21, z12, z22), ncol = 2) / denom
}

#' @rdname network_conversion
#' @export
y_to_abcd <- function(y, z0 = c(50, 50)) {
	y <- coerce_params(y)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	y11 <- y[1, 1]
	y21 <- y[2, 1]
	y12 <- y[1, 2]
	y22 <- y[2, 2]
	
	denom <- y21
	
	a <- -y22
	b <- -1
	c <- y12 * y21 - y11 * y22
	d <- -y11
	
	matrix(c(a, c, b, d), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
y_to_h <- function(y, z0 = c(50, 50)) {
	y <- coerce_params(y)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	y11 <- y[1, 1]
	y21 <- y[2, 1]
	y12 <- y[1, 2]
	y22 <- y[2, 2]
	
	denom <- y11
	
	h11 <- 1
	h12 <- -y12
	h21 <- y21
	h22 <- y11 * y22 - y12 * y21
	
	matrix(c(h11, h21, h12, h22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
y_to_s <- function(y, z0 = c(50, 50)) {
	y <- coerce_params(y)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	y11 <- y[1, 1]
	y21 <- y[2, 1]
	y12 <- y[1, 2]
	y22 <- y[2, 2]
	
	denom <- (1 + y11 * z01) * (1 + y22 * z02) - y12 * y21 * z01 * z02
	
	s11 <-
		(1 - y11 * Conj(z01)) * (1 + y22 * z02) + y12 * y21 * Conj(z01) * z02
	s12 <- -2 * y12 * sqrt(Re(z01) * Re(z02))
	s21 <- -2 * y21 * sqrt(Re(z01) * Re(z02))
	s22 <-
		(1 - y22 * Conj(z02)) * (1 + y11 * z01) + y12 * y21 * Conj(z02) * z01
	
	matrix(c(s11, s21, s12, s22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
y_to_t <- function(y, z0 = c(50, 50)) {
	y <- coerce_params(y)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	y11 <- y[1, 1]
	y21 <- y[2, 1]
	y12 <- y[1, 2]
	y22 <- y[2, 2]
	
	denom <- 2 * y21 * sqrt(Re(z01) * Re(z02))
	
	t11 <- (-1 - y11 * z01) * (1 + y22 * z02) + y12 * y21 * z01 * z02
	t12 <-
		(1 + y11 * z01) * (1 - y22 * Conj(z02)) + y12 * y21 * z01 * Conj(z02)
	t21 <-
		(y11 * Conj(z01) - 1) * (1 + y22 * z02) - y12 * y21 * Conj(z01) * z02
	t22 <-
		(1 - y11 * Conj(z01)) * (1 - y22 * Conj(z02)) - y12 * y21 * Conj(z01) *
		Conj(z02)
	
	matrix(c(t11, t21, t12, t22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
y_to_z <- function(y, z0 = c(50, 50)) {
	y <- coerce_params(y)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	y11 <- y[1, 1]
	y21 <- y[2, 1]
	y12 <- y[1, 2]
	y22 <- y[2, 2]
	
	denom <- y11 * y22 - y12 * y21
	
	z11 <- y22
	z12 <- -y12
	z21 <- -y21
	z22 <- y11
	
	matrix(c(z11, z21, z12, z22), ncol = 2) / denom
}

#' @rdname network_conversion
#' @export
z_to_abcd <- function(z, z0 = c(50, 50)) {
	z <- coerce_params(z)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	z11 <- z[1, 1]
	z21 <- z[2, 1]
	z12 <- z[1, 2]
	z22 <- z[2, 2]
	
	denom <- z21
	
	a <- z11
	b <- z11 * z22 - z12 * z21
	c <- 1
	d <- z22
	
	matrix(c(a, c, b, d), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
z_to_h <- function(z, z0 = c(50, 50)) {
	z <- coerce_params(z)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	z11 <- z[1, 1]
	z21 <- z[2, 1]
	z12 <- z[1, 2]
	z22 <- z[2, 2]
	
	denom <- z22
	
	h11 <- z11 * z22 - z12 * z21
	h12 <- z12
	h21 <- -z21
	h22 <- 1
	
	matrix(c(h11, h21, h12, h22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
z_to_s <- function(z, z0 = c(50, 50)) {
	z <- coerce_params(z)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	z11 <- z[1, 1]
	z21 <- z[2, 1]
	z12 <- z[1, 2]
	z22 <- z[2, 2]
	
	denom <- (z11 + z01) * (z22 + z02) - z12 * z21
	
	s11 <- (z11 - Conj(z01)) * (z22 + z02) - z12 * z21
	s12 <- 2 * z12 * sqrt(Re(z01) * Re(z02))
	s21 <- 2 * z21 * sqrt(Re(z01) * Re(z02))
	s22 <- (z11 + z01) * (z22 - Conj(z02)) - z12 * z21
	
	matrix(c(s11, s21, s12, s22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
z_to_t <- function(z, z0 = c(50, 50)) {
	z <- coerce_params(z)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	z11 <- z[1, 1]
	z21 <- z[2, 1]
	z12 <- z[1, 2]
	z22 <- z[2, 2]
	
	denom <- 2 * z21 * sqrt(Re(z01) * Re(z02))
	
	t11 <- (z11 + z01) * (z22 + z02) - z12 * z21
	t12 <- (z11 + z01) * (Conj(z02) - z22) + z12 * z21
	t21 <- (z11 - Conj(z01)) * (z22 + z02) - z12 * z21
	t22 <- (Conj(z01) - z11) * (z22 - Conj(z02)) + z12 * z21
	
	matrix(c(t11, t21, t12, t22), ncol = 2) / denom
}
#' @rdname network_conversion
#' @export
z_to_y <- function(z, z0 = c(50, 50)) {
	z <- coerce_params(z)
	z0 <- coerce_z0(z0)
	
	z01 <- z0[1]
	z02 <- z0[2]
	
	z11 <- z[1, 1]
	z21 <- z[2, 1]
	z12 <- z[1, 2]
	z22 <- z[2, 2]
	
	denom <- z11 * z22 - z12 * z21
	
	y11 <- z22
	y12 <- -z12
	y21 <- -z21
	y22 <- z11
	
	matrix(c(y11, y21, y12, y22), ncol = 2) / denom
}

#' Input/Output Reflection Coefficient
#' 
#' Calculates the input and output reflection coefficients of a two-port network
#' 
#' @param S Network S-parameters,
#' @param gamma_source Source reflection coefficient
#' @param gamma_load Load reflection coefficient
#' @return Gain value
#'
#' @examples
#' S <- matrix(c(complex(modulus = 0.38, argument = -158 / 180 * pi),
#'               complex(modulus = 0.11, argument = 54 / 180 * pi),
#'               complex(modulus = 3.50, argument = 80 / 180 * pi),
#'               complex(modulus = 0.40, argument = -43 / 180 * pi)), nrow = 2, byrow = TRUE)
#'               
#' gamma_source <- z_to_gamma(25)
#' gamma_load <- z_to_gamma(40)
#' 
#' gamma_in(S, gamma_load)
#' gamma_out(S, gamma_source)
#' 
#' @name network_input_output_gamma
#' @export
gamma_in <- function(S, gamma_load) {
	S[1,1] + S[1,2] * S[2,1] * gamma_load / (1 - S[2,2] * gamma_load)
}

#' @rdname network_input_output_gamma
#' @export
gamma_out <- function(S, gamma_source) {
	S[2,2] + S[1,2] * S[2,1] * gamma_source / (1 - S[1,1] * gamma_source)
}

#' Series/Shunt Impedance
#' 
#' Calculates the series or shunt impedance based on ABCD-parameters
#' 
#' @param abcd Network ABCD-parameters,
#' @return Complex impedance
#'
#' @examples
#' abcd <- matrix(c(1,
#'                  complex(modulus = 50, argument = 0),
#'                  complex(modulus = 200, argument = pi / 2),
#'                  1), nrow = 2, byrow = TRUE)
#'               
#' z_series <- abcd_to_series_z(abcd)
#' z_shunt <- abcd_to_shunt_z(abcd)
#' 
#' @name series_shunt_impedance
#' @export
abcd_to_series_z <- function(abcd) {
	abcd <- coerce_params(abcd)
	abcd[1,2]
}

#' @rdname series_shunt_impedance
#' @export
abcd_to_shunt_z <- function(abcd) {
	abcd <- coerce_params(abcd)
	abcd[2,1]
}

#' Electrical Network Gain
#'
#' Calculates various gain metrics for a two-port network
#'
#' @param S Network S-parameters,
#' @param gamma_source Source reflection coefficient
#' @param gamma_load Load reflection coefficient
#'
#' @return Gain value
#'
#' @examples
#' S <- matrix(c(complex(modulus = 0.38, argument = -158 / 180 * pi),
#'               complex(modulus = 0.11, argument = 54 / 180 * pi),
#'               complex(modulus = 3.50, argument = 80 / 180 * pi),
#'               complex(modulus = 0.40, argument = -43 / 180 * pi)), nrow = 2, byrow = TRUE)
#' 
#' gamma_load <- z_to_gamma(40)
#' gamma_source <- z_to_gamma(25)
#' 
#' available_gain(S, gamma_source)
#' power_gain(S, gamma_load)
#' transducer_gain(S, gamma_source, gamma_load)
#' 
#' @name network_gain
#' @export
available_gain <- function(S, gamma_source) {
	Mod(S[2,1])^2 * (1 - Mod(gamma_source)^2) / Mod(1 - S[1,1] * gamma_source)^2 / (1 - Mod(gamma_out(S, gamma_source))^2)
}

#' @rdname network_gain
#' @export
power_gain <- function(S, gamma_load) {
	Mod(S[2,1])^2 * (1 - Mod(gamma_load)^2) / (1 - Mod(gamma_in(S, gamma_load))^2) / Mod(1 - S[2,2] * gamma_load)^2
}

#' @rdname network_gain
#' @export
transducer_gain <- function(S, gamma_source, gamma_load) {
	Mod(S[2,1])^2 * (1 - Mod(gamma_source)^2) * (1 - Mod(gamma_load)^2) / Mod(1 - gamma_source * gamma_in(S, gamma_load))^2 / Mod(1 - S[2,2] * gamma_load)^2
}

#' 3-Port Mixed-Mode Parameters
#'
#' Converts single-ended S parameters to differential.  Functions include:
#' \describe{
#' \item{differential_gamma_3_port}{Differential-mode reflection cofficient}
#' \item{differential_gain_3_port}{Differential-mode insertion gain}
#' \item{differential_reverse_gain_3_port}{Differential-mode reverse insertion gain}
#' \item{common_gain_3_port}{Common-mode insertion gain}
#' \item{common_reverse_gain_3_port}{Common-mode reverse insertion gain}
#' \item{cmrr}{Common-mode rejection ratio}
#' \item{imbalance}{Imbalance between single-ended and differential ports}
#' }
#' Assumes port layout:
#' \enumerate{
#' \item Single-Ended Port
#' \item Balanced (Positive)
#' \item Balanced (Negative)
#' }
#'
#' @param S22 Single-ended reflection on positive balanced port
#' @param S33 Single-ended reflection on negative balanced port
#' @param S32 Single-ended isolation between balanced ports
#' @param S23 Single-ended reverse isolation between balanced ports
#' @param S21 Single-ended gain from unbalanced port to positive balanced port
#' @param S12 Single-ended reverse gain from unbalanced port to positive balanced port
#' @param S31 Single-ended gain from unbalanced port to negative balanced port
#' @param S13 Single-ended reverse gain from unbalanced port to negative balanced port
#' @param S21d Differential-mode gain
#' @param S21c Common-mode gain
#'
#' @examples
#' df <- read_snp(rftk_example("Balun.s3p"), numeric_format = "MA")
#' df <- dplyr::select(df, -ang)
#' df <- tidyr::pivot_wider(df, names_from = "parameter", values_from = "mag")
#' dplyr::mutate(df,
#'               S21ds = differential_gain_3_port(S21, S31),
#'               S21cs = common_gain_3_port(S21, S31),
#'               S12ds = differential_reverse_gain_3_port(S12, S13),
#'               S12cs = common_reverse_gain_3_port(S12, S13),
#'               S22dd = differential_gamma_3_port(S22, S23, S32, S33),
#'               cmrr = cmrr(S21ds, S21cs),
#'               imbalance = imbalance(S21, S31))
#' 
#' @name mixed_mode_s_params_3
#' @export
differential_gamma_3_port <- function(S22, S23, S32, S33) {
	0.5 * (S22 + S33 - S23 - S32)
}

#' @rdname mixed_mode_s_params_3
#' @export
differential_gain_3_port <- function(S21, S31) {
	(S21 - S31) / sqrt(2)
}

#' @rdname mixed_mode_s_params_3
#' @export
differential_reverse_gain_3_port <- function(S12, S13) {
	(S12 - S13) / sqrt(2)
}

#' @rdname mixed_mode_s_params_3
#' @export
common_gain_3_port <- function(S21, S31) {
	(S21 + S31) / sqrt(2)
}

#' @rdname mixed_mode_s_params_3
#' @export
common_reverse_gain_3_port <- function(S12, S13) {
	(S12 + S13) / sqrt(2)
}

#' @rdname mixed_mode_s_params_3
#' @export
cmrr <- function(S21d, S21c) {
	as_decibel(S21d / S21c)
}

#' @rdname mixed_mode_s_params_3
#' @export
imbalance <- function(S21, S31) {
	-S21 / S31
}