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