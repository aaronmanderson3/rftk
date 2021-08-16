#' Number Conversion
#'
#' Conversions between linear and decibel number formats
#'
#' @param ... Number(s) to convert
#' @param scalar Decibel conversion scalar (i.e. 10 for power, 20 for
#'   voltage/current)
#' @examples
#' as_decibel(0.45)
#' as_decibel(0.45, scalar = 20)
#' as_decibel(c(0.1,0.5,0.9))
#' as_linear(-6)
#' as_linear(-6, scalar = 20)
#' as_linear(c(-12,-6,-3))
#' gamma_to_z
#' @name numeric_format
#' @export
as_decibel <- function(..., scalar = 20) {

  # convert input to vector
  mag <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(mag))
    return(numeric(0))
  
  scalar * log10(Mod(mag))
}

#' @rdname numeric_format
#' @export
as_linear <- function(..., scalar = 20) {
  
  # convert input to vector
  dB <- unlist(list(...))
  
  # no need for emptiness checking, will return `numeric(0)` automatically
  
  10 ^ (dB / scalar)
}

#' Impedance Conversion
#'
#' Conversion between the reflection coefficient (gamma), S11, impedance, and
#' mismatch gain:
#'
#' \strong{Fundamental conversions:}\describe{
#' \item{`gamma_to_mismatch()`}{Converts the input reflection coefficient to
#' mismatch gain.} \item{`gamma_to_s11()`}{Converts the input reflection
#' coefficient to return \emph{gain}.} \item{`gamma_to_z()`}{Converts the input
#' reflection coefficient to impedance.  If the input is a complex number,
#' complex impedance will be output, and if the input is a scalar or magnitude,
#' only a scalar impedance is output.} \item{`mismatch_to_gamma()`}{Converts the
#' input mismatch gain to the reflection coefficient (magnitude only).}
#' \item{`s11_to_gamma()`}{Converts the input return \emph{gain} to the
#' reflection coefficient (magnitude only).} \item{`z_to_gamma()`}{Converts the
#' input impedance to the reflection coefficient.  If the input is a complex
#' number, the complex reflection coefficient will be output, and if the input
#' is a scalar or magnitude, only the reflection coefficient magnitude will be
#' output.} } \strong{Additional conversions:}\describe{
#' \item{`mismatch_to_s11()`}{Converts the input mismatch gain to return gain.}
#' \item{`s11_to_mismatch()`}{Converts the input return gain to mismatch gain.}
#' \item{`z_to_s11()`}{Converts the input impedance to return gain.}
#' \item{`z_to_mismatch()`}{Converts the input impedance to mismatch gain.} }
#'
#' @param ... Input reflection coefficient, return gain, impedance, or mismatch
#'   gain
#' @param z0 Characteristic impedance
#' @examples
#' # S11 --> reflection coefficient
#' s11_to_gamma(-3)
#'
#' # Reflection coefficient --> S11
#' gamma_to_s11(0.5)
#'
#' # Reflection coefficient --> impedance
#' gamma_to_z(0.5 + 0.5i, z0 = 50)
#' @name impedance_conversion
#' @export
gamma_to_mismatch <- function(...) {
  
  # convert input to vector
  gamma <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(gamma))
    return(numeric(0))
  
  # convert complex vector to numeric vector
  if(is.complex(gamma))
    gamma <- Mod(gamma)
  
  # throw error if gamma is out of range
  if(any(gamma > 1) | any(gamma < 0))
    stop("Reflection coefficient must be in the range [0, 1]")
  
  as_decibel(1 - gamma^2, scalar = 10)
}

#' @rdname impedance_conversion
#' @export
gamma_to_s11 <- function(...) {
  
  # convert input to vector
  gamma <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(gamma))
    return(numeric(0))
  
  # convert complex vector to numeric vector
  if(is.complex(gamma))
    gamma <- Mod(gamma)
  
  # throw error if gamma is out of range
  if(any(gamma < 0))
    stop("Reflection coefficient must be greater than zero")
  
  as_decibel(gamma, scalar = 20)
}

#' @rdname impedance_conversion
#' @export
gamma_to_z <- function(..., z0 = 50) {
  
  # convert input to vector
  gamma <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(gamma))
    return(complex(0))
  
  # if non-complex, throw warning
  if(!is.complex(gamma))
    warning("Non-complex reflection coefficient input.  Only real impedance will be output.")
  
  # throw error if gamma is out of range
  if(is.complex(gamma)) {
    if(any(Mod(gamma) >= 1) | any(Mod(gamma) < 0))
      stop("Reflection coefficient magnitude must be in the range [0, 1)")
  }
  else {
    if(any(gamma >= 1) | any(gamma < 0))
      stop("Reflection coefficient must be in the range [0, 1)")
  }
  
  # check for valid characteristic impedance
  if(length(z0) != 1)
    stop("Characteristic impedance must contain only one element")
  if(is.complex(z0) & Re(z0) < 0)
    stop("Real part of characteristic impedance must be greater than zero")
  else if(is.numeric(z0) & z0 < 0)
    stop("Characteristic impedance must be greater than zero")

  (1 + gamma) / (1 - gamma) * z0
}

#' @rdname impedance_conversion
#' @export
mismatch_to_gamma <- function(...) {
  
  # convert input to vector
  mismatch <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(mismatch))
    return(numeric(0))
  
  # throw error for positive mismatch gains
  if(any(mismatch > 0))
    stop("Mismatch gain must be less than zero")
  
  sqrt(1 - 10^(mismatch/10))
}

#' @rdname impedance_conversion
#' @export
s11_to_gamma <- function(...) {
  
  # convert input to vector
  s11 <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(s11))
    return(numeric(0))
  
  as_linear(s11, scalar = 20)
}

#' @rdname impedance_conversion
#' @export
z_to_gamma <- function(..., z0 = 50) {
  
  # convert input to vector
  z <- unlist(list(...))
  
  # return empty vector if no input
  if(is.null(z))
    return(complex(0))
  
  # check for valid impedance
  if(any(Re(z) < 0))
    stop("Real impedance must be positive")

  # check for valid characteristic impedance
  if(length(z0) != 1)
    stop("Characteristic impedance must contain only one element")
  if(is.complex(z0) & Re(z0) < 0)
    stop("Real part of characteristic impedance must be greater than zero")
  else if(is.numeric(z0) & z0 < 0)
    stop("Characteristic impedance must be greater than zero")
  
  (z - z0) / (z + z0)
}


#' @rdname impedance_conversion
#' @export
mismatch_to_s11 <- function(...) {
  gamma_to_s11(mismatch_to_gamma(...))
}

#' @rdname impedance_conversion
#' @export
s11_to_mismatch <- function(...){
  gamma_to_mismatch(s11_to_gamma(...))
}

#' @rdname impedance_conversion
#' @export
z_to_s11 <- function(..., z0 = 50) {
  gamma_to_s11(Mod(z_to_gamma(..., z0 = z0)))
}

#' @rdname impedance_conversion
#' @export
z_to_mismatch <- function(..., z0 = 50) {
  gamma_to_mismatch(Mod(z_to_gamma(..., z0 = z0)))
}

