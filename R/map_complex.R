complex_mapper <- function(method, ...) {
	exec(method, ...) %>%
		as_vector(.type = "complex")
}
	
#' Complex Mapping Functions
#'
#' The complex mapping functions transform the output lists from common mapping functions found in the `purrr` package to complex vectors.
#' 
#' Supported `purrr` mapping functions:
#' \describe{
#' \item{[purrr::map]}{Call `.f` using an argument of `.x`}
#' \item{[purrr::map2]}{Call `.f` using arguments of `.x` and `.y`}
#' \item{[purrr::imap]}{Call `.f` using arguments of `.x` and `names(.x)`}
#' \item{[purrr::pmap]}{Call `.f` using arguments contained in `.l`}
#' }
#'
#' @param .x A list or atomic vector
#' @param .y A vector of the same length as `.x`. A vector of length 1 will be recycled.
#' @param .f A function, formula, or vector (not necessarily atomic).
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function. There
#'   are three ways to refer to the arguments:
#'
#'   * For a single argument function, use `.`
#'   * For a two argument function, use `.x` and `.y`
#'   * For more arguments, use `..1`, `..2`, `..3` etc
#'
#'   This syntax allows you to create very compact anonymous
#'   functions. Note that formula functions conceptually take dots
#'   (that's why you can use `..1` etc). They silently ignore
#'   additional arguments that are not used in the formula expression.
#'
#'   If __character vector__, __numeric vector__, or __list__, it is
#'   converted to an extractor function. Character vectors index by
#'   name and numeric vectors index by position; use a list to index
#'   by position and name at different levels. If a component is not
#'   present, the value of `.default` will be returned.
#' @param .l A list of vectors, such as a data frame. The length of `.l` determines the number of arguments that `.f` will be called with. List names will be used if present.
#' @param ... Arguments to the mapping function
#'
#' @return A [complex] vector coerced from the mapping function and arguments
#' @seealso [complex], [purrr::map], [purrr::map2]
#' 
#' @name complex_map
#' @export
map_cplx       <- function(.x, .f, ...) complex_mapper(purrr::map, .x, .f, ...)

#' @rdname complex_map
#' @export
map2_cplx      <- function(.x, .y, .f, ...) complex_mapper(purrr::map2, .x, .y, .f, ...)

#' @rdname complex_map
#' @export
imap_cplx      <- function(.x, .f, ...) complex_mapper(purrr::imap, .x, .f, ...)

#' @rdname complex_map
#' @export
pmap_cplx      <- function(.l, .f, ...) complex_mapper(purrr::pmap, .l, .f, ...)