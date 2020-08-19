#' Get path to rftk example
#'
#' rftk comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' rftk_example()
#' rftk_example("dipole.s1p")
rftk_example <- function(path = NULL) {
	if (is.null(path)) {
		dir(system.file("extdata", package = "rftk"))
	} else {
		system.file("extdata", path, package = "rftk", mustWork = TRUE)
	}
}