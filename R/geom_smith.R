GeomSmith <- ggproto("GeomSmith", 
										 GeomLine,
										 default_aes = aes(colour = "black", size = 1, linetype = 1, alpha = NA),
										 required_aes = c("freq", "smag", "sang"),
										 setup_data = function(data, params) {
										 	
										 	data <- rename(data, x = smag, y = sang)
										 	data <- data[order(data$PANEL, data$group, data$freq), ]
										 	
										 })


#' Smith Chart
#'
#' `geom_smith()` displays a smith chart based on the frequency and s-parameters.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "smith")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#' @examples
#' if(require(ggplot2)) {
#' 
#'   ggplot(dipole, 
#'          aes(freq = frequency, 
#'              smag = mag,
#'              sang = ang * pi / 180,
#'              color = frequency)) +
#'     geom_smith()
#'  
#' }
#' @export
geom_smith <- function(mapping = NULL, 
											 data = NULL, 
											 stat = "identity",
											 position = "identity", 
											 ..., 
											 lineend = "round",
											 linejoin = "round",
											 linemitre = 10,
											 arrow = NULL,
											 na.rm = FALSE, 
											 show.legend = NA,
											 inherit.aes = TRUE)
{
	c(layer(data = data, 
					mapping = mapping, 
					stat = stat, 
					geom = GeomSmith,
					position = position, 
					show.legend = show.legend, 
					inherit.aes = inherit.aes,
					params = list(lineend = lineend,
												linejoin = linejoin,
												linemitre = linemitre,
												arrow = arrow,
												na.rm = na.rm, 
												...)),
		coord_smith())
}
