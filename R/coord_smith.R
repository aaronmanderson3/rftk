r_rescale <- function(coord, x, range) {
  scales::rescale(x, c(0, 0.45), range)
}

CoordSmith <- ggproto(
  "CoordSmith",
  CoordPolar,
  
  backtransform_range = function(self, panel_params) {
    setNames(
      list(panel_params$theta.range, panel_params$r.range),
      c(self$theta, self$r))
  },
  
  distance = function(self, x, y, details) {
    
    # this is needed to override coord_munch within GeomLine (which is inherited in GeomSmith)
    0.001
    
  },
  
  range = function(self, panel_params) {
    setNames(
      list(panel_params$theta.range, panel_params$r.range),
      c(self$theta, self$r)
    )
  },
  
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    list(r.range = c(0, 1),
         r.major = 1,
         theta.range = c(-pi,pi),
         r.arrange = c("primary", "secondary")
    )
  },
  
  render_axis_v = function(self, panel_params, theme) {
    list(left = zeroGrob(),
         right = zeroGrob())
  },
  
  render_axis_h = function(panel_params, theme) {
    list(top = zeroGrob(),
         bottom = zeroGrob())
  },
  
  render_bg = function(self, panel_params, theme) {
    
    scale <- 0.4
    n_theta <- 100
    
    panel_params <- ggplot2:::rename_data(self, panel_params)
    
    normalized_impedances <- c(0, 0.2, 0.5, 1, 2, 5, 10, 20, 50)
    
    # Real Impedance
    theta <- seq(0, 2 * pi, length.out = n_theta)
    radius <- (1 - (normalized_impedances - 1) / (normalized_impedances + 1)) / 2
    
    x_real = c(t((1 + radius %o% (cos(theta) - 1)) * scale + 0.5))
    y_real = c(t(radius %o% sin(theta) * scale + 0.5))
    
    # Imaginary Impedance
    circle_intercept_ang = Arg((normalized_impedances * 1i - 1) / (normalized_impedances * 1i + 1))
    arc_radius = 1 * tan(circle_intercept_ang / 2)
    
    theta = t(sapply(circle_intercept_ang,
                     function(x) seq(from = 0,
                                     to = pi - x,
                                     length.out = n_theta)))
    
    
    x_imaginary = c(t((1 - arc_radius * sin(theta)) * scale + 0.5))
    y_imaginary = c(t((arc_radius - arc_radius * cos(theta)) * scale + 0.5))
    
    ggplot2:::ggname(
      "grill",
      grid::grobTree(
        ggplot2:::element_render(theme, "panel.background"),
        ggplot2:::element_render(theme, "panel.grid.major", name = "real",
                                 x = x_real,
                                 y = y_real,
                                 id.lengths = rep(n_theta, length(normalized_impedances))),
        ggplot2:::element_render(theme, "panel.grid.major", name = "imaginary_pos",
                                 x = x_imaginary,
                                 y = y_imaginary,
                                 id.lengths = rep(n_theta, length(normalized_impedances))),
        ggplot2:::element_render(theme, "panel.grid.major", name = "imaginary_neg",
                                 x = x_imaginary,
                                 y = 1 - y_imaginary,
                                 id.lengths = rep(n_theta, length(normalized_impedances))),
        ggplot2:::element_render(theme, "panel.grid.major", name = "imaginary_zero",
                                 x = c(-1, 1) * scale + 0.5,
                                 y = c(0.5, 0.5))))
  },
  
  labels = function(self, labels, panel_params) {
    list(x = NULL,
         y = NULL)
  }
)


#' Smith Coordinates
#'
#' @details Plots S-parameter data on a smith chart.  Input data currently needs to be in magnitude/angle (rad).  No scales are provided, data is scaled to z0.
#' @seealso [geom_smith()]
#' @examples
#' if(require(ggplot2)) {
#'
#'  ggplot(dipole, aes(x = ang, y = mag)) +
#'    geom_point() +
#'    coord_smith()
#'    
#' }
#' @export
coord_smith <- function() {
  
  ggproto(
    NULL,
    CoordSmith,
    theta = "y",
    r = "x",
    start = pi / 2,
    direction = -1,
    clip = "off",
    range = c(0, 1)
  )
}