library(ggplot2)
library(rftk)

test_that("geom_smith - invalid aes", {
  
  expect_error(ggplot_build(ggplot(dipole, 
  																 aes()) + 
  														geom_smith()),
  						 regexp = "missing aesthetics")
  expect_silent(ggplot_build(ggplot(dipole, 
  																	aes(freq = frequency, 
  																			smag = mag, 
  																			sang = ang * pi / 180)) +
  													 	geom_smith()))
	
})
