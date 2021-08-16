test_that("map_cplx", {
  
	re <- rnorm(10)
  expect_equal(map_cplx(re, 
  											~ .x + 1i), 
  						 re + 1i)
  
})
test_that("map2_cplx", {
	
	re <- rnorm(10)
	im <- rnorm(10)
	expect_equal(map2_cplx(re, im, 
												 ~ .x + .y * 1i), 
							 re + im * 1i)
	
})
test_that("imap_cplx", {
	
	re <- rnorm(10)
	expect_equal(imap_cplx(re,
												 ~ .x + .y * 1i), 
							 re + 1:10 * 1i)
	
})
test_that("map2_cplx", {
	
	re <- rnorm(10)
	im <- rnorm(10)
	mult <- rnorm(10)
	
	expect_equal(pmap_cplx(list(re, im, mult), 
												 ~ (..1 + ..2 * 1i) * ..3), 
							 (re + im * 1i) * mult)
	
})
