library(rftk)

test_that("complex_approx - empty arguments", {
	expect_error(complex_approx())
	expect_error(complex_approx(1:3))
})
test_that("complex_approx - invalid input", {
	expect_error(complex_approx(NULL, 1:3))
	expect_error(complex_approx(1:3, NULL))
	expect_error(complex_approx(1:2, 1:3))
})
test_that("complex_approx results", {
	complex_approx(1:11, -5:5 + -5:5 * -1i, n = 5) %>% expect_equal(tribble(~x,  ~y,
																																					1,   -5.0 + 5.0i,
																																					3.5, -2.5 + 2.5i,
																																					6,    0.0 + 0.0i,
																																					8.5,  2.5 - 2.5i,
																																					11,   5.0 - 5.0i))
})