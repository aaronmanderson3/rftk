library(rftk)

test_that("linear_mean - empty arguments", {
	expect_error(linear_mean())
})
test_that("linear_mean - results", {
	linear_mean(-88:-90) %>% expect_equal(-88.92358, tolerance = 1e-5)
})
