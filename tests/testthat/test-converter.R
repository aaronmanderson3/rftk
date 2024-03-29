library(rftk)

test_that("as_decibel - empty arguments", {
	as_decibel() %>% expect_length(0)
})
test_that("as_decibel results", {
	as_decibel(1.5)              %>% expect_equal(3.521,   tolerance = 1e-3)
	as_decibel(1)                %>% expect_equal(0,       tolerance = 1e-3)
	as_decibel(0.5)              %>% expect_equal(-6.021,  tolerance = 1e-3)
	as_decibel(0.25)             %>% expect_equal(-12.041, tolerance = 1e-3)
	as_decibel(0.5, scalar = 10) %>% expect_equal(-3.01,   tolerance = 1e-3)
})

test_that("as_linear - empty arguments", {
	as_linear() %>% expect_length(0)
})
test_that("as_linear - results", {
	as_linear(3)               %>% expect_equal(1.413, tolerance = 1e-3)
	as_linear(0)               %>% expect_equal(1,     tolerance = 1e-3)
	as_linear(-3)              %>% expect_equal(0.708, tolerance = 1e-3)
	as_linear(-6)              %>% expect_equal(0.501, tolerance = 1e-3)
	as_linear(-3, scalar = 10) %>% expect_equal(0.501, tolerance = 1e-3)
})

test_that("gamma_to_mismatch - empty arguments", {
	gamma_to_mismatch() %>% expect_length(0)
})
test_that("gamma_to_mismatch - invalid input", {
	expect_error(gamma_to_mismatch(-0.001))
	expect_error(gamma_to_mismatch(1.0001))
})
test_that("gamma_to_mismatch results", {
	gamma_to_mismatch(0)        %>% expect_equal(0,        tolerance = 1e-3)
	gamma_to_mismatch(0.1)      %>% expect_equal(-0.04365, tolerance = 1e-3)
	gamma_to_mismatch(0.25)     %>% expect_equal(-0.2803,  tolerance = 1e-3)
	gamma_to_mismatch(0.5)      %>% expect_equal(-1.249,   tolerance = 1e-3)
	gamma_to_mismatch(0.5 + 0i) %>% expect_equal(-1.249,   tolerance = 1e-3)
	gamma_to_mismatch(0 + 0.5i) %>% expect_equal(-1.249,   tolerance = 1e-3)
	gamma_to_mismatch(0.75)     %>% expect_equal(-3.59,    tolerance = 1e-3)
	gamma_to_mismatch(0.9)      %>% expect_equal(-7.212,   tolerance = 1e-3)
	gamma_to_mismatch(1)        %>% expect_equal(-Inf,     tolerance = 1e-3)
})

test_that("gamma_to_s11 - empty arguments", {
	gamma_to_s11() %>% expect_length(0)
})
test_that("gamma_to_s11 - invalid input", {
	expect_error(gamma_to_s11(-0.001))
})
test_that("gamma_to_s11 - results", {
	gamma_to_s11(1)        %>% expect_equal(0,      tolerance = 1e-3)
	gamma_to_s11(0.75)     %>% expect_equal(-2.499, tolerance = 1e-3)
	gamma_to_s11(0.5)      %>% expect_equal(-6.021, tolerance = 1e-3)
	gamma_to_s11(0.5 + 0i) %>% expect_equal(-6.021, tolerance = 1e-3)
	gamma_to_s11(0 + 0.5i) %>% expect_equal(-6.021, tolerance = 1e-3)
	gamma_to_s11(0.25)     %>% expect_equal(-12.04, tolerance = 1e-3)
	gamma_to_s11(0)        %>% expect_equal(-Inf,   tolerance = 1e-3)
})

test_that("gamma_to_z - empty arguments", {
	gamma_to_z() %>% expect_length(0)
})
test_that("gamma_to_z - invalid input", {
	expect_warning(gamma_to_z(0.5))
	expect_error(suppressWarnings(gamma_to_z(1)))
	expect_error(gamma_to_z(complex(modulus = 1)))
	expect_error(gamma_to_z(0.5 + 0i, z0 = NULL))
	expect_error(gamma_to_z(0.5 + 0i, z0 = c(50, 100)))
	expect_error(gamma_to_z(0.5 + 0i, z0 = -0.001))
	expect_error(gamma_to_z(0.5 + 0i, z0 = -0.001 + 10i))
})
test_that("gamma_to_z - results (z0 = default)", {
	expect_equal(suppressWarnings(gamma_to_z(0.01)), 51.01, tolerance = 0.01)
	expect_equal(suppressWarnings(gamma_to_z(0.25)), 83.33, tolerance = 0.01)
	expect_equal(suppressWarnings(gamma_to_z(0.5)),  150,   tolerance = 0.01)
	expect_equal(suppressWarnings(gamma_to_z(0.75)), 350,   tolerance = 0.01)
	expect_equal(suppressWarnings(gamma_to_z(0.99)), 9950,  tolerance = 0.01)
	
	gamma_to_z(complex(modulus = 0.01, argument = 0   * pi / 180)) %>% expect_equal(51.01  + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 45  * pi / 180)) %>% expect_equal(50.71  + 0.7172i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 90  * pi / 180)) %>% expect_equal(49.99  + 0.9999i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 135 * pi / 180)) %>% expect_equal(49.29  + 0.6972i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 180 * pi / 180)) %>% expect_equal(49.01  + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 225 * pi / 180)) %>% expect_equal(49.29  - 0.6972i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 270 * pi / 180)) %>% expect_equal(49.99  - 0.9999i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.01, argument = 315 * pi / 180)) %>% expect_equal(50.71  - 0.7172i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 0   * pi / 180)) %>% expect_equal(83.33  + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 45  * pi / 180)) %>% expect_equal(66.12  + 24.94i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 90  * pi / 180)) %>% expect_equal(44.12  + 23.53i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 135 * pi / 180)) %>% expect_equal(33.1   + 12.48i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 180 * pi / 180)) %>% expect_equal(30     + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 225 * pi / 180)) %>% expect_equal(33.1   - 12.48i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 270 * pi / 180)) %>% expect_equal(44.12  - 23.53i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.25, argument = 315 * pi / 180)) %>% expect_equal(66.12  - 24.94i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 0   * pi / 180)) %>% expect_equal(150    + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 45  * pi / 180)) %>% expect_equal(69.07  + 65.12i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 90  * pi / 180)) %>% expect_equal(30     + 40i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 135 * pi / 180)) %>% expect_equal(19.16  + 18.07i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 180 * pi / 180)) %>% expect_equal(16.67  + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 225 * pi / 180)) %>% expect_equal(19.16  - 18.07i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 270 * pi / 180)) %>% expect_equal(30     - 40i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5,  argument = 315 * pi / 180)) %>% expect_equal(69.07  - 65.12i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 0   * pi / 180)) %>% expect_equal(350    + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 45  * pi / 180)) %>% expect_equal(43.59  + 105.7i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 90  * pi / 180)) %>% expect_equal(14     + 48i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 135 * pi / 180)) %>% expect_equal(8.339  + 20.22i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 180 * pi / 180)) %>% expect_equal(7.143  + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 225 * pi / 180)) %>% expect_equal(8.339  - 20.22i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 270 * pi / 180)) %>% expect_equal(14     - 48i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.75, argument = 315 * pi / 180)) %>% expect_equal(43.59  - 105.7i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 0   * pi / 180)) %>% expect_equal(9950   + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 45  * pi / 180)) %>% expect_equal(1.715  + 120.7i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 90  * pi / 180)) %>% expect_equal(0.5025 + 50i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 135 * pi / 180)) %>% expect_equal(0.2944 + 20.71i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 180 * pi / 180)) %>% expect_equal(0.2513 + 0i,      tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 225 * pi / 180)) %>% expect_equal(0.2944 - 20.71i,  tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 270 * pi / 180)) %>% expect_equal(0.5025 - 50i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.99, argument = 315 * pi / 180)) %>% expect_equal(1.715  - 120.7i,  tolerance = 0.01)
})
test_that("gamma_to_z - results (z0 = 100)", {
	expect_equal(suppressWarnings(gamma_to_z(0.5, z0 = 100)), 300, tolerance = 0.01)
	
	gamma_to_z(complex(modulus = 0.5, argument = 0   * pi / 180), z0 = 100) %>% expect_equal(300   + 0i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 45  * pi / 180), z0 = 100) %>% expect_equal(138.1 + 130.2i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 90  * pi / 180), z0 = 100) %>% expect_equal(60    + 80i,    tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 135 * pi / 180), z0 = 100) %>% expect_equal(38.32 + 36.13i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 180 * pi / 180), z0 = 100) %>% expect_equal(33.33 + 0i,     tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 225 * pi / 180), z0 = 100) %>% expect_equal(38.32 - 36.13i, tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 270 * pi / 180), z0 = 100) %>% expect_equal(60    - 80i,    tolerance = 0.01)
	gamma_to_z(complex(modulus = 0.5, argument = 315 * pi / 180), z0 = 100) %>% expect_equal(138.1 - 130.2i, tolerance = 0.01)
})

test_that("mismatch_to_gamma - empty arguments", {
	mismatch_to_gamma() %>% expect_length(0)
})
test_that("mismatch_to_gamma - invalid input", {
	expect_error(mismatch_to_gamma(0.001))
})
test_that("mismatch_to_gamma - results", {
	mismatch_to_gamma(0)    %>% expect_equal(0,      tolerance = 1e-3)
	mismatch_to_gamma(-1)   %>% expect_equal(0.4535, tolerance = 1e-3)
	mismatch_to_gamma(-3)   %>% expect_equal(0.706,  tolerance = 1e-3)
	mismatch_to_gamma(-6)   %>% expect_equal(0.865,  tolerance = 1e-3)
	mismatch_to_gamma(-10)  %>% expect_equal(0.949,  tolerance = 1e-3)
	mismatch_to_gamma(-Inf) %>% expect_equal(1,      tolerance = 1e-3)
})

test_that("s11_to_gamma - empty arguments", {
	s11_to_gamma() %>% expect_length(0)
})
test_that("s11_to_gamma - results", {
	s11_to_gamma(3)  %>% expect_equal(1.413, tolerance = 1e-3)
	s11_to_gamma(0)  %>% expect_equal(1,     tolerance = 1e-3)
	s11_to_gamma(-3) %>% expect_equal(0.708, tolerance = 1e-3)
	s11_to_gamma(-6) %>% expect_equal(0.501, tolerance = 1e-3)
})

test_that("z_to_gamma - empty arguments", {
	z_to_gamma() %>% expect_length(0)
})
test_that("z_to_gamma - invalid input", {
	expect_error(z_to_gamma(-0.001))
	expect_error(z_to_gamma(-0.001 + 10i))
	expect_error(z_to_gamma(50 + 0i, z0 = NULL))
	expect_error(z_to_gamma(50 + 0i, z0 = c(50, 100)))
	expect_error(z_to_gamma(50 + 0i, z0 = -0.001))
	expect_error(z_to_gamma(50 + 0i, z0 = -0.001 + 10i))
})
test_that("z_to_gamma - results (z0 = default)", {
	z_to_gamma(1)   %>% expect_equal(-0.9608, tolerance = 1e-3)
	z_to_gamma(25)  %>% expect_equal(-0.3333, tolerance = 1e-3)
	z_to_gamma(50)  %>% expect_equal(0,      tolerance = 1e-3)
	z_to_gamma(100) %>% expect_equal(0.3333, tolerance = 1e-3)
	z_to_gamma(200) %>% expect_equal(0.6,    tolerance = 1e-3)
	z_to_gamma(500) %>% expect_equal(0.8182, tolerance = 1e-3)
	
	z_to_gamma(1   + 0i)    %>% expect_equal(complex(modulus = 0.9608, argument = 180    / 180 * pi), tolerance = 1e-3)
	z_to_gamma(1   + 25i)   %>% expect_equal(complex(modulus = 0.9685, argument = 126.9  / 180 * pi), tolerance = 1e-2)
	z_to_gamma(1   + 100i)  %>% expect_equal(complex(modulus = 0.992,  argument = 53.13  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(1   + 200i)  %>% expect_equal(complex(modulus = 0.9976, argument = 28.07  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(1   + -200i) %>% expect_equal(complex(modulus = 0.9976, argument = -28.07 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(1   + -100i) %>% expect_equal(complex(modulus = 0.992,  argument = -53.13 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(1   + -25i)  %>% expect_equal(complex(modulus = 0.9685, argument = -126.9 / 180 * pi), tolerance = 1e-2)
	z_to_gamma(25  + 0i)    %>% expect_equal(complex(modulus = 0.3333, argument = 180    / 180 * pi), tolerance = 1e-2)
	z_to_gamma(25  + 50i)   %>% expect_equal(complex(modulus = 0.6202, argument = 82.87  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(25  + 200i)  %>% expect_equal(complex(modulus = 0.9436, argument = 27.68  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(25  + -200i) %>% expect_equal(complex(modulus = 0.9436, argument = -27.68 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(25  + -50i)  %>% expect_equal(complex(modulus = 0.6202, argument = -82.87 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50  + 0i)    %>% expect_equal(complex(modulus = 0,      argument = 0      / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50  + 100i)  %>% expect_equal(complex(modulus = 0.7071, argument = 45     / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50  + -100i) %>% expect_equal(complex(modulus = 0.7071, argument = -45    / 180 * pi), tolerance = 1e-3)
	z_to_gamma(100 + 0i)    %>% expect_equal(complex(modulus = 0.3333, argument = 0      / 180 * pi), tolerance = 1e-3)
	z_to_gamma(100 + 100i)  %>% expect_equal(complex(modulus = 0.6202, argument = 29.74  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(100 + -100i) %>% expect_equal(complex(modulus = 0.6202, argument = -29.74 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(200 + 0i)    %>% expect_equal(complex(modulus = 0.6,    argument = 0      / 180 * pi), tolerance = 1e-3)
	z_to_gamma(200 + 200i)  %>% expect_equal(complex(modulus = 0.7809, argument = 14.47  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(200 + -200i) %>% expect_equal(complex(modulus = 0.7809, argument = -14.47 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(500 + 0i)    %>% expect_equal(complex(modulus = 0.8182, argument = 0      / 180 * pi), tolerance = 1e-3)
	z_to_gamma(500 + 500i)  %>% expect_equal(complex(modulus = 0.905,  argument = 5.739  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(500 + -500i) %>% expect_equal(complex(modulus = 0.905,  argument = -5.739 / 180 * pi), tolerance = 1e-3)
})
test_that("z_to_gamma - results (z0 = 100)", {
	z_to_gamma(50, z0 = 100) %>% expect_equal(-0.3333, tolerance = 1e-3)
	
	z_to_gamma(50 + 0i,    z0 = 100) %>% expect_equal(complex(modulus = 0.3333, argument = 180    / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50 + 50i,   z0 = 100) %>% expect_equal(complex(modulus = 0.4472, argument = 116.6  / 180 * pi), tolerance = 1e-2)
	z_to_gamma(50 + 200i,  z0 = 100) %>% expect_equal(complex(modulus = 0.8246, argument = 50.91  / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50 + -200i, z0 = 100) %>% expect_equal(complex(modulus = 0.8246, argument = -50.91 / 180 * pi), tolerance = 1e-3)
	z_to_gamma(50 + -50i,  z0 = 100) %>% expect_equal(complex(modulus = 0.4472, argument = -116.6 / 180 * pi), tolerance = 1e-2)
})


test_that("mismatch_to_s11 - empty arguments", {
	mismatch_to_s11() %>% expect_length(0)
})
test_that("mismatch_to_s11 - invalid input", {
	expect_error(mismatch_to_s11(0.001))
})
test_that("mismatch_to_s11 - results", {
	mismatch_to_s11(0) %>% expect_equal(-Inf, tolerance = 1e-3)
	mismatch_to_s11(-1) %>% expect_equal(-6.868, tolerance = 1e-3)
	mismatch_to_s11(-3) %>% expect_equal(-3.021, tolerance = 1e-3)
	mismatch_to_s11(-6) %>% expect_equal(-1.256, tolerance = 1e-3)
	mismatch_to_s11(-10) %>% expect_equal(-0.458, tolerance = 1e-3)
	mismatch_to_s11(-Inf) %>% expect_equal(0, tolerance = 1e-3)
})

test_that("s11_to_mismatch - empty arguments", {
	s11_to_mismatch() %>% expect_length(0)
})
test_that("s11_to_mismatch - invalid input", {
	expect_error(s11_to_mismatch(0.0001))
})
test_that("s11_to_mismatch - results", {
	s11_to_mismatch(0) %>% expect_equal(-Inf, tolerance = 1e-3)
	s11_to_mismatch(-1) %>% expect_equal(-6.868, tolerance = 1e-3)
	s11_to_mismatch(-3) %>% expect_equal(-3.021, tolerance = 1e-3)
	s11_to_mismatch(-6) %>% expect_equal(-1.256, tolerance = 1e-3)
	s11_to_mismatch(-10) %>% expect_equal(-0.458, tolerance = 1e-3)
	s11_to_mismatch(-Inf) %>% expect_equal(0, tolerance = 1e-3)
})

test_that("z_to_s11 - empty arguments", {
	z_to_s11() %>% expect_length(0)
})
test_that("z_to_s11 - invalid input", {
	expect_error(z_to_s11(-0.001))
	expect_error(z_to_s11(-0.001 + 10i))
	expect_error(z_to_s11(50 + 0i, z0 = NULL))
	expect_error(z_to_s11(50 + 0i, z0 = c(50, 100)))
	expect_error(z_to_s11(50 + 0i, z0 = -0.001))
	expect_error(z_to_s11(50 + 0i, z0 = -0.001 + 10i))
})
test_that("z_to_s11 - results (z0 = default)", {
	z_to_s11(1)   %>% expect_equal(-0.3475, tolerance = 1e-3)
	z_to_s11(25)  %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(50)  %>% expect_equal(-Inf, tolerance = 1e-3)
	z_to_s11(100) %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(200) %>% expect_equal(-4.437, tolerance = 1e-3)
	z_to_s11(500) %>% expect_equal(-1.743, tolerance = 1e-3)
	
	z_to_s11(1 + 0i)   %>% expect_equal(-0.3475, tolerance = 1e-3)
	z_to_s11(25 + 0i)  %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(50 + 0i)  %>% expect_equal(-Inf, tolerance = 1e-3)
	z_to_s11(100 + 0i) %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(200 + 0i) %>% expect_equal(-4.437, tolerance = 1e-3)
	z_to_s11(500 + 0i) %>% expect_equal(-1.743, tolerance = 1e-3)
})
test_that("z_to_s11 - results (z0 = 100)", {
	z_to_s11(50, z0 = 100)  %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(100, z0 = 100)  %>% expect_equal(-Inf, tolerance = 1e-3)
	
	z_to_s11(50 + 0i, z0 = 100)  %>% expect_equal(-9.542, tolerance = 1e-3)
	z_to_s11(100 + 0i, z0 = 100)  %>% expect_equal(-Inf, tolerance = 1e-3)
})

test_that("z_to_mismatch - empty arguments", {
	z_to_mismatch() %>% expect_length(0)
})
test_that("z_to_mismatch - invalid input", {
	expect_error(z_to_mismatch(-0.001))
	expect_error(z_to_mismatch(-0.001 + 10i))
	expect_error(z_to_mismatch(50 + 0i, z0 = NULL))
	expect_error(z_to_mismatch(50 + 0i, z0 = c(50, 100)))
	expect_error(z_to_mismatch(50 + 0i, z0 = -0.001))
	expect_error(z_to_mismatch(50 + 0i, z0 = -0.001 + 10i))
	
})
test_that("z_to_mismatch works (z0 = default)", {
	z_to_mismatch(1)   %>% expect_equal(-11.141, tolerance = 1e-3)
	z_to_mismatch(25)  %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(50)  %>% expect_equal(0, tolerance = 1e-3)
	z_to_mismatch(100) %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(200) %>% expect_equal(-1.938, tolerance = 1e-3)
	z_to_mismatch(500) %>% expect_equal(-4.807, tolerance = 1e-3)
	
	z_to_mismatch(1 + 0i)   %>% expect_equal(-11.141, tolerance = 1e-3)
	z_to_mismatch(25 + 0i)  %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(50 + 0i)  %>% expect_equal(0, tolerance = 1e-3)
	z_to_mismatch(100 + 0i) %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(200 + 0i) %>% expect_equal(-1.938, tolerance = 1e-3)
	z_to_mismatch(500 + 0i) %>% expect_equal(-4.807, tolerance = 1e-3)
})
test_that("z_to_mismatch works (z0 = 100)", {
	z_to_mismatch(50, z0 = 100)  %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(100, z0 = 100)  %>% expect_equal(0, tolerance = 1e-3)
	
	z_to_mismatch(50 + 0i, z0 = 100)  %>% expect_equal(-0.512, tolerance = 1e-3)
	z_to_mismatch(100 + 0i, z0 = 100)  %>% expect_equal(0, tolerance = 1e-3)
})

