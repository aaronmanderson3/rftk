library(rftk)

test_that("coerce_params - invalid arguments", {
	expect_error(coerce_params(NULL))
	expect_error(coerce_params(NA))
	expect_error(coerce_params(c(50,50,50)))
	expect_error(coerce_params(numeric()))
})
test_that("coerce_params - results", {
	
	test_vector <- c(11,21,12,22)
	test_matrix <- matrix(test_vector, nrow = 2)
	
	coerce_params(test_vector) %>% expect_equal(test_matrix)
	coerce_params(test_matrix) %>% expect_equal(test_matrix)
})
test_that("coerce_z0 - invalid arguments", {
	expect_error(coerce_z0(c(50,50,50)))
	expect_error(coerce_z0(numeric()))
})
test_that("coerce_z0 - results", {
	coerce_z0(NULL)               %>% expect_equal(c(50,50))
	coerce_z0(NA)                 %>% expect_equal(c(50,50))
	coerce_z0(50-25i)             %>% expect_equal(c(50-25i, 50-25i))
	coerce_z0(c(50+50i, 25+100i)) %>% expect_equal(c(50+50i, 25+100i))
})
test_that("abcd transformations - results", {

	abcd <- matrix(c( 0.999884396265344      + 0.000129274757618717i,
									  0.314079483671772      + 2.51935878310427i,
									 -6.56176712108866e-007  + 6.67455405306704e-006i,
									  0.999806365547959      + 0.000247230611054075i),
								 nrow = 2,
								 byrow = T)
	expected <- list(h     = c( 0.31476   + 2.5198i,     0.9999    + 0.00012883i,
														 -1.0002    + 0.00024733i, 0         + 0i),
									 s_50  = c( 0.0038184 + 0.024797i,   0.99611   - 0.024999i,
									 					  0.99639   - 0.025381i,   0.0037436 + 0.024916i),
									 s_100 = c( 0.0017993 + 0.012164i,   0.99816   - 0.012701i,
									 					  0.99845   - 0.01308i,    0.0017229 + 0.012282i),
									 t_50  = c( 1.003     + 0.025549i,  -0.0031182 - 0.025086i,
									 					  0.0031962 + 0.024968i,   0.99672   - 0.025172i),
									 t_100 = c( 1.0014    + 0.013119i,  -0.0015642 - 0.012322i,
									 					  0.0016422 + 0.012204i,   0.99831   - 0.012742i),
									 y     = c( 0.048813  - 0.39076i,   -0.048859  + 0.39072i,
									 					 -0.048726  + 0.39085i,    0.048771  - 0.3908i),
									 z     = c(-14567     - 148370i,    -14528     - 148350i,
									 					 -14588     - 148390i,    -14549     - 148360i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	abcd_to_h(abcd)           %>% expect_equal(expected$h,     tolerance = 1e-4)
	abcd_to_s(abcd)           %>% expect_equal(expected$s_50,  tolerance = 1e-4)
	abcd_to_s(abcd, z0 = 100) %>% expect_equal(expected$s_100, tolerance = 1e-4)
	abcd_to_t(abcd)           %>% expect_equal(expected$t_50,  tolerance = 1e-4)
	abcd_to_t(abcd, z0 = 100) %>% expect_equal(expected$t_100, tolerance = 1e-4)
	abcd_to_y(abcd)           %>% expect_equal(expected$y,     tolerance = 1e-4)
	abcd_to_z(abcd)           %>% expect_equal(expected$z,     tolerance = 1)
})
test_that("h transformations - results", {
	
	h <- matrix(c( 0.314441556185771     + 2.51960941000598i,
								 0.999823389146385     - 0.000246785162909241i,
								-1.000115600382660     - 0.000129304649930592i,
								-6.55389515512306e-007 + 6.67541048071651e-006i),
							nrow = 2, byrow = T)
	expected <- list(abcd  = c( 0.99981   - 0.00024634i, 0.31473   + 2.5193i,
															0         + 0i,          0.99988   - 0.00012927i),
									 s_50  = c( 0.0037378 + 0.024799i,   0.9961    - 0.025374i,
									 					  0.9964    - 0.025007i,   0.0038185 + 0.024914i),
									 s_100 = c( 0.00172   + 0.012165i,   0.99816   - 0.013077i,
									 					  0.99845   - 0.012705i,   0.0017994 + 0.012281i),
									 t_50  = c( 1.003     + 0.025172i,  -0.0032027 - 0.025084i,
									 			 		  0.0031247 + 0.024967i,   0.99671   - 0.025547i),
									 t_100 = c( 1.0014    + 0.012742i,  -0.0016454 - 0.012321i,
									 			 		  0.0015674 + 0.012204i,   0.9983    - 0.013118i),
									 y     = c( 0.048771  - 0.3908i,    -0.048666  + 0.39074i,
									 					 -0.048827  + 0.39084i,    0.048721  - 0.39078i),
									 z     = c(-14583     - 148360i,    -14601     - 148340i,
									 					 -14550     - 148390i,    -14567     - 148370i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	h_to_abcd(h)        %>% expect_equal(expected$abcd,  tolerance = 1e-4)
	h_to_s(h)           %>% expect_equal(expected$s_50,  tolerance = 1e-4)
	h_to_s(h, z0 = 100) %>% expect_equal(expected$s_100, tolerance = 1e-4)
	h_to_t(h)           %>% expect_equal(expected$t_50,  tolerance = 1e-4)
	h_to_t(h, z0 = 100) %>% expect_equal(expected$t_100, tolerance = 1e-4)
	h_to_y(h)           %>% expect_equal(expected$y,     tolerance = 1e-4)
	h_to_z(h)           %>% expect_equal(expected$z,     tolerance = 1)
})
test_that("s transformations - results", {

	s <- matrix(c(-0.589214754036332 + 0.157879617512538i, 0.0371572412738697 + 0.0334565303179429i,
								 1.9159416386654   + 3.18866235861186i,  0.301108772861486  - 0.334415171464827i),
							nrow = 2, byrow = T)
	expected <- list(abcd_50  = c( 0.063337  + 0.0068829i,  1.4958     - 3.9839i,
																 0.0022096 - 0.0024324i,  0.073168   - 0.26643i),
									 abcd_100 = c( 0.063337  + 0.0068829i,  2.9915     - 7.9678i,
									 						   0.0011048 - 0.0012162i,  0.073168   - 0.26643i),
									 h_50     = c( 15.338    + 1.4019i,     0.026036   + 0.041094i,
									 						  -0.9585    - 3.4902i,     0.010608   + 0.0053805i),
									 h_100    = c( 30.676    + 2.8038i,     0.026036   + 0.041094i,
									 						  -0.9585    - 3.4902i,     0.0053038  + 0.0026902i),
									 t        = c( 0.13845   - 0.23042i,    0.035368   + 0.11568i,
									 						  -0.045199  + 0.15763i,   -0.0019457  - 0.029121i),
									 y_50     = c( 0.064657  - 0.0059096i, -0.0019262  - 0.0025032i,
									 						  -0.082599  - 0.22i,       0.0037174  + 0.014503i),
									 y_100    = c( 0.032328  - 0.0029548i, -0.00096311 - 0.0012516i,
									 						  -0.0413    - 0.11i,       0.0018587  + 0.0072513i),
									 z_50     = c( 11.409    + 15.674i,     3.5151     + 2.0911i,
									 						   204.61    + 225.24i,     74.981     - 38.033i),
									 z_100    = c( 22.818    + 31.349i,     7.0302     + 4.1822i,
									 						   409.22    + 450.48i,     149.96     - 76.065i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	s_to_abcd(s)           %>% expect_equal(expected$abcd_50,  tolerance = 1e-4)
	s_to_abcd(s, z0 = 100) %>% expect_equal(expected$abcd_100, tolerance = 1e-4)
	s_to_h(s)              %>% expect_equal(expected$h_50,     tolerance = 1e-4)
	s_to_h(s, z0 = 100)    %>% expect_equal(expected$h_100,    tolerance = 1e-4)
	s_to_t(s)              %>% expect_equal(expected$t,        tolerance = 1e-4)
	s_to_y(s)              %>% expect_equal(expected$y_50,     tolerance = 1e-4)
	s_to_y(s, z0 = 100)    %>% expect_equal(expected$y_100,    tolerance = 1e-4)
	s_to_z(s)              %>% expect_equal(expected$z_50,     tolerance = 1e-2)
	s_to_z(s, z0 = 100)    %>% expect_equal(expected$z_100,    tolerance = 1e-2)
})
test_that("t transformations - results", {

	t <- matrix(c(0.138451095405929   - 0.230421317393041i, 0.0353675449261375  + 0.115682026931012i,
								-0.0451985986689165  + 0.157626245839348i,  -0.00194567217559662 - 0.0291212122613417i),
							nrow = 2, byrow = T)
	expected <- list(abcd_50  = c( 0.063337  + 0.0068829i,  1.4958     - 3.9839i,
																 0.0022096 - 0.0024324i,  0.073168   - 0.26643i),
									 abcd_100 = c( 0.063337  + 0.0068829i,  2.9915     - 7.9678i,
									 							 0.0011048 - 0.0012162i,  0.073168   - 0.26643i),
									 h_50     = c( 15.338    + 1.4019i,     0.026036   + 0.041094i,
									 							-0.9585    - 3.4902i,     0.010608   + 0.0053805i),
									 h_100    = c( 30.676    + 2.8038i,     0.026036   + 0.041094i,
									 							-0.9585    - 3.4902i,     0.0053038  + 0.0026902i),
									 s        = c(-0.58921   + 0.15788i,    0.037157   + 0.033457i,
									 							 1.9159    + 3.1887i,     0.30111    - 0.33442i),
									 y_50     = c( 0.064657  - 0.0059096i, -0.0019262  - 0.0025032i,
									 							-0.082599  - 0.22i,       0.0037174  + 0.014503i),
									 y_100    = c( 0.032328  - 0.0029548i, -0.00096311 - 0.0012516i,
									 							-0.0413    - 0.11i,       0.0018587  + 0.0072513i),
									 z_50     = c( 11.409    + 15.674i,     3.5151     + 2.0911i,
									 							204.61    + 225.24i,     74.981     - 38.033i),
									 z_100    = c( 22.818    + 31.349i,     7.0302     + 4.1822i,
									 							409.22    + 450.48i,     149.96     - 76.065i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	t_to_abcd(t)           %>% expect_equal(expected$abcd_50,  tolerance = 1e-4)
	t_to_abcd(t, z0 = 100) %>% expect_equal(expected$abcd_100, tolerance = 1e-4)
	t_to_s(t)              %>% expect_equal(expected$s,        tolerance = 1e-4)
	t_to_y(t)              %>% expect_equal(expected$y_50,     tolerance = 1e-4)
	t_to_y(t, z0 = 100)    %>% expect_equal(expected$y_100,    tolerance = 1e-4)
	t_to_z(t)              %>% expect_equal(expected$z_50,     tolerance = 1e-2)
	t_to_z(t, z0 = 100)    %>% expect_equal(expected$z_100,    tolerance = 1e-2)
})
test_that("y transformations - results", {
	
	y <- matrix(c( 0.0488133074245012 - 0.390764155450191i,
								-0.0488588365420561 + 0.390719345880018i,
								-0.0487261119282660 + 0.390851884427087i,
								 0.0487710062903760 - 0.390800401433241i),
							nrow = 2, byrow = T)
	expected <- list(abcd  = c( 0.99988   + 0.00012927i, 0.31408   + 2.5194i,
														  0         + 0i,          0.99981   + 0.00024723i),
									 h     = c( 0.31476   + 2.5198i,     0.9999    + 0.00012883i,
									 					 -1.0002    + 0.00024733i, 0         + 0i),
									 s_50  = c( 0.0038184 + 0.024797i,   0.99611   - 0.024999i,
									 					  0.99639   - 0.025381i,   0.0037436 + 0.024916i),
									 s_100 = c( 0.0017993 + 0.012164i,   0.99816   - 0.012701i,
									 					  0.99845   - 0.01308i,    0.0017229 + 0.012282i),
									 t_50  = c( 1.003     + 0.025549i,  -0.0031182 - 0.025086i,
									 					  0.0031962 + 0.024968i,   0.99672   - 0.025172i),
									 t_100 = c( 1.0014    + 0.013119i,  -0.0015642 - 0.012322i,
									 					  0.0016422 + 0.012204i,   0.99831   - 0.012742i),
									 z     = c(-14567     - 148370i,    -14528     - 148350i,
									 					 -14588     - 148390i,    -14549     - 1.4836e+05i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	y_to_abcd(y)        %>% expect_equal(expected$abcd,  tolerance = 1e-4)
	y_to_h(y)           %>% expect_equal(expected$h,     tolerance = 1e-4)
	y_to_s(y)           %>% expect_equal(expected$s_50,  tolerance = 1e-4)
	y_to_s(y, z0 = 100) %>% expect_equal(expected$s_100, tolerance = 1e-4)
	y_to_t(y)           %>% expect_equal(expected$t_50,  tolerance = 1e-4)
	y_to_t(y, z0 = 100) %>% expect_equal(expected$t_100, tolerance = 1e-4)
	y_to_z(y)           %>% expect_equal(expected$z,     tolerance = 1)
})
test_that("z transformations - results", {
	
	z <- matrix(c(-14567.2412789287 - 148373.315116592i, -14588.1106171651 - 148388.583516562i,
								-14528.0522132692 - 148350.705757767i, -14548.5996561832 - 148363.457002006i),
							nrow = 2, byrow = T)
	expected <- list(abcd  = c( 1.0002    - 0.00024687i, 0.31512    + 2.52i,
														  0         + 0i,          1.0001     - 0.00012885i),
									 h     = c( 0.31476   +     2.5198i, 1.0002     - 0.00024733i,
									 					 -0.9999    - 0.00012883i, 0          + 0i),
									 s_50  = c( 0.0038184 +   0.024797i, 0.99639    - 0.025381i,
									 					  0.99611   -   0.024999i, 0.0037436  + 0.024916i),
									 s_100 = c( 0.0017993 +   0.012164i, 0.99845    - 0.01308i,
									 					  0.99816   -   0.012701i, 0.0017229  + 0.012282i),
									 t_50  = c( 1.0033    +   0.025179i, -0.0031285 - 0.025092i,
									 					  0.0032065 +   0.024974i, 0.997      - 0.025555i),
									 t_100 = c( 1.0017    +   0.012746i, -0.0015693 - 0.012325i,
									 					  0.0016473 +   0.012207i, 0.99859    - 0.013122i),
									 y     = c( 0.048813  -    0.39076i, -0.048726  + 0.39085i,
									 					 -0.048859  +    0.39072i, 0.048771   - 0.3908i)) %>%
		modify(matrix, nrow = 2, byrow = T)
	
	z_to_abcd(z)        %>% expect_equal(expected$abcd,  tolerance = 1e-4)
	z_to_h(z)           %>% expect_equal(expected$h,     tolerance = 1e-4)
	z_to_s(z)           %>% expect_equal(expected$s_50,  tolerance = 1e-4)
	z_to_s(z, z0 = 100) %>% expect_equal(expected$s_100, tolerance = 1e-4)
	z_to_t(z)           %>% expect_equal(expected$t_50,  tolerance = 1e-4)
	z_to_t(z, z0 = 100) %>% expect_equal(expected$t_100, tolerance = 1e-4)
	z_to_y(z)           %>% expect_equal(expected$y,     tolerance = 1)
})