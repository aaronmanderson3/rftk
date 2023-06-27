library(rftk)

test_that("get_column_names - invalid input", {
	expect_error(get_column_names(parameter = "R"))
	expect_error(get_column_names(parameter = c("S", "Y")))
	expect_error(get_column_names(numeric_format = "R"))
	expect_error(get_column_names(numeric_format = c("DB", "MA")))
	expect_error(get_column_names(num_parameters = 0))
})
test_that("get_column_names - in_matrix_format", {
	t <- get_column_names("S", "MA", 2, T)
	f <- get_column_names("S", "MA", 2, F)
	
	# same names, but in different orders
	expect_setequal(t, f)
	expect_failure(expect_mapequal(t, f))
})
test_that("get_column_names - results", {
	get_column_names("S", "MA", 1) %>% expect_equal(c("Frequency", "S_1_1_Mag", "S_1_1_Ang"))
	get_column_names("Y", "MA", 1) %>% expect_equal(c("Frequency", "Y_1_1_Mag", "Y_1_1_Ang"))
	get_column_names("Z", "MA", 1) %>% expect_equal(c("Frequency", "Z_1_1_Mag", "Z_1_1_Ang"))
	
	get_column_names("S", "DB", 1) %>% expect_equal(c("Frequency", "S_1_1_dB", "S_1_1_Ang"))
	get_column_names("S", "RI", 1) %>% expect_equal(c("Frequency", "S_1_1_Re", "S_1_1_Im"))
	
	get_column_names("S", "MA", 2) %>% expect_equal(c("Frequency", 
																										"S_1_1_Mag", "S_1_1_Ang", 
																										"S_2_1_Mag", "S_2_1_Ang",
																										"S_1_2_Mag", "S_1_2_Ang",
																										"S_2_2_Mag", "S_2_2_Ang"))
	get_column_names("S", "MA", 3) %>% expect_equal(c("Frequency", 
																										"S_1_1_Mag", "S_1_1_Ang", 
																										"S_1_2_Mag", "S_1_2_Ang",
																										"S_1_3_Mag", "S_1_3_Ang",
																										"S_2_1_Mag", "S_2_1_Ang",
																										"S_2_2_Mag", "S_2_2_Ang",
																										"S_2_3_Mag", "S_2_3_Ang",
																										"S_3_1_Mag", "S_3_1_Ang",
																										"S_3_2_Mag", "S_3_2_Ang",
																										"S_3_3_Mag", "S_3_3_Ang"))
	get_column_names("S", "MA", 4) %>% expect_equal(c("Frequency",
																										"S_1_1_Mag", "S_1_1_Ang", 
																										"S_1_2_Mag", "S_1_2_Ang",
																										"S_1_3_Mag", "S_1_3_Ang",
																										"S_1_4_Mag", "S_1_4_Ang",
																										"S_2_1_Mag", "S_2_1_Ang",
																										"S_2_2_Mag", "S_2_2_Ang",
																										"S_2_3_Mag", "S_2_3_Ang",
																										"S_2_4_Mag", "S_2_4_Ang",
																										"S_3_1_Mag", "S_3_1_Ang",
																										"S_3_2_Mag", "S_3_2_Ang",
																										"S_3_3_Mag", "S_3_3_Ang",
																										"S_3_4_Mag", "S_3_4_Ang",
																										"S_4_1_Mag", "S_4_1_Ang",
																										"S_4_2_Mag", "S_4_2_Ang",
																										"S_4_3_Mag", "S_4_3_Ang",
																										"S_4_4_Mag", "S_4_4_Ang"))
})

test_that("change_snp_numeric_type - invalid input", {
	invalid_data <- tribble(~Frequency,  ~A,  ~B,
													500, 0.5,  62,
													1000, 0.6,  70,
													2000, 0.7,  86)
	valid_data <- tribble(~Frequency, ~Mag, ~Ang,
												500L,  0.5,  62L,
												1000L,  0.6,  70L,
												2000L,  0.7,  86L)
	
	expect_error(change_snp_numeric_type(invalid_data, "MA"))
	expect_error(change_snp_numeric_type(valid_data, "A"))
})
test_that("change_snp_numeric_type - results", {
	equiv_data <- tibble::tribble(
		~Frequency, ~Mag,                      ~dB, ~Ang,         ~Re,         ~Im,
		500L,  0.5,        -6.02059991327962,  62L, 0.234735781, 0.441473796,
		1000L,  0.6,        -4.43697499232713,  70L, 0.205212086, 0.563815572,
		2000L,  0.7,        -3.09803919971486,  86L, 0.048829532, 0.698294835)
	
	equiv_data <- list("MA" = select(equiv_data, "Frequency", "Mag", "Ang"),
										 "DB" = select(equiv_data, "Frequency",  "dB", "Ang"),
										 "RI" = select(equiv_data, "Frequency",  "Re",  "Im"))
	
	change_snp_numeric_type(equiv_data$MA, "MA") %>% expect_equal(equiv_data$MA)
	change_snp_numeric_type(equiv_data$DB, "DB") %>% expect_equal(equiv_data$DB)
	change_snp_numeric_type(equiv_data$RI, "RI") %>% expect_equal(equiv_data$RI)
	
	change_snp_numeric_type(equiv_data$MA, "MA") %>% expect_equal(equiv_data$MA)
	change_snp_numeric_type(equiv_data$MA, "DB") %>% expect_equal(equiv_data$DB)
	change_snp_numeric_type(equiv_data$MA, "RI") %>% expect_equal(equiv_data$RI)
	change_snp_numeric_type(equiv_data$DB, "MA") %>% expect_equal(equiv_data$MA)
	change_snp_numeric_type(equiv_data$DB, "DB") %>% expect_equal(equiv_data$DB)
	change_snp_numeric_type(equiv_data$DB, "RI") %>% expect_equal(equiv_data$RI)
	change_snp_numeric_type(equiv_data$RI, "MA") %>% expect_equal(equiv_data$MA)
	change_snp_numeric_type(equiv_data$RI, "DB") %>% expect_equal(equiv_data$DB)
	change_snp_numeric_type(equiv_data$RI, "RI") %>% expect_equal(equiv_data$RI)
})

test_that("read_snp - empty arguments", {
	expect_null(read_snp())
})
test_that("read_snp - invalid arguments", {
	expect_error(read_snp("bad_s1p_file.txt"))
	expect_error(read_snp("../test_files/Single_Dipole_S_DB.s1p", numeric_format = "AA"))
	expect_error(read_snp("../test_files/Single_Dipole_S_DB.s1p", clean_names_case = "bad_janitor"))
})
test_that("read_snp - upper/lower file extensions", {
	expect_equal(read_snp("../test_files/Single_Dipole_S_DB.s1p"),
							 read_snp("../test_files/Single_Dipole_S_DB_Upper.S1P"))
})
test_that("read_snp - inline comments", {
	t <- read_snp("../test_files/Dipole_Array_S_DB_With_Inline_Comment.s4p")
	f <- read_snp("../test_files/Dipole_Array_S_DB.s4p")
	
	expect_equal(t, f)
})
test_that("read_snp - 2-port in_matrix_format", {
	t <- read_snp("../test_files/Dipole_Array_S_DB_Matrix_Format.s2p")
	f <- read_snp("../test_files/Dipole_Array_S_DB.s2p")
	
	expect_equal(
		arrange(t, parameter), 
		arrange(f, parameter)
	)
})
test_that("read_snp - 3+ port file single/multi line formats", {
	expect_equal(read_snp("../test_files/Dipole_Array_S_DB.s4p"),
							 read_snp("../test_files/Dipole_Array_S_DB_Single_Line.s4p"))
	expect_equal(read_snp("../test_files/Dipole_Array_S_DB.s4p"),
							 read_snp("../test_files/Dipole_Array_S_DB_Mixed_Line.s4p"))
})
test_that("read_snp - results", {
	s1p_expected <- tribble(~frequency,        ~db,     ~ang,
													1.0e9,      -0.0618204, -15.7414,
													1.5e9,      -0.3507480,	-28.9574,
													2.0e9,      -2.9681100, -57.7081,
													3.0e9,      -3.6319900,  10.6553)
	s2p_expected <- tribble(~frequency, ~parameter,        ~db,     ~ang,
                               1.0e9,      "S11", -0.0547183, -15.6158,
                               1.0e9,      "S21",   -40.5397,  104.202,
                               1.0e9,      "S12",   -40.5397,  104.202,
                               1.0e9,      "S22", -0.0533965,  -15.546,
                               1.5e9,      "S11",  -0.315383, -28.7757,
                               1.5e9,      "S21",   -29.1908,  103.631,
                               1.5e9,      "S12",   -29.1908,  103.631,
                               1.5e9,      "S22",  -0.305429, -28.5559,
                               2.0e9,      "S11",    -2.5705, -60.2941,
                               2.0e9,      "S21",   -14.0419,  54.2596,
                               2.0e9,      "S12",   -14.0419,  54.2596,
                               2.0e9,      "S22",   -2.44891, -59.4757,
                               3.0e9,      "S11",   -3.26806,  7.10791,
                               3.0e9,      "S21",   -16.1329,  -144.16,
                               3.0e9,      "S12",   -16.1329,  -144.16,
                               3.0e9,      "S22",   -3.32517,   7.2338)
	s3p_expected <- tribble(~frequency, ~parameter,        ~db,     ~ang,
                               1.0e9,      "S11", -0.0542063, -15.5695,
                               1.0e9,      "S12",   -40.7429,  104.175,
                               1.0e9,      "S13",    -48.467,  111.749,
                               1.0e9,      "S21",   -40.7429,  104.175,
                               1.0e9,      "S22",  -0.048112, -15.6134,
                               1.0e9,      "S23",   -40.7392,  104.168,
                               1.0e9,      "S31",    -48.467,  111.749,
                               1.0e9,      "S32",   -40.7392,  104.168,
                               1.0e9,      "S33", -0.0542135, -15.5605,
                               1.5e9,      "S11",  -0.306525, -28.6531,
                               1.5e9,      "S12",   -29.5084,  103.701,
                               1.5e9,      "S13",   -34.2269,  72.7083,
                               1.5e9,      "S21",   -29.5084,  103.701,
                               1.5e9,      "S22",  -0.282531, -28.7601,
                               1.5e9,      "S23",   -29.5022,  103.682,
                               1.5e9,      "S31",   -34.2269,  72.7083,
                               1.5e9,      "S32",   -29.5022,  103.682,
                               1.5e9,      "S33",  -0.306741, -28.6437,
                               2.0e9,      "S11",   -2.68414, -59.4656,
                               2.0e9,      "S12",   -14.8262,  60.4495,
                               2.0e9,      "S13",   -18.1874, -19.8744,
                               2.0e9,      "S21",   -14.8262,  60.4495,
                               2.0e9,      "S22",   -2.12994, -61.6462,
                               2.0e9,      "S23",    -14.817,  60.3668,
                               2.0e9,      "S31",   -18.1874, -19.8744,
                               2.0e9,      "S32",    -14.817,  60.3668,
                               2.0e9,      "S33",   -2.69064,  -59.493,
                               3.0e9,      "S11",   -3.31867,  7.66931,
                               3.0e9,      "S12",   -15.2444, -144.801,
                               3.0e9,      "S13",   -25.5101,  109.321,
                               3.0e9,      "S21",   -15.2444, -144.801,
                               3.0e9,      "S22",   -2.93121,  3.31577,
                               3.0e9,      "S23",   -15.2501, -144.841,
                               3.0e9,      "S31",   -25.5101,  109.321,
                               3.0e9,      "S32",   -15.2501, -144.841,
													     3.0e9,      "S33",   -3.31087,  7.67287)
	s4p_expected <- tribble(~frequency, ~parameter,        ~db,     ~ang,
                               1.0e9,      "S11", -0.0461227, -15.6565,
                               1.0e9,      "S12",   -41.0472,  100.892,
                               1.0e9,      "S13",   -41.0919,  100.923,
                               1.0e9,      "S14",   -45.5821,  110.205,
                               1.0e9,      "S21",   -41.0472,  100.892,
                               1.0e9,      "S22", -0.0453704, -15.5715,
                               1.0e9,      "S23",   -45.6919,  110.257,
                               1.0e9,      "S24",   -41.1172,  100.916,
                               1.0e9,      "S31",   -41.0919,  100.923,
                               1.0e9,      "S32",   -45.6919,  110.257,
                               1.0e9,      "S33", -0.0448997, -15.5055,
                               1.0e9,      "S34",   -41.1524,  100.922,
                               1.0e9,      "S41",   -45.5821,  110.205,
                               1.0e9,      "S42",   -41.1172,  100.916,
                               1.0e9,      "S43",   -41.1524,  100.922,
                               1.0e9,      "S44", -0.0453525, -15.5831,
                               1.5e9,      "S11",  -0.266624, -28.9472,
                               1.5e9,      "S12",   -30.3796,  100.436,
                               1.5e9,      "S13",   -30.4419,  100.533,
                               1.5e9,      "S14",    -32.446,  89.9304,
                               1.5e9,      "S21",   -30.3796,  100.436,
                               1.5e9,      "S22",   -0.26074, -28.7377,
                               1.5e9,      "S23",   -32.5917,  90.1156,
                               1.5e9,      "S24",   -30.4852,  100.571,
                               1.5e9,      "S31",   -30.4419,  100.533,
                               1.5e9,      "S32",   -32.5917,  90.1156,
                               1.5e9,      "S33",  -0.257104, -28.5848,
                               1.5e9,      "S34",   -30.5391,  100.627,
                               1.5e9,      "S41",    -32.446,  89.9304,
                               1.5e9,      "S42",   -30.4852,  100.571,
                               1.5e9,      "S43",   -30.5391,  100.627,
                               1.5e9,      "S44",  -0.260369, -28.7481,
                               2.0e9,      "S11",    -2.4679, -63.6051,
                               2.0e9,      "S12",   -16.6275,  59.6618,
                               2.0e9,      "S13",   -16.7147,  60.2308,
                               2.0e9,      "S14",   -17.4542,  10.1728,
                               2.0e9,      "S21",   -16.6275,  59.6618,
                               2.0e9,      "S22",   -2.37439, -62.8072,
                               2.0e9,      "S23",   -17.6656,  11.0425,
                               2.0e9,      "S24",   -16.7857,  60.6684,
                               2.0e9,      "S31",   -16.7147,  60.2308,
                               2.0e9,      "S32",   -17.6656,  11.0425,
                               2.0e9,      "S33",   -2.32169, -62.2378,
                               2.0e9,      "S34",    -16.867,  61.1655,
                               2.0e9,      "S41",   -17.4542,  10.1728,
                               2.0e9,      "S42",   -16.7857,  60.6684,
                               2.0e9,      "S43",    -16.867,  61.1655,
                               2.0e9,      "S44",   -2.37066, -62.6888,
                               3.0e9,      "S11",   -2.69347,   4.1391,
                               3.0e9,      "S12",   -16.1176, -126.414,
                               3.0e9,      "S13",    -16.088, -126.193,
                               3.0e9,      "S14",   -26.4921, -125.469,
                               3.0e9,      "S21",   -16.1176, -126.414,
                               3.0e9,      "S22",   -2.73294,  4.34331,
                               3.0e9,      "S23",   -26.3591, -125.131,
                               3.0e9,      "S24",   -16.0547, -126.025,
                               3.0e9,      "S31",    -16.088, -126.193,
                               3.0e9,      "S32",   -26.3591, -125.131,
                               3.0e9,      "S33",   -2.75908,  4.52888,
                               3.0e9,      "S34",   -16.0215, -125.841,
                               3.0e9,      "S41",   -26.4921, -125.469,
                               3.0e9,      "S42",   -16.0547, -126.025,
                               3.0e9,      "S43",   -16.0215, -125.841,
												       3.0e9,      "S44",    -2.7448,  4.40672)



	read_snp("../test_files/Single_Dipole_S_DB.s1p")                        %>% expect_equal(s1p_expected)
	read_snp("../test_files/Single_Dipole_S_DB_Gamma_Z0.s1p")               %>% expect_equal(s1p_expected)
	read_snp("../test_files/Single_Dipole_S_MA.s1p", numeric_format = "DB") %>% expect_equal(s1p_expected, tolerance = 1e-5)
	read_snp("../test_files/Single_Dipole_S_RI.s1p", numeric_format = "DB") %>% expect_equal(s1p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s2p")                        %>% expect_equal(s2p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s2p")               %>% expect_equal(s2p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s2p", numeric_format = "DB") %>% expect_equal(s2p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.s2p", numeric_format = "DB") %>% expect_equal(s2p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s3p")                        %>% expect_equal(s3p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s3p")               %>% expect_equal(s3p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s3p", numeric_format = "DB") %>% expect_equal(s3p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.s3p", numeric_format = "DB") %>% expect_equal(s3p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s4p")                        %>% expect_equal(s4p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s4p")               %>% expect_equal(s4p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s4p", numeric_format = "DB") %>% expect_equal(s4p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.s4p", numeric_format = "DB") %>% expect_equal(s4p_expected, tolerance = 1e-5)
	})
