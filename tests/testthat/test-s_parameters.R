library(rftk)

test_that("get_column_names - invalid input", {
	expect_error(get_column_names(parameter = "R"))
	expect_error(get_column_names(parameter = c("S", "Y")))
	expect_error(get_column_names(numeric_format = "R"))
	expect_error(get_column_names(numeric_format = c("DB", "MA")))
	expect_error(get_column_names(num_parameters = 0))
})
test_that("get_column_names - results", {
	get_column_names("S", "MA", 1) %>% expect_equal(c("Frequency", "S11_Mag", "S11_Ang"))
	get_column_names("Y", "MA", 1) %>% expect_equal(c("Frequency", "Y11_Mag", "Y11_Ang"))
	get_column_names("Z", "MA", 1) %>% expect_equal(c("Frequency", "Z11_Mag", "Z11_Ang"))
	
	get_column_names("S", "DB", 1) %>% expect_equal(c("Frequency", "S11_dB", "S11_Ang"))
	get_column_names("S", "RI", 1) %>% expect_equal(c("Frequency", "S11_Re", "S11_Im"))
	
	get_column_names("S", "MA", 2) %>% expect_equal(c("Frequency", 
																										"S11_Mag", "S11_Ang", 
																										"S21_Mag", "S21_Ang",
																										"S12_Mag", "S12_Ang",
																										"S22_Mag", "S22_Ang"))
	get_column_names("S", "MA", 3) %>% expect_equal(c("Frequency", 
																										"S11_Mag", "S11_Ang", 
																										"S12_Mag", "S12_Ang",
																										"S13_Mag", "S13_Ang",
																										"S21_Mag", "S21_Ang",
																										"S22_Mag", "S22_Ang",
																										"S23_Mag", "S23_Ang",
																										"S31_Mag", "S31_Ang",
																										"S32_Mag", "S32_Ang",
																										"S33_Mag", "S33_Ang"))
	get_column_names("S", "MA", 4) %>% expect_equal(c("Frequency",
																										"S11_Mag", "S11_Ang", 
																										"S12_Mag", "S12_Ang",
																										"S13_Mag", "S13_Ang",
																										"S14_Mag", "S14_Ang",
																										"S21_Mag", "S21_Ang",
																										"S22_Mag", "S22_Ang",
																										"S23_Mag", "S23_Ang",
																										"S24_Mag", "S24_Ang",
																										"S31_Mag", "S31_Ang",
																										"S32_Mag", "S32_Ang",
																										"S33_Mag", "S33_Ang",
																										"S34_Mag", "S34_Ang",
																										"S41_Mag", "S41_Ang",
																										"S42_Mag", "S42_Ang",
																										"S43_Mag", "S43_Ang",
																										"S44_Mag", "S44_Ang"))
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
	expect_error(read_snp("tests/test_files/Single_Dipole_S_DB.s1p", numeric_format = "AA"))
	expect_error(read_snp("tests/test_files/Single_Dipole_S_DB.s1p", clean_names_case = "bad_janitor"))
})
test_that("read_snp - upper/lower file extensions", {
	expect_equal(read_snp("../test_files/Single_Dipole_S_DB.s1p"),
							 read_snp("../test_files/Single_Dipole_S_DB_Upper.S1P"))
})
test_that("read_snp - results", {
	s1p_expected <- tribble(~frequency,        ~db,     ~ang,
													1.0e9,      -0.0618204, -15.7414,
													1.5e9,      -0.3507480,	-28.9574,
													2.0e9,      -2.9681100, -57.7081,
													3.0e9,      -3.6319900,  10.6553)
	s2p_expected <- tribble(~parameter, ~frequency,        ~db,     ~ang,
                               "S11",        1.0e9, -0.0547183, -15.6158,
                               "S21",        1.0e9,   -40.5397,  104.202,
                               "S12",        1.0e9,   -40.5397,  104.202,
                               "S22",        1.0e9, -0.0533965,  -15.546,
                               "S11",        1.5e9,  -0.315383, -28.7757,
                               "S21",        1.5e9,   -29.1908,  103.631,
                               "S12",        1.5e9,   -29.1908,  103.631,
                               "S22",        1.5e9,  -0.305429, -28.5559,
                               "S11",        2.0e9,    -2.5705, -60.2941,
                               "S21",        2.0e9,   -14.0419,  54.2596,
                               "S12",        2.0e9,   -14.0419,  54.2596,
                               "S22",        2.0e9,   -2.44891, -59.4757,
                               "S11",        3.0e9,   -3.26806,  7.10791,
                               "S21",        3.0e9,   -16.1329,  -144.16,
                               "S12",        3.0e9,   -16.1329,  -144.16,
                               "S22",        3.0e9,   -3.32517,   7.2338)
	s3p_expected <- tribble(~parameter, ~frequency,        ~db,     ~ang,
                               "S11",      1.0e9, -0.0542063, -15.5695,
                               "S12",      1.0e9,   -40.7429,  104.175,
                               "S13",      1.0e9,    -48.467,  111.749,
                               "S21",      1.0e9,   -40.7429,  104.175,
                               "S22",      1.0e9,  -0.048112, -15.6134,
                               "S23",      1.0e9,   -40.7392,  104.168,
                               "S31",      1.0e9,    -48.467,  111.749,
                               "S32",      1.0e9,   -40.7392,  104.168,
                               "S33",      1.0e9, -0.0542135, -15.5605,
                               "S11",      1.5e9,  -0.306525, -28.6531,
                               "S12",      1.5e9,   -29.5084,  103.701,
                               "S13",      1.5e9,   -34.2269,  72.7083,
                               "S21",      1.5e9,   -29.5084,  103.701,
                               "S22",      1.5e9,  -0.282531, -28.7601,
                               "S23",      1.5e9,   -29.5022,  103.682,
                               "S31",      1.5e9,   -34.2269,  72.7083,
                               "S32",      1.5e9,   -29.5022,  103.682,
                               "S33",      1.5e9,  -0.306741, -28.6437,
                               "S11",      2.0e9,   -2.68414, -59.4656,
                               "S12",      2.0e9,   -14.8262,  60.4495,
                               "S13",      2.0e9,   -18.1874, -19.8744,
                               "S21",      2.0e9,   -14.8262,  60.4495,
                               "S22",      2.0e9,   -2.12994, -61.6462,
                               "S23",      2.0e9,    -14.817,  60.3668,
                               "S31",      2.0e9,   -18.1874, -19.8744,
                               "S32",      2.0e9,    -14.817,  60.3668,
                               "S33",      2.0e9,   -2.69064,  -59.493,
                               "S11",      3.0e9,   -3.31867,  7.66931,
                               "S12",      3.0e9,   -15.2444, -144.801,
                               "S13",      3.0e9,   -25.5101,  109.321,
                               "S21",      3.0e9,   -15.2444, -144.801,
                               "S22",      3.0e9,   -2.93121,  3.31577,
                               "S23",      3.0e9,   -15.2501, -144.841,
                               "S31",      3.0e9,   -25.5101,  109.321,
                               "S32",      3.0e9,   -15.2501, -144.841,
                               "S33",      3.0e9,   -3.31087,  7.67287)
	s4p_expected <- tribble(~parameter, ~frequency,        ~db,     ~ang,
                               "S11",      1.0e9, -0.0461227, -15.6565,
                               "S12",      1.0e9,   -41.0472,  100.892,
                               "S13",      1.0e9,   -41.0919,  100.923,
                               "S14",      1.0e9,   -45.5821,  110.205,
                               "S21",      1.0e9,   -41.0472,  100.892,
                               "S22",      1.0e9, -0.0453704, -15.5715,
                               "S23",      1.0e9,   -45.6919,  110.257,
                               "S24",      1.0e9,   -41.1172,  100.916,
                               "S31",      1.0e9,   -41.0919,  100.923,
                               "S32",      1.0e9,   -45.6919,  110.257,
                               "S33",      1.0e9, -0.0448997, -15.5055,
                               "S34",      1.0e9,   -41.1524,  100.922,
                               "S41",      1.0e9,   -45.5821,  110.205,
                               "S42",      1.0e9,   -41.1172,  100.916,
                               "S43",      1.0e9,   -41.1524,  100.922,
                               "S44",      1.0e9, -0.0453525, -15.5831,
                               "S11",      1.5e9,  -0.266624, -28.9472,
                               "S12",      1.5e9,   -30.3796,  100.436,
                               "S13",      1.5e9,   -30.4419,  100.533,
                               "S14",      1.5e9,    -32.446,  89.9304,
                               "S21",      1.5e9,   -30.3796,  100.436,
                               "S22",      1.5e9,   -0.26074, -28.7377,
                               "S23",      1.5e9,   -32.5917,  90.1156,
                               "S24",      1.5e9,   -30.4852,  100.571,
                               "S31",      1.5e9,   -30.4419,  100.533,
                               "S32",      1.5e9,   -32.5917,  90.1156,
                               "S33",      1.5e9,  -0.257104, -28.5848,
                               "S34",      1.5e9,   -30.5391,  100.627,
                               "S41",      1.5e9,    -32.446,  89.9304,
                               "S42",      1.5e9,   -30.4852,  100.571,
                               "S43",      1.5e9,   -30.5391,  100.627,
                               "S44",      1.5e9,  -0.260369, -28.7481,
                               "S11",      2.0e9,    -2.4679, -63.6051,
                               "S12",      2.0e9,   -16.6275,  59.6618,
                               "S13",      2.0e9,   -16.7147,  60.2308,
                               "S14",      2.0e9,   -17.4542,  10.1728,
                               "S21",      2.0e9,   -16.6275,  59.6618,
                               "S22",      2.0e9,   -2.37439, -62.8072,
                               "S23",      2.0e9,   -17.6656,  11.0425,
                               "S24",      2.0e9,   -16.7857,  60.6684,
                               "S31",      2.0e9,   -16.7147,  60.2308,
                               "S32",      2.0e9,   -17.6656,  11.0425,
                               "S33",      2.0e9,   -2.32169, -62.2378,
                               "S34",      2.0e9,    -16.867,  61.1655,
                               "S41",      2.0e9,   -17.4542,  10.1728,
                               "S42",      2.0e9,   -16.7857,  60.6684,
                               "S43",      2.0e9,    -16.867,  61.1655,
                               "S44",      2.0e9,   -2.37066, -62.6888,
                               "S11",      3.0e9,   -2.69347,   4.1391,
                               "S12",      3.0e9,   -16.1176, -126.414,
                               "S13",      3.0e9,    -16.088, -126.193,
                               "S14",      3.0e9,   -26.4921, -125.469,
                               "S21",      3.0e9,   -16.1176, -126.414,
                               "S22",      3.0e9,   -2.73294,  4.34331,
                               "S23",      3.0e9,   -26.3591, -125.131,
                               "S24",      3.0e9,   -16.0547, -126.025,
                               "S31",      3.0e9,    -16.088, -126.193,
                               "S32",      3.0e9,   -26.3591, -125.131,
                               "S33",      3.0e9,   -2.75908,  4.52888,
                               "S34",      3.0e9,   -16.0215, -125.841,
                               "S41",      3.0e9,   -26.4921, -125.469,
                               "S42",      3.0e9,   -16.0547, -126.025,
                               "S43",      3.0e9,   -16.0215, -125.841,
                               "S44",      3.0e9,    -2.7448,  4.40672)



	read_snp("../test_files/Single_Dipole_S_DB.s1p")                        %>% expect_equal(s1p_expected)
	read_snp("../test_files/Single_Dipole_S_DB_Gamma_Z0.s1p")               %>% expect_equal(s1p_expected)
	read_snp("../test_files/Single_Dipole_S_MA.s1p", numeric_format = "DB") %>% expect_equal(s1p_expected, tolerance = 1e-5)
	read_snp("../test_files/Single_Dipole_S_RI.s1p", numeric_format = "DB") %>% expect_equal(s1p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s2p")                        %>% expect_equal(s2p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s2p")               %>% expect_equal(s2p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s2p", numeric_format = "DB") %>% expect_equal(s2p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.S2P", numeric_format = "DB") %>% expect_equal(s2p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s3p")                        %>% expect_equal(s3p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s3p")               %>% expect_equal(s3p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s3p", numeric_format = "DB") %>% expect_equal(s3p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.S3P", numeric_format = "DB") %>% expect_equal(s3p_expected, tolerance = 1e-5)
	
	read_snp("../test_files/Dipole_Array_S_DB.s4p")                        %>% expect_equal(s4p_expected)
	read_snp("../test_files/Dipole_Array_S_DB_Gamma_Z0.s4p")               %>% expect_equal(s4p_expected)
	read_snp("../test_files/Dipole_Array_S_MA.s4p", numeric_format = "DB") %>% expect_equal(s4p_expected, tolerance = 1e-5)
	read_snp("../test_files/Dipole_Array_S_RI.S4P", numeric_format = "DB") %>% expect_equal(s4p_expected, tolerance = 1e-5)
	})
