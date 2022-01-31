#' S-Parameter Column Names
#'
#' Outputs the columns names of a .sNp file based on parameter, numeric format,
#' and the number of S-parameters.
#'
#' @param parameter Type of parameter file (valid options = S, Y, Z)
#' @param numeric_format Numeric format used (valid options = MA, DB, RI)
#' @param num_parameters Number of parameters
#' @examples
#' rftk:::get_column_names("S", "MA", 2)
#' # output: "Frequency" "S11_Mag"   "S11_Ang"   "S21_Mag"   "S21_Ang"   "S12_Mag"   "S12_Ang"
#' #  "S22_Mag"   "S22_Ang"
#' @keywords internal
get_column_names <- function(parameter = "S", numeric_format, num_parameters, in_matrix_format = FALSE) {
  
  # coerce inputs
  parameter <- toupper(parameter)
  numeric_format <- toupper(numeric_format)
  
  if(length(parameter) != 1)
    stop("Only a single parameter value is supported")
  
  if(length(numeric_format) != 1)
    stop("Only a single numeric format is supported")
  
  if(!parameter %in% c("S", "Z", "Y"))
    stop("Invalid parameter value.  Supported parameters are S, Y, Z.")
  
  if(!numeric_format %in% c("MA", "DB", "RI"))
    stop("Invalid numeric format.  Supported formats are MA, DB, RI")
  
  if(num_parameters < 1)
    stop("num_paramters must be one or greater.")
  
  parameter_suffix <- switch(numeric_format,
                             "MA" = c("Mag", "Ang"),
                             "DB" = c("dB", "Ang"),
                             "RI" = c("Re", "Im"))
  
  if(num_parameters == 2 && !in_matrix_format) {
    # s2p files are a special case
    col_names <- c(paste0(parameter, "11_", parameter_suffix),
                   paste0(parameter, "21_", parameter_suffix),
                   paste0(parameter, "12_", parameter_suffix),
                   paste0(parameter, "22_", parameter_suffix))
  }
  else {
    col_names <- paste0(parameter,
                        expand.grid(1:num_parameters,1:num_parameters) %>%
                          transmute(paste0(.data$Var2,.data$Var1)) %>%
                          pull()) %>%
      map(paste, parameter_suffix, sep = "_") %>%
      unlist
  }
  
  return(c("Frequency",
           col_names))
  
  
}

#' Change numeric type of sNp file output
#' 
#' Changes the provided dataframe output to the specified type
#' 
#' @param data Dataframe to convert
#' @param format Target numeric format (valid options = MA, DB, RI)
#' @examples 
#' sparam <- data.frame(Frequency = 1:3, dB = -1:-3, Ang = 40:42)
#' rftk:::change_snp_numeric_type(sparam, "MA")
#' 
#' # # A tibble: 3 x 3
#' #   Frequency   Mag   Ang
#' #       <int> <dbl> <int>
#' # 1         1 0.891    40
#' # 2         2 0.794    41
#' # 3         3 0.708    42
#' @keywords internal
change_snp_numeric_type <- function(data, format) {
  
  # coerce arguments
  format <- toupper(format)
  data_names <- toupper(colnames(data))
  
  if(!(format %in% c("MA","DB","RI")))
    stop("Invalid numeric format.  Supported formats are MA, DB, RI")
  
  old_format <- case_when(
    all(c("MAG", "ANG") %in% data_names) ~ "MA",
    all(c("DB", "ANG") %in% data_names)  ~ "DB",
    all(c("RE", "IM") %in% data_names)   ~ "RI")
  
  if(is.na(old_format))
    stop("Invalid data")
  
  if(old_format == format)
    return(data)
  
  switch(old_format,
         "MA" = switch(format,
                       "DB" = mutate(data, 
                                     dB = as_decibel(.data$Mag, scalar = 20),
                                     Mag = NULL),
                       "RI" = mutate(data,
                                     Re = .data$Mag * cos(.data$Ang / 180 * pi),
                                     Im = .data$Mag * sin(.data$Ang / 180 * pi),
                                     Mag = NULL,
                                     Ang = NULL)),
         "DB" = switch(format,
                       "MA" = mutate(data, 
                                     Mag = as_linear(.data$dB, scalar = 20),
                                     dB = NULL),
                       "RI" = change_snp_numeric_type(change_snp_numeric_type(data, "MA"), "RI")),
         "RI" = switch(format,
                       "DB" = change_snp_numeric_type(change_snp_numeric_type(data, "MA"), "DB"),
                       "MA" = mutate(data, 
                                     Mag = sqrt(.data$Re^2 + .data$Im^2),
                                     Ang = atan2(.data$Im, .data$Re) / pi * 180,
                                     Re = NULL,
                                     Im = NULL))) %>%
    relocate(any_of(c("Mag", "dB", "Ang", "Re", "Im")), 
             .after = "Frequency")
}

#' Read Touchstone .sNp file into tibble
#'
#' Imports and optionally converts and cleans Touchstone .sNp files
#'
#' @param ... Files to read
#' @param numeric_format Output format.  Can be "DB" for dB Mag/Angle, "MA" for Mag/Angle, "RI" for Real/Imaginary, or NA for no conversion.
#' @param clean_names_case Type of character casing to reformat the column names into.  Set to \code{NULL} for no reformatting, and see \code{\link[janitor]{clean_names}} for more details.
#' @examples
#' read_snp(rftk_example("dipole.s1p"))
#' read_snp(rftk_example("dipole.s1p"), numeric_format = "MA")
#' @export
read_snp <- function(...,
                     numeric_format = "DB",
                     clean_names_case = "old_janitor") {
  
  files <- unlist(list(...))
  
  # return if no files provided
  if(is.null(files))
    return(NULL)
  
  # set names for files if not already set
  if(is.null(names(files)))
    files <- set_names(files)
  
  # filter files
  bad_files <- purrr::discard(files, grepl, pattern = "s\\dp$", ignore.case = TRUE)
  if(length(bad_files) > 0)
    stop("Non-sNp files detected")
  
  map_dfr(files, function(file) {

    num_params <- substr(file, nchar(file) - 1, nchar(file) - 1) %>% as.integer
    
    # read all lines
    x <- readr::read_lines(file, 
                           skip_empty_rows = T) %>%
      iconv(to = "UTF-8")
    
    # remove noise parameters
    noise_parameters_index = which(startsWith(x, "! NOISE PARAMETERS"))
    if(length(noise_parameters_index) != 0)
      x <- x[1:noise_parameters_index - 1]
    
    # remove comment lines
    x <- grep("^\\s*!", x, value = T, invert = T)
    
    # remove inline comments
    x <- gsub("!.*", "", x)
    
    # trim whitespace
    x <- trimws(x)
    
    # read header
    header <- x[1]
    
    # remove header
    x <- x[-1]
    
    # split all
    x <- strsplit(x, "\\s+")
    
    # Split the header by whitespace
    header <- strsplit(header, "\\s+") %>%
      unlist %>%
      toupper
    
    # Parse the frequency units, convert into a frequency multiplier
    freq_multiplier = switch(header[2],
                             "HZ"  = 1,
                             "KHZ" = 1e3,
                             "MHZ" = 1e6,
                             "GHZ" = 1e9,
                             "THZ" = 1e12)
    
    # get column names
    col_names_wide <- get_column_names(parameter = header[3],
                                       numeric_format = header[4],
                                       num_parameters = num_params,
                                       in_matrix_format = n_distinct(map_int(x, length)) > 1)
    
    # collapse list of space-split into a single vector
    x <- unlist(x)
    
    # convert data to numeric
    x <- as.numeric(x)
    
    # convert vector into a matrix
    # each row is all s-parameters per frequency
    x <- matrix(x, byrow = T, ncol = length(col_names_wide))
    
    # set column names	
    colnames(x) <- col_names_wide
    
    # convert to a tibble
    x <- as_tibble(x)
    
    # apply frequency multiplier
    x <- x %>% 
      mutate(Frequency = .data$Frequency * freq_multiplier) %>%
      tidyr::pivot_longer(-1, names_to = c("Parameter", "Complex"), names_sep = "_") %>%
      tidyr::pivot_wider(names_from = "Complex") %>%
      relocate(.data$Parameter)
    
    if(!is.null(numeric_format))
      x <- change_snp_numeric_type(x, numeric_format)
    
    if(!is.null(clean_names_case))
      x <- janitor::clean_names(x, case = clean_names_case)
    
    x
    
  }, .id = "filepath") %>%
    janitor::remove_constant()
}