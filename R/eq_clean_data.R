#' Clean data
#'
#' @details This function clean the earthquakes raw database.
#'
#' @examples
#' clean_data <- eq_clean_data(raw_data)
#'
#' @import lubridate
#' @import dplyr
#' @import assertthat
#' @import bindrcpp
#' @import glue
#' @import magrittr
#' @import methods
#' @import pkgconfig
#' @import R6
#' @import Rcpp
#' @import rlang
#' @import tibble
#' @import tidyselect
#' @import utils
#'
#' @export

eq_clean_data <- function(raw_data){

  raw_data$DATE <- lubridate::ymd(paste(stringr::str_pad(as.character(abs(raw_data$YEAR)), width = 4, side = "left", pad = "0"),
                                        raw_data$MONTH, raw_data$DAY, sep = "-"), truncated = 2)

  for (i in 1:nrow(raw_data)){

    if (raw_data$YEAR[i] < 0){
      raw_data$IS_BC[i] <- TRUE
    } else {
      raw_data$IS_BC[i] <- FALSE
    }

  }

  raw_data$LATITUDE <- as.numeric(raw_data$LATITUDE)
  raw_data$LONGITUDE <- as.numeric(raw_data$LONGITUDE)

  cleaned_data <- Earthquakes.capstone.project::eq_location_cleaning(raw_data)

  return(cleaned_data)
}
