#' Clean data
#'
#' @details This function clean the earthquakes raw database.
#'
#' @examples
#' raw_data <- eq_read_data()
#' clean_data <- eq_clean_data(raw_data)
#'
#' @param raw_data Raw dataframe from eq_read_data function.
#'
#' @import dplyr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @importFrom lubridate year
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
