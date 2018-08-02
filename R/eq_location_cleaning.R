#' Location cleaning
#'
#' @details This function cleanes the location variable
#'
#' @examples
#' cleaned_data <- eq_location_clean(raw_data)
#'
#' @import stringr
#'
#' @export

eq_location_cleaning <- function(raw_data){

  for (i in 1:nrow(raw_data)){
    if (grepl(paste0(raw_data$COUNTRY[i], ": "), raw_data$LOCATION_NAME[i], fixed = TRUE) == FALSE) {
      raw_data$LOCATION_NAME[i] <- stringr::str_split_fixed(raw_data$LOCATION_NAME[i], ": ", 2)[,1]
    } else {
      raw_data$LOCATION_NAME[i] <- stringr::str_split_fixed(raw_data$LOCATION_NAME[i], ": ", 2)[,2]
    }
  }

  return(raw_data)
}
