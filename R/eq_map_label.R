#' Earthquake map
#'
#' @details This function creates earthquake map.
#'
#' @import ggplot2
#' @import leaflet
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
#' raw_data <- eq_read_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- dplyr::filter(clean_data, COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#' eq_map(clean_data, annot_col = "DATE")
#'
#' @param data Cleaned data from eq_clean_data function.
#' @param annot_col Annotation column.
#'
#' @export

eq_map <- function(data, annot_col = "DATE") {
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)  %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_PRIMARY, weight = 1, popup = data[[annot_col]])
}



#' Earthquake label
#'
#' @details This function creates earthquake map label.
#'
#' @import ggplot2
#' @import leaflet
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
#' raw_data <- eq_read_data()
#' clean_data <- eq_clean_data(raw_data)
#' clean_data <- dplyr::filter(clean_data, COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#' clean_data <- dplyr::mutate(clean_data, popup_text = eq_create_label(clean_data))
#' eq_map(clean_data, annot_col = "popup_text")
#'
#' @param data Cleaned data from eq_clean_data function.
#'
#' @export

eq_create_label <- function(data){
  location_text <- paste("<b>Location:</b>",data$LOCATION_NAME,"<br>")
  location_text[is.na(data$LOCATION_NAME)] <- ''
  magnitude_text <- paste("<b>Magnitude:</b>",data$EQ_PRIMARY,"<br>")
  magnitude_text[is.na(data$EQ_PRIMARY)] <- ''
  deaths_text <- paste("<b>Total Deaths:</b>",data$TOTAL_DEATHS)
  deaths_text[is.na(data$TOTAL_DEATHS)] <- ''
  paste(location_text, magnitude_text, deaths_text)
}
