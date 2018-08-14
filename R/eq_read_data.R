#' Read data
#'
#' @details This function opens the earthquakes dataset.
#'
#' @examples
#' raw_data <- eq_read_data()
#'
#' @export

eq_read_data <- function(){

  raw_data <- read.delim("data/signif.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  return(raw_data)

}

