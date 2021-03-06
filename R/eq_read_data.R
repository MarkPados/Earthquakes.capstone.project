#' Read data
#'
#' @details This function opens the earthquakes dataset.
#'
#' @import utils
#' @importFrom magrittr %>%
#'
#' @examples
#' raw_data <- eq_read_data()
#'
#' @export

eq_read_data <- function(){

  raw_data <- utils::read.delim(file = system.file("extdata", "signif.txt", package="Earthquakes.capstone.project"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  return(raw_data)

}

#' This is data to be included in my package
#'
#' @name signif
#' @docType data
#' @keywords data
