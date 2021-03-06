% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_clean_data.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Clean data}
\usage{
eq_clean_data(raw_data)
}
\arguments{
\item{raw_data}{Raw dataframe from eq_read_data function.}
}
\description{
Clean data
}
\details{
This function clean the earthquakes raw database.
}
\examples{
raw_data <- eq_read_data()
clean_data <- eq_clean_data(raw_data)

}
