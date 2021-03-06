% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_timeline.R
\name{geom_timeline}
\alias{geom_timeline}
\title{Geom timeline}
\usage{
geom_timeline(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...)
}
\arguments{
\item{mapping}{Mapping}

\item{data}{Data}

\item{stat}{Stat}

\item{position}{Position}

\item{na.rm}{NA remove}

\item{show.legend}{Show legend}

\item{inherit.aes}{Inherit aes}

\item{...}{Other parameters}
}
\description{
Geom timeline
}
\details{
This function creates timeline chart.
}
\examples{
require(ggplot2)
raw_data <- eq_read_data()
clean_data <- eq_clean_data(raw_data)
clean_data <- dplyr::filter(clean_data, COUNTRY \%in\% c("HUNGARY", "CROATIA"), YEAR >= 1500)
ggplot2::ggplot(clean_data, aes(x = DATE, y = COUNTRY)) +
geom_timeline()

}
