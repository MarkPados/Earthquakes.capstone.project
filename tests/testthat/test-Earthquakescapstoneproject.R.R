context("testthat.R")

test_that("geom_timeline returns a ggplot", {

  raw_data <- Earthquakes.capstone.project::eq_read_data()

  clean_data <- Earthquakes.capstone.project::eq_clean_data(raw_data)

  eq_chart <- clean_data %>%
    filter(COUNTRY %in% c("HUNGARY", "SLOVAKIA", "CZECH REPUBLIC", "CROATIA", "POLAND"), YEAR >= 1500) %>%
    ggplot(aes(x = DATE, y = COUNTRY)) +
    Earthquakes.capstone.project::geom_timeline()

  expect_is(eq_chart, "ggplot")

})



test_that("geom_timeline_label returns also a ggplot", {

  raw_data = Earthquakes.capstone.project::eq_read_data()

  clean_data = Earthquakes.capstone.project::eq_clean_data(raw_data)

  eq_chart <- clean_data %>%
    filter(COUNTRY %in% c("JAPAN"), YEAR >= 2000) %>%
    ggplot(aes(x = DATE)) +
    geom_timeline()+
    geom_timeline_label(aes(label = LOCATION_NAME, size = EQ_PRIMARY, nmax = 2))

  expect_is(eq_chart, "ggplot")

})



test_that("ep_map works", {

  raw_data = Earthquakes.capstone.project::eq_read_data()

  clean_data = Earthquakes.capstone.project::eq_clean_data(raw_data)

  eq_chart <- clean_data %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")

  expect_is(eq_chart, "leafet")

})



test_that("ep_map_label works", {

  raw_data = Earthquakes.capstone.project::eq_read_data()

  clean_data = Earthquakes.capstone.project::eq_clean_data(raw_data)

  clean_data %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")

  expect_is(eq_chart, "leafet")

})
