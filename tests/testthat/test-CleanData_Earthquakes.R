context("Cleaning data")

test_that("eq_location_clean strips country and converts to title case", {
  s <- eq_location_clean("ITALY: VERONA")
  expect_equal(s, "Verona")
})

test_that("eq_location_clean trims whitespace", {
  s <- eq_location_clean("CANADA:  BAFFIN BAY ")
  expect_equal(s, "Baffin Bay")
})

filename <- system.file("extdata", "earthquakes.tsv.gz", package = "ProjectRGCapstone")
earthquakes <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns a data frame", {
  clean_data <- eq_clean_data(earthquakes)
  expect_equal(class(clean_data), "data.frame")
})

test_that("eq_clean_data data frame contains a DATE column", {
  clean_data <- eq_clean_data(earthquakes)
  expect_true("DATE" %in% names(clean_data))
})

test_that("eq_clean_data converts LATITUDE and LONGITUDE to numeric", {
  clean_data <- eq_clean_data(earthquakes)
  expect_equal(class(clean_data$LATITUDE), "numeric")
  expect_equal(class(clean_data$LONGITUDE), "numeric")
})

test_that("eq_clean_data converts EQ_PRIMARY and TOTAL_DEATHS to numeric", {
  clean_data <- eq_clean_data(earthquakes)
  expect_equal(class(clean_data$EQ_PRIMARY), "numeric")
  expect_equal(class(clean_data$TOTAL_DEATHS), "numeric")
})
