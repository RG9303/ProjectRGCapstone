context("Utilities")

test_that("to_date with year, month, day", {
  date <- to_date(2017, 4, 20)
  expect_equal(date, lubridate::ymd("2017-04-20"))
})

test_that("to_date with year, month", {
  date <- to_date(2017, 4, NA)
  expect_equal(date, lubridate::ymd("2017-04-1"))
})

test_that("to_date with year only", {
  date <- to_date(2017, NA, NA)
  expect_equal(date, lubridate::ymd("2017-01-01"))
})

test_that("to_date with year, day only", {
  date <- to_date(2017, NA, 20)
  expect_equal(date, lubridate::ymd("2017-01-20"))
})

test_that("to_date without year", {
  expect_error(to_date(NA, 4, 20), "missing value")
})
