context("Timeline geoms")

if (require(ggplot2)){

filename <- system.file("extdata", "earthquakes.tsv.gz", package = "ProjectRGCapstone")
earthquakes <- eq_clean_data(readr::read_delim(filename, delim = "\t"))

test_that("geom_timeline layer is a GeomTimeline", {
  g <- ggplot2::ggplot(earthquakes, ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
  g <- g + geom_timeline(alpha = 0.5)
  expect_is(g$layers[[1]]$geom, "GeomTimeline")
})

test_that("geom_timeline contains correct alpha aesthetic", {
  g <- ggplot2::ggplot(earthquakes, ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
  g <- g + geom_timeline(alpha = 0.456)
  expect_equal(g$layers[[1]]$aes_params$alpha, 0.456)
})

test_that("geom_timeline_label layer is a GeomTimelineLabel", {
  g <- ggplot2::ggplot(earthquakes, ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
  g <- g + geom_timeline(alpha = 0.5)
  g <- g + geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))
  expect_is(g$layers[[2]]$geom, "GeomTimelineLabel")
})


}
