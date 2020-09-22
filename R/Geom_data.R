



#' Earthquake timeline plot
#'
#' The timeline geom plots a time line of earthquakes with a point for each
#' earthquake. The x aesthetic is a date and the y aesthetic is a factor
#' indicating some stratification in which case multiple time lines will be
#' plotted for each level of the factor (e.g. country). Optional aesthetics
#' include color, size, and alpha.
#'
#' @inheritParams ggplot2::geom_point
#' @param data The earthquake data to be plotted. See the example for details.
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics
#' (required aesthetics are in bold):
#'
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item color
#'   \item size
#'   \item alpha
#' }
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # You can plot a basic earthquake timeline as follows:
#'
#' library(dplyr)
#'
#' recent_earthquakes <- clean_earthquakes %>%
#'     filter(COUNTRY == "CHINA", DATE >= ymd('2000-01-01'))
#'
#' g <- ggplot(recent_earthquakes,
#'             aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
#' g <- g + geom_timeline(alpha = 0.5)
#' g <- g + theme_classic()
#' g <- g + theme(legend.position = "bottom",
#'                axis.line.y = element_blank(),
#'                axis.ticks.y = element_blank(),
#'                axis.title.y = element_blank(),
#'                axis.text.y = element_blank())
#' g <- g + guides(color = guide_colorbar(title = "# deaths"),
#'                 size = guide_legend("Richter scale value"))
#' g
#'}
#' @note
#' See \code{\link{geom_timeline_label}} for additional examples.
#'
#' @export
#'
geom_timeline <- function (mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimeline, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm,
                                                          ...))
}


GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "color"),
  default_aes = ggplot2::aes(
    shape = 19,
    color = "black",
    size = 1.5,
    alpha = NA,
    fill = NA,
    stroke = 0.5
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    grid::grobTree(
      grid::segmentsGrob(
        x0 = 0.0,
        y0 = coords$y,
        x1 = 1.0,
        y1 = coords$y,
        gp = grid::gpar(col = "grey")
      ),
      grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
    )
  }
)





#' Labels for earthquake timeline plot
#'
#' This geom is intended to be used in conjunction with the \code{\link{geom_timeline}} geom to add a vertical line with a text
#' annotation (e.g. the location of the earthquake) for each data point on an earthquake timeline with the NOAA database.. The
#' x aesthetic specifies the date of the earthquake and the label aesthetic specifies the label for the annotation. An optional
#' n_max aesthetic can be used to subset to n_max earthquakes, meaning the n_max largest (by magnitude) will be labeled.
#'
#' @inheritParams ggplot2::geom_point
#' @param data The earthquake data to be plotted (NOAA database). See the example for details.
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics
#' (required aesthetics are in bold):
#'
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{label}
#'   \item n_max
#' }
#'
#' @examples
#' \dontrun{
#' # You can plot a pair of earthquake timelines with labels as follows:
#'
#' library(dplyr)
#'
#' recent_earthquakes <- clean_earthquakes %>%
#'     filter(COUNTRY == "CHINA" | COUNTRY == "USA", DATE >= ymd('2000-01-01'))
#'
#' g <- ggplot(recent_earthquakes,
#'             aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
#' g <- g + geom_timeline(alpha = 0.5)
#' g <- g + geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5))
#' g <- g + theme_classic()
#' g <- g + theme(legend.position = "bottom",
#'                axis.line.y = element_blank(),
#'                axis.ticks.y = element_blank(),
#'                axis.title.y = element_blank())
#' g <- g + guides(color = guide_colorbar(title = "# deaths"),
#'                 size = guide_legend("Richter scale value"))
#' g
#'}
#'
#' @import ggplot2
#'
#' @export
#'
geom_timeline_label <- function (mapping = NULL, data = NULL, stat = "identity",
                                 position = "identity", na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping,
                 data = data, stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes, params = list(na.rm = na.rm,
                                                          ...))
}

GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel",
    ggplot2::Geom,
    required_aes = c("x", "label"),
    default_aes = ggplot2::aes(n_max = NA),
    setup_data = function(data, params) {
      n <- data$n_max[1]
      if (is.numeric(n)) {
        dplyr::top_n(dplyr::group_by_(data, "group"), n, size)
      } else {
        data
      }
    },
    draw_panel = function(data, panel_scales, coord) {
      coords <- coord$transform(data, panel_scales)
      grid::grobTree(
        grid::segmentsGrob(
          x0 = coords$x,
          y0 = coords$y,
          x1 = coords$x,
          y1 = coords$y + 0.1,
          gp = grid::gpar()
        ),
        grid::textGrob(
          x = coords$x,
          y = coords$y + 0.1,
          label = coords$label,
          rot = 45,
          hjust = -0.1,
          vjust = -0.1,
          gp = grid::gpar()
        )
      )
    }
  )




