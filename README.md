# ProjectRGCapstone


```{r setup}
library(ProjectRGCapstone)
library(knitr)
```

The `ProjectRGCapstone` package has data from NOAA Significant Earthquakes Database, helper functions to clean and filter the data, geoms for plotting earthquake timelines and functions for plotting interactive earthquake maps.

## Data

This package uses the database from the NOAA Significant Earthquakes which containing data on destructive earthquakes from 2150 B.C. to the present (mid April 2017). For more information, it is available at https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1. 

To use the data

```{r, message=FALSE}
filename <- system.file("extdata", "earthquakes.tsv.gz", package = "ProjectRGCapstone")
earthquakes <- readr::read_delim(filename, delim = "\t")
```


## Functions for cleaning the data

There are two functions for cleaning the earthquakes data: `eq_clean_data` adds a `DATE` column based on `YEAR`, `MONTH`, `DAY`, converts several character fields to numeric, and cleans `LOCATION_NAME` by passing it to the `eq_location_clean` function. `eq_location_clean` returns a title case character string with the location's country removed. For example,

```{r}
eq_location_clean("JAPAN:  TUWANO")
```

You can create a `clean_earthquakes` data frame with the next instructions:

```{r, message=FALSE}
library(dplyr)
clean_earthquakes <- earthquakes %>%
    eq_clean_data()
```

## Geoms for earthquake time lines

This package contains two `ggplot2` geoms for plotting earthquake time lines: `geom_timeline` which plots a time line of earthquakes with a point for each earthquake. Here is an example:

```{r, fig.show="hold", fig.width=7, message=FALSE}
library(lubridate)
library(ggplot2)
recent_earthquakes <- clean_earthquakes %>%
    filter(COUNTRY == "CHINA", DATE >= ymd('2000-01-01'))#'
g <- ggplot(recent_earthquakes,
            aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
g <- g + geom_timeline(alpha = 0.5)
g <- g + theme_classic()
g <- g + theme(legend.position = "bottom",
               axis.line.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank())
g <- g + guides(color = guide_colorbar(title = "# deaths"),
                size = guide_legend("Richter scale value"))
g
```

And `geom_timeline_label` is used in conjunction with the `geom_timeline` geom to add a vertical line with a text annotation (e.g. the location of the earthquake) for each data point on an earthquake timeline. Here is an example:

```{r, fig.width=7, fig.height=7}
recent_earthquakes <- clean_earthquakes %>%
    filter(COUNTRY == "CHINA" | COUNTRY == "USA", DATE >= ymd('2000-01-01'))
g <- ggplot(recent_earthquakes,
            aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
g <- g + geom_timeline(alpha = 0.5)
g <- g + geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5))
g <- g + theme_classic()
g <- g + theme(legend.position = "bottom",
               axis.line.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title.y = element_blank())
g <- g + guides(color = guide_colorbar(title = "# deaths"),
                size = guide_legend("Richter scale value"))
g
```

## Mapping functions

The `eq_map` function produces a map of earthquake epicenters (LATITUDE/LONGITUDE) and annotates each point with a popup window containing annotation data stored in a column of the data frame. Here is an example:

```{r, fig.width=7, fig.height=7}
clean_earthquakes %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")
```

### eq_create_label

This function creates HTML labels with location, magnitude, and total deaths for each earthquake.

Example:

```{r, fig.width=7, fig.height=7}
clean_earthquakes %>%
    filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
```

