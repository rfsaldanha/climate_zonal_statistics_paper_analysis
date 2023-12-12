library(dplyr)
library(tidyr)
library(arrow)
library(ggplot2)
library(scales)
library(lubridate)
library(geobr)
library(tmap)

mun <- read_municipality() %>%
  sf::st_make_valid()

# TMAX mean
tmax <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_max.parquet") %>%
  filter(name == "2m_temperature_max_mean") %>%
  filter(
    date == as.Date("1950-01-01") | date == as.Date("1960-01-01") |
      date == as.Date("1970-01-01") | date == as.Date("1980-01-01") |
      date == as.Date("1990-01-01") | date == as.Date("2000-01-01") |
      date == as.Date("2010-01-01") | date == as.Date("2020-01-01")
  ) %>%
  collect() %>%
  mutate(date = as.character(date))

tmax_map <- tm_shape(right_join(mun, tmax, by = "code_muni")) +
  tm_polygons(fill = "value",
              col_alpha = .1,
              fill.scale = tm_scale_continuous(values = "brewer.or_rd"),
              fill.legend = tm_legend(
                title = "", orientation = "landscape", na.show = FALSE,
                position = tm_pos_out("center", "bottom"), frame = FALSE
              )) +
  tm_facets_wrap("date", ncols = 2) +
  tm_layout(frame = FALSE)

tmap_save(tm = tmax_map, filename = "tmax_map.png", dpi = 300)

# TMIN mean
tmin <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_min.parquet") %>%
  filter(name == "2m_temperature_min_mean") %>%
  filter(
    date == as.Date("1950-01-01") | date == as.Date("1960-01-01") |
      date == as.Date("1970-01-01") | date == as.Date("1980-01-01") |
      date == as.Date("1990-01-01") | date == as.Date("2000-01-01") |
      date == as.Date("2010-01-01") | date == as.Date("2020-01-01")
  ) %>%
  collect() %>%
  mutate(date = as.character(date))

tmin_map <- tm_shape(right_join(mun, tmin, by = "code_muni")) +
  tm_polygons(fill = "value",
              col_alpha = .1,
              fill.scale = tm_scale_continuous(values = "-brewer.bu_pu"),
              fill.legend = tm_legend(
                title = "", orientation = "landscape", na.show = FALSE,
                position = tm_pos_out("center", "bottom"), frame = FALSE
              )) +
  tm_facets_wrap("date", ncols = 2) +
  tm_layout(frame = FALSE)

tmap_save(tm = tmin_map, filename = "tmin_map.png", dpi = 300)


# PREC std dev
prec <- open_dataset(sources = "../brclim2/output_data/parquet/total_precipitation_sum.parquet") %>%
  filter(name == "total_precipitation_sum_stdev") %>%
  filter(
    date == as.Date("1950-01-01") | date == as.Date("1960-01-01") |
      date == as.Date("1970-01-01") | date == as.Date("1980-01-01") |
      date == as.Date("1990-01-01") | date == as.Date("2000-01-01") |
      date == as.Date("2010-01-01") | date == as.Date("2020-01-01")
  ) %>%
  collect() %>%
  mutate(date = as.character(date))

prec_map <- tm_shape(right_join(mun, prec, by = "code_muni")) +
  tm_polygons(fill = "value",
              col_alpha = .1,
              fill.scale = tm_scale_continuous(values = "brewer.blues"),
              fill.legend = tm_legend(
                title = "", orientation = "landscape", na.show = FALSE,
                position = tm_pos_out("center", "bottom"), frame = FALSE
              )) +
  tm_facets_wrap("date", ncols = 2) +
  tm_layout(frame = FALSE)

tmap_save(tm = prec_map, filename = "prec_map.png", dpi = 300)




# PREC std dev
prec <- open_dataset(sources = "../brclim2/output_data/parquet/total_precipitation_sum.parquet") %>%
  filter(name == "total_precipitation_sum_stdev" | name == "total_precipitation_sum_sum") %>%
  filter(
    date == as.Date("2010-01-01")
  ) %>%
  collect() %>%
  mutate(date = as.character(date)) %>%
  mutate(name = case_when(
    name == "total_precipitation_sum_stdev" ~ "Standard Deviation",
    name == "total_precipitation_sum_sum" ~ "Sum"
  ))

prec_map <- right_join(mun, prec, by = "code_muni") %>%
  filter(abbrev_state == "RJ") %>%
  tm_shape() +
  tm_polygons(fill = "value",
              col_alpha = .1,
              fill.free = TRUE,
              fill.scale = tm_scale_continuous(values = "brewer.blues"),
              fill.legend = tm_legend(
                title = "", orientation = "landscape", na.show = FALSE,
                position = tm_pos_out("center", "bottom"), frame = FALSE,
                width = 20
              )) +
  tm_facets_wrap("name", ncols = 1) +
  tm_layout(frame = TRUE)

prec_map

tmap_save(tm = prec_map, filename = "prec_map.png", dpi = 300)



