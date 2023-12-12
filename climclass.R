library(ClimClass)
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(purrr)
library(tmap)

prec <- open_dataset(sources = "../brclim2/output_data/parquet/total_precipitation_sum.parquet") %>%
  filter(name == "total_precipitation_sum_mean") %>%
  filter(year(date) == 2022) %>%
  mutate(value = value * 1000) %>%
  mutate(month = month(date)) %>%
  select(code_muni, month, value) %>%
  collect() %>%
  group_by(code_muni, month) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  complete(month = 1:12, fill = list(value = 0)) %>%
  ungroup() %>%
  arrange(code_muni, month) %>%
  rename(P = value)

tmin <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_min.parquet") %>%
  filter(name == "2m_temperature_min_mean") %>%
  filter(year(date) == 2022) %>%
  mutate(value = value - 273.15) %>%
  mutate(month = month(date)) %>%
  select(code_muni, month, value) %>%
  collect() %>%
  group_by(code_muni, month) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  complete(month = 1:12, fill = list(value = 0)) %>%
  ungroup() %>%
  arrange(code_muni, month) %>%
  rename(Tn = value)

tmax <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_max.parquet") %>%
  filter(name == "2m_temperature_max_mean") %>%
  filter(year(date) == 2022) %>%
  mutate(value = value - 273.15) %>%
  mutate(month = month(date)) %>%
  select(code_muni, month, value) %>%
  collect() %>%
  group_by(code_muni, month) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  complete(month = 1:12, fill = list(value = 0)) %>%
  ungroup() %>%
  arrange(code_muni, month) %>%
  rename(Tx = value)

tmean <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_mean.parquet") %>%
  filter(name == "2m_temperature_mean_mean") %>%
  filter(year(date) == 2022) %>%
  mutate(value = value - 273.15) %>%
  mutate(month = month(date)) %>%
  select(code_muni, month, value) %>%
  collect() %>%
  group_by(code_muni, month) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  complete(month = 1:12, fill = list(value = 0)) %>%
  ungroup() %>%
  arrange(code_muni, month) %>%
  rename(Tm = value)

cdata <- inner_join(prec, tmax, by = c("code_muni", "month")) %>%
  inner_join(tmin, by = c("code_muni", "month")) %>%
  inner_join(tmean, by = c("code_muni", "month"))

ldata <- group_split(cdata, code_muni, .keep = FALSE)
names(ldata) <- unique(cdata$code_muni)

lclim <- map(.x = ldata, .f = koeppen_geiger)
names(lclim) <- unique(cdata$code_muni)

kclim <- lclim %>% list_rbind()
kclim$code_muni <- unique(cdata$code_muni)

unique(kclim$class)

mun <- geobr::read_municipality() %>%
  sf::st_make_valid()

tm_shape(left_join(mun, kclim, by = "code_muni")) +
  tm_polygons(fill = "class", col_alpha = .1)





