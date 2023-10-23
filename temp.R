library(dplyr)
library(arrow)
library(ggplot2)
library(scales)
library(lubridate)
library(geofacet)
library(geobr)

# http://www.jasonfabris.com/blog/dteformat/
dte_formatter <- function(x) {
  substr(format(x, "%b"),1,1)
}

capitals <- read_capitals(as_sf = TRUE) %>%
  mutate(code_muni = ifelse(code_muni == 2803203, 2800308, code_muni)) %>%
  sf::st_drop_geometry()

br_grid <- br_states_grid1 %>%
  left_join(capitals, by = c("code" = "abbrev_state")) %>%
  select(row, col, code, name = name_muni)

temp <- open_dataset(sources = "../brclim2/output_data/parquet/2m_temperature_min.parquet") %>%
  filter(name == "2m_temperature_min_mean") %>%
  filter(code_muni %in% capitals$code_muni) %>%
  collect() %>%
  right_join(capitals) %>%
  mutate(
    year = lubridate::year(date),
    date = update(date, year = 1)
  )

ggplot(data = temp, aes(x = date, y = value, group = year, color = year)) +
  geom_line(lwd = .5, alpha = .3) +
  scale_x_date(date_breaks = "1 month", labels = dte_formatter) +
  scale_colour_viridis_c(option = "B") +
  facet_geo(~abbrev_state, grid = br_grid, label = "name") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(x = "Month", y = "Temperature (K)", color = "Year")

ggsave(filename = "temp_capitals.pdf", width = 210, height = 297, units = "mm")
