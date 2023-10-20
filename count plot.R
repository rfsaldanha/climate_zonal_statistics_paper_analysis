library(dplyr)
library(arrow)
library(ggplot2)
library(scales)

options(scipen=100)

surf <- open_dataset(sources = "surface_pressure_mean.parquet") %>%
  filter(date == as.Date("2022-01-01")) %>%
  filter(name == "surface_pressure_mean_count") %>%
  collect()

ggplot(data = surf, aes(x = value)) +
  geom_histogram(bins = 30, fill = "purple", color = "purple", alpha = 0.7) +
  scale_x_log10(breaks = breaks_log(7), labels = label_number()) +
  theme_bw() +
  labs(x = "Cell's count", y = "Frequency")

ggsave(filename = "count_plot.pdf", width = 15, height = 10, units = "cm")

surf %>%
  filter(value <= 1) %>%
  tally()

surf %>%
  filter(value > 1) %>%
  tally()


