################################################################################
# The purpose of this script is to use the search_maps() function to
# analyze all of the maps Nadeo has uploaded to TMX.
################################################################################

library(tidyverse)

# Get all Nadeo maps
nadeo <- search_maps(params = list(author = "Ubisoft Nadeo", count = 1000))

# Difficulty distribution
ggplot(nadeo, aes(Difficulty)) +
  geom_histogram(bins = 4, color = "black") +
  theme_bw() +
  scale_y_continuous("Count", breaks = seq(0, 400, by = 50), limits = c(0, 400)) +
  ggtitle("Nadeo Maps Difficulty Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Award count distribution
ggplot(nadeo, aes(AwardCount)) +
  geom_histogram(breaks = seq(0, 50, by = 1), color = "black") +
  theme_bw() +
  scale_x_continuous("Award Count", breaks = seq(0, 50, by = 5)) +
  scale_y_continuous("Count", breaks = seq(0, 500, by = 50), limits = c(0, 500)) +
  ggtitle("Nadeo Maps Award Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Look at most awarded maps
nadeo |>
  arrange(-AwardCount) |>
  select(Name, AwardCount) |>
  head(20)

# Download count distribution
ggplot(nadeo, aes(DownloadCount)) +
  geom_histogram(breaks = seq(0, 8000, by = 500), color = "black") +
  theme_bw() +
  scale_x_continuous("Download Count", breaks = seq(0, 8000, by = 1000)) +
  scale_y_continuous("Count", breaks = seq(0, 200, by = 25), limits = c(0, 200)) +
  ggtitle("Nadeo Maps Download Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Look at most downloaded maps
nadeo |>
  arrange(-DownloadCount) |>
  select(Name, DownloadCount) |>
  head(20)
