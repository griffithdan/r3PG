# This script processes raw data files into RDA files for loading by users

# Example config file
  example.config <- load_config("raw_data/default.cfg")
  save(example.config, file = "data/example.config.RData")

# Example climate file
  example.clim <- read.csv("raw_data/default.csv")
  save(example.clim, file = "data/example.clim.RData")


