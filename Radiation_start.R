# Radiation Prediction Project
library(tidyverse)
training <- untar("training.tar.gz", list = T)
untar("training.tar.gz", files = "training/100001.csv")
extract_some <- read_csv("training/100001.csv", col_names = F)

# dt is the change in time (microseconds) since last recording
# rad is the amount of radiation detected, measured in kEV
names(extract_some) <- c("dt", "rad")

