# Radiation Prediction Project
library(tidyverse)
training <- untar("training.tar.gz", list = T)
untar("training.tar.gz", files = c("training/100001.csv",
                                   "training/100002.csv",
                                   "training/100003.csv",
                                   "training/100004.csv",
                                   "training/100005.csv",
                                   "training/100006.csv",
                                   "training/100007.csv",
                                   "training/100008.csv",
                                   "training/100009.csv",
                                   "training/100010.csv"))
n_files <- 10
n_files_names <- sprintf("%0.3d", 1:n_files)
file_names <- paste0("training/100", n_files_names, ".csv")

extract_some <- lapply(file_names, read_csv, col_names = F) 

# I think I need a for loop to add identifier because of iterative nature
for(i in 1:n_files){
  extract_some[[i]] <- extract_some[[i]] %>% mutate(id = n_files_names[i])
}
rad_set <- bind_rows(extract_some)

# Add identifier to each run

# dt is the change in time (microseconds) since last recording
# rad is the amount of radiation detected, measured in kEV
names(rad_set) <- c("dt", "rad", "id")

# Get the idea of what a run looks like
rad_set <- rad_set %>% group_by(id) %>% mutate(time = cumsum(dt))
# Smaller plot sample, too big at 10 wasnt plotting

# rad_set %>% filter(id %in% c("001", "002", "003", "004", "005", "006")) %>% ggplot(aes(x = time, y = rad)) + 
#   geom_line(colour = "cyan") + geom_point() + ggtitle("10 Radiation Runs") +
#   xlab("Time in microseconds") + ylab("Count") + facet_wrap(~ id, scales = "free")

# Too many points at microseconds, can fit model here but need to summarize a bit to plot
  # Will make into hundredths of a second as this is the level of specificity we need in reporting
rad_set_sum <- rad_set %>% mutate(centiseconds = floor(time / 10000)) %>% group_by(id, centiseconds) %>% summarize(total = sum(rad),
                                                                                                               avg = mean(rad),
                                                                                                               max_rad = max(rad),
                                                                                                               sd_rad = sd(rad))

rad_set_sum %>% ungroup() %>% filter(id %in% "001") %>% ggplot(aes(x = centiseconds, y = total)) + geom_point() + geom_line(colour = "cyan") +
  ggtitle("Radiation Run in Centiseconds") + xlab("Time in centiseconds") + ylab("Count")

rad_set_sum %>% ungroup() %>% filter(id %in% "001") %>% ggplot(aes(x = centiseconds, y = avg)) + geom_point() + 
  geom_errorbar(aes(ymin = avg - sd_rad, ymax = avg + sd_rad, alpha = 0.2)) + ggtitle("Average Radiation Run in Centiseconds") + 
  xlab("Time in centiseconds") + ylab("Count and 1sd")

# The above is some plots for a signal with no source



# The following plots will have a source ID of 1 (HEU) at a time of 78.3 seconds after start
untar("training.tar.gz", files = c("training/104901.csv"))
is_radioactive <- read_csv("training/104901.csv", col_names = F) %>% mutate(id = "104901")
names(is_radioactive) <- c("dt", "rad", "id")

is_radioactive_sum <- is_radioactive %>%  mutate(time = cumsum(dt), centiseconds = floor(time / 10000)) %>% group_by(id, centiseconds) %>%
  summarize(total = sum(rad),
                     avg = mean(rad),
                     max_rad = max(rad),
                     sd_rad = sd(rad))

source_and_nosource <- is_radioactive_sum %>% bind_rows(rad_set_sum)

# Plot on the right has HEU as a source at time 78.3 seconds while plot on the left has no source and is just natural radiation
source_and_nosource %>% ungroup() %>% filter(id %in% c("001","104901")) %>% ggplot(aes(x = centiseconds, y = total)) + geom_point() + 
  geom_line(colour = "cyan") +
  ggtitle("Radiation Run in Centiseconds, Source vs. No source") + xlab("Time in centiseconds") + ylab("Count") +
  geom_vline(xintercept = 78.3*100, colour = "red") + facet_wrap(~ id)


 # # # # # # # # Separate part, has to do with the chemical curves not the vehicle readings # # # # # # # # # # 
# Identifying the curves
SourceData <- read_csv("SourceInfov3/SourceData.csv")


# Check the curves to make sure they match
head(SourceData)

# Plot looks weird in facet_wrap b/c of WGPu being so much stronger in count rate at low energy
SourceData %>% ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line() + facet_wrap(~SourceType)

# Similarity plot with WPGu scaled by 10^6 (division)
modified <- SourceData %>% mutate(new_Count = case_when(SourceType %in% "WGPu" ~ CountRate / 10^6,
                                                        TRUE ~ CountRate))
# Still no bueno
modified %>% ggplot(aes(x = PhotonEnergy, y = new_Count)) + geom_point() + geom_line() + facet_wrap(~SourceType)

# Instead just plot individually, need to filter on Shielding though
SourceData %>% filter(SourceType %in% "HEU") %>%  ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line(colour = "Blue") +
  facet_wrap( ~ Shielding, scales = "free") + ggtitle("Count against energy for different Shielding of HEU")
SourceData %>% filter(SourceType %in% "WGPu") %>% ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line(colour = "Red") +
  facet_wrap( ~ Shielding, scales = "free") + ggtitle("Count against energy for different Shielding of WGPu")
SourceData %>% filter(SourceType %in% "131I") %>% ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line(colour = "Green") +
  facet_wrap( ~ Shielding, scales = "free") + ggtitle("Count against energy for different Shielding of 131I")
SourceData %>% filter(SourceType %in% "60Co") %>% ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line(colour = "Orange") +
  facet_wrap( ~ Shielding, scales = "free") + ggtitle("Count against energy for different Shielding of 60Co")
SourceData %>% filter(SourceType %in% "99mTc") %>%ggplot(aes(x = PhotonEnergy, y = CountRate)) + geom_point() + geom_line(colour = "Black") +
  facet_wrap( ~ Shielding, scales = "free") + ggtitle("Count against energy for different Shielding of 99mTc")

# Curiously, what if we plot the ratio of each Photon energy between shieldings
  # Min countRate = 1.70403e-14 that isnt zero to prevent inf
ratio_Count <- SourceData %>% group_by(SourceType, PhotonEnergy) %>% summarize(ratio_count = last(CountRate) / max(first(CountRate), 1.70403e-14))

# Example plots of what the ratios roughly look like, slight modification to min observed value for those with underflow errors
ratio_Count %>% ungroup() %>% filter(SourceType %in% "HEU") %>% ggplot(aes(x = PhotonEnergy, y = ratio_count)) +
  geom_point() +
  geom_line(colour = "Blue")

ratio_Count %>% ungroup() %>% filter(SourceType %in% "99mTc") %>% ggplot(aes(x = PhotonEnergy, y = ratio_count)) +
  geom_point() +
  geom_line(colour = "Black")

