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


# Bold section: Working with all of the data. Going to untar everything, then sample some of the resulting sets, probably like 50% 
  # ids range from 100001 to 109700, will sample from this
#untar("training.tar.gz")


train_answers <- read_csv("trainingAnswers.csv") %>% mutate(RunID = as.character(RunID))
train_size <- train_answers %>% summarize(n = n())

sample_frac <- 0.25
n_files <- floor(train_size$n * sample_frac ) # If I dont have this set to floor it takes forever to run, might also just need to de-frag
samp_files <- sample(1:train_size$n, n_files)
n_files_names <- sprintf("%0.4d", samp_files)
file_names <- paste0("training/10", n_files_names, ".csv")

# Need to do some summarizing, will run in lapply then condense
condenser <- function(file_name){
  rad_run <- read_csv(file_name, col_names = F) %>% mutate(id = substr(file_name, start = 10, stop = 15))
  names(rad_run) <- c("dt", "rad", "id")
  
  rad_run <- rad_run %>% mutate(time = cumsum(dt), centiseconds = floor(time / 10000)) %>% group_by(id, centiseconds) %>%
    summarize(total = sum(rad),
              avg = mean(rad),
              max_rad = max(rad),
              min_rad = min(rad),
              spread_rad = max_rad - min_rad,
              spread_scaled = spread_rad * max_rad,
              sd_rad = sd(rad)) %>% select(-min_rad, -spread_rad)
  return(rad_run)
}
extract_half <- lapply(file_names, condenser) %>% bind_rows()

train_split <- train_answers %>% filter(!is.na(SourceTime)) %>% split(train_answers$SourceID)

# Also want to look into this with respect to scaled spread, just dont want to reload data (current spread_scaled not excessively useful)
extract_half %>% filter(id %in% train_split[[2]]$RunID[1:4])  %>% 
  ggplot(aes(x = centiseconds, y = total)) +
  geom_point() + geom_line(colour = "cyan") + ggtitle("Some HEU incidents") + xlab("Time in centiseconds") + ylab("Total Count") +
  geom_vline(xintercept = train_split[[2]]$SourceTime[1:2]*100, colour = rep(c("red", "green"), 2)) +
  facet_wrap(~ id, scales = "free") 




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


# Exploring just one type of radiation pattern for now
answers <- read_csv("trainingAnswers.csv",
                    col_types = list(col_integer(), col_integer(), col_double()))

# Lets do radiation type 1 which I believe is HeU
heu_training <- answers %>% filter(SourceID %in% 1) %>% select(RunID)
file_names <- paste0("training/", heu_training$RunID, ".csv")

condenser_fn <- function(file_name){
  # Do the work on each read in rather than after to save memory, this condenses the data
  temp <- read_csv(file_name, col_names = F)
  temp$run <- as.numeric(str_extract(file_name, "[0-9]+"))
  temp <- temp %>% rename(Time = X1, Count = X2) #%>% left_join(answers, by = c("run" = "RunID"))
  
  heu <- temp %>% group_by(run) %>% mutate(time_s = cumsum(Time) / 1000000,
                                                    time_s_app = round(time_s, 2)) %>% group_by(run, time_s_app) %>%
    summarize(Count = sum(Count))
  return(heu)
}

extract_some <- lapply(file_names, condenser_fn) %>% bind_rows()

# Test plot
extract_some %>% filter(run %in% c(104901, 104902, 104903, 104904)) %>% ggplot(aes(x = time_s_app, y = Count)) + geom_line() +
  ggtitle("Plot for the first Heu Run") + xlab("Time in seconds") + ylab("Count Observed") +
  geom_vline(xintercept = answers[4901,]$SourceTime, col = "red") +
  geom_vline(xintercept = answers[4902,]$SourceTime, col = "blue") +
  geom_vline(xintercept = answers[4903,]$SourceTime, col = "green") +
  geom_vline(xintercept = answers[4904,]$SourceTime, col = "orange") +
  facet_wrap( ~ run)


# Change the column to get time distance from closest point
extract_some <- extract_some %>% 
  mutate(index = run - 100000,
         dist_closest = time_s_app - answers$SourceTime[index]) %>%
  select(-index)

extract_some %>% filter(run %in% c(104901, 104902, 104903, 104904)) %>%
  ggplot(aes(x = dist_closest, y = Count)) + geom_line() +
  ggtitle("Plot for the first few Heu Run") + xlab("Time in seconds from closest instance") +
  ylab("Count Observed") +
  geom_vline(xintercept = 0, col = "red") +
  facet_wrap( ~ run)


# Try a differenced time series: Looks a little unexpected, thought there would be more bumpy areas
extract_some %>% filter(run %in% c(104901, 104902, 104903, 104904)) %>%
  group_by(run) %>%
  mutate(diff_Count = Count - lag(Count, default = min(Count))) %>% 
  ggplot(aes(x = dist_closest, y = diff_Count)) + geom_line() +
  ggtitle("Plot for the first few Heu Run") + xlab("Time in seconds from closest instance") +
  ylab("Count Observed") +
  geom_vline(xintercept = 0, col = "red") +
  facet_wrap( ~ run)

# How to understand the peaks
  # They dont all occur at the same time as us being closest
  # Perhaps it has something to do wtih travel speed

# Look into the acf of the functions above as it is of interest, high correlations are likely unnatural
  # Or something similar
dist_info <- extract_some %>% mutate(time_s = floor(time_s_app)) %>% group_by(run, time_s) %>% summarize(tot_count = sum(Count),
                                                                                            variation = var(Count))

  # Also want to investigate this as a time series
library(xts)
library(zoo)
# This should generate the desired time series object, only doing on a subset to test functionality
time_series_info <- extract_some %>% filter(run %in% c(104901, 104902, 104903, 104904)) %>%
  mutate(time_s = floor(time_s_app)) %>% group_by(time_s, run) %>% 
  nest() %>% 
  mutate(acf_res = map(data, ~ acf(.x$Count, plot = FALSE))) %>%
  mutate(acf_tot = map_dbl(acf_res, ~ sum(abs(.x$acf)))) # R apparently isnt a fan of differenced time series in here

# Something funky going on here
  # We should expect a time with radiation exposure to have a higher absolute acf than places that are just noisey
  # Or maybe thats an incorrect assumption

# Quick solution idea: maybe it is how we are keeping track of time
  # Write a new condeser that condenses on every 5th observation
condenser_fn_5obs <- function(file_name){
  # Do the work on each read in rather than after to save memory, this condenses the data
  temp <- read_csv(file_name, col_names = F)
  temp$run <- as.numeric(str_extract(file_name, "[0-9]+"))
  temp <- temp %>% rename(Time = X1, Count = X2) #%>% left_join(answers, by = c("run" = "RunID"))
  
  heu <- temp %>% group_by(run) %>% mutate(time_s = cumsum(Time) / 1000000,
                                           time_s_app = round(time_s, 2),
                                           fake_row_n = row_number() %/% 5) %>% group_by(run, fake_row_n) %>%
    summarize(Count = sum(Count),
              time_start = first(time_s),
              time_end = last(time_s))
  return(heu)
}

# Time is wrong in this
extract_some_5obs <- lapply(file_names, condenser_fn_5obs) %>% bind_rows()

extract_some_5obs %>% filter(run %in% c(104901, 104902, 104903, 104904)) %>% mutate(time = cumsum(time_taken)) %>%
  ggplot(aes(x = time, y = Count)) + geom_line() +
  ggtitle("Plot for the first Heu Run") + xlab("Time in seconds") + ylab("Count Observed") +
  geom_vline(xintercept = answers[4901,]$SourceTime, col = "red") +
  geom_vline(xintercept = answers[4902,]$SourceTime, col = "blue") +
  geom_vline(xintercept = answers[4903,]$SourceTime, col = "green") +
  geom_vline(xintercept = answers[4904,]$SourceTime, col = "orange") +
  facet_wrap( ~ run)
# Baseline
# Lets do radiation type 1 which I believe is HeU: already loaded above in extract_some. Specify different SourceID for alternates
# But before that, lets explore what a "baseline" looks like
base_training <- answers %>% filter(SourceID %in% 0) %>% select(RunID) %>% filter(row_number() < 100)
file_names <- paste0("training/", base_training$RunID, ".csv")

condenser_fn <- function(file_name){
  # Do the work on each read in rather than after to save memory, this condenses the data
  temp <- read_csv(file_name, col_names = F)
  temp$run <- as.numeric(str_extract(file_name, "[0-9]+"))
  temp <- temp %>% rename(Time = X1, Count = X2) #%>% left_join(answers, by = c("run" = "RunID"))
  
  heu <- temp %>% group_by(run) %>% mutate(time_s = cumsum(Time) / 1000000,
                                           time_s_app = round(time_s, 2)) %>% group_by(run, time_s_app) %>%
    summarize(Count = sum(Count))
  return(heu)
}

extract_base <- lapply(file_names, condenser_fn) %>% bind_rows()

# Dont need to get the answers for the baseline case because there is no signal, it is all noise
  # Try to classify the amount of noise
  # Is this a mixture model? I think assuming Gaussian or even Poisson for noise is fair
    # Can try to extend the EM work with Dani: Should first MLE the distribution parameters across the sample

