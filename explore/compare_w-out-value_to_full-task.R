# plot attention clusters data from the phase without value
# cues (only the spatial cue)
# compare to a subset of the full task data
# K. Garner, 2025
######################################################
rm(list=ls())

library(tidyverse)
library(ez)
source("../R/behavioural-data-analysis-functions.R")
source("../R/behavioural-data-plotting-functions.R")
source("../R/data-wrangles.R")

######################################################
# functions for this stage
get_participant_cueing_data <- function(subjects, sessions, data_path, ses_type = "beh") {
  # this function loads each participant's data
  # from the spatial cueing phase (no value cues)
  # and concatenates into a longform dataset
  # key args
  # -- subjects = list of subject numbers (integers)
  # -- sessions = list of session numbers (same across subjects)
  # -- data_path = path to data
  # -- ses_type = "behav" or "func"
  fn = "_ses-01_task-learnCues_events.tsv"
  d = do.call(rbind, lapply(subjects, get_dat, sessions=sessions, data_path=data_path, ses_type=ses_type, fn=fn))
  
  d <- allocate_cue_conditions_on_d(d)
  d
}

allocate_cue_conditions_on_d <- function(d){
  # allocate factors to the dataset d (output by either get_dat or get_fmri_dat)
  # ALLOCATE CUE CONDITIONS
  
  d$cert <- NA
  d$cert[ d$cue == 1 & d$loc == 1 ] = ".8" # recoded from full task function using info in json file 'sub-150_ses-01_task-learnCues_events.json'
  d$cert[ d$cue == 1 & d$loc == 2 ] = ".2"
  d$cert[ d$cue == 2 & d$loc == 2 ] = ".8"
  d$cert[ d$cue == 2 & d$loc == 1 ] = ".2"
  d$cert[ d$cue == 3 ] = ".5"
  d$cert <- as.factor(d$cert)
  
  d
}

get_raw_cue_data <- function(sub_nums, data_path, new, data_func, savefpath = '../clusters/derivatives/'){
  # read in the raw data 
  # kwargs:
  # -- sub_nums: a vector of subject nums w complete datasets
  # -- data_path: where is the data? (points to raw data for new, and derivatives for old)
  # -- new: TRUE = reading data in for the first time, FALSE = read in previously saved RData file
  # -- savefpath = where to save RData file if new == TRUE
  if (new){
    sessions <- rep(1, length(sub_nums))
    raw <- mapply(data_func, sub_nums, sessions, 
                  MoreArgs = list(data_path = data_path), SIMPLIFY = FALSE)
    raw <- do.call(rbind, raw)
    # save raw data so don't have to do this each time
    save(raw, file = paste(savefpath, 'raw_cue_data.RData', sep=""))
  } else {
    load(paste(data_path, 'raw__cue_data.RData', sep=""))
  }
  raw
}

clean_sub_cue_data <- function(subject, data, sd_reject=2.5, RT_min=0.1){
  # this function takes the subject index (defined in the variable subject)
  # and all the raw data from the cueing phase
  # it filters the data to get the individual subject's data, then trims to
  # get the correct RTs that are > .1 s, and are < 2.5 * sd from the median
  # for each certainty condition
  # key args
  # -- subject = subject number
  # -- data = dataframe
  data %>% filter(sub == subject) %>% 
    filter(rt > RT_min) %>%
    filter(resp == 1) %>% 
    group_by(sess, cert) %>%
    mutate(mu = median(rt), sigma = sd(rt), N = length(rt)) %>%
    filter(rt < (mu + (sd_reject*sigma))) %>%
    ungroup()
}

######################################################
# load data from initial learning stage
sub_nums <- t(read.csv('../clusters/derivatives/complete-participants.csv', header = FALSE))
data_path = '../clusters/data/'
raw <- get_raw_cue_data(sub_nums, data_path, new=TRUE, data_func = get_participant_cueing_data) # at this point, we have loaded the raw cueing data

# now we need to clean it
clean_dat <- do.call(rbind, lapply(sub_nums, function(x) clean_sub_cue_data(x, raw)))
excl_acc <- raw %>% group_by(sub) %>% summarise(acc = mean(resp), N=length(resp))
excls <- excl_acc$sub[excl_acc$acc < .6]
clean_dat <- clean_dat %>% filter(!sub %in% excls) # 1 sub excluded
cue_dat <- clean_dat
cue_raw <- raw %>% filter(!sub %in% excls)

######################################################
# now load data from full task for comparison
data_path = '../clusters/derivatives/'
raw <- get_raw_data(sub_nums, data_path, new=FALSE)
clean_dat <- do.call(rbind, lapply(sub_nums, function(x) clean_sub_data(x, raw)))

excl_acc <- raw %>% group_by(sub) %>% summarise(acc = mean(resp), N=length(resp))
excls <- excl_acc$sub[excl_acc$acc < .6]
clean_dat <- clean_dat %>% filter(!sub %in% excls) # 2 subs excluded

######################################################
# now I want to combine the data from both phases, so that I can look at the 
# effect of cueing phase (no value vs full task)
cue_dat$phase <- 'cue_only'
cue_dat$reward_type <- 'no_value'
clean_dat$phase <- 'all_cues'

# now I want to take the first block of trials from the full task data to compare to the cue only data

all_dat <- rbind(cue_dat %>% filter(t > 12) %>% select("sub", "t", "cert", "phase", "reward_type", "rt"),
                 clean_dat %>% filter(t < 112) %>% select("sub", "t", "cert", "phase", "reward_type", "rt")) 


######################################################
# now summarise the data from each phase, cue, and value type
sum_dat <- all_dat %>% group_by(sub, cert, phase, reward_type) %>%
              summarise(mean=mean(rt)) %>%
              ungroup()

plt_dat <- sum_dat %>% filter(mean < 2.5) %>% group_by(cert, phase, reward_type) %>%
              summarise(M=mean(mean))

######################################################
# now plot the data
sum_dat %>% filter(phase=="cue_only") %>% ggplot(aes(x=mean)) + geom_histogram() +
  facet_wrap(~cert) 
sum_dat %>% filter(phase=="all_cues") %>% ggplot(aes(x=mean)) + geom_histogram() +
  facet_wrap(~cert) 

plt_dat %>% ggplot(aes(x=cert, y=M, group=reward_type, colour=reward_type)) +
              geom_point() + geom_line() +
              facet_wrap(~phase) +
              labs(y = "Mean RT (s)", x = "Cue Certainty") +
              theme_minimal() +
              theme(legend.position = "top")
