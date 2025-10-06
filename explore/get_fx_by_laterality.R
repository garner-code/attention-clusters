# plot attention clusters data by laterality of target
# K. Garner, 2025
######################################################
rm(list=ls())

library(tidyverse)
library(ez)
source("../R/behavioural-data-analysis-functions.R")
source("../R/behavioural-data-plotting-functions.R")
source("../R/data-wrangles.R")

## load data
sub_nums <- t(read.csv('../clusters/derivatives/complete-participants.csv', header = FALSE))
data_path = '../clusters/derivatives/'
raw <- get_raw_data(sub_nums, data_path, new=FALSE)
clean_dat <- do.call(rbind, lapply(sub_nums, function(x) clean_sub_data(x, raw)))

excl_acc <- raw %>% group_by(sub) %>% summarise(acc = mean(resp), N=length(resp))
excls <- excl_acc$sub[excl_acc$acc < .6]
clean_dat <- clean_dat %>% filter(!sub %in% excls) # 2 subs excluded

## Now I want to summarise the key variables to get the mean RTs, but I will be breaking down by 
## laterality of target (left/right visual field)
sum_dat <- clean_dat %>% group_by(sub, cert, reward_type, position) %>%
              summarise(mean=mean(rt)) %>%
              ungroup() %>%
              group_by(cert, reward_type, position) %>%
              summarise(M=mean(mean)) 

## plot the data
sum_dat %>% ggplot(aes(x=cert, y=M, group=reward_type, colour=reward_type)) +
              geom_point() + geom_line() +
              facet_wrap(~position) +
              labs(y = "Mean RT (s)", x = "Cue Certainty") +
              theme_minimal() +
              theme(legend.position = "top")

## analysis dat
analysis_dat <- clean_dat %>% group_by(sub, cert, reward_type, position) %>%
  summarise(mean=mean(rt)) 

## run anova
ezANOVA(data=analysis_dat, dv=mean, wid=sub,
          within = .(cert, reward_type, position),
          type=3, detailed=TRUE)