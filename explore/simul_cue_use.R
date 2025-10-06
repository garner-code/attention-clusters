#### K. Garner
#### goal is to find out if participants use both value and spatial cues
#### on the same trials
#### to get here, I cloned my attention-clusters repo locally, and downloaded
#### the 'clusters' data folder from google drive, however, the latter
#### should also be on cloud.rdm.uq.edu.au
#### clusters is in the 'project' folder
##############################################################################
#### clear stuff and load packages
# set wd to source file location
rm(list=ls())
set.seed(42) # meaning of life
library(tidyverse)
library(pracma)
library(car)
library(gridExtra)
library(plotly)
library(htmlwidgets)
source("../R/behavioural-data-analysis-functions.R")

##############################################################################
#### functions
get_me_and_cert_ps <- function(m, bf_p = .05){
  # which subs show a sig effect of both?
  if (m["cert", 4] < bf_p & m["reward_type", 4] < bf_p){
    y = TRUE
  } else {
    y = FALSE
  }
  if (y) m
}

get_single <- function(m, bf_p = .05, fx_keep = "cert", fx_reject = "reward_type"){
  # which subs show a sig effect of both?
  if (m[fx_keep, 4] < bf_p & m[fx_reject, 4] > bf_p){
    y = TRUE
  } else {
    y = FALSE
  }
  if (y) m
}

do_sub_plts <- function(dat, sub_n, fnm = 'both_sub-%d.pdf'){
  
  ydat <- dat %>% filter(sub == sub_n) %>% filter(cert == ".2" | cert == ".8") %>%
    summarise(miny = min(rt),
              maxy = max(rt))
  
  # pa <- dat %>% filter(sub == sub_n) %>% filter(cert == ".2") %>% 
  #   ggplot(aes(x = reward_type, y = rt, group = p5_split, colour = p5_split)) +
  #   geom_point(position = position_jitter(w = .05, h = .01)) +
  #   theme(legend.position = "none") + ggtitle("cert = .2") +
  #   ylim(ydat$miny, ydat$maxy)
  pa <- dat %>% filter(sub == sub_n) %>% filter(cert == ".8") %>% 
          ggplot(aes(x = reward_type, y = rt, group = p5_split, colour = p5_split)) +
          geom_point(position = position_jitter(w = .05, h = .01)) + 
          ggtitle("cert = .8") +
          theme(legend.position = "none") +
          ylim(ydat$miny, ydat$maxy)
  pb <- dat %>% filter(sub == sub_n) %>% filter(cert == ".8") %>% 
          ggplot(aes(x=rt, group=reward_type, colour=reward_type)) +
          stat_ecdf(pad = FALSE) 
  p <- arrangeGrob(pa, pb, ncol = 2)
  ggsave(paste("../images", sprintf(fnm, sub_n), sep = "/"), p, units = "cm", width = 16, height = 16)
}

do_sub_plts_valsplt <- function(dat, sub_n, fnm = 'both_val_sub-%d.pdf'){
  
  ydat <- dat %>% filter(sub == sub_n) %>% 
    filter(reward_type == "htgt/ldst" | reward_type == "ltgt/hdst") %>%
    summarise(miny = min(rt),
              maxy = max(rt))
  
  pa <- dat %>% filter(sub == sub_n) %>% filter(reward_type == "htgt/ldst") %>% 
    ggplot(aes(x = cert, y = rt, group = p5_split, colour = p5_split)) +
    geom_point(position = position_jitter(w = .05, h = .01)) +
    theme(legend.position = "none") + ggtitle("hvl") +
    ylim(ydat$miny, ydat$maxy)
  pb <- dat %>% filter(sub == sub_n) %>% filter(reward_type == "ltgt/hdst") %>% 
    ggplot(aes(x = cert, y = rt, group = p5_split, colour = p5_split)) +
    geom_point(position = position_jitter(w = .05, h = .01)) + 
    ggtitle("lvh") +
    theme(legend.position = "none") +
    ylim(ydat$miny, ydat$maxy)
  p <- arrangeGrob(pa, pb, ncol = 2)
  ggsave(paste("../images", sprintf(fnm, sub_n), sep = "/"), p, units = "cm", width = 16, height = 16)
}

##############################################################################
#### load data 
load("../clusters/derivatives/cleaned_data.Rmd")
View(clean_dat)
# clean_dat has only correct trials, does have condition sorting
# and has had RT outliers removed

##############################################################################
#### Q1. Is it sensible to do a detrend to remove the effect of trial?
random_subs <- with(clean_dat, sample(unique(sub), size = 20))

clean_dat %>% filter(sub %in% random_subs) %>% 
  ggplot(aes(x = t, y=rt)) + 
  geom_line() + facet_wrap(~sub, scales="free_y")

clean_dat %>% filter(sub %in% random_subs) %>% 
  ggplot(aes(x = t, y=detrend(rt, tt = "linear"))) + 
  geom_line() + facet_wrap(~sub, scales="free_y")
# mixed, for some subs it does look like it might help
# have decided it is not worth it, having looked at the output

##############################################################################
#### Q2. for each sub, who shows 
mods <- lapply(unique(clean_dat$sub), sub.model, data = clean_dat)
names(mods) <- unique(clean_dat$sub)
fx <- row.names(mods[[1]])
bf_p <- .05/7
# a main effect of both certainty and value?

both <- lapply(mods, get_me_and_cert_ps)
both <- both[!sapply(both, is.null)]
both_subs <- names(both)

# a main effect of only certainty
cert <- lapply(mods, get_single)
cert <- cert[!sapply(cert, is.null)]
cert_subs <- names(cert)

# only a main effect of value
val <- lapply(mods, get_single, fx_keep = "reward_type", fx_reject = "cert")
val <- val[!sapply(val, is.null)]
val_subs <- names(val)

##### 129 subjects

#############################################################################
# Q3. do data patterns suggest single or multiple cue use per trial?

## CERT
# first, for each sub, define two columns, ranking if rts are in the 
# top or bottom 50% for cert = .8 condition, same for .2 condition
# do the same for < .25, > .75
qrtls <- clean_dat %>% group_by(sub, cert) %>%
               summarise(p33 = quantile(rt, .33),
                         p5 = quantile(rt, .5),
                         p66 = quantile(rt, .66))
dat <- inner_join(clean_dat, qrtls, by = c("sub", "cert"))
dat <- dat %>% mutate(p5_split = if_else(rt < p5, "lower", "upper"),
                      q33_split = if_else(rt < p33, "lower", if_else(rt > p66, "upper", "mid")))

# for each sub, plot .2 and .8 RTs on separate plots, by value condition
# first try points, coloured by upper vs lower
lapply(as.numeric(both_subs), do_sub_plts, dat = dat)
lapply(as.numeric(cert_subs), do_sub_plts, dat = dat, fnm = 'cert_sub-%d.pdf')
lapply(as.numeric(val_subs), do_sub_plts, dat = dat, fnm = 'val_sub-%d.pdf')


## VALUE
# first, for each sub, define two columns, ranking if rts are in the 
# top or bottom 50% for cert = .8 condition, same for .2 condition
# do the same for < .25, > .75
qrtls <- clean_dat %>% group_by(sub, reward_type) %>%
  summarise(p33 = quantile(rt, .33),
            p5 = quantile(rt, .5),
            p66 = quantile(rt, .66))
dat <- inner_join(clean_dat, qrtls, by = c("sub", "reward_type"))
dat <- dat %>% mutate(p5_split = if_else(rt < p5, "lower", "upper"),
                      q33_split = if_else(rt < p33, "lower", if_else(rt > p66, "upper", "mid")))

lapply(as.numeric(both_subs), do_sub_plts_valsplt, dat = dat)

























