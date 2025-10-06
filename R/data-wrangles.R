
get_raw_data <- function(sub_nums, data_path, new, savefpath = '~/Insync/tmp-data/clusters/derivatives/'){
  # read in the raw data 
  # kwargs:
  # -- sub_nums: a vector of subject nums w complete datasets
  # -- data_path: where is the data? (points to raw data for new, and derivatives for old)
  # -- new: TRUE = reading data in for the first time, FALSE = read in previously saved RData file
  # -- savefpath = where to save RData file if new == TRUE
  if (new){
    sessions <- rep(1, length(sub_nums))
    raw <- mapply(get_participant_data, sub_nums, sessions, 
                  MoreArgs = list(data_path = data_path), SIMPLIFY = FALSE)
    raw <- do.call(rbind, raw)
    # save raw data so don't have to do this each time
    save(raw, file = paste(savefpath, 'raw_data.RData', sep=""))
  } else {
    load(paste(data_path, 'raw_data.RData', sep=""))
  }
  raw
}

check_clean <- function(sub_nums, raw, data_path){
  # check that the cleaning is baby bear
  # kwargs: 
  # -- sub_nums: a vector of subject nums w complete datasets
  # -- raw: raw data read in by get_raw_data
  # -- data_path: derivatives data path
  rsubs <- sample(sub_nums, 5)
  b4 <- raw %>% filter(sub %in% rsubs) %>% 
    filter(rt > 0) %>%
    filter(rt < 3) %>%
    ggplot(aes(x=rt, group=cert, col=cert)) +
    geom_density() +
    facet_grid(rows = vars(sub), cols = vars(reward_type)) + xlim(c(0,3))
  
  # now clean data for r sbs and plot the difference= 
  tst <- do.call(rbind, lapply(rsubs, function(x) clean_sub_data(x, raw)))
  aftr <- tst %>% ggplot(aes(x=rt, group=cert, col=cert)) +
                  geom_density() +
                  facet_grid(rows = vars(sub), cols = vars(reward_type)) + xlim(c(0,3))
  ggsave(paste(data_path, "N5_pre_clean.pdf", sep = ""), plot = b4)
  ggsave(paste(data_path, "N5_post_clean.pdf", sep = ""), plot = aftr)
}

check_skew <- function(sub_nums, clean_dat, data_path){
  # check that the cleaning is baby bear
  # kwargs: 
  # -- sub_nums: a vector of subject nums w complete datasets
  # -- clean_dat: cleaned data given by clean_sub_data
  # -- data_path: derivatives data path
  rsubs <- sample(sub_nums, 5)
  b4skw <- clean_dat %>% filter(sub %in% rsubs) %>% 
    ggplot(aes(x=rt, group=cert, col=cert)) +
    geom_density() +
    facet_grid(rows = vars(sub), cols = vars(reward_type)) +
    xlim(c(0,1.5))
  aftskw <- clean_dat %>% filter(sub %in% rsubs) %>% 
    ggplot(aes(x=log(rt), group=cert, col=cert)) +
    geom_density() +
    facet_grid(rows = vars(sub), cols = vars(reward_type)) 
  ggsave(paste(data_path, "N5_b4skw.pdf", sep = ""), plot = b4skw)
  ggsave(paste(data_path, "N5_aftskw.pdf", sep = ""), plot = aftskw)
}

do_ks_tests <- function(clean_dat){
  # saelect random subjects and
  # run ks.test function across datas from each condition
  # kwargs:
  # -- clean_dat: cleaned data given by clean_sub_data
  cert <- rep(unique(clean_dat$cert), times = 4)
  vals <- rep(levels(clean_dat$reward_type), each = length(unique(clean_dat$cert)))
  
  this_ks_test <- function(data, x, y){
    ks.test(x = data$rt[data$cert == x & data$reward_type == y],
            y = pnorm(seq(0.1, 1, by=0.1)))
  }
  mapply(this_ks_test, x = cert, y = vals, MoreArgs=list(data=clean_dat))
}

do_qq_plots <- function(clean_dat, x, data_path, s){
  # get qq plots for RTs
  # kwargs:
  # -- data_path: derivatives path
  # -- x = subject number
  # -- clean_dat: cleaned data given by clean_sub_data
  # -- s: info re: data for filename
  pdf(paste(data_path, s, '_qq_sub', x, ".pdf", sep=""))
  car::qqPlot(clean_dat$rt[clean_dat$sub == x])
  dev.off()
}


