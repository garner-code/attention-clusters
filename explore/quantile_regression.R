#### K. Garner
#### following up hypotheses re: quantiles aka using quantile regression
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
library(diptest)
library(mclust)
library(quantreg)

source("../R/behavioural-data-analysis-functions.R")

#############################################################################
## source previous work
source("simul_cue_use.R")
head(both)
both_subs <- as.numeric(names(both)) # get the subs that showed an effect

get_qq_coefs <- function(data, sub_n, taus = seq(0.1, 0.9, by=0.1),
                         cert_val = ".8"){
  tmp = clean_dat %>% filter(sub == sub_n & cert == cert_val)

  out = do.call(rbind, lapply(taus, 
                              function(x) coef(rq(rt ~ reward_type, tau=x, 
                                                  data=tmp))))
  out <- as_tibble(out)
  names(out) <- c("int", "htgtldst", "ltgtldst", "ltgthdst")
  out$sub = sub_n
  out$q = taus
  out
}

both_subs_coefs <- do.call(rbind, lapply(both_subs, get_qq_coefs, data=clean_dat)) 
both_subs_coefs <- both_subs_coefs %>% pivot_longer(cols=c(htgtldst, ltgtldst, ltgthdst),
                                                    names_to="reward_cond",
                                                    values_to ="beta")

both_subs_coefs$new_ref <- NA
both_subs_coefs$new_ref[both_subs_coefs$reward_cond == "ltgtldst"] = 1
both_subs_coefs$new_ref[both_subs_coefs$reward_cond == "htgtldst"] = 2
both_subs_coefs$new_ref[both_subs_coefs$reward_cond == "ltgthdst"] = 3
both_subs_coefs$new_ref <- as.factor(both_subs_coefs$new_ref)
levels(both_subs_coefs$new_ref) <- c("ltgtldst", "htgtldst", "ltgthdst")

res = summary(lm(beta ~ q*new_ref, data=both_subs_coefs))
res

both_subs_coefs %>% ggplot(aes(x=q, y=beta, group_by=as.factor(sub), 
                               col=new_ref)) +
  geom_line() + facet_wrap(~new_ref) 
ggsave("../images/qreg/betas_by_q_inds.pdf", units = "cm", width = 16, height = 16)

# replot as a grouped boxplot
both_subs_coefs %>% ggplot(aes(x=as.factor(q), y=beta, colour=new_ref,
                               group_by=reward_cond)) +
  geom_boxplot(notch=TRUE)
ggsave("../images/qreg/betas_by_bp.pdf", units = "cm", width = 16, height = 16)

both_subs_coefs %>% ggplot(aes(x=beta, group_by=new_ref, colour=new_ref, fill=new_ref)) +
  geom_histogram(alpha=0.5) + facet_wrap(~as.factor(q))
ggsave("../images/qreg/betas_hists.pdf", units = "cm", width = 16, height = 16)
