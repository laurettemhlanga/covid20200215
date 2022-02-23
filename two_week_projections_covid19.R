rm(list = ls())
setwd("/home/laurette/Desktop/Github/smarts/covid20200215")
suppressPackageStartupMessages({
  require(RCurl)
  require(data.table)
  require(lubridate)
  require(bpmodels) 
  require(tidyverse)
})

#
#model parametes 
source("covid_smarts_params.R")


#chains
branches <- lapply(seq_along(1:num_sims)
                   , function(x) 
                     chain_sim(n = length(t0)
                               , offspring = "offsprings"
                               , serial = serial_interval
                               , size = ni_disp 
                               , mu = ni_mean
                               , tree = T
                               , t0 = t0
                               , tf  = tf)
)


newdata <- rbindlist(lapply(seq_along(branches), 
                            function(x) branches[[x]] %>%
                              mutate(index = x)))


#week 15/02/2022 Cumulative sum of cases


Cumulative_sum <- newdata %>%
  mutate(max_time = ceiling((time))) %>%
  group_by(index, max_time) %>%
  summarise(number = n()) %>%
  ungroup() %>% 
  tidyr::complete(max_time = 0:max(max_time), index = 1:num_sims, fill = list(number = 0)) %>% 
  arrange(index, max_time) %>% 
  group_by(index) %>% 
  mutate(cum_sum = cumsum(number)) %>% 
  ungroup() %>% 
  mutate(calendar_date = min_date + max_time)


 


median_daily_case <- Cumulative_sum %>% 
  group_by(calendar_date) %>% 
  summarise(median_cumcases= median(cum_sum))



write.csv(median_daily_case, "median_daily_case.csv")


