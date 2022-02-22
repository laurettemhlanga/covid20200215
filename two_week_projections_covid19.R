# Trying to use nano and edit file through terminal 

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
branches <- rbindlist(lapply(seq_along(1:num_sims), function(x)
  chain_sim(n = length(t0), offspring = "nbinom", stat = "length",
            serial = serial_interval, size = ni_disp, 
            mu = ni_mean,tree = T, t0 = t0, tf  = tf)
))


# max_date <- ceiling((max(branches$time)))



#number of cases 
Cumulative_sum <- branches %>% 
  mutate(max_time = ceiling((time))) %>% 
  group_by(max_time) %>% 
  summarise(number = n()) %>% 
  complete(max_time = 0:max(max_time), fill = list(number = 0)) %>% 
  mutate(cum_sum = cumsum(number), 
         calender_date = ymd(min_date) + 
           as.difftime(max_time, unit="days")) 
  

# plots limits

xlimits <- c(0, max(Cumulative_sum$max_time))
xbreaks <- seq(0, max(Cumulative_sum$max_time),  5)
ylimits <- c(0, max(Cumulative_sum$cum_sum))
ybreaks <- floor(seq(0, max(Cumulative_sum$cum_sum), 10))


Cumulative_sum[,] %>% 
  ggplot()+
  geom_line(aes(y = cum_sum, x = max_time))+
  theme_bw(base_size = 22, base_family = "") +
  labs(x = "calender date", y = "cumulative sum", color = "")+
  theme(axis.text.x = element_text())+
  scale_x_continuous(limits = xlimits, breaks = xbreaks)+
  scale_y_continuous(limits = ylimits,  breaks = ybreaks)



ggsave("cum_cases_plots.png", h = 8, w = 10)
