
#parameter_bpmodels

si_mean = 4.7; si_sd = 2.9

ni_mean = 2; ni_disp = 0.38

num_sims = 5

min_date <- ymd(20200301)

# data 
covid19za_timeline_confirmed <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv")
covid19za_24firstcase<- covid19za_timeline_confirmed[1:24, -1] %>%  
  mutate(date = as.Date(date, format = '%d-%m-%Y'))  %>% pull(date)

# Utility functions
#serial intervals  - lognormal 
serial_interval <- function(n){
  return(rlnorm(n = n, meanlog = si_mean, 
                sdlog = si_sd))
}

# offspring - ntvebinom
roffsprings <- function(n, size, mu){
  
  return(rnbinom(n = n, size = size, mu = mu)) 
  
}


#define how t0 relates to the calender dates

t0 <- as.integer(covid19za_24firstcase - min_date)
tf <- max(t0) + 14

