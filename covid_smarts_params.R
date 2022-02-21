
#parameter_bpmodels

n = 1;si_mean = 4.7;si_sd = 2.9
ni_mean = 2; ni_disp = 0.38; num_sims = 1

# data 
covid19za_timeline_confirmed <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv")
covid19za_24firstcase<- covid19za_timeline_confirmed[1:24, -1] %>%  
  mutate(date = as.Date(date))

# Utility functions
#serial intervals  - lognormal 
serial_interval <- function(n = n){
  return(rlnorm(n = n, mean = si_mean, 
                sd = si_sd))
}

# offspring - ntvebinom
offsprings <- function(n){
  
  return(rnbinom(n = n, size = ni_disp, 
                 ni_, mu =  ni_mean))
}


#define how t0 relates to the calender dates
min_date <- min(dmy(covid19za_24firstcase$date))
t0 <- as.integer(dmy(covid19za_24firstcase$date) - min_date)
tf <- max(t0) + 14