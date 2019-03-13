# Header
# Filename:       survtools.R
# Description:    Contains utility functions for survival modelling of process.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 March 2019
# Last Revision:  14 March 2019
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 March 2019      Initial issue

calHazard = function(data, normalize_hazard = F){
  if(is.null(data$died)){data$died = 0}
  if(is.null(data$censored)){data$censored = 0}

  data %<>%
    mutate(total        = died + censored) %>%
    mutate(cumtotal     = cumsum(total)) %>%
    mutate(passed       = sum(total) - c(0, cumtotal[-length(cumtotal)]) - died) %>%
    mutate(hazard       = died/(passed + died))

  # I think hazard must be normalized to make sure they add up to one, because hazard is a probability
  # I believe survival probability achieved from normalized hazard gives a better estimation however this needs to be tested!
  if(normalize_hazard){
    data %>% pull(hazard) %>% sum -> H
    if(H == 0) H = 1.0
  } else {H = 1.0}

  data %>% mutate(hazard = hazard/H) %>%
    mutate(safety = 1 - hazard) %>%
    mutate(lgsfty = log(safety)) %>%
    mutate(clsfty = cumsum(lgsfty)) %>%
    mutate(prsurv = exp(clsfty))
}




# This function calculates hazard and survival probability from aggregated observations
#   given count of observations at various tenures
# inputs:
#   data: data.frame containing aggregated observations
#   tenure_col: which column contains tenure values (column values should be integer)
#   status_col: status at the time of observation: must be either 'died' or 'censored'
#   count_col: column containing count of observations
getHazard = function(data, tenure_col, status_col, count_col, normalize_hazard = F){
  # verificationa:
  ust = data %>% pull(status_col) %>% unique
  # assert(ust %<% c('died', 'censored'), "status column must be either 'died' or 'censored'")

  tenure_col %>% paste(status_col, sep = '~') %>% as.formula -> frm
  data %>% pull(count_col) %>% sum -> N
  data %<>% reshape2::dcast(frm, value.var = count_col, fun.aggregate = sum) %>% arrange_(tenure_col)
  if(is.null(data$died)){data$died = 0}
  if(is.null(data$censored)){data$censored = 0}
  data %>% calHazard(normalize_hazard)
}
