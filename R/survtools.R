# Header
# Filename:       survtools.R
# Description:    Contains utility functions for survival modelling of process.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 March 2019
# Last Revision:  26 March 2019
# Version:        0.0.5
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 March 2019      Initial issue
# 0.0.5     26 March 2019      Functions cp2mls, el2cp, cvp2cp, cp2cpag added


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

# converts eventlog to caseprofile
el2cvp = function(eventlog, ...){
  aggregators = c(...) %>% verify('character')
  scr = "eventlog %>% arrange(caseID, eventTime) %>% group_by(caseID, attribute) %>% summarise("
  nms = names(aggregators)
  N   = length(nms)
  for(i in sequence(N)){
    if(aggregators[nms[i]] %in% c('SUM', 'AVG')){narm = ", na.rm = T"} else {narm = ""}
    scr %<>% paste0(nms[i], " = ", aggregators[nms[i]], "(value", narm, ")")
    if(i < N){scr %<>% paste0(", ")}

  }
  scr %<>% paste0(")")
  parse(text = scr) %>% eval
}

# Converts current case profile (current feature values, current case profile time and full case profile time into machine learning data for survival model)
cp2mls = function(ccp, ccpt, cpt){
  ccp %>%
    inner_join(ccpt %>% filter(is.na(deathReason)) %>% select(caseID, age = LastAge), by = 'caseID') %>%
    inner_join(cpt, by = 'caseID') %>%
    mutate(censored = is.na(deathReason),
           endAge   = ifelse(is.na(deathReason), LastAge, deathAge),
           TTE      = endAge - age) %>%
    select(- deathTime, - deathReason, - LastAgeTime, -LastAge, - deathDate, - deathAge, - LastAgeDate)
}

#' @description Directly converts eventlog to caseProfile bypassing caseVarProfile, but the aggregators must be a subset of:
#' sum, mean, last, first, min, max, count
#' @export
el2cp = function(eventlog, ...){
  aggregators = c(...) %>% verify('character', domain = c('sum', 'mean', 'last', 'first', 'min', 'max', 'count'))

  if(inherits(eventlog, 'tbl_spark')){
    aggrmap = c(sum = 'sum', mean = 'AVG', last = 'last_value', first = 'first_value', min = 'MIN', max = 'MAX', count = 'COUNT')
    agglist = aggrmap[aggregators] %>% {names(.) <- rep('value', length(.));as.list(.)}

    eventlog %>% sdf_pivot(caseID ~ attribute, fun.aggregate = agglist)
  }
}


cvp2cp = function(cvp, cvp_cols){

  lst = rep('last_value', length(cvp_cols)) %>% {names(.)<-cvp_cols;as.list(.)}

  cvp %>% sdf_pivot(caseID ~ attribute, fun.aggregate = lst)
}

cp2cpag = function(cp, catcols){
  scr = "cp %>% group_by("
  for(col in catcols){
    scr %<>% paste0(col)
    if(col != catcols[length(catcols)]){scr %<>% paste0(", ")}
  }
  scr %<>% paste0(") %>% summarise(Count = COUNT(caseID))")

  parse(text = scr) %>% eval
}

  
