library(magrittr)
library(dplyr)
library(lubridate)
library(rbig)
library(rutils)
library(rvis)
# library(rprom)

source('~/Documents/software/R/packages/rprom/R/transys.R')
source('~/Documents/software/R/packages/rprom/R/ts_tools.R')
source('~/Documents/software/R/packages/rprom/R/ts_vis.R')
source('~/Documents/software/R/packages/rprom/R/dfg_tools.R')
source('~/Documents/software/R/packages/rprom/R/dfg.R')

### Read Data:

traffic_fines_df = eventdataR::traffic_fines %>% as.data.frame

traffic_fines_df %>% View



## Build TransitionSystem object:
tsobj = new('TransitionSystem')
tsobj$feed.transition_events(
  traffic_fines_df, caseID_col = 'case_id', status_col = 'activity', startTime_col = 'timestamp')

## Event-Mapper:

traffic_fines_df %>% 
  mutate(eventType = activity %>% as.character %>% 
           stringr::str_replace_all(pattern = "\\s", replacement = "") %>% 
           paste0("Completed"),
         occurrence = as.numeric(is.na(amount) & is.na(expense) & is.na(paymentamount) & is.na(totalpaymentamount)),
         eventID = paste0("A", activity_instance_id)) %>%
  rename(caseID = case_id, eventTime = timestamp) %>% 
  reshape2::melt(id.vars = c('eventID', 'caseID', 'eventType', 'eventTime'),
               measure.vars = c('amount', 'expense', 'paymentamount', 'totalpaymentamount', 'occurrence')) %>% 
  filter(!is.na(value), (variable != 'occurrence' | value == 1)) -> el

## Events associated with resources

traffic_fines_df %>%
  filter(!is.na(resource)) %>%
  mutate(eventID = paste0('R', sequence(nrow(.))), eventType = paste0('TouchedBy', resource), variable = 'occurrence', value = 1) %>%
  select(eventID, caseID = case_id, eventType, eventTime = timestamp, variable, value) -> el_r

el = rbind(el, el_r)

# Build dynamic features:

dfg = DynamicFeatureGenerator(period = "month")

dfg$feed_eventlog(el %>% rename(attribute = variable))

#######
dfg$get.periodic.variable.sum(variables = dfg$get.variables()[1:5]) -> a

dfg$get.historic.event.time.left(eventTypes = dfg$get.eventTypes()[1:5]) -> b

dfg$get.historic.event.time.elapsed(eventTypes = dfg$get.eventTypes()[1:5]) -> d

dfg$get.historic.event.aggregated.cumulative(dfg$get.eventTypes()[1:5], periodic_aggregator = 'count', historic_aggregator = 'sum') -> e

dfg$get.periodic.event.aggregated(dfg$get.eventTypes()[1:5], periodic_aggregator = 'count') %>% head

dfg$get.historic.variable.aggregated.cumulative(dfg$get.variables()[2:5], periodic_aggregator = 'sum', historic_aggregator = 'max') -> f

dfg$get.historic.variable.aggregated.sliding(dfg$get.variables()[2:5], periodic_aggregator = 'sum', historic_aggregator = 'sum', win_size = 6) -> g

# events: elapsed, count, count.sum.cumulative, count.sum.sliding

EVENT.FEATURES = list(
  eventType = function(n, ...){
    dfg$get.eventTypes() %>% setdiff('clock') %>% sample(n, replace = T)
  },
  periodic = function(n, eventType, attribute, ...){
    c('time', 'count') %>% sample(n, replace = T)
  },
  
  winType = function(n, periodic, ...){
    out = rep('elapsed', n)
    w1  = which(periodic == 'count')
    out[w1] = c('cumulative', 'sliding') %>% sample(length(w1), replace = T)
    return(out)
  },
  
  winSize = function(n, winType, ...){
    out = rep(0, n)
    w1 = which(winType == 'cumulative')
    w2 = which(winType == 'sliding')
    out[w1] <- 0
    out[w2] <- sample(1:12, length(w2), replace = T)
    return(out)
  },
  
  historic = function(periodic, ...){
    ifelse(periodic == 'time', 'elapsed', 'sum')
  }
)


######################

source("~/Documents/software/R/packages/rprom/R/elsyd.R")
feature_configs = generate_case_profile(size = 100, EVENT.FEATURES) %>% 
  distinct %>% 
  mutate(feature_name = ifelse(winType == 'sliding', 
                               paste(eventType, '', periodic, paste0(historic, winSize), sep = '_'),
                               ifelse(winType == 'cumulative', 
                                      paste(eventType, '', periodic, paste0('cum', historic), sep = '_'),
                                      paste(eventType, '', periodic, historic, sep = '_')
                               )))
                               

########################################

groups = feature_configs %>% distinct(winType, periodic, historic, winSize)

features_df = NULL
for(i in sequence(nrow(groups))){
  
  group_configs = feature_configs %>% filter(
    periodic == groups$periodic[i],
    historic == groups$historic[i],
    winType == groups$winType[i],
    winSize == groups$winSize[i]
  )

  if(groups$periodic[i] == 'time'){
    dfg$get.historic.event.time.elapsed(
      eventTypes=group_configs$eventType) -> fdf
  } else if(groups$winType[i] == 'cumulative'){
    dfg$get.historic.event.aggregated.cumulative(
      eventTypes=group_configs$eventType, 
      periodic_aggregator = groups$periodic[i],
      historic_aggregator = groups$historic[i]) -> fdf
  } else {
    dfg$get.historic.event.aggregated.sliding(
      eventTypes=group_configs$eventType, 
      periodic_aggregator = groups$periodic[i],
      historic_aggregator = groups$historic[i],
      win_size = groups$winSize[i]) -> fdf
  }
  
  names(fdf)[3:ncol(fdf)] <- group_configs$feature_name
  
  if(is.null(features_df)){
    features_df = fdf
  } else {
    features_df %<>% left_join(fdf, by = c('caseID', 'reportTime'))
  }
}




#####################################


VARIABLE.FEATURES = list(
  variable = function(n, ...){
    
    dfg$get.variables() %>%  setdiff(c('clock_counter', charFilter(., 'occurrence'))) %>% sample(n, replace = T)
  },
  
  periodic = function(n, ...){
    c('sum', 'last', 'avg', 'min', 'max') %>% sample(n, replace = T)
  },
  
  winType = function(n, periodic, ...){
    out = c()
    w1 = which(periodic == 'last')
    w2 = which(periodic != 'last')
    
    out[w1] = 'cumulative'
    out[w2] = c('cumulative', 'sliding') %>% sample(length(w2), replace = T)
    return(out)    
  },
  
  winSize = CASEPROFILE.EVENT.FEATURE$winSize,
  
  historic = function(periodic, ...){
    out = c()
    w1 = which(periodic == 'last')
    w2 = which(periodic == 'avg')
    w3 = which(periodic == 'sum')
    w4 = which(periodic == 'min')
    w5 = which(periodic == 'max')
    
    out[w1] = 'last'
    out[w2] = c('avg', 'max', 'min') %>% sample(length(w2), replace = T)
    out[w3] = 'sum'
    out[w4] = 'min'
    out[w5] = 'max'
    return(out)    
  }
)


######################

source("~/Documents/software/R/packages/rprom/R/elsyd.R")
feature_configs = generate_case_profile(size = 100, VARIABLE.FEATURES) %>% 
  distinct %>% 
  mutate(feature_name = ifelse(winType == 'sliding', 
                               paste(variable, periodic, paste0(historic, winSize), sep = '_'),
                               paste(variable, periodic, paste0('cum', historic), sep = '_')))

########################################

groups = feature_configs %>% distinct(winType, periodic, historic, winSize)

features_df = NULL
for(i in sequence(nrow(groups))){
  group_configs = feature_configs %>% filter(
    periodic == groups$periodic[i],
    historic == groups$historic[i],
    winType == groups$winType[i],
    winSize == groups$winSize[i]
  )

  if(groups$winType[i] == 'cumulative'){
    dfg$get.historic.variable.aggregated.cumulative(
      variables=group_configs$variable, 
      periodic_aggregator = groups$periodic[i],
      historic_aggregator = groups$historic[i]) -> fdf
  } else {
    dfg$get.historic.variable.aggregated.sliding(
      variables=group_configs$variable, 
      periodic_aggregator = groups$periodic[i],
      historic_aggregator = groups$historic[i],
      win_size = groups$winSize[i]) -> fdf
  }
  
  names(fdf)[3:ncol(fdf)] <- group_configs$feature_name
  
  if(is.null(features_df)){
    features_df = fdf
  } else {
    features_df %<>% left_join(fdf, by = c('caseID', 'reportTime'))
  }
}




#############################################################################################################
el %>% rename(attribute = variable) %>% 
  dfg_pack(period = "day", 
           event_funs = c('count', 'elapsed', 'count_cumsum'), 
           attr_funs = c('last', 'cumsum')) -> fpack_np

# Although, there are lots of more features that can be generated, 
# but we want to have an estimation of how these features can predict transition targets

dfgpack.to_dataframe = function(dfpack){
  tables = names(dfpack) %-% c("case_timends", "event_attr_map", "event_time")
  for(tn in tables){
    if(tn == tables[1]){
      output = dfpack[[tn]]
    } else {
      output %<>% cbind(dfpack[[tn]] %>% select(-caseID, -eventTime))
    } 
  }
  return(output)  
}

features = dfgpack.to_dataframe(fpack_np)


features %>% left_join(
  tsobj$history %>% select(caseID, eventTime = startDate, status, nextStatus) %>% mutate(eventTime = as.Date(eventTime) + 1),
  by = c('caseID', 'eventTime')
) -> train

# we should eliminate rows with ENTER or EXIT. Since we are working with complete cases, we don't have any


# Label encode feature status
train$status_encoded = factor(train$status, levels = tsobj$get.statuses()) %>% as.integer

# pick one target: "Send Fine"
# We build a list of binary classifier models each trained to predict a certain status.
# Another alternative is to build a single multi-class classifier for all transition targets
ind_train = which(train$eventTime >= '2006-01-01' & train$eventTime <= '2008-06-01')
ind_test  = which(train$eventTime >= '2008-08-01' & train$eventTime <= '2012-12-01')
for (target in unique(train$nextStatus)){
  train$label = train$nextStatus == target

  X_train = train[ind_train, ] %>% select(-caseID, -eventTime, -status, -nextStatus, -label)
  X_test  = train[ind_test, ] %>% select(-caseID, -eventTime,  -status, -nextStatus, -label)
  y_train = train$label[ind_train]
  y_test  = train$label[ind_test]
  
  model_np = rml::CLS.XGBOOST(fe.enabled = T)
  model_np$fit(X_train, y_train)
  models[[target]] <- model_np
}

## We want to simulate from '2006-02-01' onwards:

train[ind_test, ] %>% 
  group_by(caseID) %>% summarise(eventTime = first(eventTime), status = first(status)) %>% 
  ungroup -> statuses_to_predict

features = train %>% inner_join(latest_statuses, by = c('caseID', 'eventTime', 'status')) 

my_transition_classifier = function(histobj, input, ...){
  
}

model_np$performance(X_test, y_test, 'accuracy')
model_np$performance(X_test, y_test, 'precision', quantiles = c(0.01, 0.02, 0.2))


model_np$objects$features %>% View

## Predicting durations:

# durations' distribution can be very different depending on the transition
# Let's have a look at the distribution for some transitions
source = train$status %>% sample(1)
target = train$nextStatus %>% sample(1)

tsobj$history %>% filter(status == source, nextStatus == target) %>% pull(duration) -> dur 
dur %>% hist(breaks = 1000, density = T)
dur %>% density %>% lines(col = 'red')



# The day I saw you in the Westfield foodcourt in 2018, 
# I didn't know that this very short meeting will lead to 
# such a great opportunity for me collaborate in the development of 
# 
# I am very happy and grateful for having the opportunity to meet you and work with you at Elula.
# Happy to see you are leading Elula towards being an entherprise pioneering in AI and I in turn feel very proud of it.
# 
# It is so auspicious that at the same time you are entering the 5th decade of your life, 
# you achieve such a great success by partnering with Pexa which uplifts Elula to a different level.
# Two milestones at the same time!
# I wish you best of success for you and Elula in the coming years and will be proud of having my share of contribution 
# in this success.



##################################

ind_train = which(fpack$event_count$eventTime >= '2006-01-01' & fpack$event_count$eventTime <= '2008-06-01')
ind_test  = which(fpack$event_count$eventTime >= '2008-08-01' & fpack$event_count$eventTime <= '2012-12-01')

X_train = res_np[ind_train, ]
X_test  = res_np[ind_test, ]
y_train = fpack_np$event_label$PaymentCompleted_label[ind_train]
y_test  = fpack_np$event_label$PaymentCompleted_label[ind_test]


model_np = rml::CLS.XGBOOST()
model_np$fit(X_train, y_train)

model_np$performance(X_test, y_test, 'gini')
model_np$performance(X_test, y_test, 'lift', quantiles = c(0.01, 0.02, 0.2))

#
## Periodic dataset:

el %>% rename(attribute = variable) %>% 
  dfg_pack(period = "month", sequential = T,
           event_funs = c('count', 'elapsed', 'tte', 'censored', 'count_cumsum'), 
           attr_funs = c('last', 'cumsum'), horizon = 60) %>% 
  dfg_pack.remove_clock_events -> fpack_p

# Although, there are lots of more features that can be generated, 
# but we want to have an estimation of how these features can predict the occurrence of one event within the next 60 days.

extract_features_from_dfgpack(
  fpack_p, 
  train_from = '2006-01-01', 
  train_to = '2008-06-01', 
  test_from = '2008-08-01',
  test_to = '2012-12-01',
  label_event = 'PaymentCompleted',
  importance_threshold = 0.0,
  performance_threshold = 0.0,
  silent = F
) -> res_p

ind_train = which(fpack_p$event_count$eventTime >= '2006-01-01' & fpack_p$event_count$eventTime <= '2008-06-01')
ind_test  = which(fpack_p$event_count$eventTime >= '2008-08-01' & fpack_p$event_count$eventTime <= '2012-12-01')

X_train = res_p[ind_train, ]
X_test  = res_p[ind_test, ]
y_train = fpack_p$event_label$PaymentCompleted_label[ind_train]
y_test  = fpack_p$event_label$PaymentCompleted_label[ind_test]


model_p = rml::CLS.XGBOOST()
model_p$fit(X_train, y_train)

model_p$performance(X_test, y_test, 'gini')
model_p$performance(X_test, y_test, 'precision', quantiles = c(0.01, 0.02, 0.2))

## Why performance of the non-periodic dataset is significantly better?
# The main reason is that in the non-periodic dataset, we have training rows at when the 

fpack_np$event_count %>% left_join(
  tsobj$history %>% select(caseID, eventTime = startDate, status, nextStatus) %>% mutate(eventTime = as.Date(eventTime) + 1),
  by = c('caseID', 'eventTime')
) -> event_count

