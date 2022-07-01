# elsyd: Event log synthetic data generator
# Generates eventlog for a single type
# cases: case profile table with features


# generates a case profile table with feature generators specified
# features: list of feature generator functions. each function gets an argument n and ... for any of the futures previously generated.
generate_case_profile = function(size = 10, features){
  output = size %>% sequence %>% data.frame %>% {.[,-1]}
  for(ft in names(features)){
    output[ft] <- features[[ft]] %>% do.call(args = list(n = size) %>% rlist::list.merge(output %>% as.list))
  }
  return(output)
}

# Argument event: A list containing configurations for events to be generated.
generate_eventlog_single = function(event_type, event_config, case_profile, caseID_col = 'caseID', case_startTime_col = 'case_startTime', case_endTime_col = 'case_endTime', startTime = NULL, endTime = NULL){
  emptyPOSIXct = Sys.time()[-1]
  el = data.frame(caseID = character(), eventType = character(), eventTime = emptyPOSIXct, variable = character(), value = numeric())
  case_profile[[case_startTime_col]] %<>% lubridate::as_datetime()
  case_profile[[case_endTime_col]] %<>% lubridate::as_datetime()
  for (i in case_profile %>% nrow %>% sequence){
    case_features = case_profile[i,] %>% as.list
    flag = TRUE
    orig = case_profile[i, case_startTime_col]
    endt = case_profile[i, case_endTime_col  ]
    if(!is.null(startTime)){
      orig %<>% max(startTime %>% lubridate::as_datetime())
    }
    if(!is.null(endTime)){
      endt %<>% min(endTime %>% lubridate::as_datetime())
    }
    eventTime = emptyPOSIXct
    
    while (flag){
      eventTime %<>% c(orig + (do.call(event_config$interarrival_time_generator, 
                                       args = list(n = 1000) %>% rlist::list.merge(case_features))) %>% cumsum)
      orig = max(eventTime, na.rm = T)
      flag = orig < endt
    }
    eventTime = eventTime[eventTime < endt]
    N = length(eventTime)
    if(N > 0){
      for (var_name in names(event_config$variables)){
        el %<>% rbind(
          data.frame(caseID    = case_profile[i, caseID_col],
                     eventType = event_type,
                     eventTime = eventTime,
                     variable  = var_name,
                     value     = do.call(event_config$variables[[var_name]], 
                                         list(n = N) %>% rlist::list.merge(case_features))
          ))
      }
    }
  }
  return(el)
}

genEventLog = function(cases, eventTypes, start = NULL, end = NULL){
  EL = data.frame()
  for (tp in eventTypes){
    EL %<>% rbind(genEventLogSingle(cases, event = tp, start = start, end = end))
  }
  return(EL)
}
