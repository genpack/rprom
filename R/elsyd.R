# elsyd: Event log synthetic data generator
# Generates eventlog for a single type
# cases: case profile table with features


# Argument event: A list containing configurations for events to be generated.
genEventLogSingle = function(cases, event, start, end){
  el = data.frame(eventID = character(), caseID = character(), eventType = character(), eventTime = emptime, variable = character(), value = numeric())
  for (i in cases %>% nrow %>% sequence){
    feat = cases[i,] %>% as.list
    flag = TRUE
    orig = max(start, cases$start[i])
    endt = min(end, cases$end[i])
    eventTime = emptime
    while (flag){
      eventTime %<>% c(orig + (do.call(event$IAT_generator, list(n = 1000, features = feat)) %>% cumsum))
      orig = max(eventTime, na.rm = T)
      flag = orig < endt
    }
    eventTime = eventTime[eventTime < endt]
    N = length(eventTime)
    if(N > 0){
      eid = do.call(event$ID_generator, list(n = N, features = feat))
      for (var in event$variables){
        el %<>% rbind(
          data.frame(eventID   = eid,
                     caseID    = cases$ID[i],
                     eventType = event$name,
                     eventTime = eventTime,
                     variable  = var$name,
                     value     = do.call(var$value_generator, list(n = N, features = feat))
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
