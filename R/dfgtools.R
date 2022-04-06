# dfg_tools (Dynamic Feature Generator Tools)

# Standard Feature Names:

# event-based features:
# <eventTypes>_<attributes>_<periodicAggregator>_<windowType>_<historicAggregator>_<lag>
# Example:
# transactionReported_amount_SUMG0SUML5

# Tools to map event-log into training dataset for machine learning:
# MLMpaaer.periodic converts an eventlog into a mix of multivariate time series.  Each caseID, will have a time series for itself.
dfg.periodic = function(eventlog, features, period = c('hour', 'day', 'week', 'month', 'year'), start = NULL, end = NULL){
  period = match.arg(period)
  eventlog$eventTime %<>% as.POSIXct %>% lubridate::force_tz('GMT')
  if(!is.null(start)){start %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime > start - 1)}
  if(!is.null(end))  {end   %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime < end + 1)}

  EL %<>% mutate(periodStart = eventTime %>% lubridate::floor_date(period), selected = T)
  molten = data.frame()
  for (ft in features){
    if(!is.null(ft$eventTypes)){
      EL$selected = EL$eventType %in% ft$eventTypes
    }

    if(!is.null(ft$variables)){
      EL$selected = EL$selected & (EL$variable %in% ft$variables)
    }
    EL %>% filter(selected) %>% group_by(caseID, periodStart) %>%
      summarise(aggrval = do.call(ft$aggregator, list(value))) %>%
      mutate(featureName = ft$name) %>%
      bind_rows(molten) -> molten
  }

  tt = data.frame(periodStart = seq(from = min(EL$periodStart), to = max(EL$periodStart), by = 'day'))
  data.frame(caseID = unique(eventlog$caseID)) %>% group_by(caseID) %>% do({data.frame(caseID = .$caseID, periodStart = tt)}) %>%
    left_join(molten %>% reshape2::dcast(caseID + periodStart ~ featureName, sum, value.var = 'aggrval'), by = c('caseID', 'periodStart')) -> molten
  molten[is.na(molten)] <- 0
  names(molten)[2] <- 'time'
  return(molten)
}

dfg.periodic.old = function(eventlog, features, period = c('hour', 'day', 'week', 'month', 'year'), start = NULL, end = NULL){
  period = match.arg(period)
  eventlog$eventTime %<>% as.POSIXct %>% lubridate::force_tz('GMT')
  if(!is.null(start)){start %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime > start - 1)}
  if(!is.null(end))  {end   %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime < end + 1)}

  eventlog %<>% mutate(periodStart = eventTime %>% lubridate::floor_date(period), selected = T)
  molten = data.frame()
  for (ft in features){
    if(!is.null(ft$eventTypes)){
      eventlog$selected = eventlog$eventType %in% ft$eventTypes
    } else eventlog$selected = TRUE

    if(!is.null(ft$variables)){
      eventlog$selected = eventlog$selected & (eventlog$variable %in% ft$variables)
    }
    eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>%
      summarise(aggrval = do.call(ft$aggregator, list(value))) %>%
      mutate(featureName = ft$name) -> mlt

    if(nrow(mlt) > 0){
      molten %<>% bind_rows(mlt)
    }
  }

  tt = data.frame(periodStart = seq(from = min(eventlog$periodStart), to = max(eventlog$periodStart), by = period))
  data.frame(caseID = unique(eventlog$caseID)) %>% group_by(caseID) %>% do({data.frame(caseID = .$caseID, periodStart = tt)}) %>%
    left_join(molten %>% reshape2::dcast(caseID + periodStart ~ featureName, sum, value.var = 'aggrval'), by = c('caseID', 'periodStart')) -> molten
  molten[is.na(molten)] <- 0
  names(molten)[2] <- 'time'
  return(molten)
}

# currently, only works for daily
dfg.periodic.sparklyr = function(eventlog, features, period = 'day', start = NULL, end = NULL){
  period = match.arg(period)
  if(!is.null(start)){start %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime > start - 1)}
  if(!is.null(end))  {end   %<>% as.POSIXct %>% lubridate::force_tz('GMT'); eventlog %<>% filter(eventTime < end + 1)}
  eventlog %<>% mutate(periodStart = as.Date(eventTime))
  molten = data.frame()
  for(ft in config$features){
    cat('Building PAF ', ft$name, '\n')
    if(!is.null(ft$eventTypes)){
      eventlog %<>% mutate(selected = eventType %in% ft$eventTypes)
    } else {
      eventlog %<>% mutate(selected = TRUE)
    }

    if(!is.null(ft$variables)){
      eventlog %<>% mutate(selected = selected & (variable %in% ft$variables))
    }

    switch(ft$aggregator,
           'mean' = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = AVG(value, na.rm = T)) %>% mutate(featureName = ft$name) %>% collect},
           'sum'  = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = SUM(value, na.rm = T)) %>% mutate(featureName = ft$name) %>% collect},
           'first' = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = first_value(value)) %>% mutate(featureName = ft$name) %>% collect},
           'last' = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = last_value(value)) %>% mutate(featureName = ft$name) %>% collect},
           'count'  = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = COUNT(value)) %>% mutate(featureName = ft$name) %>% collect},
           'max'  = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = MAX(value)) %>% mutate(featureName = ft$name) %>% collect},
           'sd'  = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = sd(value)) %>% mutate(featureName = ft$name) %>% collect},
           'min'  = {eventlog %>% filter(selected) %>% group_by(caseID, periodStart) %>% summarise(aggrval = MIN(value)) %>% mutate(featureName = ft$name) %>% collect}
    ) -> mlt

    if(nrow(mlt) > 0){
      molten %<>% bind_rows(mlt)
    }
  }

  tt = data.frame(periodStart = seq(from = min(molten$periodStart), to = max(molten$periodStart), by = 'day'))
  data.frame(caseID = unique(eventlog$caseID)) %>% group_by(caseID) %>% do({data.frame(caseID = .$caseID, periodStart = tt)}) %>%
    left_join(molten %>% reshape2::dcast(caseID + periodStart ~ featureName, sum, value.var = 'aggrval'), by = c('caseID', 'periodStart')) -> molten
  molten[is.na(molten)] <- 0
  names(molten)[2] <- 'time'
  return(molten)
}

# if early_call is true and location is less than win_size (early), then aggregator is applied on the smaller window from start to location 
vect.history = function(location, vector, win_size, fun, early_call = F){
  if(location < win_size){
    if(early_call){out = do.call(fun, args = list(vector[sequence(location)]))} else {out = NA}
  } else {
    out = do.call(fun, args = list(vector[(location - win_size):location]))
  }
  return(out)
}

vect.future = function(location, vector, win_size, fun, late_call = F){
  N = length(vector)
  if(N - location < win_size){
    if(late_call){out = do.call(fun, args = list(vector[location:N]))} else {out = NA}
  } else {
    out = do.call(fun, args = list(vector[location:(location + win_size)]))
  }
  return(out)
}

# Only for moving windows not growing
dfg.historic.old = function(periodic, features, early_call = T){
  drops = character()
  for (ft in features){
    if(is.null(ft$drop_reference)){ft$drop_reference = F}
    periodic %>% nrow %>% sequence %>% 
      sapply(FUN = vect.history, vector = periodic[, ft$reference], win_size = ft$win_size, fun = ft$aggregator, early_call = early_call) -> periodic[, ft$name]
    if(ft$drop_reference){drops = c(drops, ft$reference)}
  }

  return(periodic[, colnames(periodic) %>% setdiff(drops)])
}

# This function has been stolen from pracma::movavg with a modification: aggregator function can be passed as an argument and it only works for types s and t
# Types of available moving averages are:
#   
#   s for “simple”, it computes the simple moving average. n indicates the number of previous data points used with the current data point when calculating the moving average.
# 
# t for “triangular”, it computes the triangular moving average by calculating the first simple moving average with window width of ceil(n+1)/2; then it calculates a second simple moving average on the first moving average with the same window size.
# 
# w for “weighted", it calculates the weighted moving average by supplying weights for each element in the moving window. Here the reduction of weights follows a linear trend.
# 
# m for “modified", it calculates the modified moving average. The first modified moving average is calculated like a simple moving average. Subsequent values are calculated by adding the new value and subtracting the last average from the resulting sum.
# 
# e for“exponential", it computes the exponentially weighted moving average. The exponential moving average is a weighted moving average that reduces influences by applying more weight to recent data points () reduction factor 2/(n+1); or
# 
# r for“running", this is an exponential moving average with a reduction factor of 1/n [same as the modified average?].
# if n = Inf, it comutes cumulative aggregator
# for types r, e, m, w, aggregator is not used at all.
#' @export
moving_aggregator = function (x, n = Inf, type = c("s", "t", "w", "m", "e", "r"), aggregator = mean){
  stopifnot(is.numeric(x), is.numeric(n), is.character(type))
  type = match.arg(type)
  if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1) 
    stop("Window length 'n' must be a single integer greater 1.")
  nx <- length(x)
  n  <- min(n, nx - 1)
  y  <- numeric(nx)
  if (type == "s") {
    for (k in 1:(n - 1)) y[k] <- aggregator(x[1:k])
    for (k in n:nx) y[k] <- aggregator(x[(k - n + 1):k])
  }
  else if (type == "t") {
    n <- ceiling((n + 1)/2)
    s <- moving_aggregator(x, n, "s", aggregator = aggregator)
    y <- moving_aggregator(s, n, "s", aggregator = aggregator)
  }
  else if (type == "w") {
    for (k in 1:(n - 1)) y[k] <- 2 * sum((k:1)*x[k:1])/(k*(k + 1))
    for (k in n:nx) y[k] <- 2*sum((n:1)*x[k:(k - n + 1)])/(n*(n + 1))
  }
  else if (type == "m") {
    y[1] <- x[1]
    for (k in 2:nx) y[k] <- y[k - 1] + (x[k] - y[k - 1])/n
  }
  else if (type == "e") {
    a <- 2/(n + 1)
    y[1] <- x[1]
    for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
  }
  else if (type == "r") {
    a <- 1/n
    y[1] <- x[1]
    for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
  }
  else stop("The type must be one of 's', 't', 'w', 'm', 'e', or 'r'.")
  return(y)
}

#' @export
dfg.historic = function(periodic, features, caseID_col = 'caseID', eventTime_col = 'eventTime'){
  drops = character()
  periodic %<>% mutate(.row = sequence(nrow(.))) %>% arrange_(caseID_col, eventTime_col)
  
  #todo: use parallel computing  
  for (ft in features){
    if(is.null(ft$drop_reference)){ft$drop_reference = F}
    if(is.null(ft$aggregator)){ft$aggregator = mean}
    if(is.null(ft$type)){ft$type = 's'}
    if(ft$win_size < 2){ft$win_size = 2}
    
    ft_fun = function(v) moving_aggregator(x = v, n = ft$win_size, type = ft$type, aggregator = ft$aggregator)

    periodic %>% pull(ft$reference) %>% ave(id = periodic %>% pull(caseID_col), FUN = ft_fun) -> periodic[, ft$name]
      
    if(ft$drop_reference){drops = c(drops, ft$reference)}
  }
  return(periodic[, colnames(periodic) %>% setdiff(drops)] %>% arrange(.row) %>% select(-.row))
}

dfg.labeler = function(periodic, features){
  drops = character()
  for (ft in features){
    if(is.null(ft$drop_reference)){ft$drop_reference = F}
    periodic %>% nrow %>% sequence %>% sapply(FUN = vect.future, vector = periodic[, ft$reference], win_size = ft$win_size, fun = ft$aggregator) -> periodic[, ft$name]
    if(ft$drop_reference){drops = c(drops, ft$reference)}
  }
  return(periodic[, colnames(periodic) %>% setdiff(drops)])
}

# This function helps you build list of features to pass to argument features of dfg.historic and dfg.labeler:
add_features = function(flist = list(), actions){
  for (act in actions){
    for (ft in act$features){
      flist %<>% c(act$win_sizes %>% lapply(function(x) list(name = paste(ft, act$label, sep = '.') %>% paste0(x) , aggregator = act$fun, reference = ft, win_size = x)))
    }
  }
  return(flist)
}

# This function generates a package of multiple tables containing dynamic features from a given eventlog
# evenTime must be of class Date, otherwise, modify accordingly:
#' @export
dfg_pack = function(el, period = c('day', "week", "month", "quarter", "year", "sec", "min", "hour"), sequential = F,
                   event_funs = c('count', 'elapsed', 'tte', 'censored'), attr_funs = NULL, var_funs = NULL, horizon = NULL){
  cat('\n', "Verifications started ... ")
  period = match.arg(period)
  
  el %<>% as.data.frame %>% select(caseID, eventTime, eventType, attribute, value)
  el$value %<>% as.numeric

  wna = which(is.na(el$caseID) | is.na(el$eventTime))
  warnif(length(wna) > 0, paste(length(wna), 'rows', 'with missing caseID or eventTime are removed!'))
  if(length(wna) > 0){el = el[- wna, ]}
  
  wna = which(is.na(el$value))
  warnif(length(wna) > 0, paste(length(wna), 'values', 'missing or became missing through coercion replaced by zeros!'))
  if(length(wna) > 0){el$value[wna] <- 0}

  wna = which(is.na(el$attribute))
  warnif(length(wna) > 0, paste(length(wna), 'attributes', "missing replaced by 'occurrence'"))
  if(length(wna) > 0){el$attribute[wna] <- 'occurrence'}
  
  el$detailTime = el$eventTime
  
  cat("Done!", '\n')
  
  cat('\n', "Managing event times ... ")
  if(period %in% c('day', "week", "month", "quarter", "year")){
    el$eventTime = as.Date(el$eventTime)
    if(period != 'day'){
      el$eventTime %<>% cut(breaks = period) %>% as.Date
      if(period == 'week'){
        el$eventTime = el$eventTime + lubridate::weeks(1)
      } else if (period == 'month'){
        el$eventTime = el$eventTime + months(1)
      } else if (period == 'quarter'){
        el$eventTime = el$eventTime + months(3)
      } else {
      el$eventTime = el$eventTime + lubridate::years(1)
      }
    } else {
      el$eventTime = el$eventTime + lubridate::days(1)
    }
  } else {
    el$eventTime %<>% as.POSIXct
    if(period != 'sec'){
      el$eventTime %<>% cut(breaks = period)
      el$eventTime %<>% as.POSIXct
      if(period == 'min'){
        el$eventTime = el$eventTime + lubridate::minutes(1)
      } else{
        el$eventTime = el$eventTime + lubridate::hours(1)
      }
    } else {
      el$eventTime = el$eventTime + 1
    }
  }
  cat("Done!", '\n')

  out = list()
  cat('\n', "Creating time ends and event attribute maps ... ")
  el %>% group_by(caseID) %>% summarise(minTime = min(eventTime), maxTime = max(eventTime)) %>% ungroup -> out$case_timends
  el %>% distinct(eventType, attribute) -> out$event_attr_map
  cat("Done!", '\n')
  
  if(sequential){
    cat('\n', "Adding clock events ... ")
    
    # Option 1:
    # bb = out$case_timends %>% group_by(caseID) %>%
    #   do({data.frame(caseID = .$caseID, eventTime = seq(from = as.Date(.$minTime), to = as.Date(.$maxTime), by = period))})

    # Option 2: (Faster)
    lambda = function(v){seq(from = as.Date(v[2]), to = as.Date(v[3]), by = period)}
    out$case_timends %>% mutate(minTime = cut(minTime, breaks = period), maxTime = cut(maxTime, breaks = period)) %>%
      apply(1, lambda) -> a
    names(a) <- out$case_timends$caseID
    purrr::map_dfr(names(a), .f = function(v) {data.frame(caseID = v, eventTime = a[[v]])}) -> bb

    bb %>% anti_join(el %>% select(caseID, eventTime), by = c('caseID', 'eventTime')) %>%
      mutate(eventType = 'clock', attribute = 'counter', value = 1, detailTime = eventTime - 1) %>%
      select(caseID, eventType, eventTime, attribute, value, detailTime) %>%
      rbind(el) -> el
    
    cat("Done!", '\n')
  }

  if(length(event_funs) > 0){
    cat('\n', "Creating event count table ... ")
    el %>%
      reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'value', fun.aggregate = length) %>%
      arrange(caseID, eventTime) -> out$event_count

    cols = 3:ncol(out$event_count)
    cat("Done!", '\n')
  }

  if('count_cumsum' %in% event_funs){
    cat('\n', "Creating event count cumsum table ... ")
    out$event_count_cumsum = out$event_count %>% column.cumulative.forward(col = cols, id_col = 'caseID')
    cat("Done!", '\n')
  }

  if(sum(c('elapsed', 'tte', 'censored') %in% event_funs) > 0){
    cat('\n', "Creating event time table ... ")
    # out$event_time = out$event_count
    # tstr = as.character(out$event_time[, 'eventTime'])
    # out$event_time[, cols] %<>% as.matrix %>% apply(2, function(v) ifelse(v > 0, tstr, NA))
    el %>% mutate(detailTime = as.character(detailTime)) %>%
      reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'detailTime', fun.aggregate = dplyr::first) %>%
      arrange(caseID, eventTime) -> out$event_time
    cat("Done!", '\n')
  }

  #### event elapsed
  if('elapsed' %in% event_funs){
    cat('\n', "Creating elapsed table ... ")
    
    out$event_elapsed <- out$event_time %>% column.feed.forward(col = cols, id_col = 'caseID')

    for(i in cols){
      out$event_elapsed[, i] = as.numeric(out$event_elapsed$eventTime - as.Date(out$event_elapsed[, i]))
    }
    cat("Done!", '\n')
  }

  #### event tte, censored
  if(('tte' %in% event_funs) | ('censored' %in% event_funs)){
    cat('\n', "Creating tte table ... ")
    
    out$event_tte <- out$event_time %>% column.feed.backward(col = cols, id_col = 'caseID')
    for(i in cols){
      ind = which(as.Date(out$event_tte[, i]) < out$event_tte$eventTime) %-% nrow(out$event_tte)
      out$event_tte[ind, i] <- out$event_tte[ind + 1, i]
      out$event_tte[, i]    <- as.numeric(as.Date(out$event_tte[, i]) - out$event_tte$eventTime) %>% {.[.<0]<-NA;.}
    }
    cat("Done!", '\n')
  }

  if('censored' %in% event_funs){
    cat('\n', "Creating censored table ... ")
    
    out$event_censored = out$event_tte
    out$event_censored[, cols] <- is.na(out$event_tte[, cols]) %>% as.numeric

    out$event_tte %<>% left_join(out$case_timends, by = 'caseID') %>% mutate(ttc = as.numeric(maxTime - eventTime))
    for(i in cols){
      wna = which(out$event_censored[, i] == 1)
      out$event_tte[wna, i] <- out$event_tte[wna, 'ttc']
    }
    out$event_tte %<>%  select(- minTime, - maxTime, -ttc)
    cat("Done!", '\n')
  }
  
  #### Periodic(PAFs) and Historic Aggregated Features (HAFs) for events:
  if('last' %in% event_funs){
    cat('\n', "Creating event last value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'value', fun.aggregate = last) %>%
      column.feed.forward(col = 3:ncol(.), id_col = 'caseID') -> out$event_last

    cat("Done!", '\n')
  }
  
  if(sum(c('sum', 'sum_cumsum') %in% event_funs) > 0){
    cat('\n', "Creating event sum value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'value', fun.aggregate = sum) -> out$event_sum
    cat("Done!", '\n')
  }
  
  if('sum_cumsum' %in% event_funs){
    cat('\n', "Creating event cumulative sum value table ... ")
    
    out$event_sum %>% column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID') -> out$event_sum_cumsum
    cat("Done!", '\n')
  }
  
  if(sum(c('max', 'max_cummax') %in% event_funs) > 0){
    cat('\n', "Creating event max value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'value', fun.aggregate = max) -> out$event_max
    cat("Done!", '\n')
  }
  
  if('max_cummax' %in% event_funs){
    cat('\n', "Creating event cumulative max value table ... ")
    out$event_max %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummax) %>%
      {.[. == -Inf]<-NA;.} -> out$event_max_cummax
    cat("Done!", '\n')
  }
  
  if(sum(c('min', 'min_cummin') %in% event_funs) > 0){
    cat('\n', "Creating event min value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ eventType, value.var = 'value', fun.aggregate = min) -> out$event_min
    cat("Done!", '\n')
  }
  
  if('min_cummin' %in% event_funs){
    cat('\n', "Creating event cumulative min value table ... ")
    
    out$event_min %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummin) %>%
      {.[. == Inf]<-NA;.} -> out$event_min_cummin
    cat("Done!", '\n')
  }
  
  #### Periodic(PAFs) and Historic Aggregated Features (HAFs) for attributes:
  
  if('last' %in% attr_funs){
    cat('\n', "Creating attribute last value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ attribute, value.var = 'value', fun.aggregate = last) %>%
      column.feed.forward(col = 3:ncol(.), id_col = 'caseID') -> out$attr_last
    cat("Done!", '\n')
  }

  if(sum(c('sum', 'sum_cumsum') %in% attr_funs) > 0){
    cat('\n', "Creating attribute sum value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ attribute, value.var = 'value', fun.aggregate = sum) -> out$attr_sum
    cat("Done!", '\n')
  }

  if('sum_cumsum' %in% attr_funs){
    cat('\n', "Creating attribute cumulative sum value table ... ")
    out$attr_sum %>% column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID') -> out$attr_sum_cumsum
    cat("Done!", '\n')
  }

  if(sum(c('max', 'max_cummax') %in% attr_funs) > 0){
    cat('\n', "Creating attribute max value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ attribute, value.var = 'value', fun.aggregate = max) -> out$attr_max
    cat("Done!", '\n')
  }

  if('max_cummax' %in% attr_funs){
    cat('\n', "Creating attribute cumulative max value table ... ")
    out$attr_max %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummax) %>%
      {.[. == -Inf]<-NA;.} -> out$attr_max_cummax
    cat("Done!", '\n')
  }

  if(sum(c('min', 'min_cummin') %in% attr_funs) > 0){
    cat('\n', "Creating attribute min value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ attribute, value.var = 'value', fun.aggregate = min) -> out$attr_min
    cat("Done!", '\n')
  }

  if('min_cummin' %in% attr_funs){
    cat('\n', "Creating attribute cumulative min value table ... ")
    out$attr_min %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummin) %>%
      {.[. == Inf]<-NA;.} -> out$attr_min_cummin
    cat("Done!", '\n')
  }

  #### Periodic(PAFs) and Historic Aggregated Features (HAFs) for Variable:
  
  if(length(var_funs) > 0){
    el %<>% mutate(variable = paste(eventType, attribute, sep = '_'))
  }

  if('last' %in% var_funs){
    cat('\n', "Creating variable last value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ variable, value.var = 'value', fun.aggregate = last) %>%
      column.feed.forward(col = 3:ncol(.), id_col = 'caseID') -> out$var_last
    cat("Done!", '\n')
  }

  if(sum(c('sum', 'sum_cumsum') %in% var_funs) > 0){
    cat('\n', "Creating variable sum value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ variable, value.var = 'value', fun.aggregate = sum) -> out$var_sum
    cat("Done!", '\n')
  }

  if('sum_cumsum' %in% var_funs){
    cat('\n', "Creating variable cumulative sum value table ... ")
    out$var_sum %>% column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID') -> out$var_sum_cumsum
    cat("Done!", '\n')
  }

  if(sum(c('max', 'max_cummax') %in% var_funs) > 0){
    cat('\n', "Creating variable max value table ... ")
    el %>% reshape2::dcast(caseID + eventTime ~ variable, value.var = 'value', fun.aggregate = max) -> out$var_max
    cat("Done!", '\n')
  }

  if('max_cummax' %in% var_funs){
    cat('\n', "Creating variable cumulative max value table ... ")
    out$var_max %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummax) %>%
      {.[. == -Inf]<-NA;.} -> out$var_max_cummax
    cat("Done!", '\n')
  }

  if(sum(c('min', 'min_cummin') %in% var_funs) > 0){
    cat('\n', "Creating variable min value table ... ")
    
    el %>% reshape2::dcast(caseID + eventTime ~ variable, value.var = 'value', fun.aggregate = min) -> out$var_min
    cat("Done!", '\n')
  }

  if('cummin' %in% var_funs){
    cat('\n', "Creating variable cumulative min value table ... ")
    
    out$var_min %>%
      column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = cummin) %>%
      {.[. == Inf]<-NA;.} -> out$var_min_cummin
    cat("Done!", '\n')
  }
  
  if(!is.null(horizon)){
    cat('\n', "Creating event label table ... ")
    out %<>% add_event_labels(horizon %>% verify(c('integer', 'numeric'), lengths = 1, domain = c(1, Inf)))
    cat("Done!", '\n')
  }
  
  return(out %>% standard_dfgpack_feature_names)
}

standard_dfgpack_feature_names = function(dfgpack){
  for(tn in names(dfgpack) %-% c('extras', 'case_timends', 'event_attr_map')){
    suffix = strsplit(tn, '_')[[1]] %-% c('event', 'attr', 'var') %>% paste(collapse = '_')
    colnames(dfgpack[[tn]]) <- c('caseID', 'eventTime', colnames(dfgpack[[tn]])[-c(1,2)] %>% paste(suffix, sep = '_'))
  }
  return(dfgpack)
}

add_event_labels = function(dfgpack, horizon){
  cols = 3:ncol(dfgpack$event_tte)
  event_label = dfgpack$event_tte
  # I don't recall why I added extra condition: (dfgpack$event_tte[, cols] > 0). It fails when tte = 0 
  # event_label[, cols] <- as.numeric((dfgpack$event_censored[, cols] == 0) & (dfgpack$event_tte[, cols] < horizon) & (dfgpack$event_tte[, cols] > 0))
  event_label[, cols] <- as.numeric((dfgpack$event_censored[, cols] == 0) & (dfgpack$event_tte[, cols] < horizon))
  dfgpack$event_label = event_label
  return(dfgpack)
}


# The event-based features in the ML mapper, are for atomic events.
# For an interval event-type with start and stop, use this mapper.
# This function works only for one type of interval event which has start and end.
# so start and end of one event must have identical event IDs.
# Column eventType of the eventlog, must indicate if the event has started or ended. (Default tags are 'started', 'ended'. Specify if different tags are used in the dataset)
ml_event_interval = function(
  eventlog, period = 'day', sequential = F, caseID_col = 'caseID', eventID_col = 'eventID', eventTime_col = 'eventTime', eventType_col = 'eventType',
  startTags = 'started', endTags = 'ended'){

  eventlog %<>% nameColumns(columns = c(eventID = eventID_col, caseID = caseID_col, eventTime = eventTime_col, eventType = eventType_col))
  startTags %<>% verify('character', default = 'started')
  endTags   %<>% verify('character', default = 'ended')
  standradEventType <- c(rep('started', length(startTags)), rep('ended', length(endTags)))
  names(standradEventType) <- c(startTags, endTags) %>% tolower
  eventlog$eventType       <- standradEventType[eventlog$eventType %>% tolower]

  sna = sum(is.na(eventlog$type))
  warnif(sna > 0, paste('eventType tags do not match for', sna, 'rows!'))

  eventlog %>% group_by(caseID) %>%
    mutate(eventNo = paste0('E', eventID %>% as.character %>% as.factor %>% as.integer)) %>%
    mutate(eventType = paste0(eventNo, eventType)) %>%
    select(-eventID, -eventNo) %>%
    dfg_pack(period = period, event_funs = c('count_cumsum', 'elapsed'), sequential = sequential) -> pack

  colnames(pack$event_count_cumsum) %>% charFilter('started') -> start_events
  start_events %>% gsub(pattern = 'started', replacement = 'ended') -> end_events

  # current count of active events distributed by event bumber:
  en_active_count  = pack$event_count_cumsum[start_events] - pack$event_count_cumsum[end_events]
  # en_active_status = (en_active_count)

  # Current count of events active for each case (similar to count of active tasks):
  countActiveEvents = pack$event_count_cumsum %>% select(caseID, eventTime) %>%
    cbind(numEventsActive = rowSums(en_active_count, na.rm = T))


  # Current duration of active events (age of active events)
  en_active_age = pack$event_elapsed[start_events]
  en_active_age[en_active_count == 0] <- 0
  en_active_age[en_active_age < 0] <- 0 # There must be no rows with negative age, but I added in case
  caseActiveAge = pack$event_count_cumsum %>% select(caseID, eventTime) %>%
    cbind(activeAge = en_active_age %>% apply(1, max, na.rm = T))

  # a case is active when at least one event belonging to that case is active:

  countActiveEvents %>% left_join(caseActiveAge, by = c('caseID', 'eventTime')) %>% mutate(status = numEventsActive > 0)
}

# ml_sliding = function(dfg_pack, table_funs = list(event_count = c('sum90', 'mean120'), attr_max = c('mean15', 'max180'))){
#   funs = table_funs$event_count %>% gsub(pattern = "[0-9]", replacement = "")
#   nums = table_funs$event_count %>% gsub(pattern = "[a-z, A-Z]", replacement = "")
#   cols = 3:ncol(dfg_pack$event_count)
#   
#   tn = 'event_count' %>% paste(table_funs$event_count[1], sep = '_')
#   out[[tn]] <- dfg_pack$event_count
#   for(i in 1:cols){
#     which(dfg_pack$event_elapsed[, i] < nums[1]) -> ind
#     dfg_pack$event_elapsed[ind, i] %>% ave(id = dfg_pack$event_elapsed[ind, 'caseID'], FUN = validfun[funs[1]])    
#   }
# }

# This module, runs a predictive model using each history table in the dfg-pack as training data and 
# ranks features by importance and measures model performance. Features of importance greater than
# argument 'importance_threshold' from models with performance greater than 'performance_threshold'
# are selected and a single dataset is returned.
extract_features_from_dfgpack = function(dfgpack, train_from, train_to, test_from, test_to, label_event, importance_threshold, performance_threshold, performance_metric = 'gini', silent = T){
  models = list()
  
  ind_train = which((dfgpack$event_label$eventTime >= train_from) & (dfgpack$event_label$eventTime <= train_to))
  ind_test  = which((dfgpack$event_label$eventTime >= test_from) & (dfgpack$event_label$eventTime <= test_to))
  
  label_col = label_event %>% paste('label', sep = '_') 
  y_train   = dfgpack$event_label[ind_train,] %>% pull(label_col)
  y_test    = dfgpack$event_label[ind_test,]  %>% pull(label_col)
  
  history_tables = names(dfgpack) %-% c('case_timends', 'event_attr_map', 'event_time', 'event_tte', 'event_censored', 'event_label')

  ## Train models and measure performances:
  perf = numeric()
  for(tn in history_tables){
    X_train = dfgpack[[tn]][ind_train,] %>% select(-caseID, -eventTime)
    X_test  = dfgpack[[tn]][ind_test,]  %>% select(-caseID, -eventTime)
    
    mdl = new('CLS.XGBOOST', nthread = as.integer(4), rfe.enabled = T, name = tn)
    mdl$fit(X_train, y_train)
    
    perf[tn]     <- mdl$performance(X_test, y_test, metric = performance_metric)
    models[[tn]] <- mdl
  }
  
  ## Extract important features from well-performed models:
  # fl: feature list
  fl = NULL
  for(tn in names(perf[perf > performance_threshold])){
    bfet = models[[tn]]$objects$features %>% 
      filter(importance > importance_threshold) %>% 
      mutate(performance = perf[tn]) %>% 
      select(fname, importance, performance)
    if(!silent){
      cat('\n', 'Table ',tn, ':', '\n')
      print(bfet)
    }
    fl %<>% rbind(bfet)
  }
  
  tn = history_tables[1]
  mldata <- dfgpack[[tn]][fl$fname %^% colnames(dfgpack[[tn]])]
  for(tn in history_tables[-1]){
    cn = fl$fname %^% colnames(dfgpack[[tn]])
    dfgpack[[tn]][cn] %>% cbind(mldata) -> mldata
  }
  
  return(mldata)
}

# This function enrichs the given dfg package by adding various sliding window features to it.
#' @export add_swf
add_swf = function(dfgpack, tables = NULL, aggregators = NULL, win_sizes = NULL, types = 's'){
  if(is.empty(win_sizes)) return(dfgpack)
  
  aggr_list = list(sum = sum, avg = mean, med = median, min = min, max = max, sdv = sd)
  
  tables %<>% verify('character', domain = names(dfgpack), default = 
                       names(dfgpack) %-% c('case_timends', 'event_attr_map', 'event_time', 'event_tte', 'event_censored', 'event_label'))

  aggregators <- verify(aggregators, 'character', domain = names(aggr_list), default = 'sum')

  for(tn in tables){
    cat('\n', 'Adding sliding window features for table: ', tn, ' ... ')
    features = list()
    for(en in colnames(dfgpack[[tn]]) %-% c('caseID', 'eventTime', 'clock')){
      for(tp in intersect(types, c('s', 't'))){
        for(ag in aggregators){
          for(i in win_sizes){
            fn = sprintf('%s_%s%s%s', en, ag, chif(tp == 's', '', tp), i)
            features[[fn]] <- list(name = fn, reference = en, win_size = i, aggregator = aggr_list[[ag]], type = tp)
          }
        }
      }
      for(tp in intersect(types, c('w', 'm', 'e', 'r'))){
        for(i in win_sizes){
          fn = sprintf('%s_avg%s%s', en, tp, i)
          features[[fn]] <- list(name = fn, reference = en, win_size = i, type = tp)
        }
      }
    }
    dfgpack[[tn]] %<>% dfg.historic(features)
    cat('Done!', '\n')
  }
  
  return(dfgpack)
}




create_feature_trend = function(dfgpack, table, features, label_event, aggregator = mean, remove_censored = F){
  if(remove_censored){
    tbr = which(dfgpack$event_censored[, label_event] == 0)
    dfgpack[[table]] = dfgpack[[table]][tbr,]
    dfgpack[['event_label']] = dfgpack[['event_label']][tbr,]
  }
  stopifnot(table %in% names(dfgpack))
  val = dfgpack[[table]][features]
  if(length(features) > 1) val %<>% rowSums(na.rm = T)
  dfgpack[[table]][, 'value'] = val
  dfgpack[[table]][, 'label'] = ifelse(dfgpack$event_label[, label_event] == 1, 'Yes', 'No')
  
  dfgpack[[table]] %>% group_by(eventTime, label) %>%
    summarise(value = aggregator(value)) %>%
    reshape2::dcast(eventTime ~ label) %>% na2zero %>%
    arrange(eventTime)
}

dfgpack.remove_clock.events = function(pack){
  for(tn in names(pack)){
    cns = colnames(pack[[tn]])
    pack[[tn]] <- pack[[tn]][cns %-% charFilter(cns, 'clock')]
  }
  return(pack)
}

