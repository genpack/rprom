# Dynamic Feature Generator Module
# Version     Date                 Action
# ----------------------------------
# 0.0.1       14 June 2022     Initial issue by file dfg.R

DynamicFeatureGenerator = setRefClass(
  "DynamicFeatureGenerator",
  fields = list(
    settings = "list",
    # case_timends = "data.frame",
    # event_attr_map = "data.frame",
    eventlog = "data.frame",
    periodic = "list",
    historic = "list"
  ),
  
  methods = list(
    initialize = function(...){
      # default settings
      settings <<- list(period = "day", time_unit = 'day', sequential = TRUE, save_historic_cumulative = TRUE, save_historic_sliding = TRUE)
      input_settings = list(...)
      if(length(input_settings) > 0){
        settings <<- settings %>% rlist::list.merge(list(...))
      }
    },
    
    feed_eventlog = function(el){

      el %<>% as.data.frame
      
      # if(settings$time_unit %in% c('day', 'week', 'month', 'year')){
      #   el$eventTime = as.Date(el$eventTime)
      # } else {
      #   el$eventTime = lubridate::as_datetime(el$eventTime)
      # }
      el$eventTime = lubridate::as_datetime(el$eventTime)
      
      # Impute eventID column if it does not exist in the eventlog
      if(!'eventID' %in% colnames(el)){
        el$eventID = paste0("C" , 
                           factor(el$caseID) %>% as.integer, "P", 
                           factor(el$eventType) %>% as.integer, "T", 
                           factor(el$eventTime) %>% as.integer)
      }

      el %<>% 
        mutate(value = as.numeric(value)) %>% 
        select(eventID, caseID, eventTime, eventType, attribute, value)

      wna = which(is.na(el$caseID) | is.na(el$eventTime))
      warnif(length(wna) > 0, paste(length(wna), 'rows', 'with missing caseID or eventTime are removed!'))
      if(length(wna) > 0){el = el[- wna, ]}
      
      wna = which(is.na(el$value))
      warnif(length(wna) > 0, paste(length(wna), 'values', 'missing or became missing through coercion replaced by zeros!'))
      if(length(wna) > 0){el$value[wna] <- 0}
      
      wna = which(is.na(el$attribute))
      warnif(length(wna) > 0, paste(length(wna), 'attributes', "missing replaced by 'occurrence'"))
      if(length(wna) > 0){el$attribute[wna] <- 'occurrence'}
      
      el$reportTime = el$eventTime
      
      if(settings$sequential){add.clock_events(el) -> el}
      el <- add.report_time(el)
      
      el %<>% mutate(variable = paste(eventType, attribute, sep = '_'))
      
      eventlog <<- eventlog %>% rbind(el)
      
      # eventlog %>% distinct(eventType, attribute) ->> event_attr_map
      
      cat("Done!", '\n')
      
    },
    
    set.period = function(period = c('day', "week", "month", "quarter", "year")){
      period = match.arg(period)
      eventlog %>% filter(eventType != 'clock') ->> eventlog
      
      eventlog <<- add.report_time(eventlog)
      if(settings$sequential){
        eventlog <<- add.clock_events(eventlog)
      }
      clear()
    },
    
    # changes the reportTime column based on the period specified
    # also adds clock events if sequential is TRUE and eliminate exisiting clock events if there is any
    add.report_time = function(el){
      
        if(settings$period %in% c('day', "week", "month", "quarter", "year")){
          el$reportTime = as.Date(el$eventTime)
          if(settings$period != 'day'){
            el$reportTime %<>% cut(breaks = settings$period) %>% as.Date
            if(settings$period == 'week'){
              el$reportTime = el$reportTime + lubridate::weeks(1)
            } else if (settings$period == 'month'){
              el$reportTime = el$reportTime + months(1)
            } else if (settings$period == 'quarter'){
              el$reportTime = el$reportTime + months(3)
            } else {
              el$reportTime = el$reportTime + lubridate::years(1)
            }
          } else {
            el$reportTime = el$reportTime + lubridate::days(1)
          }
        } else {
          el$reportTime <- lubridate::as_datetime(el$eventTime)
          if(settings$period != 'sec'){
            el$reportTime %<>% cut(breaks = settings$period)
            el$reportTime %<>% as.POSIXct
            if(settings$period == 'min'){
              el$reportTime = el$reportTime + lubridate::minutes(1)
            } else{
              el$reportTime = el$reportTime + lubridate::hours(1)
            }
          } else {
            el$reportTime = el$reportTime + 1
          }
        }
      
        return(el)
    },
    
    clear = function(){
      periodic <<- list()
      historic <<- list()
    },
    
    add.clock_events = function(el){
        cat('\n', "Adding clock events ... ")
        
        # Option 1:
        # bb = out$case_timends %>% group_by(caseID) %>%
        #   do({data.frame(caseID = .$caseID, reportTime = seq(from = as.Date(.$minTime), to = as.Date(.$maxTime), by = period))})
        
        # Option 2: (Faster)
        el %>% group_by(caseID) %>% summarise(minTime = min(eventTime), maxTime = max(eventTime)) %>% ungroup -> case_timends
      
        ## todo define for periods other than daily
        lambda = function(v){seq(from = as.Date(v[2]), to = as.Date(v[3]), by = settings$period) %>% rutils::unname()}
        case_timends %>% mutate(minTime = cut(minTime, breaks = settings$period), maxTime = cut(maxTime, breaks = settings$period)) %>%
          apply(1, lambda) -> a
        names(a) <- case_timends$caseID
        purrr::map_dfr(names(a), .f = function(v) {data.frame(caseID = v, eventTime = a[[v]])}) -> bb
        
        bb %>% 
          # anti_join(el %>% select(caseID, eventTime), by = c('caseID', 'eventTime')) %>%
          mutate(eventType = 'clock', attribute = 'counter', value = 1, reportTime = eventTime, 
                 eventID = paste0("CL", sequence(nrow(.)))) %>%
          select(eventID, caseID, eventTime, reportTime, eventType, attribute, value) %>%
          rbind(el) -> el
        
        cat("Done!", '\n')
        return(el)
    },
    
    get.eventTypes = function(eventTypes = NULL, exclude = NULL){
      exclude <- verify(exclude, 'character', default = character())
      allEventTypes = unique(eventlog$eventType)
      if(is.null(eventTypes)){
        eventTypes = allEventTypes
      }
      eventTypes %>% intersect(allEventTypes) %>% setdiff(exclude)
    },

    get.variables = function(variables = NULL, exclude = NULL){
      exclude <- verify(exclude, 'character', default = character())
      allVariables = unique(eventlog$variable)
      if(is.null(variables)){
        variables = allVariables
      }
      variables %>% intersect(allVariables) %>% setdiff(exclude)
    },
    
    get.periodic.event.time = function(...){
      
      eventTypes = get.eventTypes(...)
      if(is.null(periodic$event.time)){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(periodic$event.time))
      }
      
      if(length(eventTypesToCreate) > 0){
        
        cat('\n', "Creating event time table ... ")
        
        eventlog %>% 
          filter(eventType %in% c(eventTypesToCreate, 'clock')) %>% 
          distinct(eventID, .keep_all = T) %>% 
          mutate(eventTime = as.character(eventTime)) %>%
          reshape2::dcast(caseID + reportTime ~ eventType, value.var = 'eventTime', fun.aggregate = dplyr::first) %>%
          arrange(caseID, reportTime) -> event_time
        
        cat("Done!", '\n')
        
        if(is.null(periodic$event.time)){
          periodic$event.time <<- event_time
        } else {
          periodic$event.time <<- periodic$event.time %>% left_join(event_time, by = c('caseID', 'reportTime'))
          ## todo: consider cbind instead of left_join
        }
      }  
      
      return(periodic$event.time[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.periodic.variable.aggregated = function(..., periodic_aggregator){
      verify(periodic_aggregator, 'character', 
             domain = names(AGGREGATOR_FUNCTION), 
             default = 'sum') -> periodic_aggregator
      
      tableName = paste('variable', periodic_aggregator, sep = '.')
      
      variables = get.variables(...)
      if(is.null(periodic[[tableName]])){
        variablesToCreate = variables
      } else {
        variablesToCreate = variables %>% setdiff(colnames(periodic[[tableName]]))
      }
      
      if(length(variablesToCreate) > 0){
        
        cat('\n', "Creating periodic variable aggregated values ... ")
        eventlog %>% 
          filter(variable %in% c(variablesToCreate, 'clock_counter')) %>% 
          distinct(eventID, .keep_all = T) %>% 
          reshape2::dcast(caseID + reportTime ~ variable, value.var = 'value', fun.aggregate = AGGREGATOR_FUNCTION[[periodic_aggregator]]) -> var_agg
        
        cat("Done!", '\n')
        
        if(is.null(periodic[[tableName]])){
          periodic[[tableName]] <<- var_agg
        } else {
          # columnsToKeep = periodic[[tableName]] %>% setdiff(colnames(var_agg)) %>% c('caseID', 'reportTime')
          if('clock_counter' %in% colnames(periodic[[tableName]]) & ('clock_counter' %in% colnames(var_agg))){
            var_agg %<>% select(-clock_counter)
          }
          periodic[[tableName]] <<- periodic[[tableName]] %>% 
            left_join(var_agg, by = c('caseID', 'reportTime'))
        }
      }
      
      return(periodic[[tableName]][c('caseID', 'reportTime', variables)])
    },
    
    get.periodic.variable.sum = function(...){
      get.periodic.variable.aggregated(..., periodic_aggregator = 'sum')
    }, 

    get.periodic.variable.avg = function(...){
      get.periodic.variable.aggregated(..., aggregator = 'avg')
    }, 

    get.periodic.event.aggregated = function(..., periodic_aggregator = 'count'){
      
      verify(periodic_aggregator, 'character', 
             domain = names(AGGREGATOR_FUNCTION), 
             default = 'count') -> periodic_aggregator
      
      tableName = paste('event', periodic_aggregator, sep = '.')
      
      eventTypes = get.eventTypes(...)
      if(is.null(periodic[[tableName]])){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(periodic[[tableName]]))
      }
      
      if(length(eventTypesToCreate) > 0){
        cat('\n', "Creating event aggregated table ... ")
        
        eventlog %>%
          filter(eventType %in% c(eventTypesToCreate, 'clock')) %>% 
          distinct(eventID, .keep_all = T) %>% 
          reshape2::dcast(caseID + reportTime ~ eventType, value.var = 'value', fun.aggregate = AGGREGATOR_FUNCTION[[periodic_aggregator]]) %>%
          arrange(caseID, reportTime) -> event_aggregated
        
        if(is.null(periodic[[tableName]])){
          periodic[[tableName]] <<- event_aggregated
        } else {
          # columnsToKeep = periodic[[tableName]] %>% setdiff(colnames(event_aggregated)) %>% c('caseID', 'reportTime')
          if('clock' %in% colnames(periodic[[tableName]]) & ('clock' %in% colnames(event_aggregated))){
            event_aggregated %<>% select(-clock)
          }
          periodic[[tableName]] <<- periodic[[tableName]] %>% left_join(event_aggregated, by = c('caseID', 'reportTime'))
          ## todo: consider cbind instead of left_join
        }
        
        cat("Done!", '\n')
      }
      
      return(periodic[[tableName]][c('caseID', 'reportTime', eventTypes)])
    },
    
    get.periodic.event.count = function(...){
      get.periodic.event.aggregated(..., periodic_aggregator = 'count')
      # eventTypes = get.eventTypes(...)
      # if(is.null(periodic$event.count)){
      #   eventTypesToCreate = eventTypes
      # } else {
      #   eventTypesToCreate = eventTypes %>% setdiff(colnames(periodic$event.count))
      # }
      # 
      # if(length(eventTypesToCreate) > 0){
      #   cat('\n', "Creating event count table ... ")
      #   
      #   eventlog %>%
      #     filter(eventType %in% c(eventTypesToCreate, 'clock')) %>% 
      #     distinct(eventID, .keep_all = T) %>% 
      #     reshape2::dcast(caseID + reportTime ~ eventType, value.var = 'value', fun.aggregate = length) %>%
      #     arrange(caseID, reportTime) -> event_count
      # 
      #     if(is.null(periodic$event.count)){
      #       periodic$event.count <<- event_count
      #     } else {
      #       periodic$event.count <<- periodic$event.count %>% left_join(event_count, by = c('caseID', 'reportTime'))
      #       ## todo: consider cbind instead of left_join
      #     }
      #     
      #     cat("Done!", '\n')
      # }
      
      # return(periodic$event.count[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.historic.event.aggregated.cumulative = function(..., periodic_aggregator = 'sum', historic_aggregator = 'sum'){
      
      eventTypes = get.eventTypes(...)
      verify(historic_aggregator, 'character', 
             domain = names(CUMULATIVE_AGGREGATOR_FUNCTION), 
             default = 'cumsum') -> historic_aggregator
      
      historicTableName = paste("event", periodic_aggregator, historic_aggregator, 'cumulative', sep = '.')

      if(is.null(historic[[historicTableName]])){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(historic[[historicTableName]]))
      }  
      if(length(eventTypesToCreate) > 0){
        cat('\n', "Creating event %s %s table ... " %>% sprintf(periodic_aggregator, historic_aggregator))
        
        if(historic_aggregator == 'last'){
          event_aggregated_cumulative <- get.periodic.event.aggregated(..., periodic_aggregator = periodic_aggregator) %>% 
            column.feed.forward(col = 3:ncol(.), id_col = 'caseID')
        } else {
          get.periodic.event.aggregated(eventTypes = eventTypesToCreate) %>% 
            column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = CUMULATIVE_AGGREGATOR_FUNCTION[[historic_aggregator]]) -> event_aggregated_cumulative
        }

        if(is.null(historic[[historicTableName]])){
          output <- event_aggregated_cumulative
        } else {
          output <- historic[[historicTableName]] %>% 
            left_join(event_aggregated_cumulative, by = c('caseID', 'reportTime'))
        }
        
        if(settings$save_historic_cumulative){historic[[historicTableName]] <<- output}
        
        cat("Done!", '\n')
      } else {output = historic[[historicTableName]]}

      return(output[c('caseID', 'reportTime', eventTypes)])
    },

    get.historic.event.count.cumsum = function(...){
      get.historic.event.aggregated.cumulative(..., periodic_aggregator = 'count', historic_aggregator = 'sum')
      # eventTypes = get.eventTypes(...)
      # if(is.null(historic$event.count.cumsum)){
      #   eventTypesToCreate = eventTypes
      # } else {
      #   eventTypesToCreate = eventTypes %>% setdiff(colnames(historic$event.count.cumsum))
      # }  
      # if(length(eventTypesToCreate) > 0){
      #   cat('\n', "Creating event count cumulative sum table ... ")
      #   
      #   get.periodic.event.count(eventTypes = eventTypesToCreate) %>% 
      #     column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID') -> event_count_cumsum
      #     
      #   cat("Done!", '\n')
      # }
      # 
      # if(is.null(historic$event.count.cumsum)){
      #   output <- event_count_cumsum
      # } else {
      #   output <- historic$event.count.cumsum %>% 
      #     left_join(event_count-cumsum, by = c('caseID', 'reportTime'))
      # }
      # 
      # if(settings$save_historic){historic$event.count.cumsum <<- output}
      # 
      # return(output[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.historic.event.time.left = function(...){
      
      eventTypes = get.eventTypes(...)
      if(is.null(historic$event.time.left)){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(historic$event.time.left))
      } 
      
      if(length(eventTypesToCreate) > 0){
        cat('\n', "Creating tte table ... ")
        
        event_time_left <- get.periodic.event.time(eventTypesToCreate) %>% 
          column.feed.backward(col = 3:ncol(.), id_col = 'caseID')
        
        for(i in 3:ncol(event_time_left)){
          ind = which(as.Date(event_time_left[, i]) < event_time_left$reportTime) %-% nrow(event_time_left)
          event_time_left[ind, i] <- event_time_left[ind + 1, i]
          event_time_left[, i]    <- as.numeric(as.Date(event_time_left[, i]) - event_time_left$reportTime) %>% {.[.<0]<-NA;.}
        }
        cat("Done!", '\n')
      }
      
      if(is.null(historic$event.time.left)){
        output <- event_time_left
      } else {
        output <- historic$event.time.left %>% 
          left_join(event_time_left, by = c('caseID', 'reportTime'))
      }
      
      if(settings$save_historic_cumulative){historic$event.time.left <<- output}
      
      return(output[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.historic.event.time.elapsed = function(...){

      eventTypes = get.eventTypes(...)
      if(is.null(historic$event.time.elapsed)){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(historic$event.time.elapsed))
      } 
      
      if(length(eventTypesToCreate) > 0){
        cat('\n', "Creating elapsed table ... ")
        
        event_time_elapsed <- get.periodic.event.time(...) %>% column.feed.forward(col = 3:ncol(.), id_col = 'caseID')
        
        for(i in 3:ncol(event_time_elapsed)){
          event_time_elapsed[, i] = as.numeric(event_time_elapsed$reportTime - as.Date(event_time_elapsed[, i]))
        }
        if(is.null(historic$event.time.elapsed)){
          output <- event_time_elapsed
        } else {
          output <- historic$event.time.elapsed %>% 
            left_join(event_time_elapsed, by = c('caseID', 'reportTime'))
        }
        
        if(settings$save_historic_cumulative){historic$event.time.elapsed <<- output}
        cat("Done!", '\n')
      } else{
        output = historic$event.time.elapsed
      }

      return(output[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.historic.variable.aggregated.cumulative = function(..., periodic_aggregator = 'sum', historic_aggregator = 'sum'){
      variables = get.variables(...)

      verify(historic_aggregator, 'character', 
             domain = names(CUMULATIVE_AGGREGATOR_FUNCTION), 
             default = 'sum') -> historic_aggregator
      
      historicTableName = paste("variable", periodic_aggregator, historic_aggregator, 'cumulative', sep = '.')
      
      if(is.null(historic[[historicTableName]])){
        variablesToCreate = variables
      } else {
        variablesToCreate = variables %>% setdiff(colnames(historic[[historicTableName]]))
      } 
      
      if(length(variablesToCreate) > 0){
        cat('\n', "Creating historic variable aggregated table ... ")
        
        if(historic_aggregator == 'last'){
          historic_variable_aggregated <- get.periodic.variable.aggregated(..., periodic_aggregator = periodic_aggregator) %>% 
            column.feed.forward(col = 3:ncol(.), id_col = 'caseID')
        } else {
          historic_variable_aggregated <- get.periodic.variable.aggregated(..., periodic_aggregator = periodic_aggregator) %>% 
            column.cumulative.forward(col = 3:ncol(.), id_col = 'caseID', aggregator = CUMULATIVE_AGGREGATOR_FUNCTION[[historic_aggregator]])
        }
        
        
        if(is.null(historic[[historicTableName]])){
          output <- historic_variable_aggregated
        } else {
          output <- historic[[historicTableName]] %>% 
            left_join(historic_variable_aggregated, by = c('caseID', 'reportTime'))
        }
        
        if(settings$save_historic_cumulative){historic[[historicTableName]] <<- output}
        
        cat("Done!", '\n')
      } else {output = historic[[historicTableName]]}
      
      
      return(output[c('caseID', 'reportTime', variables)])
    },

    get.historic.variable.aggregated.sliding = function(..., periodic_aggregator = 'sum', historic_aggregator = 'sum', win_size = 3){
      variables = get.variables(...)
      
      verify(historic_aggregator, 'character', 
             domain = names(AGGREGATOR_FUNCTION), 
             default = 'sum') -> historic_aggregator
      
      historicTableName = paste("variable", periodic_aggregator, historic_aggregator, win_size, sep = ".")
      
      if(is.null(historic[[historicTableName]])){
        variablesToCreate = variables
      } else {
        variablesToCreate = variables %>% setdiff(colnames(historic[[historicTableName]]))
      } 

      if(length(variablesToCreate) > 0){
        cat('\n', "Creating historic variable aggregated table ... ")

        get.periodic.variable.aggregated(..., periodic_aggregator = periodic_aggregator) -> periodic_variable_aggregated
        features = list()
        for(fn in colnames(periodic_variable_aggregated)[3:ncol(periodic_variable_aggregated)]){
          features[[fn]] <- list(name = fn, reference = fn, win_size = win_size, aggregator = AGGREGATOR_FUNCTION[[historic_aggregator]], type = 's')
        }
        periodic_variable_aggregated %>% dfg.historic.sliding(features, eventTime_col = 'reportTime') -> historic_variable_aggregated
                
        cat("Done!", '\n')
        if(is.null(historic[[historicTableName]])){
          output <- historic_variable_aggregated
        } else {
          output <- historic[[historicTableName]] %>% 
            left_join(historic_variable_aggregated, by = c('caseID', 'reportTime'))
        }
        
        if(settings$save_historic_sliding){historic[[historicTableName]] <<- output}
      } else {
        output = historic[[historicTableName]]
      }
      
      
      return(output[c('caseID', 'reportTime', variables)])
    },
    
    get.historic.event.aggregated.sliding = function(..., periodic_aggregator = 'count', historic_aggregator = 'sum', win_size = 3){
      eventTypes = get.eventTypes(...)
      
      verify(historic_aggregator, 'character', 
             domain = names(AGGREGATOR_FUNCTION), 
             default = 'sum') -> historic_aggregator
      
      historicTableName = paste("event", periodic_aggregator, historic_aggregator, win_size, sep = ".")
      
      if(is.null(historic[[historicTableName]])){
        eventTypesToCreate = eventTypes
      } else {
        eventTypesToCreate = eventTypes %>% setdiff(colnames(historic[[historicTableName]]))
      } 

      if(length(eventTypesToCreate) > 0){
        cat('\n', "Creating historic event aggregated table ... ")
        
        get.periodic.event.aggregated(..., periodic_aggregator = periodic_aggregator) -> periodic_event_aggregated
        features = list()
        for(fn in colnames(periodic_event_aggregated)[3:ncol(periodic_event_aggregated)]){
          features[[fn]] <- list(name = fn, reference = fn, win_size = win_size, aggregator = AGGREGATOR_FUNCTION[[historic_aggregator]], type = 's')
        }
        periodic_event_aggregated %>% dfg.historic.sliding(features, eventTime_col = 'reportTime') -> historic_event_aggregated
        
        cat("Done!", '\n')
        if(is.null(historic[[historicTableName]])){
          output <- historic_event_aggregated
        } else {
          output <- historic[[historicTableName]] %>% 
            left_join(historic_event_aggregated, by = c('caseID', 'reportTime'))
        }
        
        if(settings$save_historic_sliding){historic[[historicTableName]] <<- output}
      } else {
        output = historic[[historicTableName]]
      }

      return(output[c('caseID', 'reportTime', eventTypes)])
    },
    
    get.feature_names = function(){}
    
  )
) 


