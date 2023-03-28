# Snapshot Feature Generator from event-log for database tables
# Version     Date                 Action
# ----------------------------------
# 0.0.1       07 November 2022     Initial issue by file sfg.R
# 0.0.2       11 November 2022     keywordsUnion added to eventType filter parameters
# 0.0.7       22 November 2022     function rbig_collect() added for bigquery adressing the dbplyr version issue for bigquery
# 0.0.8       28 March 2022        minor bug in sfg method get_features.dbi() rectified: eventTypes changed to custom_eventTypes 

EVENTLOG_COLUMN_HEADERS = c('eventID', 'caseID', 'eventType', 'eventTime', 'attribute', 'value')
PERIOD_SECONDS = c(second = 1, minute = 60, hour = 3600, day = 24*3600)


rbig_collect = function(tbl, ...){
  df = try(collect(tbl), silent = T)
  if(inherits(df, 'try-error')){
    if(inherits(tbl, 'tbl_BigQueryConnection')){
      args = list(...)
      if(is.null(names(args)) & length(args) == 1 & inherits(args[[1]], 'list')){
        args = args[[1]]
      }
      sql_query = tbl %>% dbplyr::sql_render()
      tb <- bigrquery::bq_project_query(args$project, sql_query %>% as.character)
      df <- bigrquery::bq_table_download(tb) -> eventtype_attributes_web_logins
    } else stop("Argument `tbl` is not a bigquery table!")
  }
  return(df)
}



## Abbreviated variable names:
# # fn: feature name
# # fc: feature config
# # el_address: event log address

eventlog_filter_apply <- function(input, eventTypes = NULL, attributes = NULL, values = NULL){
  if(!is.null(eventTypes)){input %<>% dplyr::filter(eventType %in% eventTypes)}
  if(!is.null(attributes)){input %<>% dplyr::filter(attribute %in% attributes)}
  if(!is.null(values)){
    if(inherits(values, 'list')){
      if(!is.null(values$domain)){
        script = paste0("input = dplyr::filter(input, value %in% c(", paste(values$domain, collapse = ","), "))")
        parse(text = script) %>% eval
      }
      if(!is.null(values$min)){
        script = paste0("input = dplyr::filter(input, value > ", values$min, ")")
        parse(text = script) %>% eval
      }
      if(!is.null(values$max)){
        script = paste0("input = dplyr::filter(input, value < ", values$max, ")")
        parse(text = script) %>% eval
      }
    } else if (inherits(values, c('numeric', 'integer'))){
      script = paste0("dplyr::filter(input, value %in% c(", paste(values, collapse = ","), "))")
      input = parse(text = script) %>% eval
    }
  }
  return(input)
}

# This class currently only works with bigquery tables.
# In the future, it will support other databases as well as spark tables. 
#' @title SnapshotFeatureGenerator: A reference class for generating snapshot features from event-logs.
#' @description Reference class containing some properties and methods required 
#' for feeding event-logs and generating features based on the given feature settings.
#'
#' @field settings \code{list} list of configuration settings.
#' @field eventlogs \code{list} list of tables or data.frames containing all event-logs fed in addition to 
#' some meta-data associated with each event-log. These data are:
#' 
#' \code{eventType_attributes}: \code{data.frame} Distinct values of all eventTypes and attributes in the event-log.
#' \code{address}: \code{character} where is the event-log located. 
#' A path, url or a combination of dataset and table name to locate the event-log. 
#'  
#' @export SnapshotFeatureGenerator
SnapshotFeatureGenerator = setRefClass(
  "SnapshotFeatureGenerator",
  fields = list(
    settings = "list",
    eventlogs = "list",
    features.dbi = "list",
    features = "list"
  ),
  
  methods = list(
    initialize = function(...){
      # default settings
      settings <<- list(snapshot = Sys.time(), period = "day", time_unit = 'day', features = list(), custom_eventTypes = list())
      input_settings = list(...)
      if(length(input_settings) > 0){
        settings <<- settings %>% rlist::list.merge(input_settings)
      }
      
      for (fn in names(settings$features)){
        if (is.null(settings$features[[fn]]$period)){
          settings$features[[fn]]$period <<- settings$period
        }
        if (is.null(settings$features[[fn]]$dataset)){
          settings$features[[fn]]$dataset <<- settings$dataset
        }
        if (is.null(settings$features[[fn]]$eventlog)){
          settings$features[[fn]]$eventlog <<- settings$eventlog
        }
      }
    },
    
    feed.eventlog = function(eventlog_table, eventlog_name, ...){
      sprintf("\n Feeding eventlog %s ... ", eventlog_name) %>% cat
      if(inherits(eventlog_table, "tbl_dbi")){
        eventlogs[[eventlog_name]] <<- list(table = eventlog_table, ...) 
        columns = colnames(eventlogs[[eventlog_name]]$table)
        rutils::assert(columns %==% EVENTLOG_COLUMN_HEADERS, 
                       "These column headers are missing in the eventlog %s: %s" %>% 
                         sprintf(eventlog_name, 
                                 paste(EVENTLOG_COLUMN_HEADERS %-% columns, collapse = ", ")))
        eventlogs[[eventlog_name]]$eventType_attributes.dbi <<- eventlogs[[eventlog_name]]$table %>% distinct(eventType, attribute)
        
        try(eventlogs[[eventlog_name]]$eventType_attributes.dbi %>% rbig_collect(...), silent = T) ->>
          eventlogs[[eventlog_name]]$eventType_attributes
        
      } else {
        stop("Not any other type is supported yet. No eventlogs added.")
      }
      cat('Done! \n')
    },
    
    download.features = function(){
      for(fn in setdiff(names(features.dbi), names(features))){
        sprintf("\n Downloading feature %s ... ", fn) %>% cat
        features[[fn]] <<- try(rbig_collect(features.dbi[[fn]], eventlogs[[settings$features[[fn]]$eventlog]]), silent = T)
        cat('Done! \n')
      }  
    },
    
    get.features.dbi = function(){
      for(fn in names(settings$features)){
        if(!fn %in% names(features.dbi)){
          fc = settings$features[[fn]]
          el_address = eventlogs[[fc$eventlog]]$address
          
          ## Filter for time
          snapshot = lubridate::as_datetime(settings$snapshot)
          fc$window_type <- rutils::verify(fc$window_type, "character", domain = c('sliding', 'growing'), default = 'sliding')
          if(fc$window_type == 'sliding'){
            if(fc$period %in% names(PERIOD_SECONDS)){
              window_start = snapshot - fc$window_size*PERIOD_SECONDS[[fc$period]]
            } else switch(fc$period,
                          "week" = {window_start = snapshot - lubridate::weeks(fc$window_size)},
                          "month" = {window_start = snapshot - months(fc$window_size)},
                          "year" = {window_start = snapshot - lubridate::years(fc$window_size)})
          } else if (fc$window_type == 'growing'){
            if(is.null(fc$window_start)){
              window_start = lubridate::as_datetime('1900-01-01')
            } else {
              window_start = fc$window_start
            }
          }
          
          if(inherits(eventlogs[[fc$eventlog]]$table, "tbl")){
            fel <- eventlogs[[fc$eventlog]]$table %>% 
              dplyr::filter(eventTime <= snapshot, eventTime >= window_start)
          } else {
            stop("eventlog not found for feature %s" %>% sprintf(fn))
          }
          
          ## Find custom eventIDs
          if(fc$eventType %in% names(settings$custom_eventTypes)){
            eventType_domain = NULL
            item = settings$custom_eventTypes[[fc$eventType]]
            if(inherits(item, 'character')){
              eventType_domain = item
            } else if (inherits(item, 'list')){
              eventType_domain = item$domain
              if(!is.null(item$keywords)){
                eventType_domain %<>% 
                  union(eventlogs[[fc$eventlog]]$eventType_attributes$eventType %>% 
                          rutils::charFilter(
                            item$keywords, 
                            and = T))
              }
              if(!is.null(item$keywordsUnion)){
                eventType_domain %<>% 
                  intersect(eventlogs[[fc$eventlog]]$eventType_attributes$eventType %>% 
                              rutils::charFilter(item$keywordsUnion, and = F))
              }
            }
            if(eventType_domain %==% eventlogs[[fc$eventlog]]$eventType_attributes$eventType){
              eventType_domain = NULL
            }
            fel %>% eventlog_filter_apply(eventType_domain, item$attribute, settings$custom_eventTypes[[fc$eventType]]$value) %>% 
              distinct(eventID) -> custom_event_ids
            
            fel = custom_event_ids %>% left_join(eventlogs[[fc$eventlog]]$table, by = 'eventID') %>% 
              eventlog_filter_apply(attributes = fc$attribute, values = fc$value)
          } else {
            fel %<>% eventlog_filter_apply(eventTypes = fc$eventType, attributes = fc$attribute, values = fc$value)   
          }
          
          fel %<>% 
            dplyr::group_by(eventID) %>% 
            summarise_all(min) %>% 
            group_by(caseID)
          
          features.dbi[[fn]] <<- parse(text = "ungroup(summarise(fel, %s = %s(value)))" %>% sprintf(fn, fc$aggregator)) %>% eval
        }
      }
    },
    
    join_features = function(){
      cat("\n Joining features ... ")
      
      output = features %>% 
        rlist::list.exclude(!inherits(., 'data.frame')) %>% 
        purrr::reduce(.f = dplyr::full_join, by = 'caseID')
      
      cat('Done! \n')
      return(output)
    }
    
  )
)  




    