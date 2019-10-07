# transys.R --------------------------------------------------------------------

# Header
# Description:   This module introduces a reference class named as TRANSYS which only keeps the status changes.
#                Each case can have only one status at a time.
#                This model can also be used for processes which do not have concurrent activities.
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    08 November 2016
# Last Revision: 09 September 2019
# Version:       0.8.6

# Version   Date               Action
# -----------------------------------
# 0.0.1     08 November 2016   Initial issue
# 0.1.0     09 November 2016   Class TS.CASE modified: All methods defined within the class
# 0.2.0     15 November 2016   Queries added to SIMPLE.PROCESS objects. case_IDs should be first set as a query, then call property methods to get various process properties
# 0.2.1     15 November 2016   Method getTimeAdjacency() added to class TS.CASE
# 0.3.0     15 November 2016   Major changes in class SIMPLE.PROCESS: Property 'data' renamed to 'history', properties adjacency, visNetwork, igraph and all other process metrics moved to list property 'data'
# 0.3.1     15 November 2016   Method getTimeAdjacency() added to class SIMPLE.PROCESS
# 0.4.0     18 November 2016   Thanks to dplyr, all the sorting, next status duration is computed in the first feed. Methods feed(), fillCases() modified!
# 0.4.1     08 February 2017   SIMPLE.PROCESS renamed to TRANSYS. TS.CASE renamed to TS.CASE
# 0.4.2     08 February 2017   Filename changed to transys.R
# 0.4.3     16 February 2017   Method getAdjacency() and getTimeAdjacency() removed from class TRANSYS and replaced by method getAdjacencies()
# 0.4.4     16 February 2017   Method getStatusVolume() added. Returns an object of class TIME.SERIES
# 0.5.0     06 September 2017  Method feed.eventlog() modified: Argument add_ends added! If TRUE, statuses START and END will be added to the beginning and end of processes!
# 0.5.1     11 September 2017  Method feed.eventlog() modified: Columns paths and selected added
# 0.5.2     14 September 2017  Property query removed.
# 0.5.3     14 September 2017  Function computeFullAdjacencies() added
# 0.5.4     14 September 2017  Functions getOutlierCases() and applyCaseFilter() added
# 0.5.5     14 September 2017  Function getAdjacencies modified.
# 0.5.6     14 September 2017  Function getSimpleAdjacencies added.
# 0.6.0     30 June 2018       Fundamental changes: All methods removed and renamed.
# 0.6.1     02 July 2018       Function feed.eventlog() modified
# 0.6.3     03 July 2018       Function get.volumeIn(), get.volumeOut() and get.backlog() modified. Method compute.volumes.daily() removed
# 0.6.6     14 September 2018  Functions get.volumeIn(), get.volumeOut() and get.backlog() now can give with hourly timeseries.
# 0.6.7     12 October 2018    All plotting functions transferred to tsvis.R
# 0.7.0     23 October 2018    Properties 'modelStart' and 'modelEnd' added to class TRANSYS, functions get.volumeIn() & get.volumeOut() modified accordingly.
# 0.7.2     26 October 2018    Method filter.case() modified: two filtering arguments added: startStatuses, endStatuses
# 0.7.3     06 November 2018   Generic function summary.TRANSYS() modified: collapses multiple statuses in the summary string
# 0.7.4     24 February 2019   Function feed.eventlog() modified: convert dataset to data.frame to avoid causing error on tibble inputs
# 0.7.5     21 June 2019       Function TRANSYS.summary() modified: A minor bug was fixed.
# 0.7.9     09 July 2019       Function TRANSYS.simulate() added, methods get.mldata.tc(), get.time.status.volume() added, Some method names changed, property 'cases' embedded in 'tables' 
# 0.8.1     16 July 2019       Method filter.event() added, argument 'extra_col' added to feed.eventlog()
# 0.8.2     08 August 2019     Method simulate.TRANSYS() modified
# 0.8.3     20 August 2019     Method simulate.TRANSYS() modified: progressbar added.
# 0.8.4     20 August 2019     Method feed.eventlog() modified: remove_sst bug rectified: now updates startTime
# 0.8.5     09 September 2019  Method feed.eventlog() modified: adds new events to the existing history.
# 0.8.6     09 September 2019  Generic function simulate.TRANSYS() transfered as method run.simulate().

# Good information for working with R functions:
# http://adv-r.had.co.nz/Functions.html

# fast Data Manipulation in R:
# https://www.analyticsvidhya.com/blog/2015/12/faster-data-manipulation-7-packages/
# fast Table Reshape:
# http://seananderson.ca/2013/10/19/reshape.html
# http://stackoverflow.com/questions/26536251/comparing-gather-tidyr-to-melt-reshape2


# Required Libraries:
library(dplyr)

empt = Sys.time()[-1]
empd = Sys.Date()[-1]

# This global variable is repeated in prom, can transfer to gener
#' @export
timeUnitCoeff = c(hour = 3600, second = 1, minute = 60, day = 24*3600, week = 7*24*3600, year = 24*3600*365)

#' @title TRANSYS: A reference class for modelling Transition Systems
#' @exportClass TRANSYS
#' @description Reference class containing some properties and methods required for analysing, modelling, simulating and visualising a Transition Sytem.
#' A Transistopn System is the more general case of a Markov Chain model descibing a system which can change status over time.
#'
#' @field history data.frame holding history data of status transitions
#' @field nodes data.frame holding nodes of a transition system network. Nodes are equivalent to statuses.
#' @field links data.frame holding links of a transition system network. Links are equivalent to transitions.
TRANSYS = setRefClass('TRANSYS',
                      fields = list(
                        modelStart    = 'POSIXct',
                        modelEnd      = 'POSIXct',
                        history       = 'data.frame',
                        settings      = 'list',
                        tables        = 'list',
                        report        = 'list',
                        metrics       = 'list',
                        plots         = 'list',
                        timeseries    = 'list',
                        timeseries.full  = 'list'
                      ),

                      methods = list(
                        
                        clear = function(){
                          report     <<- list()
                          plots      <<- list()
                          timeseries <<- list()
                          metrics    <<- list()
                        },

                        initialize = function(start = NULL, end = NULL, ...){
                          callSuper(...)
                          settings$include_case_measures <<- F
                          settings$filter <<- list(complete = NULL, minLoops = NULL, maxLoops = NULL, IDs = NULL, startStatuses = NULL, endStatuses = NULL, freqThreshold = NULL)

                          history <<- data.frame(caseID = character(), status = character(), nextStatus = character(), startTime = empt, endTime = empt, caseStart = logical(), caseEnd = logical(),
                                                 selected = logical(), startDate = empd, endDate = empd, creation = empt, duration = numeric(), eventAge = numeric(), path = character(),
                                                 stringsAsFactors = F)
                          tables$profile.case <<- data.frame(caseID = character(), firstStatus = character(), lastStatus = character(), startTime = empt, endTime = empt, caseStart = logical(), caseEnd = logical(), duration = numeric(),
                                                 stringsAsFactors = F)

                          modelStart <<- start %>% as.time
                          modelEnd   <<- end %>% as.time
                        },

                        # remove_sst: remove same status transitions
                        # caseStartFlag_col
                        feed.eventlog = function(dataset, caseID_col = 'caseID', status_col = 'status', startTime_col = 'startTime', caseStartFlag_col = NULL, caseEndFlag_col = NULL, caseStartTags = NULL, caseEndTags = NULL, sort_startTime = T, add_start = T, remove_sst = F, extra_col = NULL){
                          "Feeds an eventlog as a data.frame or tibble to train the transition system model."
                          # verifications
                          dataset %<>%
                            nameColumns(columns = list(caseID = caseID_col , status = status_col , startTime = startTime_col, caseStart = caseStartFlag_col, caseEnd = caseEndFlag_col),
                                        classes = list(caseID = 'character', status = 'character', startTime = 'POSIXct', caseStart = 'logical', caseEnd = 'logical')) %>%
                            mutate(selected = T)

                          # dataset$status = gsub("[[:space:]]", "", as.character(dataset$status))

                          tbd = is.na(dataset$status) |
                            is.na(dataset$caseID) |
                            is.na(dataset$startTime) |
                            (dataset$status == "") |
                            (dataset$caseID == "") |
                            is.na(dataset$caseStart) |
                            is.na(dataset$caseEnd)

                          tbd = which(tbd)
                          if(length(tbd) > 0){
                            warnif(T, length(tbd) %++% ' events removed due to missing values in one of these critical columns: caseID, status, startTime, caseStart or caseEnd!')
                            dataset <- dataset[- tbd, ]
                          }

                          support('dplyr')
                          dataset %<>% dplyr::mutate(nextStatus = status, endTime = startTime) %>%
                            # dplyr::distinct(caseID, startTime, .keep_all = TRUE) %>% # remove duplicated combination of caseID and startTime
                            dplyr::arrange(caseID, startTime) %>% column.shift.up(c('nextStatus', 'endTime'), keep.rows = T)

                          # Removing cases with no transition (all cases who remained in their first status until the end of the dataset are removed!)
                          dp = which(!duplicated(dataset$caseID))

                          if(is.null(dataset$caseStart)){
                            if(is.null(caseStartTags)){dataset$caseStart = T} else {
                              startedcases = dataset$caseID[dataset$status %in% caseStartTags] %>% unique
                              dataset$caseStart = dataset$caseID %in% startedcases}}
                          
                          if(is.null(dataset$caseEnd)){
                            if(is.null(caseEndTags)){dataset$caseEnd = T} else {
                              endedcases = dataset$caseID[dataset$status %in% caseEndTags] %>% unique
                              dataset$caseEnd = dataset$caseID %in% endedcases}}

                          dataset %<>% spark.select('caseID', 'status', 'nextStatus', 'startTime', 'endTime', 'caseStart', 'caseEnd', 'selected', extra_col) %>% as.data.frame

                          endindx = c(dp[-1] - 1, nrow(dataset))
                          dataset[endindx, 'nextStatus'] = ifelse(dataset[endindx, 'caseEnd'], 'END', 'EXIT')
                          dataset[endindx, 'endTime'] = dataset[endindx, 'startTime'] + 0.1

                          if(add_start){
                            rb           = dataset[dp,]
                            rb$endTime   = rb$startTime
                            rb$startTime = rb$startTime - 0.1
                            rb$nextStatus = rb$status
                            rb$status     = ifelse(rb$caseStart, 'START', 'ENTER')

                            dataset %<>% rbind(rb) %>% dplyr::arrange(caseID, startTime)
                            dp = which(!duplicated(dataset$caseID))
                            endindx = c(dp[-1] - 1, nrow(dataset))
                          }
                          
                          dataset[dp, c('caseID', 'startTime', 'status', 'caseStart', 'caseEnd')] %>%
                            inner_join(dataset[endindx, c('caseID', 'endTime', 'nextStatus')], by = 'caseID') %>%
                            dplyr::select(caseID, firstStatus = status, lastStatus = nextStatus, startTime, endTime, caseStart, caseEnd) %>%
                            dplyr::mutate(duration = as.numeric(endTime - startTime), completed = caseStart & caseEnd, selected = T) %>% 
                            {rownames(.)<-.$caseID;.} -> tables.profile.case
                          
                          if(remove_sst){
                            dataset = dataset[dataset$status != dataset$nextStatus, ] 
                            dpinv   = duplicated(dataset$caseID) %>% which
                            dataset$startTime[dpinv] <- dataset$endTime[dpinv - 1]
                            # dp = which(!duplicated(dataset$caseID))
                          }

                          dataset %<>%
                            mutate(startDate = startTime %>% as_date,
                                   endDate   = endTime   %>% as_date)

                          tables.profile.status <- data.frame(status = as.character(dataset$status %U% dataset$nextStatus)) %>% {rownames(.)<-.$status;.}
                          report.caseIDs        <- tables.profile.case$caseID
                          report.statuses       <- tables.profile.status$status

                          cs = as.character(dataset$caseID)

                          dataset$creation   <- tables.profile.case[cs, 'startTime']
                          dataset$duration   <- as.numeric(dataset$endTime - dataset$startTime)
                          dataset$eventAge   <- as.numeric(dataset$startTime - dataset$creation)

                          dataset$selected = TRUE
                          dataset$path     = dataset$status %++% '-' %++% dataset$nextStatus
 
                          if(is.empty(history)){
                            history <<- dataset
                            tables$profile.case <<- tables.profile.case
                            report$caseIDs      <<- report.caseIDs
                            report$statuses     <<- report.statuses
                          } else {
                            history <<- dplyr::bind_rows(history, dataset)
                            tables$profile.case <<- dplyr::bind_rows(tables$profile.case, tables.profile.case) 
                            report$caseIDs      <<- c(report$caseIDs, report.caseIDs) %>% unique
                            report$statuses     <<- c(report$statuses, report.statuses) %>% unique
                          }
                          
                          if(is.empty(modelStart)){modelStart <<- min(history$startTime)} else {modelStart <<- min(history$startTime, modelStart)}
                          if(is.empty(modelEnd)){modelEnd     <<- max(history$endTime)} else {modelEnd <<- max(history$endTime, modelEnd)}

                          filter.case(complete = settings$filter$complete, minLoops = settings$filter$minLoops, maxLoops = settings$filter$maxLoops, IDs = settings$filter$IDs, startStatuses = settings$filter$startStatuses, endStatuses = settings$filter$endStatuses, freqThreshold = settings$filter$freqThreshold)
                        },

                        # tables in cases are always full and do not depend on the filtering
                        get.case.path = function(){
                          "Returns trace, path, DNA or status sequence of each case"
                          if(is.null(tables$profile.case$path)){
                            cat('\n', 'Aggregating cases to find paths ...')
                            history %>% dplyr::group_by(caseID) %>%
                              dplyr::summarise(path = paste(status, collapse = '-') %>% paste(last(nextStatus), sep = '-'), startStatus = status[2], endStatus = last(status), transCount = length(status), loops = sum(duplicated(status), na.rm = T)) %>%
                              # dplyr::summarise(minTime = min(startTime), maxTime = max(endTime), startFlag = caseStart[1], endFlag = caseEnd[1], ) %>%
                              # dplyr::mutate(duration2 = as.numeric(maxTime - minTime)) %>%
                              dplyr::ungroup() -> case.path
                            
                            tables$profile.case <<- tables$profile.case %>% left_join(case.path, by = 'caseID')
                            
                            cat('Done!', '\n')
                          }
                          return(tables$profile.case %>% select(caseID, path, loops, transCount, duration, completed))
                        },

                        # returns the Case Status Time (CST) matrix containing amount of time each case spent on each status. NA means the case never met the status
                        get.case.status.duration = function(){
                          "Returns a table containing duration of each case on each status"
                          if(is.null(report$case.status.duration)){
                            cat('\n', 'Aggregating cases to find status durations ...')
                            if(nrow(history) == 0){
                              report$case.status.duration <<- data.frame()
                            } else if (is.null(report$case.status.duration)){
                              history %>% dplyr::group_by(caseID, status) %>%
                                dplyr::summarise(duration = sum(duration, na.rm = T)) %>%
                                reshape2::dcast(caseID ~ status, value.var = 'duration') %>% column2Rownames('caseID') ->> report$case.status.duration
                            }
                            cat('Done!', '\n')
                          }
                          return(report$case.status.duration)
                        },

                        get.case.status.freq = function(){
                          if(nrow(history) == 0){
                            report$case.status.freq <<- data.frame()
                          } else if(is.null(report$case.status.freq)){
                            history %>% dplyr::group_by(caseID, status) %>%
                              dplyr::summarise(freq = length(duration)) %>%
                              reshape2::dcast(caseID ~ status, value.var = 'freq') %>% na2zero %>% column2Rownames('caseID') ->> report$case.status.freq
                          }
                          return(report$case.status.freq)
                        },

                        get.volumeIn = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily  = {
                                   if(full){null = is.null(timeseries.full$volin.daily)} else {null = is.null(timeseries$volin.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volin = new('TS.DAILY')} else {
                                       B.entr = hist %>% dplyr::group_by(startDate, status) %>% dplyr::summarise(length(caseID))

                                       names(B.entr) <- c('Date', 'Status', 'Entry')

                                       B.entr %<>% reshape2::dcast(Date ~ Status, sum, value.var = 'Entry')

                                       modelStartDate <- as.Date(modelStart %>% setTZ('GMT'))
                                       modelEndDate   <- as.Date(modelEnd %>% setTZ('GMT'))

                                       volin <- new('TS.DAILY', from = modelStartDate, until = modelEndDate)
                                       volin$feedData(B.entr, date_col = 'Date')
                                       volin$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volin.daily <<- volin
                                     else      timeseries$volin.daily     <<- volin
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volin.daily)} else {return(timeseries$volin.daily)}}
                                   else             {if(full){return(timeseries.full$volin.daily$data)} else {return(timeseries$volin.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$volin.hourly)} else {null = is.null(timeseries$volin.hourly)}
                                   if(null){
                                     # compute.volumes.hourly(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volin = new('TS.HOURLY')} else {
                                       B.entr = hist %>% mutate(startHour = cut(startTime, breaks = 'hour')) %>%
                                         dplyr::group_by(startHour, status) %>% dplyr::summarise(length(caseID)) %>% dplyr::ungroup()

                                       names(B.entr) <- c('Hour', 'Status', 'Entry')

                                       B.entr %<>% reshape2::dcast(Hour ~ Status, sum, value.var = 'Entry')

                                       B.entr %<>% dplyr::mutate(Hour = as.POSIXct(Hour))

                                       volin <- new('TS.HOURLY', from = modelStart, until = modelEnd)
                                       volin$feedData(B.entr, hour_col = 'Hour')
                                       volin$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volin.hourly <<- volin
                                     else      timeseries$volin.hourly     <<- volin
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volin.hourly)} else {return(timeseries$volin.hourly)}}
                                   else             {if(full){return(timeseries.full$volin.hourly$data)} else {return(timeseries$volin.hourly$data)}}
                                 })
                        },

                        get.volumeOut = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(timeseries.full$volout.daily)} else {null = is.null(timeseries$volout.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volout <- new('TS.DAILY')} else {
                                       B.exit = hist %>% dplyr::group_by(endDate, status)   %>% dplyr::summarise(length(caseID))

                                       names(B.exit) <- c('Date', 'Status', 'Exit')

                                       B.exit %<>% reshape2::dcast(Date ~ Status, sum, value.var = 'Exit')

                                       modelStartDate <- as.Date(modelStart %>% setTZ('GMT'))
                                       modelEndDate   <- as.Date(modelEnd %>% setTZ('GMT'))

                                       volout <- new('TS.DAILY', from = modelStartDate, until = modelEndDate)
                                       volout$feedData(B.exit, date_col = 'Date')
                                       volout$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volout.daily <<- volout
                                     else      timeseries$volout.daily     <<- volout
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volout.daily)} else {return(timeseries$volout.daily)}}
                                   else             {if(full){return(timeseries.full$volout.daily$data)} else {return(timeseries$volout.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$volout.hourly)} else {null = is.null(timeseries$volout.hourly)}
                                   if(null){
                                     # compute.volumes.hourly(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volout <- new('TS.HOURLY')} else {
                                       B.exit = hist %>% dplyr::mutate(endHour = cut(endTime, breaks = 'hour')) %>%
                                         dplyr::group_by(endHour, status) %>% dplyr::summarise(length(caseID)) %>% dplyr::ungroup()

                                       names(B.exit) <- c('Hour', 'Status', 'Exit')

                                       B.exit %<>% reshape2::dcast(Hour ~ Status, sum, value.var = 'Exit')
                                       B.exit %<>% dplyr::mutate(Hour = as.POSIXct(Hour))

                                       volout <- new('TS.HOURLY', from = modelStart, until = modelEnd)
                                       volout$feedData(B.exit, hour_col = 'Hour')
                                       volout$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volout.hourly <<- volout
                                     else      timeseries$volout.hourly     <<- volout
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volout.hourly)} else {return(timeseries$volout.hourly)}}
                                   else             {if(full){return(timeseries.full$volout.hourly$data)} else {return(timeseries$volout.hourly$data)}}
                                 })
                        },

                        get.backlog = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(timeseries.full$backlog.daily)} else {null = is.null(timeseries$backlog.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)

                                     volout = get.volumeOut(full = full) %>% as.data.frame %>% column2Rownames('date')
                                     volin  = get.volumeIn(full = full) %>% as.data.frame %>% column2Rownames('date')

                                     dt = rownames(volout) %U% rownames(volin)
                                     st = colnames(volout) %^% colnames(volin)

                                     BL = (volin[dt, st] %>% as.matrix) - (volout[dt, st] %>% na2zero %>% as.matrix)

                                     BL <- BL %>% cumulative

                                     # assert((sum(BL[nrow(BL),]) == 0) & (min(BL) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])

                                     BL %<>% as.data.frame %>% rownames2Column('Date')
                                     BL$Date %<>% as.Date
                                     firstDay <- min(BL$Date)
                                     lastDay  <- max(BL$Date)

                                     BL %<>% column.shift.up('Date', keep.rows = T)
                                     BL <- rbind(BL[1, ], BL)
                                     BL[1, -1] <- 0
                                     BL[1, 'Date'] <- firstDay
                                     BL[nrow(BL), 'Date'] <- lastDay + 1

                                     backlog <- new('TS.DAILY', from = min(BL$Date), until = max(BL$Date))
                                     backlog$feedData(BL, date_col = 'Date')

                                     if(full){
                                       timeseries.full$backlog.daily <<- backlog
                                     } else{
                                       timeseries$backlog.daily <<- backlog
                                     }

                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$backlog.daily)} else {return(timeseries$backlog.daily)}}
                                   else             {if(full){return(timeseries.full$backlog.daily$data)} else {return(timeseries$backlog.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$backlog.hourly)} else {null = is.null(timeseries$backlog.hourly)}
                                   if(null){
                                     volout = get.volumeOut(full = full, period = period) %>% as.data.frame %>% column2Rownames('time')
                                     volin  = get.volumeIn(full = full, period = period) %>% as.data.frame %>% column2Rownames('time')

                                     dt = rownames(volout) %U% rownames(volin)
                                     st = colnames(volout) %^% colnames(volin)

                                     BL = (volin[dt, st] %>% as.matrix) - (volout[dt, st] %>% na2zero %>% as.matrix)

                                     BL <- BL %>% cumulative

                                     # assert((sum(BL[nrow(BL),]) == 0) & (min(BL) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])

                                     BL %<>% as.data.frame %>% rownames2Column('Hour')
                                     BL$Hour %<>% as.POSIXct

                                     firstHour <- min(BL$Hour)
                                     lastHour  <- max(BL$Hour)

                                     BL %<>% column.shift.up('Hour', keep.rows = T)
                                     BL <- rbind(BL[1, ], BL)
                                     BL[1, -1] <- 0
                                     BL[1, 'Hour'] <- firstHour
                                     BL[nrow(BL), 'Hour'] <- lastHour + 3600

                                     backlog <- new('TS.HOURLY', from = min(BL$Hour), until = max(BL$Hour))
                                     backlog$feedData(BL, hour_col = 'Hour')

                                     if(full){
                                       timeseries.full$backlog.hourly <<- backlog
                                     } else{
                                       timeseries$backlog.hourly <<- backlog
                                     }

                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$backlog.hourly)} else {return(timeseries$backlog.hourly)}}
                                   else             {if(full){return(timeseries.full$backlog.hourly$data)} else {return(timeseries$backlog.hourly$data)}}
                                 })
                        },

                        get.metric = function(measure = c('freq', 'totComp', 'avgComp', 'totTT', 'avgTT', 'medTT', 'sdTT', 'totLoops', 'avgLoops', 'medLoops',  'totTrans', 'avgTrans'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          nontime   = c('freq', 'totLoops', 'avgLoops', 'medLoops', 'totComp', 'avgComp', 'totTrans', 'avgTrans')
                          measure   = match.arg(measure)
                          time_unit = match.arg(time_unit)
                          k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])

                          if(is.null(metrics[[measure]])){
                            if(measure %in% c('totLoops', 'avgLoops', 'medLoops')){get.case.path()}
                            switch(measure,
                                   'freq' = {metricval = length(get.cases()) %>% as.integer},
                                   'totTT' = {metricval = tables$profile.case[get.cases(), 'duration'] %>% sum(na.rm = T)},
                                   'avgTT' = {metricval = tables$profile.case[get.cases(), 'duration'] %>% mean(na.rm = T)},
                                   'sdTT' = {metricval = tables$profile.case[get.cases(), 'duration'] %>% sd(na.rm = T)},
                                   'medTT' = {metricval = tables$profile.case[get.cases(), 'duration'] %>% median(na.rm = T)},
                                   'totLoops' = {metricval = tables$case.path[get.cases(), 'loops'] %>% sum(na.rm = T) %>% as.integer},
                                   'avgLoops' = {metricval = tables$case.path[get.cases(), 'loops'] %>% mean(na.rm = T)},
                                   'medLoops' = {metricval = tables$case.path[get.cases(), 'loops'] %>% median(na.rm = T) %>% as.integer},
                                   'totTrans' = {metricval = tables$case.path[get.cases(), 'transCount'] %>% sum(na.rm = T) %>% as.integer},
                                   'avgTrans' = {metricval = tables$case.path[get.cases(), 'transCount'] %>% mean(na.rm = T)},
                                   'totComp' = {metricval = (tables$profile.case[get.cases(), 'caseStart'] &  tables$profile.case[get.cases(), 'caseEnd']) %>% sum(na.rm = T) %>% as.integer},
                                   'avgComp' = {metricval = (tables$profile.case[get.cases(), 'caseStart'] &  tables$profile.case[get.cases(), 'caseEnd']) %>% mean(na.rm = T)}
                            )
                            metrics[[measure]] <<- metricval
                          }
                          return(round(k*metrics[[measure]], digits = 2))
                          # add: loops, completion percentage, most frequent longest status,    distribution of cases over their longest status, distribution of total time over status
                        },

                        # valid.link.weights = c('totFreq', 'meanCaseFreq', 'medCaseFreq', 'sdCaseFreq', 'totalTime', ...)
                        # totalFreq:    Total transition frequency
                        # meanCaseFreq: average transition count per case
                        # medCaseFreq:  median case transition frequency (half of cases have transition frequency higher than this value)
                        # sdCaseFreq:   standard deviation of transition frequencies among cases
                        # totalTime:    Total transition time
                        # meanCaseTime: average transition time per case
                        # medCaseTime:  median case transition time (half of cases have transition time higher than this value)
                        # sdCaseTime:   standard deviation of transition time among cases
                        # meanTime:average transition time per transition
                        # medTime :median transition time (half of transitions have transition time higher than this value)
                        # sdTime  :standard deviation of transition time among transitions(half of transitions have transition time higher than this value)
                        # meanCaseEntryFreq: how many times in average a case has entered this status
                        # For nodes (statuses):
                        # totExitFreq: Total count of transitions from this status to other statuses
                        get.nodes = function(full = F){
                          if(full){
                            if(!is.empty(tables$nodes)){return(tables$nodes)} else {H = history}
                          } else {
                            if(!is.empty(report$nodes)){return(report$nodes)} else {H = history %>% dplyr::filter(selected)}
                          }

                          cat('\n', 'Aggregating nodes ...')

                          if(settings$include_case_measures){
                            A.node = H %>% dplyr::group_by(caseID, status) %>% dplyr::summarise(count = sum(selected, na.rm = T), duration = sum(duration, na.rm = T))
                            B.node = A.node %>% dplyr::group_by(status) %>%
                              dplyr::summarise(meanExitCaseFreq = mean(count, na.rm = T)   , medExitCaseFreq = median(count, na.rm = T)   , sdExitCaseFreq = sd(count, na.rm = T),
                                               meanCaseDuration = mean(duration, na.rm = T), medCaseDuration = median(duration, na.rm = T), sdCaseDuration = sd(duration, na.rm = T))
                          }

                          C.node = H %>% dplyr::group_by(status) %>%
                            dplyr::summarise(totalExitFreq = sum(selected, na.rm = T), totalDuration = sum(duration, na.rm = T),
                                             meanDuration = mean(duration, na.rm = T), medDuration = median(duration, na.rm = T), sdDuration = sd(duration, na.rm = T))

                          D.node = H %>% dplyr::group_by(nextStatus) %>% select(status = nextStatus, count = selected) %>%
                            dplyr::summarise(totalEntryFreq = sum(count, na.rm = T)) %>% full_join(C.node, by = 'status')

                          if(settings$include_case_measures){
                            D.node %<>% full_join(B.node, by = 'status')
                          }
                          nsel = sum(history$selected)
                          nrow = nrow(history)
                          if(full  | nsel == nrow){tables$nodes <<- D.node}
                          if(!full | nsel == nrow){report$nodes <<- D.node}
                          if(is.empty(D.node)){
                            data.frame(status = character(), totalEntryFreq = numeric(), totalExitFreq = numeric(), totalDuration = numeric(), meanDuration = numeric(), medDuration = numeric(), sdDuration = numeric(), stringsAsFactors = F) %>% as_tibble -> D.node
                          }
                          cat('Done!', '\n')
                          return(D.node)
                        },

                        get.links = function(full = F){
                          if(full){
                            if(!is.empty(tables$links)){return(tables$links)} else {H = history}
                          } else {
                            if(!is.empty(report$links)){return(report$links)} else {H = history %>% dplyr::filter(selected)}
                          }

                          cat('\n', 'Aggregating links ...')

                          if(settings$include_case_measures){
                            A.edge = H %>% dplyr::group_by(caseID, status, nextStatus) %>% dplyr::summarise(count = sum(selected, na.rm = T), duration = sum(duration, na.rm = T))
                            B.edge = A.edge %>% dplyr::group_by(status, nextStatus) %>%
                              dplyr::summarise(meanCaseFreq = mean(count, na.rm = T)   , medCaseFreq = median(count, na.rm = T)   , sdCaseFreq = sd(count, na.rm = T),
                                               meanCaseTime = mean(duration, na.rm = T), medCaseTime = median(duration, na.rm = T), sdCaseTime = sd(duration, na.rm = T))
                          }

                          C.edge = H %>% dplyr::group_by(status, nextStatus) %>%
                            dplyr::summarise(totalFreq = sum(selected, na.rm = T), totalTime = sum(duration, na.rm = T), meanTime = mean(duration, na.rm = T), medTime = median(duration, na.rm = T), sdTime = sd(duration, na.rm = T))

                          if(settings$include_case_measures){
                            C.edge %<>% full_join(B.edge, by = c('status', 'nextStatus'))
                          }
                          if(is.empty(C.edge)){
                            data.frame(status = character(), nextStatus = character(), totalFreq = numeric(), totalTime = numeric(), meanTime = numeric(), medTime = numeric(), sdTime = numeric(), stringsAsFactors = F) %>% as_tibble -> C.edge
                          }

                          nsel = sum(history$selected)
                          nrow = nrow(history)

                          if(full  | nsel == nrow){tables$links <<- C.edge}
                          if(!full | nsel == nrow){report$links <<- C.edge}
                          cat('Done!', '\n')
                          return(C.edge)
                        },

                        # Incomplete functions
                        get.metric.status = function(statusID, measure = c('freq', 'totTime', 'loopRate', 'caseRatio')){
                          measure = match.arg(measure)
                          switch(measure, 'freq' = {
                            CSF <- get.case.status.freq()
                            val = CSF[,statusID] %>% sum(na.rm = T) %>% as.integer
                          }, 'totTime' = {
                            CST <- get.case.status.duration()
                            val = CST[,statusID] %>% sum(na.rm = T)
                          }, 'loopRate' = {
                            CSF <- get.case.status.freq()
                            entry = CSF[, statusID] %>% na.omit %>% zero.omit
                            val = sum(entry > 1)/length(entry)
                          }, 'caseRatio' = {
                            CSF <- get.case.status.freq()
                            entry = CSF[, statusID]
                            val = sum(entry > 0)/length(entry)
                          })
                          return(val %>% round(digits = 2))
                        },

                        get.status.volumes = function(statusID, period = c('daily', 'hourly'), as_timeseries = T){
                          period = match.arg(period)
                          listnm = statusID %>% paste('volumes', period, sep = '.')
                          if(is.null(timeseries[[listnm]])){
                            vin   = get.volumeIn(as_timeseries = F, period = period) %>% pull(statusID)
                            vinc  = vin %>% cumulative
                            vout  = get.volumeOut(as_timeseries = F, period = period) %>% pull(statusID)
                            voutc = vout %>% cumulative
                            back  = get.backlog(as_timeseries = F, period = period) %>% pull(statusID)
                            dt    = get.backlog(as_timeseries = F, period = period) %>% pull(chif(period == 'daily', 'date', 'time'))

                            vol   = data.frame(time = dt, volumeIn = c(vin, NA), volumeOut = c(vout, NA), volumeInCum = c(vinc, NA), volumeOutCum = c(voutc, NA), backlog = back)
                            switch(period, 'daily' = {
                              volts = new('TS.DAILY', from = min(dt), until = max(dt))
                              volts$feedData(vol, date_col = 'time')
                            }, 'hourly' = {
                              volts = new('TS.HOURLY', from = min(dt), until = max(dt))
                              volts$feedData(vol, hour_col = 'time')
                            })
                            timeseries[[listnm]] <<- volts
                          }
                          if(as_timeseries){return(timeseries[[listnm]])} else {return(timeseries[[listnm]]$data)}
                        },

                        get.metric.case   = function(caseID, measure = c('freq', 'time'), aggregator = c('sum', 'mean', 'median', 'sd')){},
                        get.metric.trace  = function(traceID, measure = c('freq', 'time'), aggregator = c('sum', 'mean', 'median', 'sd')){},

                        get.time.status.volume = function(){
                          if(is.null(report$time.status.volume)){
                            hist = history %>% mutate(startTime = as.character(startTime), endTime = as.character(endTime))
                            hist %>% reshape2::dcast(startTime ~ status, fun.aggregate = length, value.var = 'caseID') %>% column2Rownames('startTime') %>% as.matrix -> vin
                            hist %>% reshape2::dcast(endTime ~ status,  fun.aggregate = length, value.var = 'caseID')  %>% column2Rownames('endTime') %>% as.matrix -> vout
                            times = sort(rownames(vin) %^% rownames(vout))
                            cols  = colnames(vin) %^% colnames(vout)
                            cumulative.sum(vin[times, cols] - vout[times, cols]) ->> report$time.status.volume
                          }
                          return(report$time.status.volume)
                        },
                        
                        get.adjacency = function(measure = c('freq', 'time'), full = F, aggregator = c('sum', 'mean', 'median', 'sd')){
                          measure      = match.arg(measure)
                          aggregator   = match.arg(aggregator)
                          tabName      = 'adjacency' %>% paste(measure, aggregator, chif(full, 'full', ''), sep = '.')

                          if(is.null(report[[tabName]])){
                            timevar = c(sum = 'totalTime', mean = 'meanTime', median = 'medTime', sd = 'sdTime')
                            edges   = get.links(full = full)
                            if(is.empty(edges)){return(data.frame())}
                            E = edges %>%
                              reshape2::dcast(status ~ nextStatus, value.var = chif(measure == 'freq', 'totalFreq', timevar[aggregator])) %>%
                              column2Rownames('status') %>% na2zero

                            for (i in get.statuses(full = full) %-% colnames(E)){E[, i] <- 0}
                            for (i in get.statuses(full = full) %-% rownames(E)){E[i, ] <- 0}

                            E[get.statuses(full = full), get.statuses(full = full)] ->> report[[tabName]]
                          }
                          return(tables[[tabName]])
                        },

                        get.statuses = function(full = F){
                          if(full){return(tables$profile.status$status)}
                          else {
                            if(is.empty(report$statuses)){
                              report$statuses <<- history$status[history$selected] %U% history$nextStatus[history$selected]
                            }
                            return(report$statuses)
                          }
                        },

                        get.cases = function(full = F){
                          if(full){return(tables$profile.case$caseID)}
                          else {
                            if(is.empty(report$caseIDs)){
                              report$caseIDs <<- unique(history[history$selected, 'caseID'])
                            }
                            return(report$caseIDs)
                          }
                        },

                        get.case.status.time = function(){
                          "Returns entry time of each case to each status in a matrix"
                          if(is.null(report$case.status.time)){
                            history %>% filter(selected) %>% 
                              reshape2::dcast(caseID ~ status, value.var = 'startTime', fun.aggregate = min) %>% column2Rownames('caseID') -> wide
                            for(i in sequence(ncol(wide))) {wide[,i] %<>% as.POSIXct(origin = '1970-01-01')}
                            report$case.status.time <<- wide
                          }
                          return(report$case.status.time)
                        },
                        
                        get.status.case.time = function(){
                          "Returns entry time to each status for each case in a matrix (transpose of 'case.status.time')"
                          if(is.null(report$status.case.time)){
                            history %>% filter(selected) %>% 
                              reshape2::dcast(status ~ caseID, value.var = 'startTime', fun.aggregate = min) %>% 
                              column2Rownames('status') %>% numeric2time -> wide
                            # for(i in sequence(ncol(wide))) {wide[,i] %<>% as.POSIXct(origin = '1970-01-01')}
                            report$status.case.time <<- wide
                          }
                          return(report$status.case.time)
                        },
                          
                        get.traces = function(){
                          if(is.null(report$traces)){
                            casePath = get.case.path()
                            cat('\n', 'Aggregating traces ...')
                            report$traces <<- casePath[get.cases(), ] %>% dplyr::group_by(path) %>%
                              dplyr::summarise(freq = length(path), totTime = sum(duration, na.rm = T), avgTime = mean(duration, na.rm = T), medTime = median(duration, na.rm = T), sdTime = sd(duration, na.rm = T), complete = completed[1]) %>%
                              dplyr::ungroup() %>% dplyr::arrange(freq) %>%
                              dplyr::mutate(variation = 'Variation ' %++% rev(sequence(nrow(.))))
                            cat('Done!', '\n')
                          }
                          return(report$traces)
                        },
                        
                        get.longest_path = function(){
                          "Returns the longest sequence observed"
                          if(is.null(report$longest_path)){
                              get.traces() %>% pull(path) %>% strsplit('-') -> a
                              report$longest_path <<- a[[a %>% lapply(length) %>% unlist %>% order %>% tail(1)]]
                            }
                            return(report$longest_path)
                        },
                        
                        get.transition_probabilities = function(){
                          if(is.null(report$transition_probabilities)){
                            get.links() %>% 
                              select(status, nextStatus, totalFreq) %>%
                              arrange(status, nextStatus) %>%
                              group_by(status) %>%
                              mutate(cum_freq = cumsum(totalFreq)) %>%
                              mutate(cum_prob = cum_freq/sum(totalFreq)) %>% ungroup() %>%
                              select(status, nextStatus, cum_prob) ->> report$transition_probabilities
                          }
                          return(report$transition_probabilities)
                        },
                        
                        feed.case_features = function(cf, caseID_col = 'caseID', features = NULL){
                          caseID_col %<>% verify('character', domain = colnames(cf), lengths = 1, default = 'caseID')
                          features %<>% verify('character', domain = colnames(cf), default = colnames(cf)) %-% c(caseID_col, 'caseID')
                          common_features = tables$profile.case %>% colnames %>% intersect(features)
                          warnif(length(common_features) > 0, 'These features are overwritten: ' %++% common_features %++% '\n')
                          tables$profile.case %>% spark.unselect(features) %>% 
                            left_join(cf %>% spark.select(caseID_col, features) %>% rename(caseID = caseID_col), by = 'caseID') ->> tables$profile.case
                        },

                        filter.reset = function(){
                          if(!is.empty(history)){history$selected <<- T}
                          if(!is.empty(tables$profile.transition)){tables$profile.transition$selected <<- T}
                          if(!is.empty(tables$profile.case)){tables$profile.case$selected <<- T}
                          clear()
                          report$caseIDs  <<- tables$profile.case$caseID
                          report$statuses <<- tables$profile.status$status
                          report$nodes    <<- tables$nodes
                          report$links    <<- tables$links
                          timeseries      <<- timeseries.full
                          settings$filter <<- list()
                        },
                        
                        filter.event = function(...){
                          history %>% select(-selected) %>% mutate(selected = ...) %>% {colnames(.)[ncol(.)]<-'selected';.} ->> history
                          clear()
                        },

                        filter.case = function(complete = NULL, minLoops = NULL, maxLoops = NULL, statusDomain = NULL, startStatuses = NULL, endStatuses = NULL, IDs = NULL, freqThreshold = NULL, reset = T){
                          if(reset){filter.reset()}
                          
                          if(nrow(history) > 0){
                            if(!is.null(complete %>% verify('logical'))){
                              chosen = tables$profile.case$caseStart & tables$profile.case$caseEnd
                              if(!complete){chosen = !chosen}
                              report$caseIDs <<- get.cases() %^% rownames(tables$profile.case)[chosen]
                              history$selected <<- history$caseID %in% report$caseIDs
                              settings$filter$complete <<- complete
                            }

                            if(!is.null(minLoops) | !is.null(maxLoops)){
                              cp = get.case.path()
                              if(is.null(minLoops)){minLoops = min(cp$loops, na.rm = T)}
                              if(is.null(maxLoops)){maxLoops = max(cp$loops, na.rm = T)}
                              chosen = cp$loops >= minLoops & cp$loops <= maxLoops
                              report$caseIDs <<- get.cases() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% report$caseIDs
                              settings$filter$minLoops <<- minLoops
                              settings$filter$maxLoops <<- maxLoops
                            }

                            if(!is.null(statusDomain)){
                              history$selected <<- history$selected & (history$status %in% statusDomain)
                              report$caseIDs   <<- history$caseID[history$selected] %>% unique
                              settings$filter$statusDomain <<- statusDomain
                            }

                            if(!is.null(startStatuses %>% verify('character'))){
                              cp      = get.case.path()
                              chosen  = cp$startStatus %in% startStatuses
                              report$caseIDs <<- get.cases() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% report$caseIDs
                              settings$filter$startStatuses <<- startStatuses
                            }

                            if(!is.null(endStatuses %>% verify('character'))){
                              cp      = get.case.path()
                              chosen  = cp$endStatus %in% endStatuses
                              report$caseIDs <<- get.cases() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% report$caseIDs
                              settings$filter$endStatuses <<- endStatuses
                            }

                            if(!is.null(IDs %>% verify('character'))){
                              report$caseIDs <<- report$caseIDs %^% IDs
                              settings$filter$IDs <<- IDs
                            }

                            history$selected <<- history$caseID %in% report$caseIDs
                            

                            if(!is.null(freqThreshold %>% verify(c('numeric','integer'), domain = c(0, 1), lengths = 1))){
                              adjcy = get.adjacency()
                              if(!is.empty(adjcy)){
                                adjcy %>% apply(1, vect.normalise) %>% t %>% as.data.frame %>% rownames2Column('status') %>%
                                  reshape2::melt(id.vars = 'status', variable.name = "nextStatus") %>% filter(value < 1 - freqThreshold) -> paths

                                histsel  <- history[history$selected, ]
                                histsel  <- histsel[histsel$path %in% (paths$status %++% '-' %++% paths$nextStatus),]
                                outliers <- histsel$caseID %>% unique
                                report$caseIDs <<- report$caseIDs %-% outliers
                                history$selected <<- history$caseID %in% report$caseIDs
                                settings$filter$freqThreshold <<- freqThreshold
                              }
                            }
                            
                            tables$profile.case$selected <<- tables$profile.case$caseID %in% report$caseIDs
                            if(!is.empty(tables$profile.transition)){tables$profile.transition$selected <<- tables$profile.transition$caseID %in% report$caseIDs}
                            
                            clear()
                          }
                        },

                        # Among the cases in the case list, returns IDs of cases who have ever been in the given 'status'
                        casesInStatus = function(status){
                          casedf  = history %>% filter(selected) %>% filter(status == status) %>% extract('caseID') %>% unique
                          return(casedf$caseID)
                        },

                        casesInTransition = function(source, target){
                          casedf  = history %>% filter(selected) %>% filter(path == source %++% '-' %++% target) %>% extract('caseID') %>% unique
                          return(casedf$caseID)
                        },
                        
                        run.simulation = function(start_dt, target_dt, new_starts = NULL, event_generator = gen_next_events, time_generator = gen_transition_times, silent = T, ...){
                          start_dt     <- as.time(start_dt)
                          target_dt    <- as.time(target_dt)
                          final_events <- tibble(caseID = character(), status = character(), startTime = empt, nextStatus = character(), nxtTrTime = empt)

                          if(is.null(new_starts)){
                            current_backlog = history %>% filter(selected) %>% filter(startTime < start_dt) %>% as.tbl() %>%  
                              group_by(caseID) %>% filter(startTime == max(startTime)) %>% ungroup %>% distinct(caseID, .keep_all = T) %>% 
                              filter(nextStatus != 'END') %>% 
                              select(caseID, status, startTime) %>% arrange(caseID, startTime)
                            new_starts = history %>% filter(selected) %>% filter(startTime > start_dt, startTime < target_dt, status == 'START') %>% select(caseID, status, startTime) %>% as.tbl()
                          } else {
                            current_backlog = NULL
                          }
                          
                          # historical_data <- history %>% filter(selected) %>% filter(startTime < start_dt) %>% as.tbl() %>%  select(caseID, status, startTime) %>% unique
                          # if(is.empty(historical_data)) return(final_events)
                          
                          # histobj <- new('TRANSYS')
                          # suppressWarnings({histfeed.eventlog(historical_data, caseStartTags = 'START', add_start = F)})
                          histobj = .self$copy()
                          histobj$history %<>% filter(endTime < start_dt)
                          histobj$filter.reset()
                          # todo: update tables and reports 
                          if(is.empty(histobj$history)) return(final_events)
                          
                          # start simulation: 
                          tracking <- rbind(current_backlog, new_starts)
                          if(!silent){cnt = 1}
                          
                          while(nrow(tracking) > 0){
                            tracking <- event_generator(tracking, histobj = histobj, ...) %>% 
                              time_generator(histobj = histobj, start_dt = start_dt, new_events = final_events, ...)
                            
                            final_events <- rbind(final_events, tracking)
                            # extract completed events and those that wont be completed before target_dt
                            # remove those rows from tracking and update
                            tracking %<>% 
                              filter(!(nextStatus == "END" | nxtTrTime > target_dt)) %>%
                              transmute(caseID, status = nextStatus, startTime = nxtTrTime)
                            
                            if(!silent){
                              if(nrow(tracking) > 0)
                                cat('iter: ', cnt, ' Time range from ', as.character(min(tracking$startTime)), ' to ', as.character(max(tracking$startTime)), '\n')
                              cnt = cnt + 1
                            }
                          }
                          if(!silent){cat('No more transitions (End of simulation)', '\n')}
                          
                          final_events %>% arrange(caseID, startTime)
                        }
                        

                      )
)


#' @export
summary.TRANSYS = function(obj){
  
  ff = obj$settings$filter %>% lapply(function(x) paste(x, collapse = ',')) %>% unlist
  "Prcoess time range from: " %>% paste0(
    obj$modelStart %>% as.character,
    " until: ",
    obj$modelEnd %>% as.character,
    " with ",
    nrow(obj$history),
    " events (",
    sum(obj$history$selected, na.rm = T),
    " filtered) and ",
    obj$get.statuses(full = T) %>% length,
    " stasuses (",
    obj$get.statuses(full = F) %>% length,
    " filtered) impacting ",
    obj$get.cases(full = T) %>% length,
    " cases (",
    obj$get.cases(full = F) %>% length,
    " filtered). Filter settings: ",
    chif(is.empty(ff), 'No filtering applied', names(ff) %>% paste(":") %>% paste(ff, collapse = ' , ')))
  # "32,400 filtered) and 24 statuses (13 filtered) impacting 2,450 cases (83 filtered). Filter on completed cases with relative frequency threshold of 0.25 and loops range between 0 and 12"
}
