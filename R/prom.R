# Header
# Filename:      prom.R
# Description:   This module provides functions for process mining and process analytics.
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    02 May 2018
# Last Revision: 01 August 2018
# Version:       0.5.1

# Version   Date               Action
# ----------------------------------
# 0.1.0     02 May 2018        Initial issue
# 0.1.2     04 May 2018        Functions addGraphTables() and addTaskHistory() added and exported.
# 0.1.4     06 June 2018       Functions addTaskHistory() and activityProcTime() modifed: Warns if no event log is fed.
# 0.1.5     06 June 2018       Functions addGraphTables() modifed: Warns if function addTaskHistory() is not called before.
# 0.1.6     14 June 2018       Function addTaskHistory() modified: filters eventlog for selected events
# 0.2.0     29 June 2018       Fundamental change: object defined as ref class
# 0.2.1     30 June 2018       Property transysData added to be a list containing objects of class TRANSITION.SYSTEM
# 0.2.2     02 July 2018       Property summaryData added to be a list containing data.frames
# 0.2.4     02 July 2018       Functions get.activity.ids() and get.agent.ids() added
# 0.2.6     02 July 2018       Functions get.activity.summary.procTime() and get.agent.summary.procTime() added.
# 0.2.8     02 July 2018       Functions plot.activity.summary.procTime() and plot.agent.summary.procTime() added.
# 0.3.0     02 July 2018       Functions get.activity.summary.idleTime() and plot.agent.summary.idleTime() added.
# 0.3.1     02 July 2018       Function get.agent.summary.activity.procTime() added.
# 0.3.2     06 July 2018       Function feedEventLog() added.
# 0.3.4     17 July 2018       Functions get.activity.summary.procTime() and get.activity.summary.idleTime() modified: Reads from tasklist
# 0.3.5     17 July 2018       Functions feedEventLog() modified: creates tasklist and agentHistory after correction.
# 0.3.7     17 July 2018       Functions get.agent.summary.procTime() and get.agent.summary.idleTime() modified: Reads from agentHistory
# 0.3.9     17 July 2018       Function get.agent.summary.activity.procTime() modified: Reads from tasklist
# 0.4.1     17 July 2018       Functions plot.agent.summary.procTime() and plot.activity.summary.procTime() modified: Reads from agentHistory and tasklist
# 0.4.3     17 July 2018       Functions get.agent.ids() and get.activity.ids() modified: Reads from agentHistory and tasklist
# 0.5.0     31 July 2018       Fundamental change in the structure of the class: contains status, activity, actGroup, agent, team as lists each contain lists named as summary, transys, timeseries, and full
# 0.5.1     01 August 2018     plot.activity.summary.procTime() and plot.agent.summary.procTime() modified: applies time_unit to total as well


# Class Constructor:
# Options for node.weight: 'frequency', 'duration'
# Options for link.weight: 'frequency', 'idleTime', 'interStartTime'
# node.weight.relative   : logical default FALSE
# node.weight.freqOnCase : logical default FALSE
# link.weight.relative   : logical default FALSE
# link.weight.freqOnCase : logical default FALSE
# link.weight.aggrFunc   : function: sum, mean, median, ...
# node.weight.aggrFunc   : function: sum, mean, median, ...
# Options for time_unit  : 'days', 'hours', 'minutes', 'seconds'

timeUnitCoeff = c(hour = 3600, second = 1, minute = 60, day = 24*3600, week = 7*24*3600, year = 24*3600*365)

#' @exportClass PROCESS
PROCESS = setRefClass('PROCESS',
                      fields = list(
                        eventlog      = 'data.frame',
                        tasklist      = 'data.frame',
                        agentHistory  = 'data.frame',
                        caseProfile   = 'data.frame',
                        settings      = 'list',
                        case          = 'list',
                        agent         = 'list',
                        activity      = 'list',
                        actGroup      = 'list',
                        team          = 'list',
                        status        = 'list',
                        map.activity.actGroup = 'character',
                        map.agent.team = 'character'
                      ),
                      methods = list(

                        clear = function(){
                          status   <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                          activity <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                          actGroup <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                          agent    <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                          team     <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                          case     <<- list(ids = character(), summary = list(), transys = list(), timeseries = list())
                        },

                        initialize = function(...){
                          callSuper(...)
                          settings$include_case_measures <<- T
                          clear()
                        },

                        #
                        feedEventLog = function(dataset, caseID_col, activity_col, status_col = NULL, actGroup_col = NULL, team_col = NULL, time_col, agent_col = NULL, eventID_col = NULL, taskID_col = NULL, eventType_col = NULL,
                                                arrivalTag = 'arrival', startTag = 'start', completeTag = 'complete', suspendTag = 'suspend', caseStartTag = 'caseStart', caseEndTag = 'caseEnd',
                                                correct = T, silent = T){

                          dataset %<>% nameColumns(
                            columns = list(caseID = caseID_col , activity = activity_col, status = status_col , actGroup = actGroup_col, team = team_col   , time = time_col , agent = agent_col  , taskID = taskID_col , type = eventType_col),
                            classes = list(caseID = 'character', activity = 'character' , status = 'character', actGroup = 'character' , team = 'character', time = 'POSIXct', agent = 'character', taskID = 'character', type = 'character'))

                          if(is.null(dataset$type)){dataset$type <- 'completed'}
                          # Step 1: Translate eventTypes to standard:
                          #    1-1: add next event for each taskID:
                          arrivalTag  %<>% verify('character', default = 'arrived')
                          startTag    %<>% verify('character', default = 'started')
                          suspendTag  %<>% verify('character', default = 'suspended')
                          completeTag %<>% verify('character', default = 'completed')

                          standradEventType        <- c(rep('arrived', length(arrivalTag)), rep('started', length(startTag)), rep('suspended', length(suspendTag)), rep('completed', length(completeTag)), rep('caseStarted', length(caseStartTag)), rep('caseEnded', length(caseEndTag)))
                          names(standradEventType) <- c(arrivalTag, startTag, suspendTag, completeTag, caseStartTag, caseEndTag) %>% tolower
                          dataset$type             <- standradEventType[dataset$type %>% tolower]

                          if(is.null(dataset$eventID)){dataset$eventID <- 'EVENT' %>% paste(sequence(nrow(dataset)), sep = '-')}
                          if(is.null(dataset$taskID)){dataset$taskID <- 'TASK' %>% paste(sequence(nrow(dataset)), sep = '-')}
                          if(is.null(dataset$caseID)){dataset$caseID <- 'CASE' %>% paste(sequence(nrow(dataset)), sep = '-')}
                          if(is.null(dataset$activity)){dataset$activity <- 'ACTIVITY 1'}
                          if(is.null(dataset$actGroup)){dataset$actGroup <- 'ACTGROUP 1'}
                          if(is.null(dataset$agent)){dataset$agent <- 'AGENT 1'}
                          if(is.null(dataset$team)){dataset$team <- 'TEAM 1'}
                          if(is.null(dataset$status)){dataset$status <- 'STATUS 1'}

                          # Step 2: Remove events with missing values with a warning (Remove unknown event Types with a warning:):
                          #         Task-based events are: arrival, start, complete and suspend. Critical information required for each task-based event:
                          #         time, taskType(activity or activity), eventType, taskID, eventID. (agent is required for all event types except arrival)
                          #         events with missing values in any of these fields will be removed
                          tbd =
                            is.na(dataset$eventID)      |          (dataset$eventID == "")    |
                            is.na(dataset$taskID)       |          (dataset$taskID == "")     |
                            is.na(dataset$caseID)       |          (dataset$caseID == "")     |
                            is.na(dataset$status)       |          (dataset$activity == "")   |
                            is.na(dataset$activity)     |          (dataset$activity == "")   |
                            is.na(dataset$actGroup)     |          (dataset$actGroup == "")   |
                            # is.na(dataset$team)         |          (dataset$team == "")       |
                            # is.na(dataset$agent)        |          (dataset$agent == "")      |
                            is.na(dataset$type)         |          (dataset$type == "")       |
                            is.na(dataset$time)

                          N = sum(tbd)
                          warnif(N > 0, N %++% ' events removed due to missing data or unknown event types')
                          if(!silent & N > 0){
                            cat('Events to be removed:', chif(N > 0, '', 'showing first 20 rows ...'), '\n')
                            print(dataset[tbd,] %>% head(n = 20))
                          }
                          dataset  <- dataset[!tbd,]

                          dataset %<>%
                            select(eventID, taskID, caseID, time, status, activity, actGroup, agent, team, type)

                          dataset$caseEnded   <- dataset$type == 'caseEnded'
                          dataset$caseStarted <- dataset$type == 'caseStarted'

                          activity.actGroup = dataset %>% group_by(activity, actGroup) %>% summarise(count = length(taskID))
                          wd = activity.actGroup$activity %>% duplicated %>% which
                          dplActGroups = activity.actGroup$activity[wd] %>% unique
                          if(length(wd) > 0){
                            cat('\n','Warning: Multiple groups observed for one activity! Concatinating with actGroup to make activities unique.', '\n')
                            wc = dataset$activity %in% dplActGroups
                            wa = activity.actGroup$activity %in% dplActGroups
                            activity.actGroup$activity[wa] <- activity.actGroup$activity[wa] %++% '-' %++% activity.actGroup$actGroup[wa]
                            dataset$activity[wc] <- dataset$activity[wc] %++% '-' %++% dataset$actGroup[wc]
                          }
                          map.activity.actGroup <<- activity.actGroup$actGroup
                          names(map.activity.actGroup) <<- activity.actGroup$activity

                          if(correct){dataset %<>% correctEventlog(silent = silent)}

                          eventlog <<- dataset %>% arrange(caseID, taskID, time)

                          tasklist <<- eventlog %>% eventlog2Tasklist(silent = silent) %>% attachPrevNext(eventlog)

                          agentHistory <<- eventlog %>% eventlog2AgentHistory(silent = silent)
                        },

                        feedTasklist = function(dataset, caseID_col, activity_col, arrTime_col, compTime_col, startTime_col = NULL, agent_col = NULL, taskID_col = NULL, status_col = NULL){
                          dataset %<>%
                            nameColumns(columns = list(caseID = caseID_col , activity = activity_col, arrTime = arrTime_col, compTime = compTime_col, startTime = startTime_col, agent = agent_col, taskID = taskID_col, status = status_col),
                                        classes = list(caseID = 'character', activity = 'character', arrTime = 'POSIXct', compTime = 'POSIXct', startTime = 'POSIXct',agent = 'character', taskID = 'character', status = 'character')) %>%
                            mutate(selected = T)

                          tbd =
                            is.na(dataset$caseID)       |
                            (dataset$caseID == "")      |
                            is.na(dataset$activity)        |
                            (dataset$activity == "")       |
                            is.na(dataset$agent)        |
                            (dataset$agent == "")       |
                            is.na(dataset$arrTime)  |
                            is.na(dataset$startTime)    |
                            is.na(dataset$compTime)

                          dataset = dataset[!tbd,]

                          support('dplyr', 'magrittr')

                          cat('Computing agent previous completion time ...')
                          dataset %<>% dplyr::mutate(compDate = compTime %>% as.Date(tz = attr(compTime, "tzone")), agentPrevCompTime = compTime) %>% dplyr::group_by(agent) %>%
                            do(.[order(.$compTime),] %>% column.shift.down('agentPrevCompTime', keep.rows = T)) %>% dplyr::ungroup()
                          cat(' Done!', '\n')

                          dataset %<>% na.omit()

                          w1 = which(dataset$arrTime > dataset$compTime)
                          if(length(w1) > 0){
                            cat("Warning: There are ", length(w1), ' tasks that completed before arrival time! Corrected by changing arrival time to completion time.', '\n')
                            dataset$arrTime[w1] <- dataset$compTime[w1]
                          }

                          w2 = which(dataset$startTime < dataset$arrTime)
                          if(length(w2) > 0){
                            cat("Warning: There are ", length(w2), ' tasks that started before arrival time! Corrected by changing start time to arrival time.', '\n')
                            dataset$startTime[w2] <- dataset$arrTime[w2]
                          }

                          w3 = which(dataset$startTime > dataset$compTime)
                          if(length(w3) > 0){
                            cat("Warning: There are ", length(w3), ' tasks that started after completion time! Corrected by changing start time to completion time.', '\n')
                            dataset$startTime[w3] <- dataset$compTime[w3]
                          }

                          w4 = which(dataset$startTime < dataset$agentPrevCompTime)
                          if(length(w4) > 0){
                            cat("Warning: There are ", length(w4), ' tasks that agent started them before his/her previous completion time! Corrected by changing start time to previous completion time', '\n')
                            dataset$startTime[w4] <- dataset$agentPrevCompTime[w4]
                          }

                          dataset %<>%
                            mutate(arrivalDate        = arrTime            %>% as.Date(tz = attr(arrTime, "tzone")),
                                   startDate          = startTime          %>% as.Date(tz = attr(startTime, "tzone")),
                                   agentPrevCompDate  = agentPrevCompTime  %>% as.Date(tz = attr(agentPrevCompTime, "tzone")))

                          dataset %<>% dplyr::arrange(caseID, compTime) %>%
                            dplyr::mutate(procTime = compTime - startTime, waitTime = startTime - arrTime, delayTime = 0)

                          tasklist <<- dataset

                        },

                        get.activity.transys = function(){
                          sts = new('TRANSYS')
                          if(!is.empty(eventlog)){
                            eventlog %>% filter(type == 'completed') %>% sts$feedStatusHistory(caseID_col = 'caseID', status_col = 'activity', startTime_col = 'time', add_start = T)
                          } else if(!is.empty(tasklist)){
                            sts$feedStatusHistory(tasklist, caseID_col = 'caseID', status_col = 'activity', startTime_col = 'compTime', add_start = T)
                          }
                          return(sts)
                        },

                        get.actGroup.transys = function(){
                          sts = new('TRANSYS')
                          if(!is.empty(eventlog)){
                            eventlog %>% filter(type == 'arrived') %>% sts$feedStatusHistory(caseID_col = 'caseID', status_col = 'actGroup', startTime_col = 'time', add_start = T, remove_sst = T)
                          } else if(!is.empty(tasklist)){
                            sts$feedStatusHistory(tasklist, caseID_col = 'caseID', status_col = 'actGroup', startTime_col = 'arrTime', add_start = T, remove_sst = T)
                          }
                          return(sts)
                        },

                        get.activity.ids = function(full = F){
                          if(full){null = is.empty(activity$full$ids)} else {null = is.empty(activity$ids)}

                          if(null){
                            if(full){TL = tasklist} else {TL = tasklist %>% dplyr::filter(selected)}
                            subset = TL$activity %>% unique
                            if(full){activity$full$ids <<- subset} else {activity$ids <<- subset}
                          }

                          if(full){return(activity$full$ids)} else {return(activity$ids)}
                        },

                        get.agent.ids = function(full = F){
                          if(full){null = is.empty(agent$full$ids)} else {null = is.empty(agent$ids)}
                          if(null){
                            if(is.empty(agentHistory)){
                              if(full){AH = tasklist} else {AH = tasklist %>% dplyr::filter(selected)}
                            } else{
                              if(full){AH = agentHistory} else {AH = agentHistory %>% dplyr::filter(selected)}
                            }
                            subset = AH$agent %>% unique
                            if(full){agent$full$ids <<- subset} else {agent$ids <<- subset}
                          }

                          if(full){return(agent$full$ids)} else {return(agent$ids)}
                        },

                        # Returns the processing time statistics of each activity(activity)
                        get.activity.summary.procTime = function(full = F, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          if(full){null = is.null(activity$summary$full$procTime)} else {null = is.null(activity$summary$procTime)}
                          if(null){
                            if(full){TL = tasklist} else {TL = tasklist %>% dplyr::filter(selected)}
                            TL %<>%
                              dplyr::group_by(activity) %>%
                              dplyr::summarise(Total = sum(procTime, na.rm = T), Average = mean(procTime, na.rm = T), Minimum = min(procTime, na.rm = T), Q1 = quantile(procTime, probs = 0.25, na.rm = T), Median = median(procTime, na.rm = T), Q3 = quantile(procTime, probs = 0.75, na.rm = T), Maximum = max(procTime, na.rm = T), 'SD' = sd(procTime, na.rm = T)) %>%
                              dplyr::arrange(desc(Total))

                            if(full){activity$summary$full$procTime <<- TL} else {activity$summary$procTime <<- TL}
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          if(full){return(activity$summary$full$procTime %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                          else    {return(activity$summary$procTime      %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                        },

                        # Returns the waiting time summary for each activity(activity)
                        get.activity.summary.idleTime = function(full = F, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          if(full){null = is.null(activity$full$summary$IdleTime)} else {null = is.null(activity$summary$IdleTime)}
                          if(null){
                            if(full){TL = eventlog} else {TL = eventlog %>% dplyr::filter(selected)}
                            TL %<>%
                              dplyr::mutate(duration = as.numeric(waitTime + delayTime)) %>% dplyr::group_by(activity) %>%
                              dplyr::summarise(Total = sum(duration, na.rm = T), Average = mean(duration, na.rm = T), Minimum = min(duration, na.rm = T), Q1 = quantile(duration, probs = 0.25, na.rm = T), Median = median(duration, na.rm = T), Q3 = quantile(duration, probs = 0.75, na.rm = T), Maximum = max(duration, na.rm = T), 'SD' = sd(duration, na.rm = T)) %>%
                              dplyr::arrange(desc(Total))

                            if(full){activity$full$summary$IdleTime <<- EL} else {activity$summary$IdleTime <<- EL}
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          if(full){return(activity$full$summary$IdleTime %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                          else    {return(activity$summary$IdleTime      %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                        },

                        # Returns the processing time statistics of each agent
                        get.agent.summary.procTime = function(full = F, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          if(full){null = is.null(agent$full$summary$procTime)} else {null = is.null(agent$summary$procTime)}
                          if(null){
                            if(full){AH = agentHistory} else {AH = agentHistory %>% dplyr::filter(selected)}
                            AH %<>% dplyr::filter(ntask > 0) %>%
                              dplyr::group_by(agent) %>%
                              dplyr::summarise(Total = sum(duration, na.rm = T), Average = mean(duration, na.rm = T), Minimum = min(duration, na.rm = T), Q1 = quantile(duration, probs = 0.25, na.rm = T), Median = median(duration, na.rm = T), Q3 = quantile(duration, probs = 0.75, na.rm = T), Maximum = max(duration, na.rm = T), 'SD' = sd(duration, na.rm = T)) %>%
                              dplyr::arrange(desc(Total))

                            if(full){agent$full$summary$procTime <<- AH} else {agent$summary$procTime <<- AH}
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          if(full){return(agent$full$summary$procTime %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                          else    {return(agent$summary$procTime      %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                        },

                        get.agent.summary.idleTime = function(full = F, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          if(full){null = is.null(agent$full$summary$idleTime)} else {null = is.null(agent$summary$idleTime)}
                          if(null){
                            if(is.empty(agentHistory)){AH = calculate.agent.summary.idleTime.fromTasklist(tasklist, full = full)} else {
                              if(full){AH = agentHistory} else {AH = agentHistory %>% dplyr::filter(selected)}
                              AH %<>% dplyr::filter(ntask == 0) %>%
                                dplyr::group_by(agent) %>%
                                dplyr::summarise(Total = sum(duration, na.rm = T), Average = mean(duration, na.rm = T), Minimum = min(duration, na.rm = T), Q1 = quantile(duration, probs = 0.25, na.rm = T), Median = median(duration, na.rm = T), Q3 = quantile(duration, probs = 0.75, na.rm = T), Maximum = max(duration, na.rm = T), 'SD' = sd(duration, na.rm = T)) %>%
                                dplyr::arrange(desc(Total))
                            }

                            if(full){agent$full$summary$idleTime <<- AH} else {agent$summary$idleTime <<- AH}
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          if(full){return(agent$full$summary$idleTime %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                          else    {return(agent$summary$idleTime      %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                        },

                        get.activity.volumeIn = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period == 'daily') # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(activity$full$timeseries$volin.daily)} else {null = is.null(activity$timeseries$volin.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = tasklist} else {hist = tasklist %>% filter(selected)}
                                     if(is.null(tasklist$arrDate)){tasklist$arrDate <<- tasklist$arrTime %>% setTZ('GMT') %>% as.Date}
                                     B.entr = hist %>% dplyr::group_by(arrDate, activity) %>% dplyr::summarise(length(caseID))

                                     names(B.entr) <- c('Date', 'activity', 'Entry')

                                     B.entr %<>% reshape2::dcast(Date ~ activity, sum, value.var = 'Entry')

                                     volin <- new('TS.DAILY', from = min(B.entr$Date), until = max(B.entr$Date))
                                     volin$feedData(B.entr, dateCol = 'Date')
                                     volin$data %<>% na2zero

                                     if(full) activity$full$timeseries$volin.daily <<- volin
                                     else      activity$timeseries$volin.daily     <<- volin
                                   }
                                   if(as_timeseries){if(full){return(activity$full$timeseries$volin.daily)} else {return(activity$timeseries$volin.daily)}}
                                   else             {if(full){return(activity$full$timeseries$volin.daily$data)} else {return(activity$timeseries$volin.daily$data)}}
                                 })
                        },

                        get.activity.volumeOut = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period == 'daily') # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(activity$full$timeseries$volout.daily)} else {null = is.null(activity$timeseries$volout.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = tasklist} else {hist = tasklist %>% filter(selected)}

                                     B.exit = hist %>% dplyr::group_by(compDate, activity)   %>% dplyr::summarise(length(caseID))

                                     names(B.exit) <- c('Date', 'activity', 'Exit')

                                     B.exit %<>% reshape2::dcast(Date ~ activity, sum, value.var = 'Exit')

                                     volout <- new('TS.DAILY', from = min(B.exit$Date), until = max(B.exit$Date))
                                     volout$feedData(B.exit, dateCol = 'Date')
                                     volout$data %<>% na2zero

                                     if(full) activity$full$timeseries$volout.daily <<- volout
                                     else      activity$timeseries$volout.daily     <<- volout
                                   }
                                   if(as_timeseries){if(full){return(activity$full$timeseries$volout.daily)} else {return(activity$timeseries$volout.daily)}}
                                   else             {if(full){return(activity$full$timeseries$volout.daily$data)} else {return(activity$timeseries$volout.daily$data)}}
                                 })
                        },

                        get.activity.backlog = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period == 'daily') # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(activity$full$timeseries$backlog.daily)} else {null = is.null(activity$timeseries$backlog.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)

                                     volout = get.activity.volumeOut(full = full) %>% as.data.frame %>% column2Rownames('date')
                                     volin  = get.activity.volumeIn(full = full) %>% as.data.frame %>% column2Rownames('date')

                                     dt = rownames(volout) %U% rownames(volin)
                                     st = colnames(volout) %^% colnames(volin)

                                     BL = (volin[dt, st] %>% as.matrix) - (volout[dt, st] %>% na2zero %>% as.matrix)

                                     BL <- BL %>% cumulative

                                     assert((sum(BL[nrow(BL),]) == 0) & (min(BL) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])

                                     BL %<>% as.data.frame %>% rownames2Column('Date')
                                     BL$Date %<>% as.Date

                                     backlog <- new('TS.DAILY', from = min(BL$Date), until = max(BL$Date))
                                     backlog$feedData(BL, dateCol = 'Date')

                                     if(full){
                                       activity$full$timeseries$backlog.daily <<- backlog
                                     } else{
                                       activity$timeseries$backlog.daily <<- backlog
                                     }

                                   }
                                   if(as_timeseries){if(full){return(activity$full$timeseries$backlog.daily)} else {return(activity$timeseries$backlog.daily)}}
                                   else             {if(full){return(activity$full$timeseries$backlog.daily$data)} else {return(activity$timeseries$backlog.daily$data)}}
                                 })
                        },


                        ### Filters:

                        filter.task.arrTime = function(from = NULL, until = NULL){
                          if(is.null(from)) {from  = min(tasklist$arrTime)}
                          if(is.null(until)){until = max(tasklist$arrTime) + 1}
                          tasklist$selected <<- (tasklist$arrTime >= from) & (tasklist$arrTime < until)
                          activity$summary     <<- list()
                          activity$timeseries  <<- list()
                          agent$ids     <<- character()
                          case$ids      <<- character()
                          activity$ids  <<- character()
                        },

                        ### Visualisations:

                        # Returns the processing time statistics of each agent-activity
                        get.agent.summary.activityProcTime = function(full = F, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          if(full){null = is.null(agent$full$summary$activityProcTime)} else {null = is.null(agent$summary$activityProcTime)}
                          if(null){
                            if(full){TL = tasklist} else {TL = tasklist %>% dplyr::filter(selected)}
                            TL %<>%
                              dplyr::group_by(agent, activity) %>%
                              dplyr::summarise(Total = sum(procTime, na.rm = T), Average = mean(procTime, na.rm = T), Minimum = min(procTime, na.rm = T), Q1 = quantile(procTime, probs = 0.25, na.rm = T), Median = median(procTime, na.rm = T), Q3 = quantile(procTime, probs = 0.75, na.rm = T), Maximum = max(procTime, na.rm = T), 'SD' = sd(procTime, na.rm = T)) %>%
                              dplyr::arrange(desc(Total))

                            if(full){agent$full$summary$activityProcTime <<- TL} else {agent$summary$activityProcTime <<- TL}
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          if(full){return(agent$full$summary$activityProcTime %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                          else    {return(agent$summary$activityProcTime      %>% dplyr::mutate(Total = k*Total, Average = k*Average, Minimum = k*Minimum, Q1 = k*Q1, Median = k*Median, Q3 = k*Q3, Maximum = k*Maximum, SD = k*SD))}
                        },

                        plot.activity.summary.procTime = function(plotter = 'plotly', time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), subset = NULL, n_trim = 12, horizontal = T){
                          if(is.null(subset)){subset = get.activity.ids()}
                          if(!is.null(n_trim)){
                            if(length(subset) > n_trim){
                              # SPCS = get.agent.summary.procTime() %>% filter(agent %in% subset) # SPCS: agent Processing Time Summary
                              # subset = SPCS$agent[sequence(n_trim)]
                              subset = subset[sequence(n_trim)]
                            }
                          }

                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]

                          # todo: make it faster by feeding summary to the plot rather than the whole dataset (add additional value to make mean value consistent if plot shows mean as well)
                          tasklist %>% filter(selected & (activity %in% subset)) %>%
                            viserPlot(y = chif(horizontal, 'activity', 'procTime'), x = chif(horizontal, 'procTime', 'activity'), type = 'box', plotter = plotter)
                        },

                        plot.agent.summary.procTime = function(plotter = 'plotly', time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), subset = NULL, n_trim = 12, horizontal = T){
                          if(is.null(subset)){subset = get.agent.ids()}
                          if(!is.null(n_trim)){
                            if(length(subset) > n_trim){
                              # SPCS = get.agent.summary.procTime() %>% filter(agent %in% subset) # SPCS: agent Processing Time Summary
                              # subset = SPCS$agent[sequence(n_trim)]
                              subset = subset[sequence(n_trim)]
                            }
                          }

                          # todo: make it faster by feeding summary to the plot rather than the whole dataset (add additional value to make mean value consistent if plot shows mean as well)
                          time_unit = match.arg(time_unit)
                          k = 1.0/timeUnitCoeff[time_unit]
                          agentHistory %>% filter(selected & (agent %in% subset) & ntask > 0) %>%
                            viserPlot(y = chif(horizontal, 'agent', 'duration'), x = chif(horizontal, 'duration', 'agent'), type = 'box', plotter = plotter)
                        }
                      )
)




