# Header
# Filename:       promtools.R
# Description:    Contains utility functions for process modelling.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     06 July 2018
# Last Revision:  31 July 2018
# Version:        0.1.4
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     06 July 2018      Initial issue
# 0.1.1     17 July 2018      Function eventlog2Tasklist() added
# 0.1.2     17 July 2018      Function eventlog2AgentHistory() added
# 0.1.3     17 July 2018      Function agentCorrect() modified: For each agent, fills missing nextTime of the last event with argument maxTime.
# 0.1.4     31 July 2018      Function eventlog2Tasklist() modified: Faster algorithm to compute durations


taskCorrect = function(tbl, silent = T){

  tbl %<>% arrange(time)

  # Step 3: Check if multiple caseIDs or taskTypes come with one taskID:
  cid = unique(tbl$caseID);
  ttp = unique(tbl$activity);
  if(length(cid) > 1 | length(ttp) > 1){
    if(!silent){
      warnif(length(cid) > 1, 'Multiple case IDs observed for taskID: ' %++% tbl$taskID[1] %++% '! Corrected by removing less frequent case IDs.');
      warnif(length(ttp) > 1, 'Multiple activities observed for taskID:   ' %++% tbl$taskID[1] %++% '! Corrected by removing less frequent activities.');
    }
    tbl %<>% filter(caseID == most.common(caseID), activity == most.common(activity))
  }

  cnt = nrow(tbl)
  # Step 4: Check all tasks begin with an arrived event type
  w = which(tbl$type == 'arrived')
  if(length(w) > 1){tbl = tbl[- w[-1], ]} #todo: add warning deleted extra arriveds and keeps the earliest
  if(length(w) > 0){
    if(w[1] != 1){
      # First arrived event is not the first event
      tbl$time[w[1]] <- tbl$time[1] - 5
    } # else arrived is the first event so it's Ok :-)
  } else {
    # No arrived event exists
    cnt = cnt + 1
    tbl[cnt, ]       <- tbl[1, ]
    tbl$time[cnt]    <- tbl$time[cnt] - 5
    tbl$type[cnt]    <- 'arrived'
    tbl$eventID[cnt] %<>% paste('Arrived', sep = '.')
    tbl$agent[cnt]   <- NA
  }

  # Step 5: Check all tasks end with a complete event type
  tbl %<>% arrange(time)
  cnt = nrow(tbl)
  w   = which(tbl$type == 'completed')
  N   = length(w)
  if(N > 1){tbl = tbl[- w[-N], ]} #todo: add warning deleted extra arriveds and keeps the latest
  if(N > 0){
    if(w[N] != cnt){
      # Last completed event is not the last event
      tbl$time[w[N]] <- tbl$time[cnt] + 5
    } # else completed is the last event so it's Ok :-)
  } else {
    # No completed event exists
    cnt = cnt + 1
    tbl[cnt, ]       <- tbl[cnt - 1, ]
    tbl$time[cnt]    <- tbl$time[cnt - 1] + 5
    tbl$type[cnt]    <- 'completed'
    tbl$eventID[cnt] %<>% paste('Completed', sep = '.')
    tbl$agent[cnt]   <- tbl$agent[cnt - 1]
  }

  # Check until here:
  tbl %<>% arrange(time)
  cnt = nrow(tbl)
  assert(tbl$type[1] == 'arrived')
  assert(sum(tbl$type == 'arrived', na.rm = T) == 1)
  assert(tbl$type[cnt] == 'completed')
  assert(sum(tbl$type == 'completed', na.rm = T) == 1)

  tbl %<>% mutate(nextType = type, nextAgent = agent) %>%
    column.shift.up('nextType' , keep.rows = T) %>%
    column.shift.up('nextAgent', keep.rows = T)

  # if(tbl$type[2] != 'started'){
  #   cnt = cnt + 1
  #   tbl[cnt, ]       <- tbl[2, ]
  #   tbl$time[cnt]    <- tbl$time[2] - 1
  #   tbl$eventID[cnt] <- tbl$taskID[2] %>% paste('StartedAfterArrival', sep = '.')
  # }
  # tbl %<>% arrange(time)
  # cnt = nrow(tbl)
  return(tbl)
}

correctEventlog = function(dataset, silent = T){
  # Step 3: Check if multiple caseIDs or taskTypes come with one taskID:
  cat('Correcting inconsistencies ...', '\n')
  dataset %<>% dplyr::group_by(taskID) %>%
    do(taskCorrect(., silent = silent)) %>% dplyr::ungroup()

  # dataset %>%
  #   dplyr::group_by(taskID) %>%
  #   dplyr::summarise(caseID = first(caseID), initType = first(type), lastType = last(type), activity = first(activity), agent = first(agent), initTime = min(time), endTime = max(time)) %>%
  #   dplyr::ungroup() -> tasklist
  #
  #
  # #
  # # w = which(tasklist$dplCID > 0)
  # # assert(length(w) == 0, as.character(length(w)) %++% ' events are associated with multiple case IDs!')
  # # todo: make it a warning with correction
  #
  # # Step 4: Check all tasks begin with an arrival event type
  # M = 0
  # w = which(tasklist$initType != 'arrived')
  # N = length(w)
  # warnif(N > 0, as.character(N) %++% ' events did not start with arrival. Corrected by creating arrival events at the beginning.')
  # if(N > 0){
  #   aa = data.frame(eventID = 'Correction' %>% paste((M+1):(M+N), sep = '.'), taskID = tasklist$taskID[w], caseID = tasklist$caseID[w], time = tasklist$initTime[w] - 1, type = 'arrived', activity = tasklist$activity[w], agent = NA, stringsAsFactors = F)
  #   dataset %<>% bind_rows(aa)
  #   M = M + N
  # }
  #
  # # Step 5: Check all tasks end with a complete event type
  # w = which(tasklist$lastType != 'completed')
  # N = length(w)
  # warnif(N > 0, as.character(N) %++% ' events did not end with completion. Corrected by creating completed events at the end.')
  # if(N > 0){
  #   aa = data.frame(eventID = 'Correction' %>% paste((M+1):(M+N), sep = '.'), taskID = tasklist$taskID[w], caseID = tasklist$caseID[w], time = tasklist$endTime[w] + 1 , type = 'completed', activity = tasklist$activity[w], agent = tasklist$agent[w], stringsAsFactors = F)
  #   dataset %<>% bind_rows(aa)
  #   M = M + N
  # }

  # # Step 6: Make sure arrival and complete events are unique
  # dataset %<>% dplyr::group_by(taskID) %>%
  #   do(arrange(., time) %>% mutate(nextType = type, nextAgent = agent) %>%
  #        column.shift.up('nextType' , keep.rows = T) %>%
  #        column.shift.up('nextAgent', keep.rows = T) %>%
  #        taskCorrect2(typeTag = 'completed') %>%
  #        taskCorrect2(typeTag = 'arrived')) %>%
  #   ungroup()

  # Step 7: Check all arrival events follow a start event
  w = which(dataset$type == 'arrived' & dataset$nextType != 'started') # check for speed compare with filter
  N = length(w)
  M = 0
  if(!silent){
    warnif(N > 0, as.character(N) %++% ' events did not face a start event after arrival! Corrected by creating start events after arrival.')
  }
  if(N > 0){
    add                 <- dataset[w,] %>% mutate(eventID = paste('Correction', (M+1):(M+N), sep = '.'), type = 'started', agent = nextAgent, time = time + 1)
    dataset$nextType[w] <- 'started'
    dataset %<>% rbind(add)
    M = M + N
  }

  # Step 8: Check all start events follow either a complete or suspend event
  w = which(dataset$type == 'started' & !(dataset$nextType %in% c('completed', 'suspended')))
  # This must be started followed by a started event, so we delete it:
  assert(sum(dataset$nextType[w] != 'started') == 0, "I should not be here!")
  N = length(w)
  if(!silent){
    warnif(N > 0, as.character(N) %++% ' events faced a start event after start! Corrected by deleting the former start event.')
  }
  if(N > 0){
    dataset <- dataset[-w, ]
  }

  # Step 9: Check all suspend events follow a start event
  w = which(dataset$type == 'suspended' & dataset$nextType != 'started')
  N = length(w)
  if(!silent){
    warnif(N > 0, as.character(N) %++% ' events did not face a start event after suspension! Corrected by creating start events after suspension.')
  }
  if(N > 0){
    add                 <- dataset[w,] %>% mutate(eventID = paste('Correction', (M+1):(M+N), sep = '.'), type = 'started', agent = nextAgent, time = time + 1)
    dataset$nextType[w] <- 'started'
    dataset %<>% rbind(add)
  }

  dataset %<>% arrange(taskID, time)

  w = which(dataset$type == 'started')
  # assert(dataset$agent[w + 1] == dataset$nextAgent[w])

  # Step 10: Check each start is completed or suspended by the same person
  w = which(dataset$type == 'started' & dataset$agent != dataset$nextAgent)
  N = length(w)
  if(!silent){
    warnif(N > 0, N %++% ' events started by an agent and completed or suspended by a different agent! Corrected by changing starter agent by the completer or suspender one.')
  }
  if(N > 0){
    dataset$agent[w] <- dataset$nextAgent[w]
  }
  cat('Correcting inconsistencies completed successfully!', '\n')
  return(dataset)
}

taskCorrect.old = function(tbl){
  cid = unique(tbl$caseID);
  ttp = unique(tbl$activity);
  warnif(length(cid) > 1, 'Multiple case IDs observed for taskID: ' %++% tbl$taskID[1] %++% '! Corrected by removing less frequent case IDs.');
  warnif(length(ttp) > 1, 'Multiple activities observed for taskID:   ' %++% tbl$taskID[1] %++% '! Corrected by removing less frequent activities.');
  filter(tbl, caseID == most.common(caseID), activity == most.common(activity))
}

# taskCorrect2 = function(tbl, typeTag = 'completed'){
#   sm  = (tbl$type == typeTag)
#   nsm = sum(sm, na.rm = T)
#   if(nsm > 1){
#     warnif(nsm > 1, as.character(nsm) %++% typeTag %++% ' event types observed for taskID: ' %++% tbl$taskID[1])
#     tbl = rbind(tbl[!sm, ], tbl[sm, ][chif(typeTag == 'completed', nsm, 1), ])
#   }
#   return(tbl)
# }
#

agentCorrect = function(tbl, maxTime){
  impactval = c(started = 1, completed = -1, suspended = -1) #todo: will add more event types for agenst
  tbl %<>% mutate(impact = impactval[type]) %>% na.omit %>%
    mutate(ntask = cumulative(impact)) %>%
    mutate(nextTime = time, index = sequence(nrow(.)), ntask = ntask - min(0, min(ntask))) %>%
    column.shift.up('nextTime', keep.rows = T)
  tbl$nextTime[is.na(tbl$nextTime)] <- maxTime
  tbl %>%
    mutate(gap = difftime(nextTime , time, units = 'secs') %>% as.numeric) %>% mutate(duration = ifelse(ntask == 0, gap, gap/ntask))
}

eventlog2Tasklist = function(el, silent = T){
  cat('Creating task list ...', '\n')
  el %<>% dplyr::mutate(nextTime = time) %>%
    dplyr::arrange(taskID, time) %>% column.shift.up('nextTime', keep.rows = T)

  dp = which(!duplicated(el$taskID))

  endindx = c(dp[-1] - 1, nrow(el))
  el[endindx, 'nextTime'] = NA

  el %<>% mutate(duration = nextTime - time)
  el1 = el %>% reshape2::dcast(taskID ~ type, value.var = 'duration', fun.aggregate = sum) %>% select(taskID, waitTime = arrived, procTime = started, delayTime = suspended)
  el2 = el %>% reshape2::dcast(taskID ~ type, value.var = 'time', fun.aggregate = last) %>%
    mutate(arrTime = as.POSIXct(arrived, origin = '1970-01-01'), startTime = as.POSIXct(started, origin = '1970-01-01'), compTime = as.POSIXct(completed, origin = '1970-01-01')) %>%
    select(taskID, arrTime, startTime, compTime)
  el %<>% dplyr::group_by(taskID) %>%
    dplyr::summarise(caseID = caseID[1], activity = activity[1], numTouch = sum(type == 'started', na.rm = T), agent = agent[type == 'completed'][1], caseStarted = caseStarted[1], caseEnded = caseEnded[1]) %>%
    dplyr::inner_join(el2, by = 'taskID') %>%
    dplyr::inner_join(el1, by = 'taskID') %>%
    dplyr::mutate(selected = T) %>%
    dplyr::mutate(arrDate = arrTime %>% setTZ('GMT') %>% as.Date, compDate = compTime %>% setTZ('GMT') %>% as.Date) %>%
    select(caseID, taskID, activity, agent, numTouch, arrTime, compTime, waitTime, procTime, delayTime, arrDate, compDate, caseStarted, caseEnded, selected) %>%
    arrange(caseID, taskID, compTime)
  cat('Task list successfully created!', '\n')
  return(el)
}

eventlog2AgentHistory = function(el, silent = T){
  cat('Creating agent history ...', '\n')
  maxTime = max(el$time)
  el %<>% dplyr::select(eventID, taskID, caseID, time, activity, agent, type) %>%
    dplyr::filter(!is.na(agent)) %>% dplyr::arrange(agent, time) %>% dplyr::mutate(index = sequence(nrow(.))) %>% dplyr::group_by(agent) %>%
    do(agentCorrect(., maxTime = maxTime)) %>% mutate(selected = T)
  cat('Agent history created successfully!', '\n')
  return(el)
}

find_prev = function(tbl){
  tbl %<>% column.shift.down(c('prevActivity', 'prevType', 'prevTaskID'), keep.rows = T)
  tbl$prevActivity[1] = 'ENTER'
  tbl$prevType[1]  = 'completed'

  w = which(tbl$type == 'arrived' & tbl$prevType == 'arrived')
  for(i in w){
    ww = which(tbl$type[sequence(i)] == 'completed') %>% last
    tbl$prevType[i]   <- 'completed'
    tbl$prevActivity[i]  <- tbl$activity[ww]
    tbl$prevTaskID[i] <- tbl$taskID[ww]
  }
  return(tbl %>% filter(type == 'arrived' & prevType == 'completed'))
}

find_next = function(tbl){
  tbl %<>% column.shift.up(c('nextActivity', 'nextType', 'nextTaskID', 'nextTime'), keep.rows = T)

  N = nrow(tbl)
  tbl$nextActivity[N] <- 'EXIT'
  tbl$nextType[N]  <- 'arrived'

  w = which(tbl$type == 'completed' & tbl$nextType == 'completed')
  for(i in w){
    ww = which(tbl$type[sequence(N) %-% sequence(i)] == 'arrived') %>% last
    tbl$nextType[i]     <- 'arrived'
    tbl$nextActivity[i]    <- tbl$activity[i + ww]
    tbl$nextTaskID[i]   <- tbl$taskID[i + ww]
    tbl$nextTime[i]     <- tbl$time[i + ww]
  }
  return(tbl %>% dplyr::filter(type == 'completed' & nextType == 'arrived'))
}

# Returns the idle time summary for each agent from tasklist
calculate.agent.summary.idleTime.fromTasklist = function(tasklist, full = F){
  if(!full){tasklist %<>% filter(selected)}
  if(is.null(tasklist$agentPrevCompTime)){
    cat('Computing agent previous completion time ...')
    tasklist %<>% dplyr::mutate(compDate = compTime %>% as.Date(tz = attr(compTime, "tzone")), agentPrevCompTime = compTime) %>% dplyr::group_by(agent) %>%
      do(.[order(.$compTime),] %>% column.shift.down('agentPrevCompTime', keep.rows = T)) %>% dplyr::ungroup()
    cat(' Done!', '\n')
  }

  w  = which(tasklist$agentPrevCompDate != tasklist$compDate | is.na(tasklist$agentPrevCompTime))
  t8 = tasklist$compDate[w] %>% as.character %>% paste('08:00:00') %>% as.POSIXct
  # todo: This will change when function diffTime in rutils can handle non-working hours
  w2 = which(tasklist$startTime[w] < t8)

  tasklist$agentPrevCompTime[w] = t8
  tasklist$agentPrevCompTime[w][w2] = tasklist$startTime[w][w2]

  tasklist %>%
    dplyr::mutate(duration = as.numeric(startTime - agentPrevCompTime)) %>% dplyr::group_by(agent) %>%
    dplyr::summarise(Total = sum(duration, na.rm = T), Average = mean(duration, na.rm = T), Minimum = min(duration, na.rm = T), Q1 = quantile(duration, probs = 0.25, na.rm = T), Median = median(duration, na.rm = T), Q3 = quantile(duration, probs = 0.75, na.rm = T), Maximum = max(duration, na.rm = T), 'SD' = sd(duration, na.rm = T)) %>%
    dplyr::arrange(desc(Total))
}

# Given the eventlog and tasklist, this function attaches two columns to the tasklist:
# The previous and next activity to each task.
# For multiple tasks starting at the same time, the task with the latest completion time is selected as the previous task to the task in question
# and for multiple parallel tasks joining into one task, it attaches the next task as the task with the ealiest arrival time after the completion time of the task in question
attachPrevNext = function(tl, el){
  nxt = el %>%
    dplyr::select(caseID, taskID, time, activity, type) %>%
    dplyr::filter(type %in% c('arrived', 'completed')) %>%
    dplyr::arrange(caseID, time) %>%
    dplyr::mutate(nextActivity = activity, nextType = type, nextTaskID = taskID, nextTime = time) %>%
    dplyr::group_by(caseID) %>%
    do(find_next(.)) %>% ungroup %>% select(taskID, nextActivity, nextTaskID)

  prv = el %>%
    dplyr::select(caseID, taskID, time, activity, type) %>%
    dplyr::filter(type %in% c('arrived', 'completed')) %>%
    dplyr::arrange(caseID, time) %>%
    dplyr::mutate(prevActivity = activity, prevType = type, prevTaskID = taskID, prevTime = time) %>%
    dplyr::group_by(caseID) %>%
    do(find_prev(.)) %>% ungroup %>% select(taskID, prevActivity, prevTaskID)

  tl %>% inner_join(prv, by = 'taskID') %>% inner_join(nxt, by = 'taskID')
}

# One agent (employee) can only be in one team. This function verifies that one employee is in one and only one team.
verifyTeams = function(){

}




