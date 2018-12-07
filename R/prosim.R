# prosim.R -----------------------------------------------

# Header
# Filename:      prosim.R
# Description:   This module provides functions for simulating the process including queue simulation and transition simulations
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    09 March 2018
# Last Revision: 21 June 2018
# Version:       0.0.5

# Version   Date               Action
# -----------------------------------
# 0.0.1     09 March 2018      Initial issue with function simQueue()
# 0.0.2     27 April 2018      Function simQueue() modified: Text progress bar added with argument show_process
# 0.0.3     27 April 2018      Function simQueue() modified: Generates precalculated arrays for process times and wrap times
# 0.0.4     02 May 2018        Function generateTasks() added
# 0.0.5     21 June 2018       Function generateTasks() modified: skips skills(task types) if they are empty



# This is a single queue-multi agent queue simulator:
#' @export
simQueue = function(taskEventLog, callEventLog, from, until, agents, settings, show_progress = T, show_details = F){
  # todo: add argument and setting verifications and defaults

  addCall = function(calls, id, time, task, agent, skill){
    calls[id, 'callID']   = id
    calls[id, 'callTime'] = time
    calls[id, 'taskID']   = task
    calls[id, 'agent']    = agent
    calls[id, 'skill']    = skill
    return(calls)
  }

  TL <- taskEventLog %>% nameColumns(
    columns = list(taskID = settings$taskEventLog$taskID_col, skill = settings$taskEventLog$skill_col, agent = settings$taskEventLog$agent_col, priority = settings$taskEventLog$priority_col, arrTime = settings$taskEventLog$arrivalTime_col, startTime = settings$taskEventLog$startTime_col, compTime = settings$taskEventLog$completedTime_col),
    classes = list(taskID = 'character', skill = 'character', agent = 'character', priority = 'numeric', arrTime = 'POSIXct', startTime = 'POSIXct', compTime = 'POSIXct')) %>% column2Rownames('taskID', remove = F)

  GNC <- callEventLog %>% nameColumns(
    columns = list(callID = settings$callEventLog$callID_col, agent = settings$callEventLog$agent_col, taskID = settings$callEventLog$taskID_col, skill = settings$callEventLog$skill_col, callTime = settings$callEventLog$callTime_col, compTime = settings$callEventLog$completedTime_col),
    classes = list(callID = 'character', agent = 'character', taskID = 'character', skill = 'character', callTime = 'POSIXct', compTime = 'POSIXct')) %>% filter(agent %in% agents) %>% column2Rownames('callID', remove = F)
  # Assumptions:
  # 1- All agents are available and productive 100% of the simulation time. todo: scheduled time can be given as an input table
  # 2- The simulator does not generate task arrivals, so does not require arrival rate! All task arrivals should be given in the input table taskEventLog

  agents = rownames(settings$agentSkillAPT) %^% rownames(settings$agentSkillAWT)
  skills = colnames(settings$agentSkillAPT) %^% colnames(settings$agentSkillAWT)

  settings$agentSkillAPT = settings$agentSkillAPT[agents, skills, drop = F]
  settings$agentSkillAWT = settings$agentSkillAWT[agents, skills, drop = F]

  skills = skills[which(colSums(settings$agentSkillAPT, na.rm = T) > 0) %^% which(colSums(settings$agentSkillAWT, na.rm = T) > 0)]

  settings$agentSkillAPT = settings$agentSkillAPT[agents, skills, drop = F]
  settings$agentSkillAWT = settings$agentSkillAWT[agents, skills, drop = F]

  settings$agentSkillAPT[settings$agentSkillAPT < settings$minAPT] <- settings$minAPT
  settings$agentSkillAWT[settings$agentSkillAWT < settings$minAWT] <- settings$minAWT

  procRate = 1.0/(settings$agentSkillAPT*60)
  wrapRate = 1.0/(settings$agentSkillAWT*60)

  # Agent Aaverage Wrap Rate:
  AAWR = wrapRate %>% rowMeans(na.rm = T)


  Na = length(agents)
  Ns = length(skills)
  N  = Na*Ns*1000

  fpt = numeric(N) %>% array(dim = c(Na, Ns, 1000), dimnames = list(agents, skills, 1:1000))
  fwt = fpt
  awt = numeric(Na*1000) %>% array(dim = c(Na, 1000), dimnames = list(agents, 1:1000))

  for(i in agents){
    for(j in skills[which(!is.na(procRate[i, ]))]){
      fpt[i,j, ] <- rexp(1000, rate = procRate[i, j])
      fwt[i,j, ] <- rexp(1000, rate = wrapRate[i, j])
    }
    awt[i,] <- rexp(1000, rate = AAWR[i])
  }
  smpl = sample(1:1000, 10000, replace = T)

  simend   = verify(until, 'POSIXct', lengths = 1, null_allowed = F)
  # now      = min(TL$arrTime[is.na(TL$startTime)]) could serve as default value for argument 'from'

  cntr = 1
  # If there is no call event triggered, each agent must trigger one:
  for(e in agents){
    if(GNC %>% filter(agent == e) %>% filter(is.na(taskID)) %>% nrow < 1){
      nextCallTime = from + awt[e, smpl[cntr]]
      cntr = cntr + 1
      if(nextCallTime < simend){GNC %<>% addCall(id = e %>% paste(1, sep = '.'), time = nextCallTime, agent = e, skill = NA, task = NA)}
    }
  }

  if(show_progress){pb = txtProgressBar(min = as.numeric(from), max = as.numeric(simend), style = 3)}
  while((GNC$taskID %>% is.na %>% sum) > 0){
    if(cntr > 9990){smpl = sample(1:1000, 10000, replace = T);cntr = 1}

    cl = GNC[which(is.na(GNC$taskID)),] %>% filter(callTime == min(callTime))
    for (i in cl$callID){
      e = GNC[i, 'agent']
      if(show_progress){setTxtProgressBar(pb, as.numeric(GNC[i, 'callTime']))}
      if(show_details){
        cat('Agent: ', e, ' Called at: ', as.character(GNC[i, 'callTime']), ',')
      }
      # What skills does this agent have?
      skls = colnames(procTime)[which(!is.na(procTime[e, ]))]
      tl = TL %>% filter(is.na(agent)) %>% filter(arrTime < GNC[i, 'callTime']) %>% filter(skill %in% skls) %>% filter(arrTime == min(arrTime))
      if(nrow(tl) > 0){
        TL[tl$taskID[1], 'agent'] <- e
        GNC[i, 'taskID'] <- tl[1, 'taskID']
        GNC[i, 'skill']  <- tl[1, 'skill']
        GNC[i, 'compTime'] <- GNC[i, 'callTime'] + fpt[e, tl[1, 'skill'], smpl[cntr]]

        if(show_details){
          cat(' Completed at: ', as.character(GNC[i, 'compTime']), ',')
        }

        cntr = cntr + 1;
        TL[tl$taskID[1], 'startTime'] <- GNC[i, 'callTime']
        TL[tl$taskID[1], 'compTime']  <- GNC[i, 'compTime']
        # Add a row to the getNext call eventlog:
        nextCallTime = TL[tl$taskID[1], 'compTime'] + fwt[e, tl[1, 'skill'], smpl[cntr]]

        if(show_details){
          cat(' Next Call at: ', as.character(nextCallTime), '\n')
        }

        cntr = cntr + 1;

        if(nextCallTime < simend){
          nCalls    = sum(GNC$agent == e & !is.na(GNC$taskID))
          GNC %<>% addCall(id = e %>% paste(nCalls + 1, sep = '.'), agent = e, time = nextCallTime, task = NA, skill = NA)
        }
      } else {
        # If by the time an agent calls getNext and no unallocated task matching his/her skills are found, there are two possible scenarios:
        # 1: call getNext again after a randomly generated time with rate wrapRate. A new row is added with skill = 'idle' for the unsuccessful getNext call
        # 2: the next task matching skills arriving will be immidiately allocated to the agent when arrived
        # Currently we model option 1
        nIdleCalls    = sum(GNC$agent == e & GNC$skill == 'idle', na.rm = T)
        GNC[i, 'taskID'] <- e %>% paste('idle', nIdleCalls + 1, sep = '.')
        GNC[i, 'skill']  <- 'idle'
        nextCallTime = GNC[i, 'callTime'] + awt[e, smpl[cntr]]
        cntr = cntr + 1

        if(show_details){
          cat(' No tasks found! Next Call at: ', as.character(nextCallTime), '\n')
        }

        GNC[i, 'compTime'] <- nextCallTime

        if(nextCallTime < simend){
          nCalls    = sum(GNC$agent == e & !is.na(GNC$taskID))
          GNC %<>% addCall(id = e %>% paste(nCalls + 1, sep = '.'), agent = e, time = nextCallTime, task = NA, skill = NA)
        }
      }
    }
  }

  if(show_progress){setTxtProgressBar(pb, as.numeric(simend));close(pb)}

  output = list(taskEventLog = TL %>% arrange(arrTime), callEventLog = GNC %>% arrange(callTime))

  return(output)
}



# This function simulates arrival of tasks and generates a tasklist
# from: start time (Date)
# to: end time  (Date)
# rate: arrival rate in (tasks per hour) must be a named numeric vector where names specify task types or skills
#' @export
generateTasks = function(tasklist, from, to, rate){
  maxcount = as.integer(2*difftime(to, from, units = 'hour')*rate)
  skills   = names(rate)
  names(maxcount) <- skills

  for (sk in skills){
    if(!is.empty(sk)){
      TL = data.frame(intrArrival = rexp(n = maxcount[sk], rate = rate[sk]/3600.0), taskID = sk %>% paste(1:maxcount[sk], sep = '.'), skill = sk, queue = NA, agent = NA, priority = NA, startTime = as.POSIXct(NA), compTime = as.POSIXct(NA), stringsAsFactors = F) %>% mutate(arrTime = from + cumulative(intrArrival)) %>% filter(arrTime < to)
      tasklist %<>% rbind(TL)
    }
  }
  return(tasklist)
}

