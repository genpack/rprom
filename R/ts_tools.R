# tstools.R -------------------------------------------------------------------------

# Header
# Filename:       tstools.R
# Description:    Contains utility functions for process modelling.
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     06 July 2018
# Last Revision:  05 April 2022
# Version:        0.1.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     06 July 2018      Initial issue
# 0.0.2     16 October 2018   Function buildProcessMapPackage() modified: argument linkLabel added
# 0.0.7     08 August 2019    Functions markovchain_transition_classifier(), markovchain_transition_time_estimator(), gen_next_events(), gen_transition_times() , gen_transition_times_exp()  modified: argument linkLabel added
# 0.0.8     09 September 2019 Functions gen_next_events() modified: grouping should be done before generating a random value.
# 0.0.9     28 March 2022     Functions markovchain_transition_time_estimator() modified: function difftime() with units set as secs used for computing time difference between current_time and transition time.
# 0.1.0     05 April 2022     Functions gen_next_events() and gen_transition_times() removed and embedded in markovchain_transition_classifier() and markovchain_transition_time_estimator() respectively.
# 0.1.1     04 May 2022       Bug fixed in function markovchain_transition_time_estimator(): normal dist, were generating negative durations

buildProcessMapPackage = function(obj, nodeSize = 'totalEntryFreq', nodeColor = 'totalEntryFreq', linkColor = 'totalFreq', linkWidth = 'totalFreq', linkLabel = NULL, linkLength = NULL, config = NULL){
  # todo: build tooltip, link label
  obj %>% verify('list', names_identical = c('nodes', 'links'), null_allowed = F)

  nodes = obj$tables$nodes
  links = obj$tables$links

  # if(is.empty(nodes)){return(NULL)}
  if(!inherits(nodeSize, c('numeric', 'integer'))){
    nodeSize  %<>% verify('character', lengths = 1, domain = numerics(nodes), default = 100)
  }

  nodeColor %<>% verify('character', lengths = 1, default = 'lightblue')
  if(nodeColor %in% names(nodes)){nodes %<>% mutate_(color = nodeColor)} else {nodes %<>% mutate(color = nodeColor)}

  linkColor %<>% verify('character', lengths = 1, default = 'blue')
  if(linkColor %in% names(links)){links %<>% mutate_(color = linkColor)} else {links %<>% mutate(color = linkColor)}

  nodes %<>% mutate_(size = nodeSize, tooltip = 'status', label = 'status') %>%
    mutate(shape = ifelse(status %in% c('CASE START', 'CASE END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle'))) %>%
    select(ID = status, label, size, shape, color, tooltip) %>% na2zero

  if(nodeSize == 'totalEntryFreq'){nodes$size[nodes$label == 'CASE START'] <- nodes$size[nodes$label == 'CASE END'] %>% sum}

  if(!inherits(linkWidth, c('numeric', 'integer'))){
    linkWidth  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  if(!inherits(linkLength, c('numeric', 'integer'))){
    linkLength  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  links %<>% mutate_(width = linkWidth, length = linkLength, label = linkLabel) %>%
    select(source = status, target = nextStatus, color, length, width, label) %>% na2zero

  cfg = config %<==>%
    list(link.width.max = 5, link.width.min = 1, link.smooth = list(enabled = T, type = 'curvedCCW'),
         node.label.size = 25, node.physics.enabled = T, layout = 'hierarchical', direction = 'up.down')

  return(list(data = list(nodes = nodes, links = links), config = cfg))
}

buildTreeTable = function(traces){
  bindrow = function(df, rn, row){
    df[rn, sequence(length(row))] <- row
    return(df)
  }
  aa = traces %>% pull(path) %>% strsplit('-')
  df = data.frame.na(nrow = length(aa), ncol = aa %>% sapply(length) %>% max)
  for (i in sequence(length(aa))){
    df %<>% bindrow(i, aa[[i]])
  }
  colnames(df) <- 'Step' %>% paste(sequence(ncol(df)), sep = '.')
  rownames(df) <- traces$variation
  df$freq      <- traces$freq
  return(df)
}


#' @title Target Generator for Markov-chain Random Walk Simulation 
#' @description This function is a simple transition probability model based on memory-less markov-chain model.
#' It randomly picks destination for each given transition respecting 
#' the probability distribution of each transition target.
#' Transition probability distribution are extracted from given argument \code{histobj}.
#' This is the default target generator engine for the transition system monte-carlo simulation.
#' The \code{TransitionSystem} simulator engine calls this function multiple times during the sumulation.
#'
#' @param histobj object of class \code{TransitionSystem} containing history of all transitions upto the current time \code{current_time}
#' @param input dataframe containing transitions for which a transition end time needs to be generated
#' @param current_time POSIXct containing the current time. All transition end times will be after the specified current time.
#' @return dataframe with additional column \code{nextStatus}. 
#' @export
markovchain_transition_classifier = function(histobj, input, ...){
  input %>% 
    left_join(histobj$get.transition_probabilities(), by = "status") %>% 
    group_by(caseID, status) %>% 
    mutate(rand_var = runif(1)) %>%
    filter(rand_var < cum_prob) %>%
    filter(cum_prob == min(cum_prob)) %>%
    ungroup() %>%
    select(caseID, status, nextStatus, startTime)
}


#' @title Transition Time Generator for Markov-chanin Random Walk Simulation 
#' @description This function generates random transition time (status durations) 
#' based on the average observed transition times
#' (given by argument \code{histobj}). 
#' MarkovChain transition time generator assumes transition times have normal distribution.
#' This is the default time generator engine for the transition system monte-carlo simulation.
#'
#' @param histobj object of class \code{TransitionSystem} containing history of all transitions upto the current time \code{current_time}
#' @param input dataframe containing transitions for which a transition end time needs to be generated
#' @param current_time POSIXct containing the current time. All transition end times will be after the specified current time.
#' @return dataframe with additional column \code{pred_duration} in seconds. 
#' @export
markovchain_transition_time_estimator = function(
  histobj, input, current_time,
  family = c('normal', 'exponential', 'exp', 'gaussian', 'norm'), ...){
  
  ## todo: add more distributions
  ## todo: add auto-detect distributiion functionality (future major version realeases)
  
  family %<>% tolower
  family = match.arg(family)
  
  transitions <- histobj$get.links()
  
  transition_durations <- transitions %>% 
    select(status, nextStatus, meanTime, sdTime) %>%
    arrange(status, nextStatus) %>%
    na2zero
  
  input %<>% left_join(transition_durations, by = c("status", "nextStatus")) %>% na.omit
  difftime(current_time, input$startTime, units = 'secs') %>% 
    as.numeric %>% {.[.<0]<-0;.} -> durationLowerBound
  
  if(family %in% c('normal', 'nprm', 'gaussian')){
    input %>% mutate(pred_duration = gen.random.highpass(N = n(), family = 'normal', mean = meanTime, sd = sdTime, x0 = durationLowerBound))
  } else if (family %in% c('exp', 'exponential')){
    input %>% mutate(pred_duration = gen.random.highpass(N = n(), family = 'exp', rate = 1.0/meanTime, x0 = durationLowerBound))
  } else {
    stop('Unknown family %s!' %>% sprintf(family))
  }
}


