# tstools.R -------------------------------------------------------------------------

# Header
# Filename:       tstools.R
# Description:    Contains utility functions for process modelling.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     06 July 2018
# Last Revision:  09 September 2019
# Version:        0.0.8
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     06 July 2018      Initial issue
# 0.0.2     16 October 2018   Function buildProcessMapPackage() modified: argument linkLabel added
# 0.0.7     08 August 2019    Functions markovchain_transition_classifier(), markovchain_transition_time_estimator(), gen_next_events(), gen_transition_times() , gen_transition_times_exp()  modified: argument linkLabel added
# 0.0.8     09 September 2019 Functions gen_next_events() modified: grouping should be done before generating a random value.

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


# simple transition probability model based on memory-less markov-chain model
markovchain_transition_classifier = function(histobj, input, ...){
  events      <- histobj$get.nodes()
  # transitions <- histobj$get.links()
  # 
  # transition_probabilities <- transitions %>% 
  #   select(status, nextStatus, totalFreq) %>%
  #   arrange(status, nextStatus) %>%
  #   group_by(status) %>%
  #   mutate(cum_freq = cumsum(totalFreq)) %>%
  #   mutate(cum_prob = cum_freq/sum(totalFreq)) %>%
  #   ungroup() %>%
  #   select(status, nextStatus, cum_prob)
  
  input %>% left_join(histobj$get.transition_probabilities(), by = "status")
}

markovchain_transition_time_estimator = function(histobj, input, start_dt, ...){
  transitions <- histobj$get.links()
  
  transition_durations <- transitions %>% 
    select(status, nextStatus, meanTime, sdTime) %>%
    arrange(status, nextStatus) %>%
    na2zero
  
  input %>% left_join(transition_durations, by = c("status", "nextStatus")) %>% na.omit %>%
    mutate(pred_duration = gen.random.cond(N = n(), family = 'normal', mean = meanTime, sd = sdTime, x0 = as.numeric(start_dt - startTime)))
}

# Default next events generator
gen_next_events <- function(input, histobj, transition_classifier = markovchain_transition_classifier, ...) {
  transition_classifier(histobj = histobj, input = input, ...) %>% 
    group_by(caseID, status) %>% 
    mutate(rand_var = runif(1)) %>%
    filter(rand_var < cum_prob) %>%
    filter(cum_prob == min(cum_prob)) %>%
    ungroup() %>%
    select(caseID, status, nextStatus, startTime)
}

## Argument 'transition_time_estimator' must be class function. This function should have three and only three inputs:
# 'histobj': an object of class TRANSYS. 'histobj' will be passed to this function directly
# 
# Default next transition time generator
gen_transition_times = function(input, histobj, start_dt, transition_time_estimator = markovchain_transition_time_estimator, ...){
  transition_time_estimator(histobj = histobj, input = input, start_dt = start_dt, ...) %>% 
    mutate(nxtTrTime = startTime + pred_duration)
}

gen_transition_times_exp = function(input, transition_durations, start_dt, ...){
  input %>% 
    left_join(transition_durations, by = c("status", "nextStatus")) %>% na.omit %>%
    mutate(nxtTrTime = startTime + gen.random.cond(N = n(), family = 'exp', rate = 1.0/meanTime, x0 = as.numeric(start_dt - startTime)))
}
