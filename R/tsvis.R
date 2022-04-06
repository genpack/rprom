# tsvis.R -------------------------------------------------------------------

# Header
# Filename:       tsvis.R
# Description:    Visualisation functions for transys objects
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Start Date:     12 October 2018
# Last Revision:  05 April 2022
# Version:        0.1.6
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     12 October 2018    Initial issue transferred from transys.R
# 0.1.0     16 October 2018    Function plot_statuses_box() added.
# 0.1.1     18 October 2018    Function plot_process_map() modified: does not use plot package any more but prepares customized nodes & links tables
# 0.1.2     05 November 2018   Function plot_process_map() modified: uses package grviz (renamed from diagramer)
# 0.1.3     06 November 2018   Function plot_traces_sunburst() modified: builds chartname to avoid re-plotting with same spec
# 0.1.4     20 June 2019       Function plot_process_map() modified: config argument is merged with default config
# 0.1.5     27 June 2019       Additional arguments passed to plotting functions.
# 0.1.6     05 April 2022      Function plot_transition_time_histogram() added.

############### Process Overview Visualisations:

# plots process map for the transition system
#' @export plot_process_map
plot_process_map = function(obj, measure = c('freq', 'time', 'rate'), node_size = 'auto', link_size = 'auto', time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = c('grviz', 'visNetwork'), config = NULL, remove_ends = F, ...){
  # todo: add argument time_aggregator
  plotter   = match.arg(plotter)
  measure   = match.arg(measure)
  time_unit = match.arg(time_unit)
  nontime   = c('freq', 'rate')
  k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])
  
  nodes = obj$get.nodes() %>% rutils::na2zero()
  links = obj$get.links() %>% rutils::na2zero()
  
  if(remove_ends){
    endstats = c('START', 'END', 'ENTER', 'EXIT')
    nodes = nodes[!(nodes$status %in% endstats),]
    links = links[(!(links$status %in% endstats)) & (!(links$nextStatus %in% endstats)),]
  } else {
    nodes %<>% dplyr::mutate(shape = ifelse(status %in% c('START', 'END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle')))
    if(measure %in% nontime){
      nodes$totalEntryFreq[nodes$status == 'START'] <- nodes$totalEntryFreq[nodes$status == 'END'] %>% sum(na.rm = T)
    }
  }
  
  if(is.null(config)){config = list()}
  
  cfg = list(node.fixedSize = F) %<==>% config
  
  nodes %<>%
    dplyr::mutate(shape = ifelse(status %in% c('START', 'END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle')))
  
  if(measure %in% nontime){
    nodes$totalEntryFreq[nodes$status == 'START'] <- nodes$totalEntryFreq[nodes$status == 'END'] %>% sum(na.rm = T)
    
    nodes %<>%
      dplyr::mutate(label = status %>% paste0('\n', '(', totalEntryFreq, ')')) %>%
      dplyr::mutate(id = status, color = totalEntryFreq)
    
    if(measure == 'freq'){
      links %<>% dplyr::mutate(linkLabel = ' ' %++% totalFreq)
    } else {
      links %<>% left_join(links %>% group_by(status) %>% summarise(den = sum(totalFreq)) %>% ungroup, by = 'status') %>% 
        dplyr::mutate(linkLabel = paste0(' ', round(100*totalFreq/den, 2), '%'))
    }
    links %<>%
      dplyr::mutate(linkTooltip = status %>% paste(nextStatus, sep = '-')) %>%
      dplyr::mutate(source = status, target = nextStatus, linkColor = totalFreq, linkWidth = totalFreq)
    
    arguments = list(key = 'status', shape = 'shape', label = 'label', color = list(color = 'totalDuration'), source = 'status', target = 'nextStatus', linkColor = list(color = 'totalTime'), linkLabel = 'linkLabel', linkTooltip = 'linkTooltip', tooltip = 'label', plotter = plotter, type = 'graph', ...)
    if(node_size == 'measure'){
      cfg = cfg %<==>% list(node.label.size = 40, node.size.min = 2, node.size.max = 4, node.size = 3, node.size.ratio = 0.6)
    }
    
    if(link_size == 'measure'){
      cfg = cfg %<==>% list(link.width.max = 12, link.width.min = 2, link.label.size = 35)
      arguments$linkWidth = 'totalFreq'
    }
    
    arguments$obj    = list(nodes = nodes, links = links)
    arguments$config = cfg
    do.call(rvis::rvisPlot, args = arguments)
  } else {
    cfg$palette$color = c('white', 'red')
    
    nodes %<>%
      dplyr::mutate(meanDuration = k*meanDuration) %>%
      dplyr::mutate(label = status %>% paste0('\n', '(', meanDuration %>% round(digits = 2), ' ', time_unit %>% substr(1,1), ')'))
    
    links %<>%
      dplyr::mutate(meanTime = k*meanTime) %>%
      dplyr::mutate(linkLabel = ' ' %++% (meanTime %>% round(digits = 2) %>% paste(time_unit %>% substr(1,1))), linkTooltip = status %>% paste(nextStatus, sep = '-'))
    
    arguments = list(key = 'status', shape = 'shape', label = 'label', color = list(color = 'totalDuration'), source = 'status', target = 'nextStatus', linkColor = list(color = 'totalTime'), linkLabel = 'linkLabel', linkTooltip = 'linkTooltip', tooltip = 'label', config = cfg, plotter = plotter, type = 'graph', ...)
    if(node_size == 'measure'){
      cfg = cfg %<==>% list(node.label.size = 40, node.size.min = 2, node.size.max = 4, node.size = 3, node.size.ratio = 0.6)
      arguments$size = 'totalDuration'
    }
    
    if(link_size == 'measure'){
      cfg = cfg %<==>% list(link.width.max = 12, link.width.min = 2, link.label.size = 35)
      arguments$linkWidth = 'totalTime'
    }
    arguments$obj = list(nodes = nodes, links = links)
    arguments$config = cfg
    do.call(rvis::rvisPlot, args = arguments)
  }
}

#' plots process tree for the transition system
#' @export plot_process_tree
plot_process_tree = function(obj, plotter = 'sankeytree', config = NULL, ...){
  # plotter = match.arg(plotter)
  tbl = obj$get.traces() %>% buildTreeTable()
  
  if(is.null(config)){config = list()}
  cfg = list(node.label.size.max = 10, node.height = 90) %<==>% config
  
  cls = names(tbl) %-% 'freq'
  
  tbl %>% rvis::rvisPlot(type = 'tree', plotter = 'sankeytree', label = as.list(cls[sequence(min(8, length(cls)))]), size = 'freq', config = cfg, ...)
}

#' @export plot_process_sankey
plot_process_sankey = function(obj, config = list(), plotter = 'networkD3', ...){
  cfg = list(node.label.size = 20, node.width = 25) %<==>% config
  
  list(nodes = obj$get.nodes(), links = obj$get.links()) %>%
    rvis::rvisPlot(key = 'status', label = list(label = 'status'), source = list(source = 'status'), target = 'nextStatus', linkWidth = 'totalFreq', type = 'sankey', config = cfg, plotter = plotter, ...)
}

############### Trace Overview Visualisations:

#' @export plot_traces_bar
plot_traces_bar  = function(obj, measure = c('freq', 'time'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), aggregator = c('sum', 'mean', 'median', 'sd'), plotter = 'plotly', ...){
  agg = c('sum' = 'tot', 'mean' = 'avg', 'median' = 'med', 'sd' = 'sd')
  ags = c('sum' = 'Total', 'mean' = 'Average', 'median' = 'Median', 'sd' = 'Standard Deviation of')

  # plotter    <- match.arg(plotter)
  measure    <- match.arg(measure)
  time_unit  <- match.arg(time_unit)
  k          <- 1.0/timeUnitCoeff[time_unit]
  aggregator <- match.arg(aggregator)

  TBL        <- obj$get.traces()

  if(measure == 'freq'){
    sumfreq = sum(TBL$freq, na.rm = T)

    TBL %>%
      dplyr::mutate(freqper = 100*freq/sumfreq) %>%
      dplyr::mutate(tooltip = path %>% paste0('  Freq: ', freq, ' (', freqper %>% round(digits = 1), '%)')) %>%
      dplyr::arrange(freq) %>%
      rvis::rvisPlot(x = 'freq', y = 'variation', tooltip = 'tooltip', type = 'bar', plotter = plotter, ...)

  } else {
    coln = agg[aggregator] %++% 'Time'
    TBL[, coln] <- TBL[, coln]*k
    TBL$tooltip = TBL$path %>% paste0('  ', ags[aggregator], ' Process Time: ', TBL %>% pull(agg[aggregator] %++% 'Time') %>% round(digits = 2), ' ',time_unit, 's')
    TBL %>%
      dplyr::arrange_(coln) %>%
      rvis::rvisPlot(x = coln, y = 'variation', tooltip = 'tooltip', type = 'bar', plotter = plotter, color = rutils::color.mean('red', 'white'), ...)
  }
}

#' @export plot_traces_sunburst
plot_traces_sunburst = function(obj, min_freq = 10, plotter = c('sunburstr', 'd2b'), width = 600, height = 600, ...){
  plotter   = match.arg(plotter)
  chartname = paste('traces', 'sunburst', plotter, min_freq, sep = '.')
  if(is.null(obj$plots[[chartname]])){
    tbl = obj$get.traces() %>%
      dplyr::select(path, freq) %>% dplyr::filter(freq > min_freq - 1)
    if(!is.empty(tbl)){
      if(plotter == 'sunburstr'){
        tbl %>% sunburstR::sunburst(count = T, width = width, height = height, legend = list(h = 30, w = 120, r = 10, s = 5), ...) -> obj$plots[[chartname]]
      } else if (plotter == 'd2b'){
        tbl %>% sunburstR::sund2b(width = width, height = height, ...) -> obj$plots[[chartname]]
      }
    }
  }

  return(obj$plots[[chartname]])
}

############### Status Overview Visualisations:

#' @export plot_statuses_bar
plot_statuses_bar = function(obj, measure = c('freq', 'totTime'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = 'plotly', ...){
  if(is.empty(obj$history)) return(NULL)
  colname   = c(freq = 'totalEntryFreq', totTime = 'totalDuration')
  vrs       = c(freq = 'Total Entry Freq', totTime = 'Total Duration')
  nontime   = 'freq'
  measure   = match.arg(measure)
  time_unit = match.arg(time_unit)
  k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])
  tus       = chif(measure == 'freq', '', ' ' %++% time_unit %++% 's')

  ND = obj$get.nodes() %>% as.data.frame

  if(measure %in% 'totTime'){ND[, colname[measure]] <- k*ND[, colname[measure]]}

  cfg = list(title = chif(measure == 'freq', 'Status Entry Frequencies', 'Status Durations'),
             xAxis.label = chif(measure == 'freq', 'Frequency', 'Duration'),
             yAxis.label = 'Status',
             point.color = chif(measure == 'freq', NULL, rutils::color.mean('white', 'red')))

  ND[, colname[measure]] <- k*ND[, colname[measure]]
  ND$tooltip <- ND$status %>% paste0('  (', vrs[measure],': ', ND %>% pull(colname[measure]) %>% round(digits = 2), tus, ')')

  ND %>% arrange_(colname[measure]) %>%
    rvis::rvisPlot(x = colname[measure], y = 'status', tooltip = 'tooltip', plotter = plotter, type = 'bar', config = cfg, ...)
}

#' @export plot_statuses_box
plot_statuses_box = function(obj, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = 'plotly', ...){
  time_unit = match.arg(time_unit)
  k         = 1.0/timeUnitCoeff[time_unit]

  H = obj$history[obj$history$selected, ]
  H$duration  <- (k*H$duration) %>% round(digits = 2)

  cfg = list(title = 'Status Durations', xAxis.label = 'Duration' %>% paste0(' (', substr(time_unit,1,1), ')'), yAxis.label = 'Status')
  H %>% rvis::rvisPlot(x = 'duration', y = 'status', plotter = plotter, type = 'box', config = cfg, ...)
}

############### Case Overview Visualisations:

#' @export plot_cases_status_pie
plot_cases_status_pie = function(obj, measure = 'time', aggregator = 'sum', time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), trim = NULL, plotter = 'plotly', ...){
  measure    = match.arg(measure)
  aggregator = match.arg(aggregator)
  time_unit  = match.arg(time_unit)

  cst          <- obj$get.case.status.duration()
  aa           <- cst[obj$get.cases(), ] %>% colSums(na.rm = T) %>% as.data.frame %>% rownames2Column('status')
  names(aa)[2] <- 'duration'
  if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim'))){
    sm <- sum(aa$duration, na.rm = T)
    aa <- aa[aa$duration >= trim*sm, ]
  }
  aa$duration  <- aa$duration/timeUnitCoeff[time_unit]

  aa %>% rvis::rvisPlot(theta = 'duration', label = 'status', config = list(legend.enabled = F), type = 'pie', plotter = plotter, ...)
}

#' @export plot_cases_table
plot_cases_table = function(obj, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = 'DT', config = list(), ...){
  plotter   = match.arg(plotter)
  time_unit = match.arg(time_unit)
  k         = 1.0/timeUnitCoeff[time_unit]

  tbl = obj$tables$profile.case[obj$get.cases(), ]
  tbl$completed = tbl$caseStart & tbl$caseEnd
  tbl$duration = (k*tbl$duration) %>% round(digits = 2)

  tbl = tbl[, c('startTime', 'endTime', 'completed', 'duration')]
  cln = list( 'Started at' = 'startTime', 'Ended at' = 'endTime', Completed = 'completed', Duration = 'duration')
  names(cln)[4] %<>% paste0(" (", substr(time_unit,1,1), ")")
  cfg = list(paging.length = 20) %<==>% config
  tbl %>% rvis::rvisPlot(label = cln, plotter = plotter, type = 'table', config = cfg, filter = 'top', ...)
}

############### Status Card Visualisations:

#' @export plot_status_gauge
plot_status_gauge = function(obj, statusID, measure = c('loopRate', 'caseRatio'), ...){
  measure = match.arg(measure)
  # cfg = list(thetaAxis.zone = list( list(min = 0, max = 30, color = 'green'), list(min = 30, max = 60, color = 'yellow'), list(min = 60, max = 100, color = 'red')))
  c3.gauge(100*obj$get.status.metric(statusID, measure), ...)
}

#' @export plot_status_next_pie
plot_status_next_pie = function(obj, statusID, trim = NULL, plotter = 'plotly', config = list(), ...){
  if(verify(statusID, 'character', varname = 'statusID') %in% obj$get.statuses()){
    AD = obj$get.adjacency(measure = 'freq')
    SN = AD[statusID, ] %>% t %>% as.data.frame %>% rownames2Column('status') %>%
      dplyr::select_('status', freq = 'statusID')
    if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim')))
    {SN = SN[SN$freq/sum(SN$freq, na.rm = T) > trim, ]}
  } else {
    SN = data.frame(status = character(), freq = numeric(), stringsAsFactors = F)
  }
  cfg = list(title = 'Next Status Distribution') %<==>% config
  
  SN %>% na.omit %>% rvis::rvisPlot(label = 'status', theta = 'freq', type = 'pie', plotter = plotter, config = cfg, ...)
}

#' @export plot_status_prev_pie
plot_status_prev_pie = function(obj, statusID, trim = NULL, plotter = 'plotly', config = list(), ...){
  if(verify(statusID, 'character', varname = 'statusID') %in% obj$get.statuses()){
    AD = obj$get.adjacency(measure = 'freq')
    SP = AD[, statusID] %>% as.data.frame
    SP$status = rownames(AD)
    names(SP)[1] <- 'freq'
    if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim')))
    {SP = SP[SP$freq/sum(SP$freq, na.rm = T) > trim, ]}
  } else {
    SP = data.frame(status = character(), freq = numeric(), stringsAsFactors = F)
  }
  cfg = list(title = 'Previous Status Distribution') %<==>% config
  SP %>%  na.omit %>% 
    rvis::rvisPlot(label = 'status', theta = 'freq', type = 'pie', plotter = plotter, config = cfg, ...)
}

############### Markov Chain Probability Trends:
plot_prob_trend = function(obj, initial_status, n_steps = 10, plotter = 'plotly'){
  obj$get.adjacency(measure = 'rate', remove_ends = T) %>% t -> M
  v = rep(0, nrow(M)) %>% {names(.) <- rownames(M); .[initial_status] <- 1.0; .}
  trend = NULL
  for(i in sequence(n_steps)){
    if(i == 1){MP <- M} else {MP <- MP %*% M}
    trend %<>% cbind(MP %*% v)
  }
  
  colnames(trend) <- paste0('M', sequence(ncol(trend)))
  trend %>% t %>% as.data.frame %>% rownames2Column('status') %>% 
    rvis::rvisPlot(x = 'status', y = colnames(.) %>% setdiff('status') %>% as.list,
              type = 'bar', plotter = plotter, config = list(barMode = 'stack'))
}


############### Volume Time Series Visualisations:

#' @export plot_volumes_area
plot_volumes_area = function(obj, period = 'daily', trim = 0.01, plotter = 'streamgraph', ...){
  period = match.arg(period)
  back   = obj$get.backlog(as_timeseries = T, period = period)
  colns  = names(back$data) %-% c('date', 'START', 'END', 'ENTER', 'EXIT')
  sumps  = colSums(back$data[, colns]) %>% vect.normalise
  colns  = names(sumps)[which(sumps >= trim)]
  back$data[, c('date', colns)] %>%
    rvis::rvisPlot(x = 'date', y = colns %>% as.list, plotter = plotter, type = 'tsarea', ...)
}

# Do not call this function without filtering if you have more many cases.
# Don't forget to filter for a few cases before calling this function. 
plot_case_timeline = function(obj){
  tbl = obj$history %>% filter(selected) %>% select(start = startTime, content = status, end = endTime, group = caseID) %>% mutate(id = 1:nrow(.), type = 'range')
  tblgrp = data.frame(id = unique(tbl$group)) %>% mutate(content = id)
  timevis::timevis(data = tbl, groups = tblgrp)
}

############### Non-exported incomplete functions: ###############

plot_transition_time_distribution.graphics = function(obj, source, target, remove_outliers = T){
  
  vect = obj$history %>% filter(status == source, nextStatus == target) %>% pull(duration)
  if(remove_outliers){
    wol  = vect %>% rml::outlier(recursive = T) %>% which 
    vect = vect[-wol]
  }
  hist(vect, breaks = 1000)
  density(vect) %>% lines
  
}

#' Shows the distribution of transition times (durations) in a histogram plot.
#' @param obj object of class \code{TranSys}: The Transition System object from which transition durations are extracted
#' @param source character: Transition source status. Must be within the source statuses of the transition system.
#' @param target character: Transition destination status. Must be within the target statuses of the transition system.
#' If you do not specify any of the two arguments \code{source} or \code{target}, 
#' a drop-down menu will apear where you can select your desired transition.
#' This feature is using package \code{crosstalk} and currently, 
#' only works when plotter is \code{plotly}.
#' @param remove_outliers logical: Should outliers be removed? 
#' If set to \code{TRUE}, 
#' outliers beyond four standard deviations from the mean, will be removed recursively.
#' @param time_unit character: time unit to be used in the output visualization. 
#' Can be one of these options: \code{'second', 'minute', 'hour', 'day', 'week', 'year'}.
#' @param plotter character: Which plotter package do you want to generate the visualization?
#' Currently, only these options are supported:
#' \code{'plotly', 'ggplot2'} 
#' 
#' @export plot_transition_time_histogram
plot_transition_time_histogram = function(
  obj, source = NULL, target = NULL, remove_outliers = F,
  time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), 
  plotter = c('plotly', 'ggplot2'), ...){
  
  plotter   = match.arg(plotter)
  time_unit = match.arg(time_unit)
  k         = 1.0/timeUnitCoeff[time_unit]
  
  elog = obj$history %>% filter(selected) %>% 
    mutate(path = factor(path), duration = k*duration)

  if(remove_outliers){
    elog  %<>%
      group_by(path) %>%
      mutate(isoutlier = rml::outlier(duration, recursive = T)) %>%
      ungroup %>% filter(!isoutlier)
  }

  if(is.null(source) | is.null(target)){
    shared_history <- SharedData$new(elog %>% select(path, duration))

    if(plotter == 'plotly'){
      bscols(widths = 12,
             list(
               crosstalk::filter_select("path", "Transition", shared_history, ~path, multiple = F),
               plotly::plot_ly(shared_history, x = ~duration, type = 'histogram', ...)
             )
      )
    } else {
      stop('crosstalk does not support given plotter %s. You need to specify source and target arguments.')
    }
  } else {
    elog %<>% filter(status == source, nextStatus == target) %>% select(path, duration)
    if(plotter == 'plotly'){
        plotly::plot_ly(elog, x = ~duration, type = 'histogram', ...)
    } else if (plotter == 'ggplot2'){
        ggplot2::ggplot(elog, aes(x = duration)) + geom_histogram(...)
    }
  }
}

# Example:
# plot_transition_time_distribution(tsobj, source = 'BILLED', target = 'STORNO', remove_outliers = T, plotter = 'ggplot2')


