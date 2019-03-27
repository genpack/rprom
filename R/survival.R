# Header
# Filename:       survival.R
# Description:    Contains utility functions for survival modelling.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 March 2019
# Last Revision:  26 March 2019
# Version:        0.0.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 March 2019      Initial issue
# 0.0.2     19 March 2019      Method goto() added impacting all methods by introducing cel as Current Event Log and similarly for cp, cpt, cvp, and other tables
#                              Methods get.caseVarProfile() and ... work with argument 'full'
# 0.0.3     26 March 2019      case profile is now directly generated from eventlog for certain aggregators



SURVIVAL = setRefClass('SURVIVAL',
                       fields = list(data = 'list', report = 'list', settings = 'list'),
                       methods = list(
                         initialize = function(config = list(), ...){
                           callSuper(settings = config, ...)

                           settings %>%
                             list.default(caseID_col = 'caseID', eventTime_col = 'eventTime', eventType_col = 'eventType', variable_col = 'variable', value_col = 'value') %>%
                             list.default(caseStart_tag = NULL, caseEnd_tag = NULL) %>%
                             list.default(age_varname = 'age', deathReason_varname = 'deathReason', cp_variables = character()) %>%
                             list.default(normalize_hazard = F, cp_aggregators = 'last') ->> settings
                         },

                         goto = function(time){
                           time %<>% as.time
                           data$cel <<- data$el %>% filter(eventTime < time)

                           tags = names(data)

                           reset('ccp', 'ccvp', 'ccpt', 'ccvpt', 'mlsurv')
                         },

                         set = function(...){
                           args  = list(...)
                           if(length(args) == 1 & inherits(args[[1]], 'list')){args = args[[1]]}
                           for(arg in names(args)){
                             settings[[arg]] <<- args[[arg]]
                             ## todo: considerations for change of settings
                             if(arg %in% 'cp_variables'){reset('cp', 'ccp', 'cvp', 'ccvp')}
                           }

                         },

                         reset = function(...){
                           args  = c(...) %>% verify('character')
                           for(i in args){
                             data[[i]] <<- NULL
                           }
                         },

                         # caseStart_tag specifies eventType that triggers start of a case (when a case is born)
                         # if caseStart_tag is not specified, age_varname must be specified.
                         # age_varname specifies the value in variable column whose associated value column contains the case age or tenure.
                         # if age_varname is not specifies, then age is computed from the eventTime of the earliest row whose associated value in eventType column equals caseStart_tag.
                         feed.eventLog = function(dataset){
                           data$el  <<- dataset %>% rename(caseID = settings$caseID_col, eventTime = settings$eventTime_col, eventType = settings$eventType_col, variable = settings$variable_col, value = settings$value_col)
                           data$cel <<- data$el
                         },

                         feed.caseProfile = function(dataset){
                           rename(caseID = settings$caseID_col)
                         },

                         feed.caseProfile.aggregated = function(dataset, count_col = 'count'){
                           data$cpag <<- dataset %>%
                             rename(age = settings$age_varname, reason = settings$deathReason_varname, count = count_col) %>%
                             arrange(age, reason)
                         },

                         get.caseProfile.aggregated = function(full = T){
                           if(full){cpagname = 'cpag'} else {cpagname = 'ccpag'}
                           if(is.null(data[[cpagname]])){
                             get.caseProfile(full = full) %>% cp2cpag(settings$cp_categoricals) ->> data[[cpagname]]
                           }
                           return(data[[cpagname]])
                         },

                         get.eventVariables = function(){
                           if(is.null(data$ev)){
                             if(!is.null(data$el)){
                               data$ev <<- data$el %>% group_by(eventType, variable) %>% summarise(count = COUNT(value)) %>% collect
                             } else {stop("No eventLog found!")}
                             # todo: get it from other tables if available
                           }
                           return(data$ev)
                         },

                         get.caseVarProfile = function(full = F){
                           # For now, assume age_varname and deathReason_varname are not NULL:
                           assert(!is.null(settings$age_varname))
                           assert(!is.null(settings$deathReason_varname))
                           variables = settings$cp_variables %U% c(settings$age_varname, settings$deathReason_varname)

                           if(full){cvpname = 'cvp'; elname = 'el'} else {cvpname = 'ccvp'; elname = 'cel'}

                           ### Get case profile:
                           if(is.null(data[[cvpname]])){
                             cat("\n", "Table ", cvpname, " is not found in the dataset. Building from event log ... ")
                             if(is.null(data[[elname]])){stop("Table ", elname, " was not found in the dataset!")}

                             data[[elname]] %>% el2cvp(settings$cp_aggregators) ->> data[[cvpname]]

                             cat("Done!", "\n")
                           }
                           return(data[[cvpname]])
                         },

                         get.caseVarProfileTime = function(full = T){
                           if(full){cvptname = 'cvpt';elname = 'el'} else {cvptname = 'ccvpt'; elname = 'cel'}
                           if(is.null(data[[cvptname]])){
                             data[[elname]] %>% filter(variable %in% c(settings$age_varname, settings$deathReason_varname)) %>%
                               arrange(caseID, eventTime) %>% group_by(caseID, variable) %>%
                               summarise(firstValue = first_value(value), lastValue = last_value(value), firstTime = first_value(eventTime), lastTime = last_value(eventTime)) ->> data[[cvptname]]
                           }
                           return(data[[cvptname]])
                         },

                         get.caseProfileTime = function(full = T){
                           if(full){cptname = 'cpt'} else {cptname = 'ccpt'}

                           if(is.null(data[[cptname]])){
                             get.caseVarProfileTime(full = full) %>%
                               sdf_pivot(caseID ~ variable, fun.aggregate = list(lastValue = "sum", lastTime = "last_value")) %>%
                               rename(deathTime   = settings$deathReason_varname %>% paste0('_last(lastTime, false)'),
                                      deathReason = settings$deathReason_varname %>% paste0('_sum(CAST(lastValue AS DOUBLE))'),
                                      LastAgeTime = settings$age_varname %>% paste0('_last(lastTime, false)'),
                                      LastAge     = settings$age_varname %>% paste0('_sum(CAST(lastValue AS DOUBLE))')) %>%
                               mutate(deathDate = as.Date(deathTime), LastAgeDate = as.Date(LastAgeTime)) %>%
                               mutate(deathAge  = as.numeric(datediff(deathDate, LastAgeDate)) + LastAge) ->> data[[cptname]]
                           }
                           return(data[[cptname]])
                         },

                         #
                         get.caseProfile = function(full = F, cvp_cols = 'lastValue'){
                           if (full) {cpname = 'cp'; elname = 'el'} else {cpname = 'ccp'; elname = 'cel'}


                           if(is.null(data[[cpname]])){

                             if(settings$cp_aggregators %<% c('sum', 'mean', 'last', 'first', 'min', 'max', 'count')){
                               data[[elname]] %>% filter(variable %in% settings$cp_variables) %>%
                                 el2cp(settings$cp_aggregators) ->> data[[cpname]]
                             } else {
                               get.caseVarProfile(full = full) %>% filter(variable %in% settings$cp_variables) %>%
                                 cvp2cp(cp_cols) ->> data[[cpname]]
                             }

                             if(!is.null(settings$cp_binners)){
                               data[[cpname]] <<- data[[cpname]] %>% bin_columns(settings$cp_binners)
                             }

                           }

                           return(data[[cpname]])
                         },

                         get.mldata.survival = function(){
                           if(is.null(data$mlsurv)){
                             data$mlsurv <<- cp2mls(ccp = get.caseProfile(), ccpt = get.caseProfileTime(full = F), cpt = get.caseProfileTime(full = T))
                           }
                           return(data$mlsurv)
                         },

                         get.aggReportName = function(type, reasons = NULL, categoricals = NULL){
                           str = paste0(type, '.'); sp = ''
                           if(!is.empty(categoricals)) {str %<>% paste0(categoricals %>% sort %>% paste(collapse = '.')); sp = '.'}
                           if(!is.empty(reasons))  {str %<>% paste0(sp, reasons %>% sort %>% paste(collapse = '.'))}
                           return(str)
                         },

                         # Computes the aggregated observations based on the given categorical categoricals from caseProfile Aggregated table:
                         get.aggregates = function(categoricals = NULL){
                           tname = get.aggReportName('aggr', categoricals)
                           if(is.null(report$aggregates[[tname]])){
                             if(!is.empty(categoricals)){
                               scr = paste0("data$cpag %>% group_by(age, reason, ", categoricals %>% paste(collapse = ', '),") %>% summarise(count = sum(count))")
                               parse(text = scr) %>% eval -> tbl
                               tbd = F
                               for(col in categoricals){
                                 tbd = tbd | is.na(tbl[, col])
                               }
                               report$aggregates[[tname]] <<- tbl[!tbd, ]
                             } else {
                               data$cpag %>% group_by(age, reason) %>% summarise(count = sum(count)) ->> report$aggregates[[tname]]
                             }
                           }
                           return(report$aggregates[[tname]])
                         },

                         # This function calculates hazard and survival probability from aggregated observations
                         #   given count of observations at various tenures
                         # inputs:
                         #   data$cpag: data.frame containing aggregated observations
                         #   tenure_col: which column contains age values (column values should be integer)
                         #   status_col: status at the time of observation: must be either 'died' or 'censored'
                         #   count_col: column containing count of observations
                         get.hazard = function(reasons = NULL, categoricals){
                           # verifications:
                           if(is.null(reasons)){reasons = data$cpag$reason %>% unique %>% na.omit}
                           assert(reasons %<% unique(data$cpag$reason))

                           tname = get.aggReportName('hazard', categoricals, reasons)
                           if(is.null(report$hazard[[tname]])){

                             tbl = get.aggregates(categoricals)

                             # tbl %<>% mutate(status = ifelse(reason %in% reasons, 'died', 'censored'))
                             tbl %<>% mutate(status = ifelse(is.na(reason), 'censored', ifelse(reason %in% reasons, 'died', NA)))

                             if(!is.empty(categoricals)){
                               tbl = parse(text = paste0("tbl %>% mutate(featval = paste(", categoricals %>% paste(collapse = ','), ", sep = '-'))")) %>% eval
                             } else {tbl$featval <- 'Entire Data'}


                             tbl %<>% reshape2::dcast(age + featval ~ status, value.var = 'count', fun.aggregate = sum) %>% arrange(age)

                             tbl %<>% group_by(featval) %>% do({
                               calHazard(., normalize_hazard = settings$normalize_hazard)
                             })

                             report$hazard[[tname]] <<- tbl
                           }
                           return(report$hazard[[tname]])
                         },

                         plot.hazard = function(reasons = NULL, categoricals = NULL, plotter = 'plotly', smooth_k = 6, gain = 100){
                           haz = get.hazard(reasons = reasons, categoricals = categoricals) %>% mutate(hazard = hazard*gain) %>%
                             filter(hazard < mean(hazard, na.rm = T) + 3*sd(hazard, na.rm = T))

                           if(!is.null(smooth_k)){
                             haz %<>% group_by(featval) %>% do({mutate(., hazard = zoo::rollmean(hazard, k = smooth_k, fill = NA)) %>% na.omit})
                           }

                           cols = unique(haz$featval) %>% as.list

                           haz %>% reshape2::dcast(age ~ featval, value.var = 'hazard') %>%
                             viser::viserPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         },

                         plot.survival = function(reasons = NULL, categoricals = NULL, plotter = 'plotly', gain = 100, t0 = 0){
                           haz = get.hazard(reasons = reasons, categoricals = categoricals) %>% mutate(prsurv = prsurv)

                           cols = unique(haz$featval) %>% as.list

                           haz %<>% reshape2::dcast(age ~ featval, value.var = 'prsurv')
                           haz[haz$age >= t0, ] %>% column2Rownames('age') %>%
                             apply(2, function(x) gain*x/x[1]) %>% as.data.frame %>% mutate(age = rownames(.) %>% as.integer) %>%
                             mutate(age = age - age[1]) %>%
                             viser::viserPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         },

                         plot.death = function(reasons = NULL, categoricals = NULL, plotter = 'plotly', gain = 100, t0 = 0){
                           haz = get.hazard(reasons = reasons, categoricals = categoricals) %>% mutate(prdeath = (1.0 - prsurv))

                           cols = unique(haz$featval) %>% as.list

                           haz %<>% reshape2::dcast(age ~ featval, value.var = 'prdeath')

                           haz[haz$age >= t0, ] %>% column2Rownames('age') %>%
                             apply(2, function(x) gain*(x - x[1])/(1.0 - x[1])) %>% as.data.frame %>% mutate(age = rownames(.) %>% as.integer) %>%
                             mutate(age = age - age[1]) %>%
                             viser::viserPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         }


                       ))
