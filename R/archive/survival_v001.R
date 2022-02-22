# Header
# Filename:       survival.R
# Description:    Contains utility functions for survival modelling.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 March 2019
# Last Revision:  14 March 2019
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 March 2019      Initial issue



SURVIVAL = setRefClass('SURVIVAL',
                       fields = list(data = 'list', report = 'list', settings = 'list'),
                       methods = list(
                         initialize = function(config = NULL, ...){
                           callSuper(settings = config, ...)

                           settings %>%
                             list.default(caseID_col = 'caseID', eventTime_col = 'eventTime', eventType_col = 'eventType', variable_col = 'variable', value_col = 'value') %>%
                             list.default(caseStart_tag = NULL, caseEnd_tag = NULL) %>%
                             list.default(age_varname = 'age', deathReason_varname = 'deathReason', caseProfile_features = character()) ->> settings
                         },

                         # caseStart_tag specifies eventType that triggers start of a case (when a case is born)
                         # if caseStart_tag is not specified, age_varname must be specified.
                         # age_varname specifies the value in variable column whose associated value column contains the case age or tenure.
                         # if age_varname is not specifies, then age is computed from the eventTime of the earliest row whose associated value in eventType column equals caseStart_tag.
                         feed.eventLog = function(dataset){
                           data$el <<- dataset %>% rename(caseID = settings$caseID_col, eventTime = settings$eventTime_col, eventType = settings$eventType_col, variable = settings$variable_col, value = settings$value_col)
                         },

                         feed.caseProfile = function(dataset){
                           rename(caseID = settings$caseID_col)
                         },

                         feed.caseProfile.aggregated = function(dataset, age_col = 'age', death_reason_col = 'reason', count = 'count'){
                           data$cpag <<- dataset %>%
                             nameColumns(columns = list(age = age_col, reason = death_reason_col, count = count_col), classes = list(age = 'integer', reason = 'character', count = 'integer')) %>%
                             arrange(age, reason)
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

                         get.caseVarProfile = function(){
                           # For now, assume age_varname and deathReason_varname are not NULL:
                           assert(!is.null(settings$age_varname))
                           assert(!is.null(settings$deathReason_varname))

                           variables = settings$caseProfile_features %U% c(settings$age_varname, settings$deathReason_varname)

                           ### Get case profile:
                           if(is.null(data$cvp)){
                             cat("\n", "No caseVarProfile found in the dataset. Building from event log ... ")
                             if(is.null(data$el)){stop("No event log found in the dataset")}
                             data$el %>% filter(variable %in% variables) %>%
                               arrange(caseID, eventTime) %>% group_by(caseID, variable) %>%
                               summarise(firstValue = first_value(value), lastValue = last_value(Value), firstTime = first_value(eventTime), lastTime = last_value(eventTime)) ->> data$cvp
                             cat("Done!", "\n")
                           }
                           return(data$cvp)
                         },

                         get.caseProfile = function(){
                           if(is.null(data$cp)){
                             cat("\n", "No caseVarProfile found in the dataset. Building from caseVarProfile ...")

                             get.caseVarProfile() %>% filter(variable %in% settings$caseProfile_features) %>%
                               sdf_pivot(caseID ~ variable, fun.aggregate = list(firstValue = "sum")) -> cp

                             get.caseVarProfile() %>% filter(variable %in% c(settings$age_varname, settings$deathReason_varname)) %>%
                               sdf_pivot(caseID ~ variable, fun.aggregate = list(lastValue = "sum", lastTime = "last_value")) %>%
                               rename(deathTime   = settings$deathReason_varname %>% paste0('_last(lastTime, false)'),
                                      deathReason = settings$deathReason_varname %>% paste0('_sum(CAST(lastValue AS DOUBLE))'),
                                      LastAgeTime = settings$age_varname %>% paste0('_last(lastTime, false)'),
                                      LastAge     = settings$age_varname %>% paste0('_sum(CAST(lastValue AS DOUBLE))')) %>%
                               mutate(deathDate = as.Date(deathTime), LastAgeDate = as.Date(LastAgeTime)) %>%
                               mutate(deathAge  = as.numeric(datediff(deathDate, LastAgeDate)) + LastAge) -> cpt

                             data$cp <<- left_join(cp, cpt, by = 'caseID')
                             cat("Done!", "\n")
                           }
                           return(data$cp)
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
                             rvis::rvisPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         },

                         plot.survival = function(reasons = NULL, categoricals = NULL, plotter = 'plotly', gain = 100, t0 = 0){
                           haz = get.hazard(reasons = reasons, categoricals = categoricals) %>% mutate(prsurv = prsurv)

                           cols = unique(haz$featval) %>% as.list

                           haz %<>% reshape2::dcast(age ~ featval, value.var = 'prsurv')
                           haz[haz$age >= t0, ] %>% column2Rownames('age') %>%
                             apply(2, function(x) gain*x/x[1]) %>% as.data.frame %>% mutate(age = rownames(.) %>% as.integer) %>%
                             mutate(age = age - age[1]) %>%
                             rvis::rvisPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         },

                         plot.death = function(reasons = NULL, categoricals = NULL, plotter = 'plotly', gain = 100, t0 = 0){
                           haz = get.hazard(reasons = reasons, categoricals = categoricals) %>% mutate(prdeath = (1.0 - prsurv))

                           cols = unique(haz$featval) %>% as.list

                           haz %<>% reshape2::dcast(age ~ featval, value.var = 'prdeath')

                           haz[haz$age >= t0, ] %>% column2Rownames('age') %>%
                             apply(2, function(x) gain*(x - x[1])/(1.0 - x[1])) %>% as.data.frame %>% mutate(age = rownames(.) %>% as.integer) %>%
                             mutate(age = age - age[1]) %>%
                             rvis::rvisPlot(type = 'scatter', plotter = plotter, x = 'age', y = cols, shape = 'line')
                         }


                       ))
