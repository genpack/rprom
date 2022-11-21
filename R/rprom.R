#' A package for comprehensive process modelling in R
#'
#' This package has whatever you need to implement a process modelling.
#'
#' @docType package
#' @name rprom
#' @include transys.R
#' @include prom.R
#' @include prom_tools.R
#' @include prosim.R
#' @include ts_tools.R
#' @include ts_vis.R
#' @include dfg.R
#' @include sfg.R
#' @include dfg_tools.R
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @import rutils

# Current Version: 2.2.8
# Issue Date: 08 November 2016
# Last Issue: 14 June 2022

# Version     Date                 Action
# ----------------------------------
# 0.0.1       08 November 2016     Initial issue by file transys.R
# 0.5.0       06 September 2017    transys.R modified to version 0.5.0
# 0.5.6       14 September 2017    transys.R modified to version 0.5.6
# 0.5.7       09 March 2018        prosim.R added ver 0.0.1 with function simQueue()
# 0.6.0       02 May 2018          prosim.R added with two functions and updated to version (0.0.4)
# 0.7.0       02 May 2018          prom.R added with class constructor Process() a function at version (0.1.0)
# 0.7.2       04 May 2018          prom.R updated to version (0.1.2)
# 0.7.3       04 May 2018          promvis.R added with one function at version (0.0.1)
# 0.7.5       04 June 2018         promvis.R updated to version (0.0.3)
# 0.7.9       14 June 2018         prom.R modified to version 0.1.6
# 0.8.4       30 June 2018         prom.R modified to version 0.2.1
# 0.8.9       02 July 2018         trsnsys.R modified to version 0.6.1
# 0.9.9       02 July 2018         prom.R modified to version 0.3.1
# 1.0.9       06 July 2018         promtools.R added (Version 0.1.0)
# 1.1.0       06 July 2018         tstools.R added (Version 0.0.1)
# 1.1.3       17 July 2018         promtools.R changed to version 0.1.3
# 1.2.5       17 July 2018         prom.R changed to version 0.4.3
# 1.3.2       31 July 2018         prom.R changed to version 0.5.0, promtools.R changed to ver 0.1.4
# 1.3.3       01 August 2018       prom.R changed to version 0.5.1
# 1.3.4       12 October 2018      tsvis.R added (version 0.0.1)
# 1.3.5       12 October 2018      transys.R changed to version 0.6.7
# 1.3.6       16 October 2018      tstools.R changed to version 0.0.2
# 1.4.6       18 October 2018      tsvis.R changed to version 0.1.1
# 1.4.9       23 October 2018      transys.R changed to version 0.7.0.
# 1.5.1       26 October 2018      transys.R changed to version 0.7.2
# 1.5.3       06 November 2018     tsvis.R changed to version 0.1.3
# 1.5.4       06 November 2018     transys.R changed to version 0.7.3
# 1.5.5       24 February 2019     transys.R changed to version 0.7.4
# 1.5.6       14 March 2019        survtools.R added (version 0.0.1)
# 1.5.7       14 March 2019        survival.R added (version 0.0.1)
# 1.5.9       26 March 2019        survival.R changed to version 0.0.3
# 1.6.3       26 March 2019        survtools.R changed to version 0.0.5
# 1.6.4       20 June 2019         tsvis.R changed to version 0.1.4
# 1.6.5       21 June 2019         transys.R changed to version 0.7.5
# 1.6.6       27 June 2019         tsvis.R changed to version 0.1.5
# 1.7.0       09 July 2019         transys.R changed to version 0.7.9
# 1.7.2       16 July 2019         transys.R changed to version 0.8.1
# 1.7.7       08 August 2019       tstools.R changed to version 0.0.7
# 1.7.9       20 August 2019       transys.R changed to version 0.8.3
# 1.8.0       9 September 2019     tstools.R changed to version 0.0.8
# 1.8.3       9 September 2019     transys.R changed to version 0.8.6
# 1.9.2       11 March 2021        dfgtools.R added
# 1.9.3       16 April 2021        minor change in dfgtools.R function dfg_pack() 
# 1.9.7       17 April 2021        function moving_aggregator() added: function dfg.historic() updated. 
# 2.0.5       03 August 2021       Revisit to update. 
# 2.0.6       05 August 2021       method get.nodes() in transys.R modified: column nCaseExit added.
# 2.1.0       17 March 2022        transys.R updated to version 0.9.0
# 2.1.4       18 March 2022        transys.R updated to version 0.9.4
# 2.1.5       05 April 2022        tsvis.R updated to version 0.1.6
# 2.1.6       05 April 2022        tstools.R updated to version 0.1.0
# 2.1.7       21 April 2022        transys.R updated to version 0.9.5
# 2.1.8       21 April 2022        prom.R updated to version 0.5.2
# 2.2.5       03 May 2022          transys.R updated to version 1.0.1
# 2.2.6       04 May 2022          tstools.R updated to version 0.1.1
# 2.2.7       18 May 2022          transys.R updated to version 1.0.2
# 2.2.8       14 June 2022         dfg.R added.
# 2.2.9       07 November 2022     sfg.R added.
# 2.3.1       21 November 2022     sfg.R updated to version 0.0.3

globalVariables(c("new", "type", "caseID", "taskID", "eventTime", "time", "mean", "median"))