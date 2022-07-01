
library(magrittr)
library(dplyr)
library(lubridate)
library(rbig)
library(rutils)
library(rvis)
# library(rprom)
source('~/Documents/software/R/packages/rprom/R/transys.R')
source('~/Documents/software/R/packages/rprom/R/tstools.R')
source('~/Documents/software/R/packages/rprom/R/tsvis.R')
source('~/Documents/software/R/packages/rprom/R/dfgtools.R')

### Read Data:
hospital_df = eventdataR::hospital_billing %>% as.data.frame
tsobj = new('TransitionSystem')

case_profile = hospital_df %>% 
  arrange(case_id, timestamp) %>% 
  group_by(case_id) %>% 
  summarise(first_activity = first(activity), 
            last_activity = last(activity), 
            num_transitions = length(activity) - 1) %>%
  ungroup
  
cases_ending_outside = case_profile  %>% filter(num_transitions == 0) %>% pull(case_id)
hospital_df$case_ended = TRUE
hospital_df$case_ended[hospital_df$case_id %in% cases_ending_outside] = FALSE

## Build and observe the object

tsobj = new('TransitionSystem')
tsobj$feed.transition_events(hospital_df, 
                    caseID_col = 'case_id', 
                    status_col = 'activity',
                    startTime_col = 'timestamp', 
                    caseEndFlag_col = 'case_ended')

tsobj$set.filter.case(complete = T, freq_rate_cut = 0.99)
tsobj$plot.process.map(config = list(direction = 'left.right'), width = "800px")

new_transitions = tsobj$run.simulation(start_dt = '2013-03-01', target_dt = '2014-03-01')

index_started_before = which(new_transitions$startTime < lubridate::as_datetime('2013-03-01'))
new_transitions %>% group_by(caseID) %>% 
  summarise(first_status = first(status), last_status = last(nextStatus),
            min_time = min(startTime), max_time = max(startTime)) %>% ungroup -> case_profile


simulated = new("TransitionSystem")
new_transitions %>% left_join(case_profile, by = "caseID") %>% 
  mutate(case_started = (first_status == "START"), case_ended = (last_status == "END")) %>% 
  simulated$feed.transition_events(caseStartFlag_col = 'case_started', caseEndFlag_col = 'case_ended')

case_profile$last_status %>% unique %>% print
case_profile$max_time %>% max %>% print

simulated$set.filter.case(complete = T)

actual = tsobj$extract_subset(time_from = '2013-03-01', time_to = '2014-03-01')
actual$set.filter.case(case_domain = simulated$get.cases(), complete = T)

simulated$get.case.profile() %>% pull(duration) %>% hist(breaks = 1000)
actual$get.case.profile() %>% pull(duration) %>% hist(breaks = 1000)

if(!require(plotly)){install.packages('plotly')}
if(!require(crosstalk)){install.packages('crosstalk')}

tsobj %>% plot_transition_time_histogram(remove_outliers = TRUE)


new_transitions_exp = tsobj$run.simulation(start_dt = '2013-03-01', target_dt = '2014-03-01', family = 'exp')
knitr::kable(new_transitions_exp %>% head(20))

new_transitions %>% filter(status == "FIN", nextStatus == "RELEASE") %>% pull(pred_duration) %>% 
  hist(breaks = 1000, main = "Histogram of Generated durations for FIN-RELEASE \n (family = 'normal')",
       xlab = "transition time (sec)")

new_transitions_exp %>% filter(status == "FIN", nextStatus == "RELEASE") %>% pull(pred_duration) %>% 
  hist(breaks = 1000, main = "Histogram of Generated durations for FIN-RELEASE \n (family = 'exp')",
       xlab = "transition time (sec)")


ts_monthly = tsobj$to_periodic('month')
new_transitions_daily = ts_monthly$run.simulation(start_dt = '2013-03-01', target_dt = '2014-03-01')
simulated_periodic = new('TransitionSystem')

ended_cases = new_transitions_daily[new_transitions_daily$nextStatus == "END", "caseID", drop = T] %>% unique
new_transitions_daily$case_ended = new_transitions_daily$caseID %in% ended_cases

simulated_periodic$feed.transition_events(new_transitions_daily, remove_sst = T, caseStartTags = "START", caseEndFlag_col = "case_ended")

# 
# todos: 
# 1- create_subset(from, to) added filter.time
# 2- caseStartTags, caseEndTags (skip for now) Done.

simulated_periodic$set.filter.case(complete = T)
