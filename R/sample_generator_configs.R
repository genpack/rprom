# test_elsyd

reticulate::use_virtualenv("faker_test")
module = reticulate::import(module = 'faker')

faker = module$Faker()

# Sample list of features for case profile person:
CASEPROFILE.PERSON = list(
  id = function(n, ...){sequence(n)},
  gender = function(n, mf_ratio = 1.0, ...){
    n_male   = as.integer(n*mf_ratio/(1 + mf_ratio))
    n_female = n - n_male
    c(rep('female', n_female), rep('male', n_male)) %>% sample(size = n, replace = F)
  },
  firstname = function(n, gender, ...){
    lambda = function(u) if (u == 'female') faker$first_name_female() else faker$first_name_male()
    gender %>% sapply(lambda) %>% unlist %>% unname
  },
  lastname = function(n, gender, ...){
    lambda = function(u) if (u == 'female') faker$last_name_female() else faker$last_name_male()
    gender %>% sapply(lambda) %>% unlist %>% unname
  },
  annual_income = function(n, gender, mean_male = 80000, mean_female = 60000, minimum = 20000, ...){
    gen.random.highpass(N = n, family = 'exponential', 
                                rate = ifelse(gender == 'male', 1.0/(mean_male - minimum), 1.0/(mean_female - minimum)), 
                                x0 = minimum) %>% as.integer
  },
  
  start_date = function(n, ...){
    as.Date('2020-01-01') + as.integer(rexp(rate = 0.01, n = n))
  },
  
  end_date = function(n, start_date, ...){
    return(start_date + 600)
  },
  
  loan_amount = function(n, annual_income, minimum = 100000, ...){
    as.integer(minimum + rexp(n = n, rate = 0.1/annual_income))
  },
  
  interest_rate = function(n, ...){
    runif(n, min = 0.01, max = 0.05)
  }
  
)


# Example: 
cp = generate_case_profile(size = 10, features = CASEPROFILE.PERSON)

EVENTLOG.BILLS = list(
  PrincipalBillIssued = list(
    interarrival_time_generator = function(n, ...){
      # monthly 
      30*24*3600 + rnorm(n, mean = 0, sd = 2*24*3600)
    },
    variables = list(
      amount = function(n, loan_amount, interest_rate, terms = 30, ...){
        monthly_payment = loan_amount*(1 + interest_rate*terms/2)/(12*terms)
        balance = loan_amount
        interests = c()
        for(i in sequence(n)){
          interest = balance*interest_rate/12
          balance = balance + interest - monthly_payment
          interests = c(interests, interest)
        }
        return(interests)}
    )
  )  
)

# 1065-0322
# Ref No:
#   
# eb0013662705


# Example:
generate_eventlog_single(case_profile = cp, event_type = 'HomeLoanBillArrived', event_config = EVENTLOG.BILLS$PrincipalBillIssued, 
                         caseID_col = 'id', case_startTime_col = 'start_date', case_endTime_col = 'end_date')
