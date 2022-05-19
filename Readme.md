rprom
================

## rprom: Process Modelling with R

**rprom** is a R package for modelling any type of process model from
event-based time-series dataset. This package contains useful tools
which provide functionalities for several types of process modelling.
Some of these functionalities help you to:

  - Generate descriptive insights through various output tables, charts
    and visualisations
  - Monitor and visualize the process
  - Do a random walk analysis to get preogressive state probabilities
  - Obtain the impact matrix showing the impact of each event on the
    likelihood of every other event happening
  - Build a Markov-Chain model and run Monte-Carlo simulation
  - Generate wide table of dynamic case features
  - Build classifer/regression models to predict future events
  - Generate predictive monitoring analysis

## Installation:

The package is currently available in Github only. You can install rprom
directly from github:

``` r
devtools::install_github("https://github.com/genpack/rprom.git")
```

Package `rutils` is also required for `rprom` as a dependency. This
package is also available in github:

``` r
devtools::install_github("https://github.com/genpack/rutils.git")
```

## Getting Started:

The documentation of this package is yet under construction. We are
constantly working to prepare the full documentation, however, here we
show you how to build a simple model and generate some insights and
visualisations:

``` r
library(rutils)
library(rprom)

tsobj = new('TransitionSystem')
class(tsobj)
```

    ## [1] "TransitionSystem"
    ## attr(,"package")
    ## [1] "rprom"

Now, you have built an object of type `TransitionSystem` which is a **Transition
System**. To feed your **Transition System** with some data, you need a
dataset in the event-log format. You can get one from package
[eventdataR](https://cran.r-project.org/web/packages/eventdataR/index.html):

``` r
install.packages('eventdataR')
```

``` r
library(eventdataR)

patients_df = patients %>% as.data.frame()

patients_df %>% head %>% knitr::kable()
```

| handling     | patient | employee | handling\_id | registration\_type | time                | .order |
| :----------- | :------ | :------- | :----------- | :----------------- | :------------------ | -----: |
| Registration | 1       | r1       | 1            | start              | 2017-01-02 11:41:53 |      1 |
| Registration | 2       | r1       | 2            | start              | 2017-01-02 11:41:53 |      2 |
| Registration | 3       | r1       | 3            | start              | 2017-01-04 01:34:05 |      3 |
| Registration | 4       | r1       | 4            | start              | 2017-01-04 01:34:04 |      4 |
| Registration | 5       | r1       | 5            | start              | 2017-01-04 16:07:47 |      5 |
| Registration | 6       | r1       | 6            | start              | 2017-01-04 16:07:47 |      6 |

A Transition System is any entity which can take various stasuses at
different times but can only be in one status at a time. Depending on
our definition of statuses, we can make changes to an eventlog so that
our custom define status column is built. These changes can include
filtering some rows or modifying or combining one or multiple columns.
In this example, we simply define completion of a task as a status, so
we remove all rows where `registration_type` is not `complete`.

Now feed the object with the eventlog you have and for this, you need to
specify the critical column
headers:

``` r
tsobj$feed.eventlog(patients_df %>% filter(registration_type == 'complete'), 
                    caseID_col = 'patient', 
                    status_col = 'handling',
                    startTime_col = 'time')
```

Now you have built your Transition System object and fed it with data.

### Adjacency matrix for a random walk analysis:

``` r
tsobj$get.adjacency(measure = 'rate')
```

    ## 
    ##  Aggregating links ...Done!

    ##                       START Registration Triage and Assessment Blood test
    ## START                     0            1                     0      0.000
    ## Registration              0            0                     1      0.000
    ## Triage and Assessment     0            0                     0      0.474
    ## Blood test                0            0                     0      0.000
    ## MRI SCAN                  0            0                     0      0.000
    ## Discuss Results           0            0                     0      0.000
    ## Check-out                 0            0                     0      0.000
    ## X-Ray                     0            0                     0      0.000
    ## END                     NaN          NaN                   NaN        NaN
    ##                        MRI SCAN Discuss Results Check-out X-Ray         END
    ## START                 0.0000000       0.0000000 0.0000000 0.000 0.000000000
    ## Registration          0.0000000       0.0000000 0.0000000 0.000 0.000000000
    ## Triage and Assessment 0.0000000       0.0000000 0.0000000 0.522 0.004000000
    ## Blood test            0.9957806       0.0000000 0.0000000 0.000 0.004219409
    ## MRI SCAN              0.0000000       1.0000000 0.0000000 0.000 0.000000000
    ## Discuss Results       0.0000000       0.0000000 0.9939394 0.000 0.006060606
    ## Check-out             0.0000000       0.0000000 0.0000000 0.000 1.000000000
    ## X-Ray                 0.0000000       0.9923372 0.0000000 0.000 0.007662835
    ## END                         NaN             NaN       NaN   NaN         NaN

If everything has successfully run up to here, you are ready to continue
with more complex analysis.
