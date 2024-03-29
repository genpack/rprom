% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfg.R
\docType{class}
\name{SnapshotFeatureGenerator-class}
\alias{SnapshotFeatureGenerator-class}
\alias{SnapshotFeatureGenerator}
\title{SnapshotFeatureGenerator: A reference class for generating snapshot features from event-logs.}
\description{
Reference class containing some properties and methods required 
for feeding event-logs and generating features based on the given feature settings.
}
\section{Fields}{

\describe{
\item{\code{settings}}{\code{list} list of configuration settings.}

\item{\code{eventlogs}}{\code{list} list of tables or data.frames containing all event-logs fed in addition to 
some meta-data associated with each event-log. These data are:

\code{eventType_attributes}: \code{data.frame} Distinct values of all eventTypes and attributes in the event-log.
\code{address}: \code{character} where is the event-log located. 
A path, url or a combination of dataset and table name to locate the event-log.}
}}


