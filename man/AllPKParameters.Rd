% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{AllPKParameters}
\alias{AllPKParameters}
\title{All possible PK parameters for \code{\link{extractPK}} (version for coding
purposes)}
\format{
A data.frame with 6 columns: \describe{\item{PKparameter}{the PK
  parameter name to use with \code{\link{extractPK}} for the argument
  \code{PKparameters}}

  \item{BasePKparameter}{PKparameter without any mention of whether it was
  in the presence of a perpetrator}

  \item{PKparameter_nodosenum}{PKparameter without any mention of the dose
  number. This is for matching up prettified names when the user has
  requested that no dose number be shown, e.g., when all the doses were for
  dose 1, it's redundant to see "Dose 1" on all of the table columns. This is
  DISTINCT from PK parameters that are specifically for user-defined AUC
  intervals. See column "UserInterval". }

  \item{Sheet}{the sheet in a simulator output file
  where the PK parameter will be extracted, if possible}
  \item{AppliesToSingleDose}{TRUE or FALSE for whether this item applies only
  to single-dose data}

  \item{SearchText}{Regex to use for finding the parameter}

  \item{AUCtab_StartColText}{For the older AUC tabs, you had to look at one
  row above the main row to figure out which sets of PK values were for
  baseline, which were for the DDI, which were in plasma, which were in
  blood, etc., so this addresses at least the DDI part of the question.}

  \item{AppliesToSingleDose}{TRUE or FALSE for whether this applies to
  scenarios where only a single dose was administered. Note that all
  UserInterval parameters have this set to true b/c you could conceivably
  have a situation where someone had a user-defined interval that was
  shorter than the full simulation.}

  \item{AppliesOnlyWhenPerpPresent}{TRUE or FALSE
  for whether this item only applies when a perpetrator is present in the
  simulation}

  \item{UserInterval}{Applies specifically to user-defined interval data}

  \item{SwitchWhenInhib}{Sometimes, the regex changes from what is used for
  the substrate when the compound of interest is a perpetrator. This notes
  what the regex changes to in that situation.}

  \item{SortOrder}{the order to arrange columns for pk_table}
  
  \item{PrettifiedNames}{Pretty names to use in tables, etc.}
  
  \item{PrettifiedNames_nodosenum}{Pretty names to use in tables, etc., when
  the user doesn't want the dose number included.}

  \item{Notes}{an explanation of what
  the parameter is}

  }
}
\usage{
AllPKParameters
}
\description{
All possible PK parameters that can be extracted from a simulator output file
using \code{\link{extractPK}}. For the more user-friendly version of this
data.frame, which omits columns used only for coding purposes, please see
\code{\link{PKParameterDefinitions}}.
}
\keyword{datasets}
