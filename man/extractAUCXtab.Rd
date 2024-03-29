% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractAUCXtab.R
\name{extractAUCXtab}
\alias{extractAUCXtab}
\title{FOR INTERNAL USE ONLY: Extract PK data from a generic AUC tab, e.g., NOT one
formatted like the "AUC" tab from Simcyp V21 and earlier}
\usage{
extractAUCXtab(
  PKparameters,
  PKparameters_orig,
  compoundToExtract = "substrate",
  sim_data_file,
  Sheet,
  PKset,
  UserSpecified,
  Deets,
  DataCheck,
  includeTrialInfo
)
}
\arguments{
\item{PKparameters}{parameters to extract; character vector only (a set of
parameters like in extractPK is not acceptable here)}

\item{PKparameters_orig}{original PK parameters from extractPK function}

\item{compoundToExtract}{For which compound do you want to extract
PK data? Options are: \itemize{\item{"substrate"
(default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
\item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
inducer, inhibitor, activator, or suppresesor, but it's labeled as
"Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
metabolite of inhibitor 1}}}

\item{sim_data_file}{simulator output Excel file to read}

\item{Sheet}{sheet to read from the Excel file}

\item{PKset}{which set of PK parameters to use. Options: AUC0, AUClast, AUCX}

\item{UserSpecified}{TRUE or FALSE for whether this was a user-specified
sheet, in which case we don't know which dose it is}

\item{Deets}{output from running extractExpDetails on the provided
sim_data_file and setting \code{Deets <- existing_exp_details$MainDetails}}

\item{DataCheck}{DataCheck data.frame}

\item{includeTrialInfo}{TRUE or FALSE}
}
\value{
a list for use with the extractPK function
}
\description{
FOR INTERNAL USE ONLY: Extract PK data from a generic AUC tab, e.g., NOT one
formatted like the "AUC" tab from Simcyp V21 and earlier
}
\examples{
# none

}
