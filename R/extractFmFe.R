#' Extract fm and fe values that change with time from a Simulator output Excel
#' file
#'
#' \code{extractFmFe} extracts fm and fe data from a simulator output Excel
#' file. A tab named "Time variant \%fm and fe" must be present to get dynamic
#' fm and fe values, and a tab named "\% fm and fe SS" must be present to get
#' static values. Dynamic fm and fe data are required if you want to use these
#' data with the function \code{\link{fm_plot}} to make a graph of how the fm
#' values change over time.
#'
#' @param sim_data_files names of the Excel files containing the simulated
#'   fm and fe data, in quotes
#' @param returnOnlyMaxMin TRUE (default) or FALSE for whether to return only
#'   maximum and minimum fm values for dynamic fm values -- basically, return
#'   the table in the upper left corner of the "Time variant \%fm and fe" tab.
#' @param static_or_dynamic get "static" or "dynamic" (default) fm values.
#'   "static" will retrieve information from the "\% fm and fe SS" tab, if
#'   present, and "dynamic" will retrieve it from the "Time variant \%fm and fe"
#'   tab.
#' @param returnAggregateOrIndiv Return aggregate and/or individual simulated fm
#'   and fe data? This currently only applies to the dynamic fms and has not yet
#'   been developed for the static fms, which will only return aggregate data at
#'   present. Options are "individual", "aggregate", or "both" (default).
#'   Aggregated data are not calculated here but are pulled from the simulator
#'   output rows labeled as "mean".
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#'
#' @return a data.frame
#'
#' @export
#' @examples
#' # None yet
#' 


extractFmFe <- function(sim_data_files,
                        static_or_dynamic = "dynamic", 
                        returnOnlyMaxMin = TRUE, 
                        returnAggregateOrIndiv = "both", 
                        existing_exp_details = NA){
   
   # Error catching --------------------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   sim_data_files <- paste0(sub("\\.wksz$|\\.dscw$|\\.xlsx$", "", 
                                sim_data_files), ".xlsx")
   
   static_or_dynamic <- tolower(static_or_dynamic)[1]
   if(static_or_dynamic %in% c("static", "dynamic") == FALSE){
      warning(wrapn("You have supplied something other than 'static' or 'dynamic' for the argument 'static_or_dynamic', and those are the only options. We'll use the default of 'dynamic'."), 
              call. = FALSE)
      static_or_dynamic <- "dynamic"
   }
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(sim_data_files)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   if(any(c(length(returnAggregateOrIndiv) < 1,
            length(returnAggregateOrIndiv) > 2,
            any(unique(returnAggregateOrIndiv) %in% c("aggregate", "individual", "both") == FALSE)))) {
      stop("returnAggregateOrIndiv must be 'aggregate', 'individual', or 'both'.",
           call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------------------
   
   Out <- list()
   
   for(ff in sim_data_files){
      Out[[ff]] <- extractFmFe_subfun(
         sim_data_file = ff, 
         static_or_dynamic = static_or_dynamic, 
         returnOnlyMaxMin = returnOnlyMaxMin, 
         returnAggregateOrIndiv = returnAggregateOrIndiv, 
         existing_exp_details = existing_exp_details)
   }
   
   Out <- bind_rows(Out)
   
   return(Out)
   
}


