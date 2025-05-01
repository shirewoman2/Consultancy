#' Estimate the sample size necessary for determining whether there's a
#' significant DDI - UNDER CONSTRUCTION
#'
#' @description \code{estimate_sample_size_DDI} uses a DDI simulation with
#'   paired subjects to determine how large your sample size would need to be to
#'   reject the null hypothesis of no DDI. If alpha is 0.05, i.e., a 95\%
#'   confidence level, and 1 - beta is 0.8, i.e., 80\% power, this function
#'   answers the question: What is the estimated sample size needed such that,
#'   at the 95\% confidence level, the null hypothesis should be rejected 80\%
#'   percent of the time?
#'
#' @param alpha alpha required, default is 0.05 for the 95\% confidence level
#' @param power power (1-beta) required, default is 0.8.
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes. \strong{If you want more than one,
#'   please supply a data.frame or .csv file to the argument \code{PKparameters}.}
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param PKparameters the PK parameters to include. For this, you only need to
#'   list baseline parameters; we'll find the matching DDI one for each. There
#'   are two main options for supplying this information: 1) supply a file to
#'   read or a data.frame (R speak for "a table") that lists which simulation
#'   files, compounds, tissues, and PK you want or 2) supply a character vector
#'   of which PK parameters you want and then also specify what you need in
#'   terms of which tissues, which compounds, which simulation files, and which
#'   tab to get the data from with the arguments \code{tissues},
#'   \code{compoundsToExtract}, \code{sim_data_files}, and
#'   \code{sheet_PKparameters}.
#'   \strong{Details on each option:} \describe{
#'
#'   \item{\strong{Option 1: }a file to read or a data.frame}{This
#'   is the most versatile option and, we think, the clearest in terms of
#'   getting what you expected. Please try running \code{\link{make_example_PK_input}}
#'   to see examples for how to set up a csv or Excel file or data.frame to
#'   specify exactly which simulation file should get which PK parameter from
#'   which tissue and, when user-specified intervals are involved, from which
#'   tab in the Excel file those data should be pulled. Whatever you supply, the
#'   columns that will be read are: \itemize{\item{"File" (same thing as the argument
#'   \code{sim_data_files})} \item{"Sheet" (same thing as the argument
#'   \code{sheet_PKparameters})} \item{"Tissue" (same as the argument \code{tissues})}
#'   \item{"CompoundID" (same as the argument \code{compoundsToExtract})
#'   \item{"ObsValue" for any observed data (no equivalent argument)}
#'   \item{"Variability" for any observed variability (no equivalent argument
#'   here, either)}} If you
#'   omit any of those columns, whatever you supply for their respective
#'   arguments will be used instead. If you supply something for one of them
#'   in the data.frame or file and \emph{also} something for its argument, the
#'   argument will be ignored. \cr
#'
#'   Here is how to specify each of the possible
#'   inputs for Option 1:\describe{\item{a csv file}{list the file
#'   name, including the file extension ".csv" inside quotes, e.g., "PK needed.csv"}
#'
#'   \item{an Excel file}{list the file name, including the file extension
#'   ".xlsx", inside quotes, e.g., "PK needed.xlsx". We will be looking for a
#'   tab titled "PKparameters" (all one word and with the same capitalization).}
#'
#'   \item{a data.frame}{If you'd like to supply a data.frame with the same
#'   columns you would have had in the .csv or Excel file, that works just the
#'   same.}}}}
#'
#'   \item{\strong{Option 2: }specify the PK parameters you want for all your
#'   simulations}{This is a good option when you want the same information
#'   from all your simulations. List the PK parameters you want here and then,
#'   in the arguments
#'   \code{tissues}, \code{compoundsToExtract}, \code{sim_data_files}, and
#'   \code{sheet_PKparameters} specify which of each of those items you want.
#'   You'll get all possible combinations of these, so, for example, if you say
#'   \code{PKparameters = c("AUCinf_dose1", "Cmax_dose1")} and
#'   \code{tissues = c("blood", "plasma")}, you'll get the dose 1 AUCinf and
#'   Cmax for both blood and plasma for all the simulation files you list with
#'   \code{sim_data_files}. If you're going this route, here are the two options
#'   you have for the argument \code{PKparameters}: \describe{
#'
#'   \item{NA}{If you leave this as NA, by default, if you have a single-dose
#'   simulation, the parameters will
#'   include AUC and Cmax for dose 1, or, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have a
#'   perpetrator present, the parameters will include the AUC and Cmax values with
#'   and without the perpetrator as well as those ratios.}
#'
#'   \item{a character vector of any combination of specific, individual
#'   parameters}{This character vector must contain SimcypConsultancy package
#'   coded names for each parameter you want, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}. Please try running \code{\link{make_example_PK_input}} to
#'   see examples, or, to see the full set of all possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console.}}}}
#'
#'   Parameters that don't make sense for your scenario -- such as asking for
#'   \code{AUCinf_dose1_withInhib} when your simulation did not include a
#'   perpetrator -- will be ignored.
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}} \strong{If you want more than one,
#'   please supply a data.frame or .csv file to the argument \code{PKparameters}.}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", "unbound blood",
#'   "peripheral plasma", or "peripheral blood". \strong{If you want more than one,
#'   please supply a data.frame or .csv file to the argument \code{PKparameters}.}
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a \strong{user-defined interval tab} in the simulator output
#'   file, list that tab here. Otherwise, this should be left as NA.
#'   \code{sheet_PKparameters} can only have a \emph{single value}, though. If
#'   you want some parameters from a custom-interval tab and others from the
#'   regular tabs, you must supply that as part of a data.frame or csv file for
#'   the argument \code{PKparameters}. Please try running
#'   \code{\link{make_example_PK_input}} to see examples for how to do this.
#'   \strong{If you want more than one,
#'   please supply a data.frame or .csv file to the argument \code{PKparameters}.}
#'
#' @returns a data.frame of the sample sizes needed for each combination of
#'   simulation file, tissue, compound ID, and PK parameter.
#' @export
#' 

estimate_sample_size_DDI <- function(alpha = 0.05, 
                           power = 0.8, 
                           sim_data_file = NA, 
                           existing_exp_details = NA, 
                           PKparameters = c("AUCinf_dose1", 
                                            "AUCt_dose1", 
                                            "Cmax_dose1"), 
                           compoundToExtract = "substrate", 
                           tissue = "plasma", 
                           sheet_PKparameters = NA){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   TidyPK <- tidy_input_PK(PKparameters = PKparameters, 
                           sim_data_files = sim_data_file, 
                           existing_exp_details = existing_exp_details, 
                           compoundsToExtract = compoundToExtract, 
                           tissues = tissue, 
                           sheet_PKparameters = sheet_PKparameters)
   
   existing_exp_details <- TidyPK$existing_exp_details
   
   TidyPK <- TidyPK$PKparameters %>% 
      mutate(Sheet = case_when(is.na(Sheet) ~ "default"))
   
   # Check whether pwr and effsize installed and install if not.
   if(length(find.package("pwr", quiet = TRUE)) == 0){
      message(paste0("\n", wrapn("We need to install the package pwr to perform the power calculation.")))
      Install <- readline(prompt = wrapn("Is it ok to install pwr for you? (y or n)   "))
      
      if(tolower(str_sub(Install, 1, 1)) == "y"){
         install.packages("pwr")
      } else {
         stop(wrapn("Ok, we won't install the pwr package, but we cannot perform your calculations."), 
              call. = FALSE)
      }
   }
   
   if(length(find.package("effsize", quiet = TRUE)) == 0){
      message(paste0("\n", wrapn("We need to install the package effsize to perform the power calculation.")))
      Install <- readline(prompt = wrapn("Is it ok to install effsize for you? (y or n)   "))
      
      if(tolower(str_sub(Install, 1, 1)) == "y"){
         install.packages("effsize")
      } else {
         stop(wrapn("Ok, we won't install the effsize package, but we cannot perform your calculations."), 
              call. = FALSE)
      }
   }
   
   
   # Main body of function ---------------------------------------------------
   
   Out <- list()
   
   TidyPK <- split(TidyPK, 
                   f = list(TidyPK$File, 
                            TidyPK$Sheet, 
                            TidyPK$CompoundID, 
                            TidyPK$Tissue))
   
   for(i in names(TidyPK)){
      
      Out[[i]] <- list()
      
      Params <- c(TidyPK[[i]]$PKparameter,
                  paste0(TidyPK[[i]]$PKparameter, "_withInhib"))
      Params <- unique(sub("_withInhib_withInhib", "_withInhib", Params))
      
      # Getting PK
      MyPK <- extractPK(sim_data_file = unique(TidyPK[[i]]$File), 
                        PKparameters = Params, 
                        compoundToExtract = unique(TidyPK[[i]]$CompoundID), 
                        tissue = unique(TidyPK[[i]]$Tissue), 
                        existing_exp_details = existing_exp_details)
      
      TestData <- 
         MyPK$individual %>% 
         select(Individual, PKparameter, Value) %>% 
         mutate(DDI = case_when(str_detect(PKparameter, "_withInhib") ~ "DDI", 
                                .default = "BL"), 
                PKparameter = sub("_withInhib", "", PKparameter)) %>% 
         pivot_wider(names_from = DDI, 
                     values_from = Value) %>% 
         mutate(Difference = DDI - BL)
      
      # Splitting by PK parameter
      TestData <- split(TestData, f = TestData$PKparameter)
      
      for(param in names(TestData)){
         
         if(any(is.na(c(TestData[[param]]$BL, 
                        TestData[[param]]$DDI)))){
            warning(wrapn(paste0("There are missing values in the data for ", 
                                 param, " in the simulation '", 
                                 unique(TidyPK[[i]]$File), "' for the ", 
                                 unique(TidyPK[[i]]$CompoundID), " in ", 
                                 unique(TidyPK[[i]]$Tissue), 
                                 ", so we will have to skip this parameter.")), 
                    call. = FALSE)
            next
         }
         
         EffectSize <- effsize::cohen.d(TestData[[param]]$BL, 
                                        TestData[[param]]$DDI)$estimate
         
         N <- pwr::pwr.t.test(n = NULL, d = EffectSize, 
                              sig.level = alpha, power = power)$n
         
         Out[[i]][[param]] <- tibble(
            File = unique(TidyPK[[i]]$File), 
            Tissue = unique(TidyPK[[i]]$Tissue), 
            CompoundID = unique(TidyPK[[i]]$CompoundID), 
            Sheet = unique(TidyPK[[i]]$Sheet), 
            PKparameter = param, 
            Mean_baseline = mean(TestData[[param]]$BL), 
            Mean_DDI = mean(TestData[[param]]$DDI), 
            Mean_difference = mean(TestData[[param]]$Difference), 
            alpha = alpha, 
            power = power, 
            N_required = N)
         
         rm(EffectSize, N)
      }
      
      rm(TestData, MyPK, Params)
      
      Out[[i]] <- bind_rows(Out[[i]])
      
   }
   
   Out <- bind_rows(Out)
   
   return(Out)
   
}



