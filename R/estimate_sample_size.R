#' Estimate the sample size necessary for determining whether there's a
#' significant difference between the control and test conditions - UNDER
#' CONSTRUCTION
#'
#' @description \code{estimate_sample_size} uses a DDI simulation (haven't set
#'   this up w/unpaired study design yet) with paired subjects to determine how
#'   large your sample size would need to be to reject the null hypothesis of no
#'   DDI. If alpha is 0.05, i.e., a 95\% confidence level, and 1 - beta is 0.8,
#'   i.e., 80\% power, this function answers the question: What is the estimated
#'   sample size needed such that, at the 95\% confidence level, the null
#'   hypothesis should be rejected 80\% percent of the time?
#'
#'   \strong{Notes on the statistics being used:} Under the hood, this is using
#'   the function pwr.t.test from the pwr package to calculate N. The statistics
#'   involved require an estimate of the effect size, which depends on the
#'   sample means and on the standard deviations. If you think the Simulator may
#'   underestimate the amount of variability present, you might want to see how
#'   things look if you only use 1 of the simulated trials rather than all of
#'   them.
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
#'   \item{"secondary metabolite",}} \strong{If you want more than one,
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
#' @param round_up_N TRUE (default) or FALSE for whether to round up to the
#'   nearest whole human
#' @param interaction_type What type of interaction was it? Default is
#'   "inhibition", and acceptable values are also "induction", "unknown" (e.g.,
#'   you've got multiple interactions at play and you're not sure which effect
#'   will dominate), or "two sided". This will determine which direction we're
#'   expecting PK values to go. Under the hood, this evaluates things similar to
#'   a one-sided t test, and the DDI value should be higher for AUC and Cmax if
#'   it's inhibition and lower if it's induction. Conversely, CL values should
#'   be lower for inhibition and higher for induction. If you set the
#'   interaction type to "unknown" or "two sided", this will assume that the
#'   direction expected is unknown and will evaluate things like a two-sided t
#'   test instead of a one-sided one.
#' @param sim_trials_to_include Which simulated trials should be included in the
#'   calculation? Options are all (default) to include all the data or any
#'   numbers in the trials included in your simulation, e.g., 1 or 5:10.
#' @param PK_data You can either supply the PK data we'll use for estimating the
#'   sample size or you can have us extract it for you. The PK data you supply
#'   here MUST be in the format you'd get from running \code{\link{extractPK}}
#'   and it MUST have both the baseline and the DDI version of each PK
#'   parameter, e.g., both AUCinf_dose1 and AUCinf_dose1_withInhib. If you
#'   supply something here, we'll only pay attention to the arguments
#'   sim_data_file, existing_exp_details, PKparameters, compoundToExtract,
#'   tissue, and sheet_PKparameters if what you supply doesn't have that same
#'   structure.
#' @param paired TRUE (default) or FALSE for whether the study design is paired,
#'   as in, the subjects are \emph{identical} between the two simulations.
#'   \strong{THIS IS AN IMPORTANT DISTINCTION AND WILL AFFECT HOW THE
#'   CALCULATIONS ARE PERFORMED!} An example of a paired study would be a DDI
#'   study where each subject has a measurement without the perpetrator of
#'   interest and then has a second measurement \emph{with} the perpetrator. The
#'   comparison is for repeated measurements of the \emph{same subject}. An
#'   example of an unpaired study design would be comparing healthy volunteers
#'   to subjects with hepatic impairment because those are measurements on
#'   \emph{different} subjects.
#' @param save_result file name for optionally saving the result of the
#'   calculation(s) as either an Excel file (file name must end in .xlsx) or a
#'   Word document (file name must end in .docx). May not contain special
#'   characters or symbols. If you save the output as a Word file, it will
#'   include background information on how the calculations were performed and
#'   possible wording for including in a report.
#'
#' @returns a data.frame of the sample sizes needed for each combination of
#'   simulation file, tissue, compound ID, and PK parameter.
#' @export
#' 

estimate_sample_size <- function(alpha = 0.05, 
                                 power = 0.8, 
                                 paired = TRUE, 
                                 interaction_type = NA, 
                                 PK_data = NA, 
                                 sim_data_file = NA, 
                                 existing_exp_details = NA, 
                                 PKparameters = c("AUCinf_dose1", 
                                                  "AUCt_dose1", 
                                                  "Cmax_dose1"), 
                                 compoundToExtract = "substrate", 
                                 tissue = "plasma", 
                                 sheet_PKparameters = NA, 
                                 round_up_N = TRUE, 
                                 sim_trials_to_include = "all", 
                                 save_result = NA){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   interaction_type <- tolower(interaction_type)[1]
   interaction_type <- ifelse(is.na(interaction_type), "two.sided", interaction_type)
   interaction_type <- case_when(str_detect(interaction_type, "two|2") ~ "two.sided", 
                                 interaction_type == "unknown" ~ "two.sided", 
                                 interaction_type == "inhibitor" ~ "inhibition", 
                                 interaction_type == "inducer" ~ "induction", 
                                 .default = interaction_type)
   
   if("list" %in% class(PK_data) && 
      "individual" %in% names(PK_data)){
      
      TidyPK <- PK_data$individual
      
      if("Sheet" %in% names(TidyPK) == FALSE){
         TidyPK$Sheet <- "default"
      }
      
      PKprovided <- TRUE
      
   } else {
      
      PKprovided <- FALSE 
      
      TidyPK <- tidy_input_PK(PKparameters = PKparameters, 
                              sim_data_files = sim_data_file, 
                              existing_exp_details = existing_exp_details, 
                              compoundsToExtract = compoundToExtract, 
                              tissues = tissue, 
                              sheet_PKparameters = sheet_PKparameters)
      
      existing_exp_details <- TidyPK$existing_exp_details
      
      TidyPK <- TidyPK$PKparameters %>% 
         mutate(Sheet = case_when(is.na(Sheet) ~ "default"))
   }
   
   GoodCompounds <- AllCompounds$CompoundID[AllCompounds$DDIrole == "victim"]
   
   if(any(TidyPK$CompoundID %in% GoodCompounds == FALSE)){
      
      BadCompounds <- setdiff(TidyPK$CompoundID, 
                              GoodCompounds)
      
      TidyPK <- TidyPK %>% filter(CompoundID %in% GoodCompounds)
      
      warning(wrapn(paste0("We can only get sample size estimates for the victim drug, so the following compounds will be ignored: ", 
                           str_comma(BadCompounds))), 
              call. = FALSE)
      
   }
   
   # Check whether pwr is installed and install if not.
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
   
   
   # Main body of function ---------------------------------------------------
   
   Out <- list()
   
   TidyPK <- split(TidyPK, 
                   f = list(TidyPK$File, 
                            TidyPK$Sheet, 
                            TidyPK$CompoundID, 
                            TidyPK$Tissue))
   
   for(i in names(TidyPK)){
      
      Out[[i]] <- list()
      
      if(PKprovided){
         MyPK <- list("individual" = TidyPK[[i]])
      } else {
         
         Params <- c(TidyPK[[i]]$PKparameter,
                     paste0(TidyPK[[i]]$PKparameter, "_withInhib"))
         Params <- unique(sub("_withInhib_withInhib", "_withInhib", Params))
         
         # Getting PK
         MyPK <- extractPK(sim_data_file = unique(TidyPK[[i]]$File), 
                           PKparameters = Params, 
                           compoundToExtract = unique(TidyPK[[i]]$CompoundID), 
                           tissue = unique(TidyPK[[i]]$Tissue), 
                           existing_exp_details = existing_exp_details)
      }
      
      TestData <- 
         MyPK$individual %>% 
         select(Individual, Trial, PKparameter, Value) %>% 
         mutate(Test = case_when(str_detect(PKparameter, "_withInhib") ~ "Test", 
                                 .default = "Control"), 
                PKparameter = sub("_withInhib", "", PKparameter))
      
      if(all(c("Control", "Test") %in% TestData$Test) == FALSE){
         stop(wrapn("Something has gone wrong with the PK data provided or extracted from your file because we cannot find both baseline and DDI PK data here. Please check your inputs and try again."), 
              call. = FALSE)
      }
      
      if(sim_trials_to_include == "all"){
         MyTrials <- unique(TestData$Trial)
      } else {
         MyTrials <- as.numeric(sim_trials_to_include)
      }
      
      if(any(is.na(MyTrials))){
         warning(wrapn("Something is amiss with what you've provided for which simulated trials to include, so we'll use the default of all the data."), 
                 call. = FALSE)
         MyTrials <- unique(TestData$Trial)
      }
      
      TestData <- TestData %>% 
         filter(Trial %in% MyTrials) %>% 
         pivot_wider(names_from = Test, 
                     values_from = Value) %>% 
         mutate(Difference = Test - Control)
      
      # Splitting by PK parameter
      TestData <- split(TestData, f = TestData$PKparameter)
      
      for(param in names(TestData)){
         
         if(any(is.na(c(TestData[[param]]$Control, 
                        TestData[[param]]$Test)))){
            warning(wrapn(paste0("There are missing values in the data for ", 
                                 param, " in the simulation '", 
                                 unique(TidyPK[[i]]$File), "' for the ", 
                                 unique(TidyPK[[i]]$CompoundID), " in ", 
                                 unique(TidyPK[[i]]$Tissue), 
                                 ", so we will have to skip this parameter.")), 
                    call. = FALSE)
            next
         }
         
         n1 <- length(TestData[[param]]$Test)
         n2 <- length(TestData[[param]]$Control)
         
         if(paired){
            
            # NB: This is d_ave_ in Lakens 2013 Front Psychol. 
            EffectSize <- mean(TestData[[param]]$Difference) / 
               ((sd(TestData[[param]]$Test) + sd(TestData[[param]]$Control))/2)
            
            # Listing other options for estimating Cohen's d for paired study
            # design for reference here:
            
            # d_z <- mean(TestData[[param]]$Difference) /
            #    sd(TestData[[param]]$Difference)
            # # Likely overestimates effect, based on my understanding. 
            # 
            # r <- cor(TestData[[param]]$Test,
            #          TestData[[param]]$Control)
            # d_rm <- mean(TestData[[param]]$Difference) /
            #    sqrt(sd(TestData[[param]]$Test)^2 +
            #            sd(TestData[[param]]$Control)^2 -
            #            2 * r * sd(TestData[[param]]$Test) * sd(TestData[[param]]$Control)) *
            #    sqrt(2 * (1-r))
            # # This is d_repeated measures and is likely too conservative. This
            # # is the calculation you get if you run: 
            # effsize::cohen.d(d = TestData[[param]]$Test, 
            #                  f = TestData[[param]]$Control, 
            #                  paired = T, hedges.correction = F)
            
            
         } else {
            
            PoolSD <- sqrt(((n1-1)*sd(TestData[[param]]$Test)^2 +
                               (n2-1)*sd(TestData[[param]]$Control)^2) / (n1 + n2 -2))
            
            EffectSize <- (mean(TestData[[param]]$Test) - 
                              mean(TestData[[param]]$Control)) / PoolSD
            
         }
         
         # Applying Hedge's g correction. This is effectively the same for both
         # paired and unpaired tests.
         EffSize_g <- EffectSize * (1 - (3/(4*(n1+n2)-9)))
         
         
         # This is asking the question will the difference of (control - test)
         # be "less" or "greater" than the baseline value.
         AltHyp <- case_when(
            interaction_type == "induction" & str_detect(param, "CL") ~ "greater", 
            interaction_type == "induction" & !str_detect(param, "CL") ~ "less", 
            interaction_type == "inhibition" & str_detect(param, "CL") ~ "less",
            interaction_type == "inhibition" & !str_detect(param, "CL") ~ "greater", 
            interaction_type == "two.sided" ~ interaction_type)
         
         N <- pwr::pwr.t.test(
            n = NULL, 
            d = EffSize_g, 
            type = ifelse(paired, "paired", "two.sample"), 
            alternative = AltHyp, 
            sig.level = alpha, 
            power = power)$n
         
         if(paired){
            Out[[i]][[param]] <- tibble(
               File = unique(TidyPK[[i]]$File), 
               Tissue = unique(TidyPK[[i]]$Tissue), 
               CompoundID = unique(TidyPK[[i]]$CompoundID), 
               Sheet = unique(TidyPK[[i]]$Sheet), 
               PKparameter = param, 
               `Mean of control` = round_consultancy(mean(TestData[[param]]$Control)), 
               `Standard deviation of control` = round_consultancy(sd(TestData[[param]]$Control)), 
               `Mean of test` = round_consultancy(mean(TestData[[param]]$Test)), 
               `Standard deviation of test` = round_consultancy(sd(TestData[[param]]$Test)), 
               `Mean difference` = round_consultancy(mean(TestData[[param]]$Difference)), 
               `Standard deviation of difference` = round_consultancy(sd(TestData[[param]]$Difference)),
               `Corrected Cohen's d` = signif(EffSize_g, 4), 
               alpha = alpha, 
               power = power, 
               `Alternative hypothesis` = AltHyp, 
               `N required` = N)
         } else {
            #FIXME - Set this up
         }
         
         suppressWarnings(rm(EffectSize, EffSize_g, N, AltHyp, n1, n2, PoolSD))
      }
      
      suppressWarnings(rm(TestData, MyPK, Params))
      
      Out[[i]] <- bind_rows(Out[[i]])
      
   }
   
   Out <- bind_rows(Out)
   
   if(round_up_N){
      Out <- Out %>% 
         mutate(`N required` = ceiling(`N required`))
   }
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_result)){
      
      FileName <- save_result
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("xlsx", "docx") == FALSE){
            warning(wrapn(paste0("The only option for file extensions for saving the result is .docx or .xlsx, and you provided some other file extension. We'll save your file as a .docx file.")),
                    call. = FALSE)
            Ext <- "xlsx"
         }
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".docx")
         Ext <- "docx"
      }
      
      OutPath <- dirname(FileName)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(FileName)
      
      if(Ext == "docx"){
         
         TemplatePath <- system.file("Word/landscape_report_template.dotx",
                                     package="SimcypConsultancy")
         
         if(paired){
            rmarkdown::render(system.file("rmarkdown/templates/power-n-paired/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                              output_dir = OutPath, 
                              output_file = FileName, 
                              quiet = TRUE)
            
         } else {
            
            rmarkdown::render(system.file("rmarkdown/templates/power-n-unpaired/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                              output_dir = OutPath, 
                              output_file = FileName, 
                              quiet = TRUE)
            # Note: The "system.file" part of the call means "go to where the
            # package is installed, search for the file listed, and return its
            # full path.
            
         }
      } else {
         save_table_to_Excel(table = Out, save_table = FileName, 
                             output_tab_name = "Power calc for N")
      } 
   }
   
   
   return(Out)
   
}



