#' Estimate the sample size necessary for determining whether there's a
#' significant difference between the PK under control and test conditions -
#' UNDER CONSTRUCTION
#'
#' @description \code{estimate_sample_size} compares simulated PK data under a
#'   control condition with PK data under a test condition to determine how
#'   large your sample size would need to be to reject the null hypothesis of no
#'   difference between the two. The comparison may be paired -- e.g., comparing
#'   baseline to DDI -- or unpaired -- e.g., comparing healthy subjects to those
#'   with hepatic impairment. If alpha is 0.05, i.e., a 95\% confidence level,
#'   and 1 - beta is 0.8, i.e., 80\% power, this function answers the question:
#'   What is the estimated sample size needed such that, at the 95\% confidence
#'   level, the null hypothesis should be rejected 80\% percent of the time?
#'
#'   \strong{Notes on the statistics being used:} Under the hood, this function
#'   will estimate the Cohen's d effect size based on the simulated data,
#'   correct with the Hedge's correction g, and then use the function pwr.t.test
#'   from the pwr package to calculate N. For references, please see:
#'   \itemize{\item{Cohen, J. (1988). Statistical power analysis for the
#'   behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.}
#'
#'   \item{Lakens, D. (2013). “Calculating and
#'   reporting effect sizes to facilitate cumulative science: a practical primer
#'   for t-tests and ANOVAs.” Frontiers in Psychology, 4:863. doi: 10.3389/fpsyg.2013.00863}}
#'
#'   If you save the results from running this as a Word file, that document
#'   will contain more information on the methods and possible text to include
#'   in a report.
#'
#' @param alpha alpha required, default is 0.05 for the 95\% confidence level
#' @param power power (1-beta) required, default is 0.8.
#' @param sim_data_files name of the Excel file(s) containing the simulator
#'   output, in quotes.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param PKparameters the PK parameters to include. There are two main options
#'   for supplying this information: 1) supply a file to read or a data.frame (R
#'   speak for "a table") that lists which simulation files, compounds, tissues,
#'   and PK you want or 2) supply a character vector of which PK parameters you
#'   want and then also specify what you need in terms of which tissues, which
#'   compounds, which simulation files, and which tab to get the data from with
#'   the arguments \code{tissues}, \code{compoundsToExtract},
#'   \code{sim_data_files}, and \code{sheet_PKparameters}.
#'   \strong{Details on each option:} \describe{
#'
#'   \item{\strong{Option 1:} a file to read or a data.frame}{This
#'   is the most versatile option and, we think, the clearest in terms of
#'   getting what you expected. Please try running \code{\link{make_example_PK_input}}
#'   to see examples for how to set up a csv or Excel file or data.frame to
#'   specify exactly which simulation file should get which PK parameter from
#'   which tissue and, when user-specified intervals are involved, from which
#'   tab in the Excel file those data should be pulled. If you want to make
#'   comparisons across anything other than a standard DDI simulation, you
#'   should use this option to specify exactly what should be compared for each
#'   simulation. You'll need to set this up as if you're comparing ratios of
#'   PK parameters (answer "2" to the 1st question if you run
#'   \code{make_example_PK_input}), and you'll set the control scenario to be
#'   the denominator and the test scenario to be the numerator.
#'
#'   Whatever you supply, the
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
#' @param paired TRUE (default) or FALSE for whether the study design was
#'   paired, as in, the subjects were \emph{identical} between the two
#'   simulations.
#'   \strong{THIS IS AN IMPORTANT DISTINCTION AND WILL AFFECT HOW THE
#'   CALCULATIONS ARE PERFORMED!} An example of a paired study would be a DDI
#'   study where each subject has a measurement without the perpetrator of
#'   interest and then has a second measurement \emph{with} the perpetrator. The
#'   comparison is for repeated measurements of the \emph{same subject}. An
#'   example of an unpaired study design would be comparing healthy subjects to
#'   subjects with hepatic impairment because those are measurements on
#'   \emph{different} subjects.
#' @param alternative_hypothesis Is the test condition expected to be greater
#'   than or less than the control or is that unknown? Acceptable values:
#'   \describe{\item{"two sided" (default)}{unknown \emph{a priori} whether the
#'   test condition will be greater or less than control, i.e., a two-sided
#'   t test would be used to compare the groups}
#'
#'   \item{"less"}{test condition is expected to be less than control and a
#'   one-sided t test would be used to compare the groups. Example: Comparing
#'   the AUC in a DDI study with an inducer.}
#'
#'   \item{"greater"}{test condition is expected to be greater than control and a
#'   one-sided t test would be used to compare the groups. Example: Comparing
#'   the AUC in a DDI study with an inhibitor.}}
#'
#' @param sim_trials_to_include Which simulated trials should be included in the
#'   calculation? Options are all (default) to include all the data or any
#'   numbers in the trials included in your simulation, e.g., 1 or 5:10.
#' @param PK_data You can either supply the PK data we'll use for estimating the
#'   sample size or you can have us extract it for you. The PK data you supply
#'   here MUST be in the format you'd get from running \code{\link{extractPK}}
#'   and it MUST have both the baseline and the DDI version of each PK
#'   parameter, e.g., both AUCinf_dose1 and AUCinf_dose1_withInhib. If you
#'   supply something here, we'll only pay attention to the arguments
#'   sim_data_files, existing_exp_details, PKparameters, compoundToExtract,
#'   tissue, and sheet_PKparameters if what you supply doesn't have that same
#'   structure. This ONLY works with the simplest of possible comparisons: a
#'   standard DDI simulation.
#' @param save_result file name for optionally saving the results as either an
#'   Excel file (file name must end in .xlsx) or a Word document (file name must
#'   end in .docx). May not contain special characters or symbols. If you save
#'   the output as a Word file, it will include background information on how
#'   the calculations were performed and possible wording for including in a
#'   report.
#' @param log_transform TRUE (default) or FALSE for whether to log tranform the
#'   data before calculating the effect size and number of subjects required.
#'   Most PK parameters are log-normally distributed, so most of the time, you
#'   probably want this to be TRUE.
#'
#' @returns a data.frame of the sample sizes needed for each combination of
#'   simulation file, tissue, compound ID, and PK parameter.
#' @export
#' 

estimate_sample_size <- function(
      sim_data_files = NA, 
      PKparameters = c("AUCinf_dose1", 
                       "AUCt_dose1"), 
      alpha = 0.05, 
      power = 0.8, 
      paired = TRUE, 
      alternative_hypothesis = "two sided", 
      log_transform = TRUE, 
      existing_exp_details = NA, 
      compoundToExtract = "substrate", 
      tissue = "plasma", 
      sheet_PKparameters = NA, 
      PK_data = NA, 
      round_up_N = TRUE, 
      sim_trials_to_include = "all", 
      save_result = NA){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   alternative_hypothesis <- tolower(alternative_hypothesis)[1]
   alternative_hypothesis <- ifelse(is.na(alternative_hypothesis), 
                                    "two.sided", alternative_hypothesis)
   alternative_hypothesis <- case_when(
      str_detect(alternative_hypothesis, "two|2") ~ "two.sided", 
      alternative_hypothesis == "unknown" ~ "two.sided", 
      str_detect(alternative_hypothesis, "less") ~ "less", 
      str_detect(alternative_hypothesis, "great") ~ "greater", 
      .default = alternative_hypothesis)
   
   if(alternative_hypothesis %in% c("less", "greater", "two.sided") == FALSE){
      warning(wrapn("You have supplied a value for the alternative hypothesis other than the acceptable options of 'two sided', 'less', or 'greater', so we will use the default of 'two sided'."), 
              call. = FALSE)
      alternative_hypothesis <- "two.sided"
   }
   
   if("list" %in% class(PK_data) && 
      "individual" %in% names(PK_data)){
      
      InputPK <- PK_data$individual
      
      if("Sheet" %in% names(InputPK) == FALSE){
         InputPK$Sheet <- "default"
      }
      
      PKprovided <- TRUE
      
   } else {
      
      PKprovided <- FALSE 
      
      if("character" %in% class(PKparameters) & 
         length(PKparameters) == 1 &&
         str_detect(PKparameters, "csv$")){
         PKparameters <- read.csv(PKparameters)
         
         PKwasDF <- TRUE
      }
      
      if("character" %in% class(PKparameters) & 
         paired == TRUE){
         
         PKparameters <- c(PKparameters, paste0(PKparameters, "_withInhib"))
         PKparameters <- unique(sub("_withInhib_withInhib", "_withInhib", PKparameters))
         
         PKwasDF <- FALSE
      } else {
         PKwasDF <- TRUE
      }
      
      InputPK <- tidy_input_PK(PKparameters = PKparameters, 
                               sim_data_files = sim_data_files, 
                               existing_exp_details = existing_exp_details, 
                               compoundsToExtract = compoundToExtract, 
                               tissues = tissue, 
                               sheet_PKparameters = sheet_PKparameters)
      
      existing_exp_details <- InputPK$existing_exp_details
      
      InputPK <- InputPK$PKparameters %>% 
         mutate(Sheet = case_when(is.na(Sheet) ~ "default"))
      
      # Adjusting to make this like calc_PK_ratios format
      if(PKwasDF == FALSE){
         
         # Denominator is control. Numerator is test. 
         InputPK <- InputPK %>% 
            mutate(PKparameter = sub("_withInhib", "", PKparameter)) %>% 
            unique() %>% 
            rename(Denominator_File = File, 
                   Denominator_Sheet = Sheet, 
                   Denominator_CompoundID = CompoundID, 
                   Denominator_PKparameter = PKparameter, 
                   Denominator_Tissue = Tissue) %>% 
            mutate(Numerator_File = Denominator_File, 
                   Numerator_Sheet = Denominator_Sheet, 
                   Numerator_CompoundID = Denominator_CompoundID, 
                   Numerator_PKparameter = paste0(Denominator_PKparameter, "_withInhib"), 
                   Numerator_Tissue = Denominator_Tissue, 
                   PairID = 1:nrow(.)) %>% 
            select(PairID, matches("File"), 
                   matches("Sheet"), matches("PKparameter"), 
                   matches("Tissue"), matches("CompoundID"))
         
      } else {
         InputPK <- InputPK %>% 
            select(matches("File"), matches("PKparameter"), 
                   matches("Tissue"), matches("CompoundID")) %>% 
            mutate(PairID = 1:nrow(.))
      }
   }
   
   Pairs <- InputPK
   
   # Now that everything is tidy and harmonized and we have pair names, need to
   # pivot longer to get all the possible PK we need.
   InputPK <- InputPK %>% 
      pivot_longer(cols = -PairID, 
                   names_to = "Parameter", 
                   values_to = "Value") %>% 
      separate_wider_delim(cols = Parameter, 
                           delim = "_", 
                           names = c("Group", "Parameter")) %>% 
      mutate(Group = case_match(Group, 
                                "Denominator" ~ "Control", 
                                "Numerator" ~ "Test")) %>% 
      pivot_wider(names_from = Parameter, 
                  values_from = Value)
   
   
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
   
   
   TidyPK <- list()
   
   InputPK <- split(InputPK, 
                    f = list(InputPK$File, 
                             InputPK$Sheet, 
                             InputPK$CompoundID, 
                             InputPK$Tissue))
   
   for(i in names(InputPK)){
      
      if(PKprovided){
         TidyPK[[i]] <- list("individual" = InputPK[[i]])
         
      } else {
         # Getting PK
         TEMP <- extractPK(
            sim_data_file = unique(InputPK[[i]]$File), 
            PKparameters = unique(InputPK[[i]]$PKparameter), 
            compoundToExtract = unique(InputPK[[i]]$CompoundID), 
            tissue = unique(InputPK[[i]]$Tissue), 
            existing_exp_details = existing_exp_details)
         
         TidyPK[[i]] <- TEMP$individual %>% 
            mutate(Sheet = ifelse(str_detect(PKparameter, "_dose1|_last"), 
                                  "default", unique(TEMP$QC$Tab)))
         
         rm(TEMP)
      }
   }
   
   TidyPK <- bind_rows(TidyPK)
   
   if(log_transform){
      TidyPK$Value <- log(TidyPK$Value)
   }
   
   
   ## Making comparisons ----------------------------------------------------
   
   Out <- list()
   
   for(i in 1:nrow(Pairs)){
      
      PK_control <- TidyPK %>%
         filter(File == Pairs$Denominator_File[i] & 
                   Sheet == Pairs$Denominator_Sheet[i] & 
                   Tissue == Pairs$Denominator_Tissue[i] & 
                   CompoundID == Pairs$Denominator_CompoundID[i] & 
                   PKparameter == Pairs$Denominator_PKparameter[i]) %>% 
         select(-Compound, -Dose) %>% 
         rename(Denominator_File = File, 
                Denominator_Sheet = Sheet, 
                Denominator_Tissue = Tissue, 
                Denominator_CompoundID = CompoundID, 
                Denominator_PKparameter = PKparameter, 
                Control = Value)
      
      PK_test <- TidyPK %>%
         filter(File == Pairs$Numerator_File[i] & 
                   Sheet == Pairs$Numerator_Sheet[i] & 
                   Tissue == Pairs$Numerator_Tissue[i] & 
                   CompoundID == Pairs$Numerator_CompoundID[i] & 
                   PKparameter == Pairs$Numerator_PKparameter[i]) %>% 
         select(-Compound, -Dose) %>% 
         rename(Numerator_File = File, 
                Numerator_Sheet = Sheet, 
                Numerator_Tissue = Tissue, 
                Numerator_CompoundID = CompoundID, 
                Numerator_PKparameter = PKparameter, 
                Test = Value)
      
      if(paired){
         
         PK_temp <- 
            full_join(PK_control, PK_test, 
                      by = c("Individual", "Trial", "Simulated"))
         
         if(all(c("Control", "Test") %in% names(PK_temp)) == FALSE){
            stop(wrapn("Something has gone wrong with the PK data provided or extracted from your file because we cannot find both control and test PK data here. Please check your inputs and try again."), 
                 call. = FALSE)
         }
         
         if(sim_trials_to_include == "all"){
            MyTrials <- unique(PK_temp$Trial)
         } else {
            MyTrials <- as.numeric(sim_trials_to_include)
         }
         
         if(any(is.na(MyTrials))){
            warning(wrapn("Something is amiss with what you've provided for which simulated trials to include, so we'll use the default of all the data."), 
                    call. = FALSE)
            MyTrials <- unique(PK_temp$Trial)
         }
         
         PK_temp <- PK_temp %>% 
            filter(Trial %in% MyTrials) %>% 
            mutate(Difference = Test - Control)
         
         if(any(is.na(c(PK_temp$Control, 
                        PK_temp$Test)))){
            warning(wrapn(paste0("There are missing values in the data for ", 
                                 unique(InputPK[[i]]$PKparameter), " in the simulation '", 
                                 unique(InputPK[[i]]$File), "' for the ", 
                                 unique(InputPK[[i]]$CompoundID), " in ", 
                                 unique(InputPK[[i]]$Tissue), 
                                 ", so we will have to skip this parameter.")), 
                    call. = FALSE)
            next
         }
         
         x1 <- PK_temp$Control
         x2 <- PK_temp$Test
         
      } else {
         
         x1 <- PK_control$Value
         x2 <- PK_test$Value
         
      }
      
      n1 <- length(x1)
      n2 <- length(x2)
      
      if(paired){
         
         xdiff <- PK_temp$Difference
         
         # NB: This is d_ave in Lakens 2013 Front Psychol. 
         d <- mean(xdiff) / ((sd(x1) + sd(x2))/2)
         
         # Listing other options for estimating Cohen's d for paired study
         # design for reference here:
         
         # d_z <- mean(xdiff) / sd(xdiff)
         # # Likely overestimates effect, based on my understanding. 
         # 
         # r <- cor(x2, x1)
         # d_rm <- mean(xdiff) / sqrt(sd(x2)^2 + sd(x1)^2 -
         #                               2 * r * sd(x2) * sd(x1)) *
         #    sqrt(2 * (1-r))
         # # This is d_repeated measures and is likely too conservative. This
         # # is the calculation you get if you run: 
         # effsize::cohen.d(d = x2, 
         #                  f = x1, 
         #                  paired = T, hedges.correction = F)
         
         
      } else {
         
         PoolSD <- sqrt(((n1-1)*sd(x2)^2 + (n2-1)*sd(x1)^2) / (n1 + n2 -2))
         
         d <- (mean(x2) - mean(x1)) / PoolSD
         
      }
      
      # Applying Hedge's g correction. This is effectively the same for both
      # paired and unpaired tests.
      g <- d * (1 - (3/(4*(n1+n2)-9)))
      
      N <- pwr::pwr.t.test(
         n = NULL, 
         d = g, 
         type = ifelse(paired, "paired", "two.sample"), 
         alternative = alternative_hypothesis, 
         sig.level = alpha, 
         power = power)$n
      
      if(paired){
         Out[[i]] <- tibble(
            File = str_comma(sort(unique(c(PK_temp$Denominator_File, 
                                           PK_temp$Numerator_File)))),  
            Tissue = str_comma(sort(unique(c(PK_temp$Denominator_Tissue,
                                             PK_temp$Numerator_Tissue)))), 
            CompoundID = str_comma(sort(unique(c(PK_temp$Denominator_CompoundID, 
                                                 PK_temp$Numerator_CompoundID)))), 
            Sheet = str_comma(sort(unique(c(PK_temp$Denominator_Sheet, 
                                            PK_temp$Numerator_Sheet)))), 
            PKparameter = str_comma(sort(unique(c(PK_temp$Denominator_PKparameter, 
                                                  PK_temp$Numerator_PKparameter)))), 
            
            `Mean of control` = ifelse(log_transform, 
                                       round_consultancy(exp(mean(x1))), 
                                       round_consultancy(mean(x1))), 
            
            `Standard deviation of control` = ifelse(log_transform, 
                                                     round_consultancy(exp(sd(x1))), 
                                                     round_consultancy(sd(x1))), 
            
            `Mean of test` = ifelse(log_transform, 
                                    round_consultancy(exp(mean(x2))), 
                                    round_consultancy(mean(x2))),
            
            `Standard deviation of test` = ifelse(log_transform, 
                                                  round_consultancy(exp(sd(x2))),
                                                  round_consultancy(sd(x2))),
            
            `Mean difference` = ifelse(log_transform, 
                                       round_consultancy(exp(mean(xdiff))), 
                                       round_consultancy(mean(xdiff))), 
            
            `Standard deviation of difference` = ifelse(log_transform, 
                                                        round_consultancy(exp(sd(xdiff))),
                                                        round_consultancy(sd(xdiff))), 
            
            `Corrected Cohen's d` = signif(g, 4), 
            alpha = alpha, 
            power = power, 
            `Alternative hypothesis` = case_match(
               alternative_hypothesis, 
               "two.sided" ~ "two sided", 
               .default = paste0("test is ", 
                                 alternative_hypothesis, 
                                 " than control")), 
            `N required` = N)
         
         if(round_up_N){
            Out[[i]] <- Out[[i]] %>% 
               mutate(`N required` = 
                         ceiling(`N required`))
         }
         
      } else {
         Out[[i]] <- tibble(
            File = str_comma(sort(unique(c(PK_control$File, 
                                           PK_test$File)))), 
            Tissue = str_comma(sort(unique(c(PK_control$Tissue, 
                                             PK_test$Tissue)))), 
            CompoundID = str_comma(sort(unique(c(PK_control$CompoundID, 
                                                 PK_test$CompoundID)))), 
            Sheet = str_comma(sort(unique(c(PK_control$Sheet, 
                                            PK_test$Sheet)))), 
            PKparameter = str_comma(sort(unique(PK_control$PKparameter))), 
            
            `Mean of control` = ifelse(log_transform, 
                                       round_consultancy(exp(mean(x1))), 
                                       round_consultancy(mean(x1))), 
            
            `Standard deviation of control` = ifelse(log_transform, 
                                                     round_consultancy(exp(sd(x1))), 
                                                     round_consultancy(sd(x1))), 
            
            `Mean of test` = ifelse(log_transform, 
                                    round_consultancy(exp(mean(x2))), 
                                    round_consultancy(mean(x2))), 
            
            `Standard deviation of test` = ifelse(log_transform,
                                                  round_consultancy(exp(sd(x2))),
                                                  round_consultancy(sd(x2))), 
            
            `Mean difference` = ifelse(log_transform, 
                                       round_consultancy(exp(mean(x2)) - exp(mean(x1))), 
                                       round_consultancy(mean(x2) - mean(x1))), 
            
            `Corrected Cohen's d` = signif(g, 4), 
            alpha = alpha, 
            power = power, 
            `Alternative hypothesis` = case_match(
               alternative_hypothesis, 
               "two.sided" ~ "two sided", 
               .default = paste0("test is ", 
                                 alternative_hypothesis, 
                                 " than control")), 
            
            `N required in each group` = N)
         
         if(round_up_N){
            Out[[i]] <- Out[[i]] %>% 
               mutate(`N required in each group` = 
                         ceiling(`N required in each group`))
         }
      }
      
      suppressWarnings(rm(d, g, x1, x2, xdiff, N, n1, n2, PoolSD,
                          PK_control, PK_test, PK_temp))
      
   }
   
   Out <- bind_rows(Out)
   
   
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



