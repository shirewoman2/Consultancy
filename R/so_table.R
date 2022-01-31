#' Make simulated vs. observed tables for reports
#'
#' \code{so_table} creates simulated vs. observed tables for reports and
#' presentations, including reporting means, CVs, confidence intervals or
#' percentiles, and ratios of simulated vs. observed mean values. There are two
#' main ways to approach using this function 1. Select which PK parameters you
#' want to compare (\code{PKparameters} argument) and, later, manually fill out
#' rows for observed data that you've calculated elsewhere, or 2. Fill out an
#' Excel form with information about your observed data, in which case R will
#' calculate the comparisons for you here. Setting up the input for this
#' approach requires a few steps:\enumerate{\item{Use the function
#' \code{\link{generateReportInputForm}} to create an Excel file where you can
#' enter information about your project.} \item{Go to the tab "observed data"
#' and enter details about your observed data. It's ok if you don't have all the
#' information; anything that's missing won't be included in the final S/O
#' table. It's also ok to rename this tab and/or make copies of it within the
#' same Excel file for making other S/O tables.} \item{Go to the tab "table and
#' graph input" and fill out information here for the specific report section
#' you're making this S/O table for. Make sure that whatever you list as the tab
#' that contains information about the observed data is \emph{exactly} the same
#' as the actual tab name that you filled out in step 2. Also make sure that the
#' file names include the full file path.}\item{Save your Excel file.}
#' \item{Here, within RStudio (or within the shiny app that we plan to make!),
#' run this function using the name of that Excel file as input for
#' \code{report_input_file} and the name of the "table and graph input" tab as
#' the input for \code{sheet}. Note: If the Excel file lives on SharePoint,
#' you'll need to close it or this function will just keep running and not
#' generate any output while it waits for access to the file.} }
#'
#'
#' @param report_input_file the name of the Excel file created by running
#'   \code{\link{generateReportInputForm}}, which you have now filled out,
#'   including the path if it's in any other directory than the current one
#' @param sheet the sheet in the Excel file that contains information about this
#'   section of the report. In the original template, this was the tab titled
#'   "table and graph input".
#' @param sectionInfo information about the simulated and observed data. This is
#'   output from the function \code{\link{getSectionInfo}} and can be used
#'   instead of listing the Excel file and sheet name as input.
#' @param sim_data_file the simulator output file. This is for when you DON'T
#'   fill out a report input form and instead plan to add information about the
#'   observed data later manually.
#' @param PKparameters the PK parameters to include as a character vector. To
#'   see the full set of possible parameters to extract, enter
#'   \code{data(AllPKParameters)} into the console. By default, if you supply a
#'   file for \code{report_input_file}, the PK parameters included are only
#'   those included for the observed data in that file. Otherwise, the PK
#'   parameters will be automatically selected. An example of acceptable input
#'   here: \code{c("AUCtau_ss", "AUCtau_ss_withInhib", "Cmax_ss",
#'   "Cmax_ss_withInhib", "AUCtau_ratio_ss", "Cmax_ratio_ss")}. Parameters that
#'   don't make sense for your scenario -- like asking for
#'   \code{AUCinf_ss_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.
#' @param mean_type Arithmetic or geometric means? Only specify this if you'd
#'   like to override the value listed in \code{sectionInfo}. If no value is
#'   specified here or in \code{sectionInfo}, the default is "geometric".
#' @param variability_option What type of variability would you like the table
#'   to include? Options are: "90\% CI", "95\% CI", "95th percentiles", or any
#'   combination of those, e.g. \code{variability_option = c("90\% CI", "95th
#'   percentiles"). The arithmetic CV will automatically be included.}
#' @param concatVariability Would you like to have the variability concatenated?
#'   TRUE or FALSE. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile. Ex: "2400 to 2700"
#' @param includeHalfLife TRUE or FALSE for whether to include half life as a
#'   parameter in the output table
#' @param includeTrialMeans TRUE or FALSE for whether to include the range of
#'   trial means for a given parameter. Note: This is calculated from individual
#'   values rather than pulled directly from the output.
#' @param includeCV TRUE or FALSE for whether to include rows for CV in the
#'   table
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#'
#' @return a data.frame
#' @export
#' @examples
#' # so_table(report_input_file = "//certara.com/data/sites/SHF/Consult/abc-1a/Report input.xlsx",
#' #          sheet = "table and graph input", includeTrialMeans = TRUE)


so_table <- function(report_input_file = NA,
                     sheet = NA,
                     sectionInfo = NA,
                     sim_data_file = NA,
                     PKparameters = NA,
                     mean_type = NA,
                     variability_option = "90% CI",
                     concatVariability = FALSE,
                     includeHalfLife = FALSE,
                     includeTrialMeans = FALSE,
                     includeCV = TRUE,
                     checkDataSource = TRUE){

      # Error catching
      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) &
         is.na(report_input_file) & is.na(sim_data_file)){
            stop("You must enter a value for either 'sectionInfo' or 'report_input_file' or include a specific simulator output file for 'sim_data_file'.")
      }

      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) &
         complete.cases(report_input_file) & is.na(sheet)){
            stop("If you specify an Excel file to get the section information from, you must also specify which sheet to read.")
      }

      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) &
         complete.cases(report_input_file) & is.na(sim_data_file)){
            sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                          sheet = sheet)

            # Should we add an error catch here for when user fills out
            # report_input_file but doesn't include any observed data to
            # compare? Maybe not. If the user doesn't want to include any obs
            # data there, just fill out sim_data_file.
      }

      # sectionInfo is logical if it's not filled out.
      if(class(sectionInfo) == "logical"){
            Deets <- extractExpDetails(sim_data_file)
            EffectorPresent <- complete.cases(Deets$Inhibitor1)
            DoseRegimen <- Deets$Regimen_sub
            SimFile <- sim_data_file
            MeanType <- ifelse(is.na(mean_type),
                               "geometric", MeanType)
            GMR_mean_type <- "geometric"

      } else {
            EffectorPresent <- complete.cases(sectionInfo$Inhibitor)
            DoseRegimen <- sectionInfo$Regimen_sub
            SimFile <- sectionInfo$SimFile
            MeanType <- ifelse(is.na(mean_type),
                               sectionInfo$MeanType,
                               mean_type)
            MeanType <- ifelse(is.na(MeanType), "geometric", MeanType)
            GMR_mean_type <- sectionInfo$GMR_mean_type

      }

      # All possible PK parameters
      data("AllPKParameters")

      if(complete.cases(PKparameters[1])){
            PKToPull <- PKparameters
      } else {
            # Just the most commonly requested PK parameters
            PKToPull <- AllPKParameters %>%
                  filter(str_detect(PKparameter, "AUCinf|AUCtau|CL|Cmax|HalfLife|tmax")) %>%
                  filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>%
                  pull(PKparameter) %>% unique()

            # If dose regimen were single dose, we don't generally want AUCtau
            # since, most of the time, we need AUCinf instead.
            if(Deets$Regimen_sub == "Single Dose"){
                  PKtoPull <- PKToPull[!str_detect(PKToPull, "tau")]
            } else {
                  # If the regimen were multiple dose, only pull ss parameters.
                  PKToPull <- PKToPull[!str_detect(PKToPull, "_dose1")]
            }
      }

      # If dose regimen were single-dose, then only pull dose 1 data.
      if(Deets$Regimen_sub == "Single Dose"){
            SDParam <- AllPKParameters %>%
                  filter(AppliesToSingleDose == TRUE) %>%
                  pull(PKparameter)

            PKToPull <- PKToPull[PKToPull %in% SDParam]
      }

      # If there was no effector, then don't pull any interaction info
      if(is.na(Deets$Inhibitor1)){
            EffParam <- AllPKParameters %>%
                  filter(AppliesOnlyWhenEffectorPresent == TRUE) %>%
                  pull(PKparameter)

            PKToPull <- PKToPull[!PKToPull %in% EffParam]
      }

      # Getting PK parameters from the AUC tab
      MyPKResults_all <- extractPK(sim_data_file = SimFile,
                                   PKparameters = PKToPull,
                                   returnAggregateOrIndiv =
                                         switch(as.character(includeTrialMeans),
                                                "TRUE" = c("aggregate", "individual"),
                                                "FALSE" = "aggregate"))

      MyPKResults <- MyPKResults_all$aggregate

      VarOpt1 <- variability_option[1]
      VariabilityNames <- switch(VarOpt1,
                                 "90% CI" = c("CI_10", "CI_90"),
                                 "95% CI" = c("CI_05", "CI_95"),
                                 "95th percentiles" = c("Q5th", "Q95th"))

      # Accounting for when mean_type is arithmetic but the user requests that
      # the ratio for + effector over - effector be a GMR.
      if(MeanType == "arithmetic" &&
         EffectorPresent == TRUE &&
         complete.cases(GMR_mean_type) &&
         GMR_mean_type == "geometric"){

            MyPKResults[MyPKResults$Statistic == "Mean",
                        str_detect(names(MyPKResults), "ratio")] <-
                  MyPKResults[MyPKResults$Statistic == "Geometric Mean",
                              str_detect(names(MyPKResults), "ratio")]
      }

      # Trial means
      if(includeTrialMeans){
            TrialMeans <- MyPKResults_all$individual %>%
                  group_by(Trial) %>%
                  summarize(across(.cols = -Individual,
                                   .fns = switch(MeanType,
                                                 "geometric" = gm_mean,
                                                 "arithmetic" = mean))) %>%
                  ungroup() %>%
                  pivot_longer(cols = -Trial, names_to = "Parameter",
                               values_to = "Value") %>%
                  group_by(Parameter) %>%
                  summarize(MinMean = min(Value),
                            MaxMean = max(Value)) %>%
                  pivot_longer(cols = -Parameter,
                               names_to = "Statistic", values_to = "Value") %>%
                  pivot_wider(names_from = Parameter, values_from = Value)

            MyPKResults <- MyPKResults %>% bind_rows(TrialMeans)
      }

      MyPKResults <- MyPKResults %>%
            filter(!Statistic == ifelse(MeanType == "geometric",
                                        "Mean", "Geometric Mean")) %>%
            mutate(Stat = recode(Statistic,
                                 "Mean" = "GMean",
                                 "Geometric Mean" = "GMean",
                                 "90% confidence interval around the geometric mean(lower limit)" = "CI_10",
                                 "90% confidence interval around the geometric mean(upper limit)" = "CI_90",
                                 "95% confidence interval around the geometric mean(lower limit)" = "CI_05",
                                 "95% confidence interval around the geometric mean(upper limit)" = "CI_95",
                                 "5th centile" = "Q5th",
                                 "95th centile" = "Q95th",
                                 "cv" = "CV",
                                 "Std Dev" = "SD")) %>%
            select(-Statistic)

      # Adjusting tmax values since the geometric mean row will actually be the
      # median, VariabilityNames[1] will be the min, and VariabilityNames[2] will
      # be the max.
      if("tmax_dose1" %in% names(MyPKResults)){
            MyPKResults$tmax_dose1[MyPKResults$Stat == "GMean"] <-
                  MyPKResults$tmax_dose1[MyPKResults$Stat == "Median"]
            MyPKResults$tmax_dose1[MyPKResults$Stat == VariabilityNames[1]] <-
                  MyPKResults$tmax_dose1[MyPKResults$Stat == "Min Val"]
            MyPKResults$tmax_dose1[MyPKResults$Stat == VariabilityNames[2]] <-
                  MyPKResults$tmax_dose1[MyPKResults$Stat == "Max Val"]

            if(EffectorPresent){
                  MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "GMean"] <-
                        MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Median"]
                  MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == VariabilityNames[1]] <-
                        MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Min Val"]
                  MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == VariabilityNames[2]] <-
                        MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "Max Val"]
            }

      }

      if("tmax_ss" %in% names(MyPKResults)){
            MyPKResults$tmax_ss[MyPKResults$Stat == "GMean"] <-
                  MyPKResults$tmax_ss[MyPKResults$Stat == "Median"]
            MyPKResults$tmax_ss[MyPKResults$Stat == VariabilityNames[1]] <-
                  MyPKResults$tmax_ss[MyPKResults$Stat == "Min Val"]
            MyPKResults$tmax_ss[MyPKResults$Stat == VariabilityNames[2]] <-
                  MyPKResults$tmax_ss[MyPKResults$Stat == "Max Val"]

            if(EffectorPresent){
                  MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "GMean"] <-
                        MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "Median"]
                  MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == VariabilityNames[1]] <-
                        MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "Min Val"]
                  MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == VariabilityNames[2]] <-
                        MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "Max Val"]
            }

      }

      # Formatting
      MyPKResults <- MyPKResults %>%
            pivot_longer(cols = -Stat, names_to = "PKParam",
                         values_to = "Value") %>%
            mutate(Value = if_else(Stat == "CV",
                                   round(Value * 100, 0),
                                   # Per Christiane: 3 sig figs for everything
                                   # or full number when > 100
                                   if_else(Value > 100,
                                           round(Value, 0), signif(Value, 3)))) %>%
            filter(Stat %in% c("GMean", "CI_10", "CI_90", "CI_05", "CI_95",
                               "Q5th", "Q95th", "CV", "MinMean", "MaxMean")) %>%
            pivot_wider(names_from = PKParam, values_from = Value)

      MyObsPKParam <- c(PKToPull, paste0(PKToPull, "_CV"))
      if(EffectorPresent){
            MyObsPKParam <- c(MyObsPKParam,
                              "Cmax_ratio_dose1_90CIL", "Cmax_ratio_dose1_90CIU",
                              "AUCinf_ratio_dose1_90CIL", "AUCinf_ratio_dose1_90CIU",
                              "Cmax_ratio_ss_90CIL", "Cmax_ratio_ss_90CIU",
                              "AUCtau_ratio_ss_90CIL", "AUCtau_ratio_ss_90CIU")
      }


      # observed data -----------------------------------------------------
      if(class(sectionInfo) != "logical"){

            MyObsPK <- sectionInfo[names(sectionInfo)[
                  names(sectionInfo) %in% MyObsPKParam]] %>%
                  as.data.frame() %>% t() %>% as.data.frame() %>%
                  rename("Value" = V1) %>%
                  mutate(RName = row.names(.),
                         Value = as.numeric(Value),
                         Stat = ifelse(str_detect(RName, "CV"), "CV_obs", "GMean"),
                         Stat = ifelse(str_detect(RName, "CIU"), "CIU_obs", Stat),
                         Stat = ifelse(str_detect(RName, "CIL"), "CIL_obs", Stat),
                         RName = sub("_CV|_[0-9]{2}CI[UL]", "", RName),
                         Value = if_else(Stat == "CV_obs",
                                         round(Value * 100, 0),
                                         # Per Christiane: 3 sig figs for everything
                                         # or full number when > 100
                                         if_else(Value > 100,
                                                 round(Value, 0), signif(Value, 3)))) %>%
                  pivot_wider(names_from = RName, values_from = Value)

            # Only want to keep the columns where there are values for the observed
            # mean data.
            MyObsPK <- MyObsPK %>% select(where(function(x) complete.cases(x[1])))

            # Calculating S/O
            GMeans <- MyPKResults %>% filter(Stat == "GMean") %>%
                  pivot_longer(names_to = "PKParam_short", values_to = "Sim",
                               cols = -Stat) %>%
                  left_join(MyObsPK %>% filter(Stat == "GMean") %>%
                                  pivot_longer(names_to = "PKParam_short", values_to = "Obs",
                                               cols = -Stat)) %>%
                  mutate(S_O = round(Sim / Obs, 2)) %>%
                  select(PKParam_short, S_O) %>%
                  pivot_wider(names_from = PKParam_short, values_from = S_O) %>%
                  mutate(Stat = "S/O")

            # Next, selecting only the appropriate columns for table, making
            # everything character, and adding % symbols to CV rows
            MyPKResults <- MyPKResults %>%
                  bind_rows(MyObsPK %>% mutate(Stat = ifelse(Stat == "GMean",
                                                             "GMean_obs", Stat))) %>%
                  bind_rows(GMeans)

            MyPKResults <- MyPKResults[, names(MyObsPK)]

            if(EffectorPresent){
                  # Only want AUC, Cmax, tmax, t12, GMR w/ and w/out effector
                  MyPKResults <- MyPKResults %>%
                        select(Stat, matches("AUC|[tC]max|HalfLife|ratio"))

                  PKToPull <-  c(PKToPull,
                                 "AUCinf_ratio_dose1", "Cmax_ratio_dose1",
                                 "AUCinf_ratio_ss", "Cmax_ratio_ss",
                                 "AUCtau_ratio_ss", "Cmax_ratio_ss",
                                 "AUCtau_ratio_dose1", "Cmax_ratio_dose1")

            }

      }

      MyPKResults <- MyPKResults %>%
            mutate_all(.funs = as.character) %>%
            mutate(across(.cols = !Stat,
                          .fns = function(x) ifelse(str_detect(Stat, "CV"),
                                                    paste0(x, "%"), x)),
                   across(.cols = everything(),
                          .fns = function(x) ifelse(x == "NA%", NA, x)))
      # If this throws an error for you, try running "tidyverse_update()", copy
      # whatever it says is out of date, restart your R session (Ctrl Shift
      # F10), and then paste the output (something like
      # "install.packages(c("dbplyr", "dplyr", "dtplyr", ... ") and execute.


      # Talked w/Christiane about including t1/2 since that's included in the table,
      # and she said that Massoud prefers to NOT include it b/c there can be glitches
      # with its calculation. It's only included if there's a specific reason the
      # client wants it. Optionally including it.
      if(includeHalfLife == FALSE){
            MyPKResults <- MyPKResults %>% select(-matches("HalfLife"))
      }

      # Filtering rows as requested by user
      VarOpts_tableRows <- list("90% CI" = c("CI_10", "CI_90"),
                                "95% CI" = c("CI_05", "CI_95"),
                                "95th percentiles" = c("Q5th", "Q95th"))
      VarOptsNotChosen <- setdiff(c("90% CI", "95% CI", "95th percentiles"),
                                  variability_option)

      if(length(VarOptsNotChosen) > 0){
            MyPKResults <- MyPKResults %>%
                  filter(!Stat %in% unlist(VarOpts_tableRows[VarOptsNotChosen]))
      }

      # If the user selected variability options that were not calculated in the
      # simulator output, give them a warning about that.
      VarOptsChosen <- unlist(VarOpts_tableRows[variability_option])
      MissingOpts <- setdiff(VarOptsChosen, unique(MyPKResults$Stat))
      if(length(MissingOpts) > 0){
            MissingOpts <- unlist(VarOpts_tableRows) == MissingOpts
            MissingOpts <- unique(sub("1$|2$", "", names(MissingOpts)[MissingOpts]))
            MissingOpts <- str_comma(MissingOpts)
            warning(paste0("The ",
                           MissingOpts,
                           " was/were requested but is/are not present in the simulator output. This will not be included in the table."))
      }

      # Putting trial means into appropriate format
      if(includeTrialMeans){
            TM <- MyPKResults %>% filter(Stat %in% c("MinMean", "MaxMean")) %>%
                  summarize(across(.cols = -Stat,
                                   .fns = function(x) {paste(x[1], "to", x[2])}))
            MyPKResults <- MyPKResults %>%
                  filter(Stat != "MaxMean")
            MyPKResults[which(MyPKResults$Stat == "MinMean"), 2:ncol(MyPKResults)] <-
                  TM
      }

      # Concatenating the rows w/lower and upper limits of variability when
      # requested
      if(concatVariability){

            # Note: When multiple options are chosen for variability type,
            # concatVariability doesn't work. Will need to fix this later.
            VarRows <- VarOpts_tableRows[variability_option]
            VarRows[["obs"]] <- c("CIL_obs", "CIU_obs")
            for(j in names(VarRows)){
                  temp <- MyPKResults %>%
                        filter(Stat %in% as.character(unlist(VarRows[[j]]))) %>%
                        mutate(across(.cols = !matches("Stat"),
                                      .fns = function(x) {
                                            ifelse(all(complete.cases(c(x[1], x[2]))),
                                                   paste(x[1], "to", x[2]), NA)}),
                               Stat = switch(j,
                                             "90% CI" = "CI90concat",
                                             "95% CI" = "CI95concat",
                                             "95th percentiles" = "Q95thconcat",
                                             "obs" = "CIobsconcat"))

                  MyPKResults[which(MyPKResults$Stat == VarRows[[j]][1]), ] <-
                        temp[1, ]
                  MyPKResults <- MyPKResults %>% filter(Stat != VarRows[[j]][2])
                  rm(temp)
            }
      }

      # Renaming statistics to match what's in template
      StatNames <- c("GMean" = "Simulated",
                     "CV" = "CV%",
                     "CI_10" = "90% CI - Lower",
                     "CI_90" = "90% CI - Upper",
                     "CI90concat" = "90% CI",
                     "CI_05" = "95% CI - Lower",
                     "CI_95" = "95% CI - Upper",
                     "CI95concat" = "95% CI",
                     "Q5th" = "5th Percentile",
                     "Q95th" = "95th Percentile",
                     "Q95thconcat" = "5th to 95th Percentile",
                     "GMean_obs" = "Observed",
                     "CV_obs" = "CV%",
                     "CIL_obs" = "observed CI - Lower",
                     "CIU_obs" = "observed CI - Upper",
                     "CIobsconcat" = "Observed CI",
                     "S/O" = "S/O",
                     "MinMean" = "Range of trial means")

      MyPKResults <- MyPKResults %>%
            mutate(Statistic = as.character(Stat),
                   Statistic = StatNames[Statistic]) %>%
            select(-Stat) %>%
            select(Statistic, everything())

      # Getting columns in a good order
      MyPKResults <- MyPKResults %>%
            select(any_of(c("Statistic", PKToPull)))

      if(includeCV == FALSE){
            MyPKResults <- MyPKResults %>%
                  filter(!str_detect(Statistic, "^CV"))
      }

      # Adding final column names
      PKToPull_pretty <-
            sapply(PKToPull,
                   FUN = function(x) ifelse(str_detect(x, "_dose1"),
                                            paste("Dose 1", sub("_dose1", "", x)),
                                            paste("Steady-state", sub("_ss", "", x))))
      PKToPull_pretty <- sub("_withInhib", " with inhibitor", PKToPull_pretty)
      PKToPull_pretty <- sub("CL", "CL/F (L/h)", PKToPull_pretty)
      PKToPull_pretty <- sub("AUCinf", "AUC0 to inf (h*ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("AUCtau", "AUC0 to tau (h*ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("Cmax", "Cmax (ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("HalfLife", "t1/2 (h)", PKToPull_pretty)
      PKToPull_pretty <- sub("tmax", "tmax (h)", PKToPull_pretty)
      PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")] <-
            sub(" \\(", " with inhibitor (", PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")])
      PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")] <-
            sub(" with inhibitor$", "", PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")])
      PKToPull_pretty <- sub("GMR_", "geometric mean ratio ", PKToPull_pretty)

      PKToPull_pretty <- c("Statistic" = "Statistic", PKToPull_pretty)

      names(MyPKResults) <- PKToPull_pretty[names(MyPKResults)]

      if(checkDataSource){
            MyPKResults <- list("so_table" = MyPKResults,
                                "QC" = MyPKResults_all$QC)
      }

      return(MyPKResults)
}



