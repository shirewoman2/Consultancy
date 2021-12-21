#' Make simulated vs. observed tables for reports
#'
#' \code{so_table} creates simulated vs. observed tables for reports and
#' presentations, including reporting means, CVs, confidence intervals or
#' percentiles, and ratios of simulated vs. observed mean values. Since this
#' requires comparing observed data to a simulator output file, setting up the
#' input for this function requires a few steps: \enumerate{ \item{Use the
#' function \code{\link{generateReportInputForm}} to create an Excel file where
#' you can enter information about your project.} \item{Go to the tab "observed
#' data" and enter details about your observed data. It's ok if you don't have
#' all the information; anything that's missing won't be included in the final
#' S/O table. It's also ok to rename this tab and/or make copies of it within
#' the same Excel file for making other S/O tables.} \item{Go to the tab
#' "section input" and fill out information here for the specific section you're
#' making this S/O table for. Make sure that whatever you list for the tab that
#' contains information about the observed data is exactly the same as the
#' actual tab name that you filled out in step 2. Also, make sure that the file
#' names include the full file path.} \item{Save your Excel file.} \item{Here,
#' within RStudio (or within the shiny app that we plan to make!), run this
#' function using the name of that Excel file as input for
#' \code{report_input_file} and the name of the "section input" tab as the input
#' for \code{sheet}. Note: If the Excel file lives on SharePoint, you'll need to
#' close it or this function will just keep running and not generate any output
#' while it waits for access to the file.} }
#'
#' @param report_input_file the name of the Excel file to be piped to
#'   \code{\link{getSectionInfo}}
#' @param sheet the sheet name to read
#' @param sectionInfo information about the simulated and observed data. This is
#'   output from the function \code{\link{getSectionInfo}} and can be used
#'   instead of listing the Excel file and sheet name as input.
#' @param mean_type Arithmetic or geometric means? Only specify this if you'd
#'   like to override the value listed in \code{sectionInfo}. If no value is
#'   specified here or in \code{sectionInfo}, the default is "geometric".
#' @param variability_option  What type of variability would you like the table
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
#'
#' @return a data.frame
#' @export
#' @examples
#' # No examples yet.


so_table <- function(report_input_file = NA,
                     sheet = NA,
                     sectionInfo = NA,
                     mean_type = NA,
                     variability_option = "90% CI",
                     concatVariability = FALSE,
                     includeHalfLife = FALSE){

      # Error catching
      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) & is.na(report_input_file)){
            stop("You must enter a value for either 'sectionInfo' or 'report_input_file'.")
      }

      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) &
         complete.cases(report_input_file) & is.na(sheet)){
            stop("If you specify an Excel file to get the section information from, you must also specify which sheet to read.")
      }

      if(length(sectionInfo) == 1 & is.na(sectionInfo[1]) &
         complete.cases(report_input_file)){
            sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                          sheet = sheet)
      }

      D1 <- c("AUCinf", "CL", "Cmax", "HalfLife", "tmax")
      D2 <- sub("AUCinf", "AUCtau", D1)

      D1 <- paste0(D1, "_dose1")
      D2 <- paste0(D2, "_ss")

      Dcomp <- c("AUCinf_ratio_dose1", "AUCtau_ratio_dose1", "AUCtau_ratio_ss",
                 "Cmax_ratio_dose1", "Cmax_ratio_ss")

      ObsToPull <- c(D1, paste0(D1, "_withEffector"),
                     D2, paste0(D2, "_withEffector"), Dcomp)

      PKToPull <- ObsToPull[!str_detect(ObsToPull, "_CV")]

      EffectorPresent <- complete.cases(sectionInfo$Inhibitor)
      DoseRegimen <- sectionInfo$Regimen_sub

      # Getting PK parameters from the AUC tab
      MyPKParam <- switch(
            paste(DoseRegimen, EffectorPresent),
            "Single Dose FALSE" = PKToPull[
                  str_detect(PKToPull, "dose1") &
                        !str_detect(PKToPull, "withEffector")],
            "Single Dose TRUE" = PKToPull[
                  str_detect(PKToPull, "dose1")],
            "Multiple Dose FALSE" = PKToPull[!str_detect(PKToPull, "withEffector")],
            "Multiple Dose TRUE" = PKToPull)

      MyPKResults_all <- extractPK(sim_data_file = sectionInfo$SimFile,
                                   PKparameters = MyPKParam,
                                   returnAggregateOrIndiv =
                                         switch(as.character(includeTrialMeans),
                                                "TRUE" = c("aggregate", "individual"),
                                                "FALSE" = "aggregate"))
      MyPKResults <- switch(as.character(includeTrialMeans),
                            "TRUE" = MyPKResults_all$Aggregate,
                            "FALSE" = MyPKResults_all)

      MeanType <- ifelse(is.na(mean_type),
                         sectionInfo$MeanType,
                         mean_type)
      MeanType <- ifelse(is.na(MeanType), "geometric", MeanType)

      VarOpt1 <- variability_option[1]
      VariabilityNames <- switch(VarOpt1,
                                 "90% CI" = c("CI_10", "CI_90"),
                                 "95% CI" = c("CI_05", "CI_95"),
                                 "95th percentiles" = c("Q5th", "Q95th"))

      # Accounting for when mean_type is arithmetic but the user requests that
      # the ratio for + effector over - effector be a GMR.
      if(MeanType == "arithmetic" &
         complete.cases(sectionInfo$GMR_mean_type) &
         sectionInfo$GMR_mean_type == "geometric"){

            MyPKResults[MyPKResults$Statistic == "Mean",
                        str_detect(names(MyPKResults), "ratio")] <-
                  MyPKResults[MyPKResults$Statistic == "Geometric Mean",
                              str_detect(names(MyPKResults), "ratio")]
      }

      # Trial means
      if(includeTrialMeans){
            TrialMeans <- MyPKResults_all$Individual %>%
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
      MyPKResults$tmax_dose1[MyPKResults$Stat == "GMean"] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "Median"]
      MyPKResults$tmax_dose1[MyPKResults$Stat == VariabilityNames[1]] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "Min Val"]
      MyPKResults$tmax_dose1[MyPKResults$Stat == VariabilityNames[2]] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "Max Val"]

      MyPKResults$tmax_ss[MyPKResults$Stat == "GMean"] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "Median"]
      MyPKResults$tmax_ss[MyPKResults$Stat == VariabilityNames[1]] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "Min Val"]
      MyPKResults$tmax_ss[MyPKResults$Stat == VariabilityNames[2]] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "Max Val"]

      MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == "GMean"] <-
            MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == "Median"]
      MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == VariabilityNames[1]] <-
            MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == "Min Val"]
      MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == VariabilityNames[2]] <-
            MyPKResults$tmax_ss_withEffector[MyPKResults$Stat == "Max Val"]

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

      # Observed data. Not included when section is model application.
      if(tolower(sectionInfo$ModelPurpose) != "application"){

            MyObsPKParam <- c(MyPKParam, paste0(MyPKParam, "_CV"))
            if(EffectorPresent){
                  MyObsPKParam <- c(MyObsPKParam,
                                    "Cmax_ratio_dose1_90CIL", "Cmax_ratio_dose1_90CIU",
                                    "AUCinf_ratio_dose1_90CIL", "AUCinf_ratio_dose1_90CIU",
                                    "Cmax_ratio_ss_90CIL", "Cmax_ratio_ss_90CIU",
                                    "AUCtau_ratio_ss_90CIL", "AUCtau_ratio_ss_90CIU")
            }

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

      if(EffectorPresent){
            # Only want AUC, Cmax, tmax, t12, GMR w/ and w/out effector
            MyPKResults <- MyPKResults %>%
                  select(Stat, matches("AUC|[tC]max|HalfLife|ratio"))

            PKToPull <-  c(PKToPull,
                           "AUCinf_ratio_dose1", "Cmax_ratio_dose1",
                           "AUCtau_ratio_ss", "Cmax_ratio_ss")

      }

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
                                             "95th percentile" = "Q95thconcat",
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

      # Adding final column names
      PKToPull_pretty <-
            sapply(PKToPull,
                   FUN = function(x) ifelse(str_detect(x, "_dose1"),
                                            paste("Dose 1", sub("_dose1", "", x)),
                                            paste("Steady-state", sub("_ss", "", x))))
      PKToPull_pretty <- sub("_withEffector", " with effector", PKToPull_pretty)
      PKToPull_pretty <- sub("CL", "CL/F (L/h)", PKToPull_pretty)
      PKToPull_pretty <- sub("AUCinf", "AUC0 to inf (h*ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("AUCtau", "AUC0 to tau (h*ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("Cmax", "Cmax (ng/mL)", PKToPull_pretty)
      PKToPull_pretty <- sub("HalfLife", "t1/2 (h)", PKToPull_pretty)
      PKToPull_pretty <- sub("tmax", "tmax (h)", PKToPull_pretty)
      PKToPull_pretty[str_detect(PKToPull_pretty, "with effector")] <-
            sub(" \\(", " with effector (", PKToPull_pretty[str_detect(PKToPull_pretty, "with effector")])
      PKToPull_pretty[str_detect(PKToPull_pretty, "with effector")] <-
            sub(" with effector$", "", PKToPull_pretty[str_detect(PKToPull_pretty, "with effector")])
      PKToPull_pretty <- sub("GMR_", "geometric mean ratio ", PKToPull_pretty)

      PKToPull_pretty <- c("Statistic" = "Statistic", PKToPull_pretty)

      names(MyPKResults) <- PKToPull_pretty[names(MyPKResults)]

      return(MyPKResults)
}
