#' Make simulated vs. observed tables for reports
#'
#' \code{so_table} creates simulated vs. observed tables for reports and
#' presentations, including reporting means, CVs, confidence intervals or
#' percentiles, and ratios of simulated vs. observed mean values. Please see the
#' notes at the bottom of this help file for options for how to supply observed
#' data in a standardized fashion that this function can read.
#'
#' Because we need to have a standardized way to input observed data, setting up
#' the input for this function requires filling out an Excel template form. Here
#' are the steps to take: \enumerate{\item{Use the function
#' \code{\link{generateReportInputForm}} to create an Excel file where you can
#' enter information about your project. Example:
#' \code{generateReportInputForm("My report input form.xlsx")}} \item{Go to the
#' tab "study info - DDI" or "study info - no DDI", whichever is appropriate for
#' your situation. Under the heading "Observed data", enter details about your
#' observed data. It's ok if you don't have all the information; anything that's
#' missing won't be included in the final S/O table. It's also ok to rename this
#' tab and/or make copies of it within the same Excel file for making other S/O
#' tables.} \item{Under the heading "Simulated data" on that same tab, fill out
#' the name of the specific simulator output Excel file you want to compare.}
#' \item{Save the report form.} \item{Back in RStudio (or within the shiny app
#' that we plan to make!), run this function using the file name of that Excel
#' report form as input for \code{report_input_file} and the name of the "study
#' info - DDI/no DDI" tab as the input for \code{sheet_report}. Note: If the
#' Excel file lives on SharePoint, you'll need to close it or this function will
#' just keep running and not generate any output while it waits for access to
#' the file.} }
#'
#'
#' @param report_input_file the name of the Excel file created by running
#'   \code{\link{generateReportInputForm}}, which you have now filled out,
#'   including the path if it's in any other directory than the current one
#' @param sheet_report the sheet in the Excel report template file that contains
#'   information about the study, e.g., "study info - DDI" or "study info - no
#'   DDI" if you haven't renamed the tab.
#' @param PKparameters the PK parameters to include as a character vector.
#'   Notes: \itemize{ \item{To see the full set of possible parameters to
#'   extract, enter \code{data(AllPKParameters)} into the console.} \item{By
#'   default, if you supply a file for \code{report_input_file}, the PK
#'   parameters included are only those included for the observed data in that
#'   file. Otherwise, the PK parameters will be automatically selected.}
#'   \item{Parameters that don't make sense for your scenario -- like asking for
#'   \code{AUCinf_ss_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.} \item{tmax will be listed
#'   as median, min, and max rather than mean, lower and higher X\% confidence
#'   interval or X percentiles. Similarly, if you request trial means, the
#'   values for tmax will be the range of medians for the trials rather than the
#'   range of means.}} An example of acceptable input here: \code{c("AUCtau_ss",
#'   "AUCtau_ss_withInhib", "Cmax_ss", "Cmax_ss_withInhib", "AUCtau_ratio_ss",
#'   "Cmax_ratio_ss")}.
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Most of the time, this should be left as NA.
#' @param mean_type return "arithmetic" or "geometric" (default) means and CVs.
#'   If you supplied a report input form, only specify this if you'd like to
#'   override the value listed there. If no value is specified here or in
#'   \code{sectionInfo}, the default is "geometric".
#' @param includeTrialMeans TRUE or FALSE for whether to include the range of
#'   trial means for a given parameter. Note: This is calculated from individual
#'   values rather than being pulled directly from the output.
#' @param includeCV TRUE or FALSE for whether to include rows for CV in the
#'   table
#' @param includeConfInt TRUE or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includePerc TRUE or FALSE for whether to include 5th to 95th
#'   percentiles
#' @param concatVariability Would you like to have the variability concatenated?
#'   TRUE or FALSE. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile. Ex: "2400 to 2700"
#' @param prettify_columns TRUE or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUC0
#'   to inf (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name
#'   from \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param checkDataSource TRUE or FALSE: Include in the output a data.frame that
#'   lists exactly where the data were pulled from the simulator output file.
#'   Useful for QCing.
#' @param sim_data_file the simulator output file. This is only for when you
#'   don't fill out a report input form because you either have no observed data
#'   or you want to compare those data later manually.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.csv". If you leave off ".csv", it will still be saved as a csv file.
#'   If you requested both the table and the QC info, the QC file will have "-
#'   QC" added to the end of the file name.
#'
#' @return a data.frame of S/O values or a list of that data.frame (named
#'   "Table") plus information on where the values came from for QCing (named
#'   "QC")
#' @export
#' @examples
#' # so_table(report_input_file = "//certara.com/data/sites/SHF/Consult/abc-1a/Report input.xlsx",
#' #          sheet_report = "table and graph input", includeTrialMeans = TRUE)


so_table <- function(report_input_file = NA,
                     sheet_report = NA,
                     sim_data_file = NA, 
                     PKparameters = NA,
                     sheet_PKparameters = NA,
                     mean_type = NA,
                     includeCV = TRUE,
                     includeConfInt = TRUE,
                     includePerc = FALSE,
                     includeTrialMeans = FALSE,
                     concatVariability = FALSE,
                     prettify_columns = TRUE,
                     checkDataSource = TRUE, 
                     save_table = NA){
    
    # If they didn't include ".xlsx" at the end, add that.
    sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                            sim_data_file, paste0(sim_data_file, ".xlsx"))
    
    # If they didn't include ".xlsx" at the end, add that.
    report_input_file <- ifelse(str_detect(report_input_file, "xlsx$"), 
                                report_input_file, paste0(report_input_file, ".xlsx"))
    
    # Error catching
    if(is.na(report_input_file) & is.na(sim_data_file)){
        stop("You must enter a value for 'report_input_file' or include a specific simulator output file for 'sim_data_file'.")
    }
    
    if(complete.cases(report_input_file) & is.na(sim_data_file)){
        
        sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                      sheet_report = sheet_report)
        
        # Should we add an error catch here for when user fills out
        # report_input_file but doesn't include any observed data to
        # compare? Maybe not. If the user doesn't want to include any obs
        # data there, just fill out sim_data_file.
    } else {
        sectionInfo <- FALSE
    }
    
    # Figuring out what kind of means user wants, experimental details, etc.
    
    # First, the scenarios where there are observed data to compare (sectionInfo
    # exists)
    if(class(sectionInfo) != "logical"){
        
        sim_data_file <- sectionInfo$sim_data_file
        MeanType <- ifelse(is.na(mean_type),
                           sectionInfo$ObsData$MeanType,
                           mean_type)
        MeanType <- ifelse(is.na(MeanType), "geometric", MeanType)
        GMR_mean_type <- sectionInfo$ObsData$GMR_mean_type
        if(is.null(GMR_mean_type)){GMR_mean_type <- MeanType}
        Deets <- sectionInfo
        EffectorPresent <- complete.cases(Deets$Inhibitor1)
        DoseRegimen <- Deets$Regimen_sub
        
    } else {
        
        # And second, the scenario where user has only supplied a simulator
        # output file, so there are no observed data for comparisons.
        MeanType <- ifelse(is.na(mean_type), "geometric", mean_type)
        GMR_mean_type <- "geometric"
        Deets <- extractExpDetails(sim_data_file = sim_data_file)
        EffectorPresent <- complete.cases(Deets$Inhibitor1)
        DoseRegimen <- Deets$Regimen_sub
        sim_data_file <- sim_data_file
    }
    
    if(complete.cases(PKparameters[1])){
        PKToPull <- PKparameters
        
    } else {
        if(class(sectionInfo) == "logical"){
            # The most commonly requested PK parameters
            PKToPull <- AllPKParameters %>%
                # Per Hannah and template: Only include CL/F, t1/2, or tmax if
                # there's a specific reason to.
                filter(str_detect(PKparameter, "AUCinf|AUCtau|Cmax")) %>%
                filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>%
                pull(PKparameter) %>% unique()
        } else {
            # The PK parameters that match the observed data
            PKToPull <- AllPKParameters %>% 
                filter(PKparameter %in% names(sectionInfo$ObsData)) %>% 
                pull(PKparameter) %>% unique()
        }
    }
    
    # If dose regimen were single-dose, then only pull dose 1 data.
    if(Deets$Regimen_sub == "Single Dose"){
        SDParam <- AllPKParameters %>%
            filter(AppliesToSingleDose == TRUE) %>%
            pull(PKparameter)
        PKToPull <- PKToPull[PKToPull %in% SDParam]
    } else {
        # If it were multiple dose *and* if they did not specify PK parameters
        # to pull or have observed data to compare, then only pull ss
        # parameters.
        if(is.na(PKparameters[1]) & class(sectionInfo) == "logical"){
            PKToPull <- PKToPull[!str_detect(PKToPull, "_dose1")]
        }
    }
    
    # If there was no effector, then don't pull any interaction info
    if(is.na(Deets$Inhibitor1)){
        EffParam <- AllPKParameters %>%
            filter(AppliesOnlyWhenEffectorPresent == TRUE) %>%
            pull(PKparameter)
        
        PKToPull <- PKToPull[!PKToPull %in% EffParam]
    }
    
    # Getting PK parameters from the AUC tab
    suppressMessages(
        MyPKResults_all <- extractPK(sim_data_file = sim_data_file,
                                     PKparameters = PKToPull,
                                     sheet = sheet_PKparameters, 
                                     returnAggregateOrIndiv =
                                         switch(as.character(includeTrialMeans),
                                                "TRUE" = c("aggregate", "individual"),
                                                "FALSE" = "aggregate"))
    )
    
    # If they only wanted one parameter, then extractPK returns only the
    # aggregate data for that one parameter. In that situation, the names of the
    # items in the extractPK output list are that parameter and "QC" -- not
    # "aggregate" and "QC".
    if(length(PKToPull) == 1){
        if("aggregate" %in% names(MyPKResults_all)){
            MyPKResults <- data.frame(MyPKResults_all$aggregate)
            MyPKResults$Statistic = names(MyPKResults_all$aggregate[[1]])
        } else {
            MyPKResults <- data.frame(MyPKResults_all[[1]])
            MyPKResults$Statistic = names(MyPKResults_all[[1]])
        }
        
        names(MyPKResults)[1] <- PKToPull
        
    } else {
        MyPKResults <- MyPKResults_all$aggregate
    }
    
    # Accounting for when mean_type is arithmetic but the user requests that the
    # ratio for + effector over - effector be a GMR. This will replace the
    # arithmetic mean ratio data with geometric mean ratio data. However,
    # because later we need to join that with obs data and we need to use the
    # correct mean type throughout, this will be labeled as "mean" rather than
    # "geomean". Yes, that's confusing, so my apologies, but I couldn't come up
    # with a better way to do this. -LS
    if(MeanType == "arithmetic" &&
       EffectorPresent == TRUE &&
       complete.cases(GMR_mean_type) &&
       GMR_mean_type == "geometric"){
        
        MyPKResults[MyPKResults$Statistic == "Mean",
                    str_detect(names(MyPKResults), "ratio")] <-
            MyPKResults[MyPKResults$Statistic == "Geometric Mean",
                        str_detect(names(MyPKResults), "ratio")]
    }
    
    
    # Adding trial means since they're not part of the default output
    if(includeTrialMeans){
        # TrialMeans <- MyPKResults_all$individual %>%
        #     group_by(Trial) %>%
        #     summarize(across(.cols = -Individual,
        #                      .fns = list(switch(MeanType,
        #                                    "geometric" = gm_mean,
        #                                    "arithmetic" = mean), 
        #                               median), 
        #                      .names = "{.col}_{.fn}")) %>%
        #     ungroup()
        
        TrialMeans <- MyPKResults_all$individual %>%
            group_by(Trial) %>%
            summarize(across(.cols = -Individual,
                             .fns = list("geomean" = gm_mean, 
                                         "mean" = mean, 
                                         "median" = median), 
                             .names = "{.col}-{.fn}")) %>%
            ungroup() %>%
            pivot_longer(cols = -Trial, names_to = "Parameter",
                         values_to = "Value") %>%
            separate(col = Parameter, into = c("Parameter", "Stat"), 
                     sep = "-") %>% 
            filter((str_detect(Parameter, "tmax") & Stat == "median") |
                       (!str_detect(Parameter, "tmax") & 
                            Stat == switch(MeanType, "geometric" = "geomean", 
                                           "arithmetic" = "mean"))) %>% 
            group_by(Parameter) %>%
            summarize(MinMean = min(Value),
                      MaxMean = max(Value)) %>%
            pivot_longer(cols = -Parameter,
                         names_to = "Statistic", values_to = "Value") %>%
            pivot_wider(names_from = Parameter, values_from = Value)
        
        MyPKResults <- MyPKResults %>% bind_rows(TrialMeans)
    }
    
    # Renaming stats for ease of coding
    MyPKResults <- MyPKResults %>% mutate(Stat = renameStats(Statistic))
    
    # Adjusting tmax values since the mean row will actually be the median, the
    # lower range of conf interval and percentiles will be the min, and the
    # upper range will be the max.
    if("tmax_dose1" %in% names(MyPKResults)){
        MyPKResults$tmax_dose1[
            which(MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean"))] <-
            MyPKResults$tmax_dose1[which(MyPKResults$Stat == "median")]
        
        MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "min"]
        
        MyPKResults$tmax_dose1[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_dose1[MyPKResults$Stat == "max"]
        
        if(EffectorPresent){
            MyPKResults$tmax_dose1_withInhib[
                MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
                MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "median"]
            
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
                MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "min"]
            
            MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
                MyPKResults$tmax_dose1_withInhib[MyPKResults$Stat == "max"]
        }
    }
    
    if("tmax_ss" %in% names(MyPKResults)){
        MyPKResults$tmax_ss[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "median"]
        MyPKResults$tmax_ss[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "min"]
        MyPKResults$tmax_ss[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_ss[MyPKResults$Stat == "max"]
        
        if(EffectorPresent){
            MyPKResults$tmax_ss_withInhib[
                MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
                MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "median"]
            MyPKResults$tmax_ss_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
                MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "min"]
            MyPKResults$tmax_ss_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
                MyPKResults$tmax_ss_withInhib[MyPKResults$Stat == "max"]
        }
        
    }
    
    # For scenario where user specifies which tab to get data from
    if("tmax" %in% names(MyPKResults)){
        MyPKResults$tmax[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax[MyPKResults$Stat == "median"]
        MyPKResults$tmax[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax[MyPKResults$Stat == "min"]
        MyPKResults$tmax[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax[MyPKResults$Stat == "max"]
        
        if(EffectorPresent){
            MyPKResults$tmax_withInhib[
                MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
                MyPKResults$tmax_withInhib[MyPKResults$Stat == "median"]
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
                MyPKResults$tmax_withInhib[MyPKResults$Stat == "min"]
            MyPKResults$tmax_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
                MyPKResults$tmax_withInhib[MyPKResults$Stat == "max"]
        }
        
    }
    
    # CV and SD should be NA for all tmax values b/c we're reporting medians and
    # range and NOT reporting a mean or geometric mean. Setting that.
    MyPKResults <- MyPKResults %>% 
        mutate(across(.cols = matches("tmax"), 
                      ~replace(., Stat %in% c("CV", "GCV", "SD"), NA)))
    
    VarOptions <- c("CV" = includeCV & MeanType == "arithmetic", 
                    "GCV" = includeCV & MeanType == "geometric",
                    "CI90_low" = includeConfInt,
                    "CI90_high" = includeConfInt, 
                    "CI95_low" = includeConfInt,
                    "CI95_high" = includeConfInt, 
                    "per5" = includePerc, 
                    "per95" = includePerc, 
                    "MinMean" = includeTrialMeans, 
                    "MaxMean" = includeTrialMeans)
    VarOptions <- names(VarOptions)[which(VarOptions)]
    VarOptions <- intersect(VarOptions, MyPKResults$Stat)
    
    MyPKResults <- MyPKResults %>%
        filter(Stat %in% c(VarOptions, 
                           switch(MeanType, "geometric" = "geomean", 
                                  "arithmetic" = "mean"))) %>%
        select(-Statistic) %>%
        select(Stat, everything()) %>%
        pivot_longer(cols = -Stat, names_to = "PKParam",
                     values_to = "Sim")
    
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
        
        MyObsPK <- sectionInfo$ObsData[names(sectionInfo$ObsData) %in% MyObsPKParam] %>%
            as.data.frame() %>% t() %>% as.data.frame() %>%
            rename("Obs" = V1) %>%
            mutate(PKParam = row.names(.),
                   Obs = as.numeric(Obs), 
                   Stat = ifelse(str_detect(PKParam, "_CV"), 
                                 ifelse({{MeanType}} == "geometric", "GCV", "CV"), 
                                 ifelse({{MeanType}} == "geometric", "geomean", "mean")), 
                   # Accounting for when the mean ratios for obs data are
                   # actually geometric even though the other obs data means are
                   # arithmetic. This will label observed data GMR values as
                   # "mean" (for arithmetic means) rather than "geomean" so that
                   # it will be easier to return only the correct mean types. I
                   # know that's confusing, but I couldn't come up with a better
                   # way to do that, so my apologies! -LS
                   Stat = ifelse({{MeanType}} == "arithmetic" &
                                     {{GMR_mean_type}} == "geometric" &
                                     str_detect(PKParam, "ratio") &
                                     str_detect(Stat, "mean"), # detecting mean or geomean but not CV 
                                 "mean", Stat), 
                   PKParam = sub("_CV", "", PKParam))
        
        # Calculating S/O
        suppressMessages(
            SOratios <- MyPKResults %>% 
                filter(Stat == ifelse(MeanType == "geometric", "geomean", "mean")) %>%
                left_join(MyObsPK %>% 
                              filter(Stat == 
                                         ifelse(MeanType == "geometric", "geomean", "mean"))) %>%
                mutate(Value = Sim / Obs,
                       Stat = "S_O", 
                       SorO = "S_O") %>%
                select(PKParam, Stat, Value, SorO)
        )
        
        suppressMessage(
            MyPKResults <- MyPKResults %>% 
                full_join(MyObsPK) %>% 
                pivot_longer(names_to = "SorO", values_to = "Value", 
                             cols = c(Sim, Obs)) %>% 
                full_join(SOratios)
        )
        
        # If user supplied obs data and no PK parameters that they specifically
        # wanted, only keep PK parameters where there are values for the observed
        # mean data. 
        PKToPull <- MyObsPK %>% filter(complete.cases(Obs)) %>% 
            pull(PKParam) %>% unique()
        
    } else {
        MyPKResults <- MyPKResults %>% 
            rename(Value = Sim) %>% 
            mutate(SorO = "Sim")
    }
    
    
    # Putting everything together and formatting -------------------------
    
    # Formatting and selecting only rows where there are data
    MyPKResults <- MyPKResults %>%
        mutate(Value = if_else(str_detect(Stat, "CV"),
                               as.character(round(Value * 100, 0)),
                               # Per Christiane: 3 sig figs for everything
                               # or full number when > 100
                               if_else(Value > 100,
                                       as.character(round(Value, 0)), 
                                       # This next convoluted bit will
                                       # retain trailing zeroes since
                                       # "signif" alone will not
                                       formatC(signif(Value,digits=3), 
                                               digits=3,format="fg", 
                                               flag="#"))),
               # Removing any trailing decimal points with nothing after them
               Value = sub("\\.$", "", Value)) %>%
        filter(Stat %in% c(ifelse(MeanType == "geometric", "geomean", "mean"),
                           "CI90_low", "CI90_high", "CI95_low", "CI95_high",
                           "per5", "per95", 
                           ifelse(MeanType == "geometric", "GCV", "CV"), 
                           "MinMean", "MaxMean", "S_O")) %>%
        pivot_wider(names_from = PKParam, values_from = Value) %>% 
        mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O")), 
               Stat = factor(Stat, levels = c("mean", "geomean", "CV", "GCV",
                                              "CI90_low", "CI90_high", "CI95_low", 
                                              "CI95_high", "per5", "per95",
                                              "MinMean", "MaxMean", "S_O"))) %>% 
        arrange(SorO, Stat) %>% 
        filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
        mutate(across(.cols = everything(), .fns = as.character)
               # ,
               # across(.cols = -c(Stat, SorO),
               #        .fns = function(x) ifelse(str_detect(Stat, "CV"),
               #                                  paste0(x, "%"), x)),
               # across(.cols = everything(),
               #        .fns = function(x) ifelse(x == "NA%", NA, x))
        ) 
    # If this throws an error for you, try running "tidyverse_update()", copy
    # whatever it says is out of date, restart your R session (Ctrl Shift
    # F10), and then paste the output (something like
    # "install.packages(c("dbplyr", "dplyr", "dtplyr", ... ") and execute.
    
    # Putting trial means into appropriate format
    if(includeTrialMeans){
        TM <- MyPKResults %>% filter(Stat %in% c("MinMean", "MaxMean")) %>%
            summarize(across(.cols = -c(Stat, SorO),
                             .fns = function(x) {paste(x[1], "to", x[2])}))
        MyPKResults <- MyPKResults %>%
            filter(Stat != "MaxMean")
        MyPKResults[which(MyPKResults$Stat == "MinMean"), 
                    3:ncol(MyPKResults)] <-
            TM
    }
    
    # Concatenating the rows w/lower and upper limits of variability when
    # requested
    if(concatVariability){
        
        # Note: When multiple options are chosen for variability type,
        # concatVariability doesn't work. Will need to fix this later.
        VarRows <- list("ConfInt90" = c("CI90_low", "CI90_high"), 
                        "ConfInt95" = c("CI95_low", "CI95_high"),
                        "Perc" = c("per5", "per95"))
        VarRows <- VarRows[sapply(VarRows, function(x) unlist(x[[1]])) %in% VarOptions]
        
        VarRows[["obs"]] <- c("CIL_obs", "CIU_obs")
        for(j in names(VarRows)){
            temp <- MyPKResults %>%
                filter(Stat %in% as.character(unlist(VarRows[[j]]))) %>%
                mutate(across(.cols = !matches("Stat"),
                              .fns = function(x) {
                                  ifelse(all(complete.cases(c(x[1], x[2]))),
                                         paste(x[1], "to", x[2]), NA)}),
                       Stat = switch(j,
                                     "ConfInt90" = "CI90concat",
                                     "ConfInt95" = "CI95concat",
                                     "Perc" = "per95concat",
                                     "obs" = "CIobsconcat"))
            
            MyPKResults[which(MyPKResults$Stat == VarRows[[j]][1]), ] <-
                temp[1, ]
            MyPKResults <- MyPKResults %>% filter(Stat != VarRows[[j]][2])
            rm(temp)
        }
    }
    
    # Renaming statistics to match what's in template
    StatNames <- c("geomean" = "Simulated",
                   "mean" = "Simulated",
                   "GCV" = "CV%",
                   "CV" = "CV%",
                   "CI90_low" = "90% CI - Lower",
                   "CI90_high" = "90% CI - Upper",
                   "CI90concat" = "90% CI",
                   "CI95_low" = "95% CI - Lower",
                   "CI95_high" = "95% CI - Upper",
                   "CI95concat" = "95% CI",
                   "per5" = "5th Percentile",
                   "per95" = "95th Percentile",
                   "per95concat" = "5th to 95th Percentile",
                   # "geomean_obs" = "Observed",
                   # "CV_obs" = "CV%",
                   # "CIL_obs" = "observed CI - Lower",
                   # "CIU_obs" = "observed CI - Upper",
                   # "CIobsconcat" = "Observed CI",
                   "S_O" = "S/O",
                   "MinMean" = "Range of trial means")
    
    MyPKResults <- MyPKResults %>%
        mutate(Statistic = as.character(Stat),
               Statistic = StatNames[Statistic], 
               Statistic = ifelse(SorO == "Obs" & Statistic == "Simulated", 
                                  "Observed", Statistic)) %>%
        select(-Stat) %>%
        select(Statistic, everything())
    
    # setting levels for PK parameters so that they're in a nice order
    PKToPull <- factor(PKToPull, 
                       levels = AllPKParameters %>% select(PKparameter, SortOrder) %>% 
                           unique() %>% arrange(SortOrder) %>% pull(PKparameter)) 
    PKToPull <- sort(PKToPull)
    
    # If the user specified the sheet to use, we don't actually know whether
    # those were dose 1 or ss values, so extractPK removes the suffix from the
    # parameter. PKToPull still has the suffix, though, so removing it here or
    # else there will be 0 columns that match.
    if(complete.cases(sheet_PKparameters)){
        PKToPull <- as.character(sub("_ss|_dose1", "", as.character(PKToPull)))
        PKToPull <- factor(PKToPull, levels = PKToPull)
    }
    
    # Getting columns in a good order
    MyPKResults <- MyPKResults %>%
        select(any_of(c("Statistic", as.character(PKToPull))))
    
    # Optionally adding final column names
    if(prettify_columns){
        
        PKToPull_pretty <-
            sapply(as.character(PKToPull),
                   FUN = function(x) ifelse(str_detect(x, "_dose1"),
                                            paste("Dose 1", sub("_dose1", "", x)),
                                            paste("Steady-state", sub("_ss", "", x))))
        PKToPull_pretty <- sub("_withInhib", " with inhibitor", PKToPull_pretty)
        PKToPull_pretty <- sub("CL", "CL/F (L/h)", PKToPull_pretty)
        PKToPull_pretty <- sub("\\(L/h\\)_ratio", "ratio", PKToPull_pretty) # CL ratios
        PKToPull_pretty <- sub("AUCinf", "AUC0 to inf (h*ng/mL)", PKToPull_pretty)
        PKToPull_pretty <- sub("AUCtau", "AUC0 to tau (h*ng/mL)", PKToPull_pretty)
        PKToPull_pretty <- sub("\\(h\\*ng/mL\\)_ratio", "ratio", PKToPull_pretty) # AUCtau or AUCinf ratios
        PKToPull_pretty <- sub("Cmax", "Cmax (ng/mL)", PKToPull_pretty)
        PKToPull_pretty <- sub("\\(ng/mL\\)_ratio", "ratio", PKToPull_pretty) # Cmax ratios
        PKToPull_pretty <- sub("HalfLife", "t1/2 (h)", PKToPull_pretty)
        PKToPull_pretty <- sub("tmax", "tmax (h)", PKToPull_pretty)
        PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")] <-
            sub(" \\(", " with inhibitor (", PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")])
        PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")] <-
            sub(" with inhibitor$", "", PKToPull_pretty[str_detect(PKToPull_pretty, "with inhibitor")])
        PKToPull_pretty <- sub("GMR_", "geometric mean ratio ", PKToPull_pretty)
        PKToPull_pretty <- c("Statistic" = "Statistic", PKToPull_pretty)
        
        names(MyPKResults) <- PKToPull_pretty[names(MyPKResults)]
        
    }
    
    if(complete.cases(save_table)){
        if(str_detect(save_table, "\\.")){
            FileName <- sub("\\..*", ".csv", save_table)
        } else {
            FileName <- paste0(save_table, ".csv")
        }
        write.csv(MyPKResults, FileName, row.names = F)
    }
    
    if(checkDataSource){
        MyPKResults <- list("Table" = MyPKResults,
                            "QC" = MyPKResults_all$QC)
        
        if(complete.cases(save_table)){
            if(str_detect(save_table, "\\.")){
                FileName <- sub("\\..*", " - QC.csv", save_table)
            } else {
                FileName <- paste0(save_table, " - QC.csv")
            }
            write.csv(MyPKResults_all$QC, FileName, row.names = F)
        }
    }
    
    return(MyPKResults)
}



