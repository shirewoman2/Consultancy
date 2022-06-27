#' Make summary PK tables for reports
#'
#' \code{pksummary_table} creates tables of PK parameters for reports and
#' presentations, including reporting means, CVs, and confidence intervals or
#' percentiles and, optionally, comparisons to observed data. This function
#' automatically finds the correct tab and the correct cells in a Simulator
#' output Excel file to obtain those data. \strong{Notes:} \itemize{\item{Please
#' see the notes at the bottom of this help file for how to supply observed data
#' in a standardized fashion that this function can read.} \item{Nearly all
#' parameters are for the \emph{substrate}. We're still validating this for
#' extracting PK for an effector. \strong{A request for assistance:} If you
#' extract PK data for an effector by specifying an Excel sheet for that
#' compound, please check the values and tell Laura Shireman how well it works!}
#' \item{ If the simulator output Excel file lives on SharePoint, you'll need to
#' close it or this function will just keep running and not generate any output
#' while it waits for access to the file.}}
#'
#' Because we need to have a standardized way to input observed data, setting up
#' the input for this function requires creating a data.frame of the observed PK
#' data, supplying a csv or Excel file with observed PK data, or filling out an
#' Excel form.
#'
#' To create a data.frame or an Excel or csv file of observed PK data, you'll
#' need columns for each of the PK parameters for which you have observed data.
#' If you have CV values for any observed data that you'd like to include in the
#' table, title that column with the PK parameter name and a suffix of "_CV". An
#' example of how to format observed data: \code{data.frame(AUCinf_dose1 = 60,
#' AUCinf_dose1_CV = 0.38, Cmax_dose1 = 22, Cmax_dose1_CV = 0.24)}
#'
#' To use an Excel form, here are the steps to take: \enumerate{\item{Use the
#' function \code{\link{generateReportInputForm}} to create an Excel file where
#' you can enter information about your project. Example:
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
#' @param sim_data_file a simulator output file. If you supply a filled-out
#'   report input form to the argument \code{report_input_file}, you can leave
#'   this blank.
#' @param report_input_file (an optional alternative to \code{sim_data_file})
#'   the name of the Excel file created by running
#'   \code{\link{generateReportInputForm}}, which you have now filled out,
#'   including the path if it's in any other directory than the current one
#' @param sheet_report the sheet in the Excel report file that contains
#'   information about the study, e.g., "study info - DDI" or "study info - no
#'   DDI" if you haven't renamed the tab. This only applies if you have supplied
#'   an Excel file name for \code{report_input_file}. If you're supplying a
#'   simulator output Excel file for \code{sim_data_file}, ignore this.
#' @param PKparameters (optional) the PK parameters to include as a character
#'   vector. Notes: \itemize{
#'
#'   \item{By default, if you have a single-dose simulation, the parameters will
#'   include AUC and Cmax for dose 1, and, if you have a multiple-dose
#'   simulation, AUC and Cmax for the last dose. Also by default, if you have an
#'   effector present, the parameters will include the AUC and Cmax values with
#'   and without the effector as well as those ratios.}
#'
#'   \item{Alternatively, you can specify a vector of any combination of
#'   specific, individual parameters, e.g., \code{c("Cmax_dose1",
#'   "AUCtau_last").} Be sure to encapsulate the parameters you want with
#'   \code{c(...)}! To see the full set of possible parameters to extract, enter
#'   \code{data(PKParameterDefinitions); view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}
#'
#'   \item{If you supply observed data using either the argument
#'   \code{report_input_file} or the argument \code{observed_PK}, the PK
#'   parameters included are only those available for the observed data.}
#'
#'   \item{Parameters that don't make sense for your scenario -- such as asking
#'   for \code{AUCinf_dose1_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.}
#'
#'   \item{tmax will be listed as median, min, and max rather than mean, lower
#'   and higher X\% confidence interval or X percentiles. Similarly, if you
#'   request trial means, the values for tmax will be the range of medians for
#'   the trials rather than the range of means.}} An example of acceptable input
#'   here: \code{c("AUCtau_last", "AUCtau_last_withInhib", "Cmax_last",
#'   "Cmax_last_withInhib", "AUCtau_ratio_last", "Cmax_ratio_last")}.
#' @param PKorder Would you like the order of the PK parameters to be the the
#'   order specified in the Consultancy Report Template (default), or would you
#'   like the order to match the order you specified with the argument
#'   \code{PKparameters}? Options are "default" or "user specified".
#' @param sheet_PKparameters (optional) If you want the PK parameters to be
#'   pulled from a specific tab in the simulator output file, list that tab
#'   here. Most of the time, this should be left as NA.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default) or "blood" (possible but not as thoroughly
#'   tested).
#' @param observed_PK (optional) If you have a data.frame, a named numeric
#'   vector, or an Excel or csv file with observed PK parameters, supply the
#'   full file name in quotes or the data.frame or vector here, and the
#'   simulated-to-observed mean ratios will be calculated. (If you supply an
#'   Excel file, it should have only one tab. We prefer supplying csv files here
#'   since they're faster to read in anyway.) The supplied data.frame or file
#'   must include columns for each of the PK parameters you would like to
#'   compare, and those column names \emph{must} be among the PK parameter
#'   options listed in \code{data(PKParameterDefinitions)}. If you would like
#'   the output table to include the observed data CV for any of the parameters,
#'   add "_CV" to the end of the parameter name, e.g., "AUCinf_dose1_CV". Please
#'   see the "Example" section of this help file for examples of how to set this
#'   up.
#' @param mean_type return "arithmetic" or "geometric" (default) means and CVs.
#'   If you supplied a report input form, only specify this if you'd like to
#'   override the value listed there.
#' @param includeTrialMeans TRUE or FALSE (default) for whether to include the
#'   range of trial means for a given parameter. Note: This is calculated from
#'   individual values rather than being pulled directly from the output.
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param includePerc TRUE or FALSE (default) for whether to include 5th to 95th
#'   percentiles
#' @param concatVariability TRUE or FALSE (default) for whether to concatenate
#'   the variability. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile, e.g., "2400 to 2700". Please note that the current
#'   SimcypConsultancy template lists one row for each of the upper and lower
#'   values, so this should be set to FALSE for official reports.
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1".
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, effectors, or effector metabolites are among the standard
#'   options for the simulator, and leaving \code{prettify_compound_names =
#'   TRUE} will make the name of those compounds something more human readable.
#'   For example, "SV-Rifampicin-MD" will become "rifampicin", and
#'   "Sim-Midazolam" will become "midazolam". Set each compound to the name
#'   you'd prefer to see in your column titles if you would like something
#'   different. For example, \code{prettify_compound_names = c("inhibitor" =
#'   "defartinib", "substrate" = "superstatin")}. Please note that "inhibitor"
#'   includes \emph{all} the effectors and effector metabolites present, so, if
#'   you're setting the effector name, you really should use something like this
#'   if you're including effector metabolites: \code{prettify_compound_names =
#'   c("inhibitor" = "defartinib and 1-OH-defartinib", "substrate" =
#'   "superstatin")}.
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file. If you supply only the file
#'   extension, e.g., \code{save_table = "docx"}, the name of the file will be
#'   the file name plus "PK summary table" with that extension and output will
#'   be located in the same folder as \code{sim_data_file}. If you supply
#'   something other than just "docx" or just "csv" for the file name but you
#'   leave off the file extension, we'll assume you want it to be ".csv". While
#'   the main PK table data will be in whatever file format you requsted, if you
#'   set \code{checkDataSource = TRUE}, the QC data will be in a csv file on its
#'   own and will have "- QC" added to the end of the file name.
#'   \strong{WARNING:} SAVING TO WORD DOES NOT WORK ON SHAREPOINT. This is a
#'   Microsoft permissions issue, not an R issue. If you try to save on
#'   SharePoint, you will get a warning that R will save your file instead to
#'   your Documents folder.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return Returns a data.frame of PK summary data and, if observed data were
#'   provided, simulated-to-observed ratios. If \code{checkDataSource = TRUE},
#'   output will instead be a list of that data.frame (named "Table") and
#'   information on where the values came from for QCing (named "QC").
#' @export
#' @examples
#' pksummary_table(report_input_file = "//certara.com/data/sites/SHF/Consult/abc-1a/Report input.xlsx",
#'          sheet_report = "study info - DDI",
#'          includeTrialMeans = TRUE)
#'
#' # An example of how to format observed data as a data.frame:
#' MyObsPK <- data.frame(AUCinf_dose1 = 60,
#'                       AUCinf_dose1_CV = 0.38,
#'                       Cmax_dose1 = 22,
#'                       Cmax_dose1_CV = 0.24)
#'
#' # Or you can supply a named numeric vector:
#' MyObsPK <- c("AUCinf_dose1" = 60,
#'              "AUCinf_dose1_CV" = 0.38,
#'              "Cmax_dose1" = 22,
#'              "Cmax_dose1_CV" = 0.24)
#'
#' pksummary_table(sim_data_file = "mdz-5mg-sd.xlsx", observed_PK = MyObsPK)

pksummary_table <- function(sim_data_file = NA, 
                            PKparameters = NA,
                            PKorder = "default", 
                            sheet_PKparameters = NA,
                            mean_type = NA,
                            tissue = "plasma",
                            observed_PK = NA, 
                            report_input_file = NA,
                            sheet_report = NA,
                            includeCV = TRUE,
                            includeConfInt = TRUE,
                            includePerc = FALSE,
                            includeTrialMeans = FALSE,
                            concatVariability = FALSE,
                            prettify_columns = TRUE,
                            prettify_compound_names = TRUE, 
                            checkDataSource = TRUE, 
                            save_table = NA, 
                            fontsize = 11){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    
    # Main body of function --------------------------------------------------
    # If they supplied observed_PK, get the sim_data_file from that. 
    if(complete.cases(observed_PK[1]) && 
       (class(observed_PK) == "character" | "data.frame" %in% class(observed_PK))){
        if(class(observed_PK) == "character"){
            observed_PK <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                  "csv" = read.csv(observed_PK), 
                                  "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                           sheetIndex = 1))
        }
        
        # At this point, observed_PK should be a data.frame b/c it either was a
        # data.frame at the outset or it has been created by reading an Excel or
        # csv file.
        
        # There should be only 1 row in observed_PK, so removing any extras that
        # might have gotten included accidentally.
        observed_PK <- observed_PK[1, ]
        
    }
    
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
    
    if(PKorder %in% c("default", "user specified") == FALSE){
        stop(paste0("The value '", PKorder, "' is not one of the possibilities for the argument 'PKorder'. Please enter one of 'default' or 'user specified'."))
    }
    
    if(PKorder != "default" & is.na(PKparameters[1])){
        warning("You have requested 'user specified' for the argument 'PKorder', which sets the order of columns in the table, but you have not specified what that order should be with the argument 'PKparameters'. The order will be the default order from the Consultancy Report Template.", 
                call. = FALSE)
        PKorder <- "default"
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
        # If user specified "_first" instead of "_dose1", make that work, too. 
        PKToPull <- sub("_first", "_dose1", PKparameters)
        
    } else {
        
        if(class(sectionInfo) == "logical"){ # sectionInfo is logical if they did not supply a report input form
            if(complete.cases(observed_PK[1])){
                # If user supplies an observed file, then pull the parameters
                # they want to match.
                if(class(observed_PK) == "character"){
                    observed_PK <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                          "csv" = read.csv(observed_PK), 
                                          "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                                   sheetIndex = 1))
                }
                
                # If user specified "_first" instead of "_dose1", make that
                # work, too. 
                PKToPull <- sub("_first", "_dose1", tolower(names(observed_PK)))
                
            } else {
                # If the user didn't specify an observed file, didn't list
                # specific parameters they want, and didn't fill out a report
                # input form, then pull the most commonly requested PK
                # parameters.
                PKToPull <- AllPKParameters %>%
                    # Per Hannah and template: Only include CL/F, t1/2, or tmax
                    # if there's a specific reason to.
                    filter(str_detect(PKparameter, "AUCinf|AUCtau|Cmax")) %>%
                    filter(!str_detect(PKparameter, "_hepatic|CLpo")) %>%
                    pull(PKparameter) %>% unique()
            }
            
        } else {
            # Pull the PK parameters that match the observed data in the report
            # input form if one was provided.
            PKToPull <- AllPKParameters %>% 
                filter(PKparameter %in% names(sectionInfo$ObsData)) %>% 
                pull(PKparameter) %>% unique()
        }
    }
    
    # Allowing for flexibility in case. Get the lower-case version of whatever
    # PKparameters user specified and match them to the correct PKparameters in
    # AllPKParameters.
    PKToPull <- AllPKParameters %>%
        mutate(PKparameter_lower = tolower(PKparameter)) %>% 
        filter(PKparameter_lower %in% tolower(PKToPull)) %>% 
        pull(PKparameter) %>% unique()
    
    # If dose regimen were single-dose, then only pull dose 1 data.
    if(Deets$Regimen_sub == "Single Dose"){
        SDParam <- AllPKParameters %>%
            filter(AppliesToSingleDose == TRUE) %>%
            pull(PKparameter)
        PKToPull <- PKToPull[PKToPull %in% SDParam]
    } else {
        # If it were multiple dose *and* if they did not specify PK parameters
        # to pull or have observed data to compare, then only pull last dose
        # parameters.
        if(is.na(PKparameters[1]) & class(sectionInfo) == "logical" & 
           is.na(observed_PK[[1]])){
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
    
    # Give a useful message if there are no parameters to pull
    if(length(PKToPull) == 0){
        stop("None of the parameters you requested are available from the supplied simulator output file. Please check that the parameters requested make sense for the simulation. For example, did you request multiple-dose parameters for a single-dose regimen?")
    }
    
    # Getting PK parameters. 
    suppressWarnings(
            MyPKResults_all <- extractPK(sim_data_file = sim_data_file,
                                         PKparameters = PKToPull,
                                         tissue = tissue,
                                         sheet = sheet_PKparameters, 
                                         returnAggregateOrIndiv =
                                             switch(as.character(includeTrialMeans),
                                                    "TRUE" = c("aggregate", "individual"),
                                                    "FALSE" = "aggregate")))
    
    # PKToPull must be changed if user specified a tab b/c then the parameters
    # won't have _last or _dose1 suffixes.
    if(complete.cases(sheet_PKparameters)){
        PKToPull <- sub("_last|_dose1", "", PKToPull)
    }
    
    # If they requested AUCinf but there was trouble with that extrapolation,
    # AUCinf won't be present in the data but AUCt will be. Check for that and
    # change PKToPull to reflect that change.
    if(any(str_detect(PKToPull, "AUCinf")) & 
       (("data.frame" %in% class(MyPKResults_all[[1]]) & 
         any(str_detect(names(MyPKResults_all[[1]]), "AUCinf")) == FALSE) |
        ("data.frame" %in% class(MyPKResults_all[[1]]) == FALSE &
         !str_detect(names(MyPKResults_all)[1], "AUCinf")))){
        warning("AUCinf included NA values, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. AUCt will be returned to use in place of AUCinf as you deem appropriate.",
                call. = FALSE)
        PKToPull <- sub("AUCinf", "AUCt", PKToPull)
    }
    
    # If they requested multiple parameters but only some were present, need to
    # change PKToPull. This is especially a problem if there's only 1 parameter
    # remaining for which there are data.
    if("data.frame" %in% class(MyPKResults_all[[1]])){
        Missing <- setdiff(PKToPull, names(MyPKResults_all[[1]]))
        PKToPull <- intersect(PKToPull, names(MyPKResults_all[[1]]))
    } else {
        # This is when there was only 1 parameter found.
        PKToPull <- names(MyPKResults_all)[1]
        Missing <- setdiff(AllPKParameters %>%
                               mutate(PKparameter_lower = tolower(PKparameter)) %>% 
                               filter(PKparameter_lower %in% tolower(PKparameters)) %>% 
                               pull(PKparameter), PKToPull)
    }
    
    if(length(Missing) > 0 & complete.cases(PKparameters[1])){
        warning(paste("The following parameters were requested but not found in your simulator output file:",
                      str_comma(Missing)),
                call. = FALSE)
    }
    
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
    
    if("tmax_last" %in% names(MyPKResults)){
        MyPKResults$tmax_last[
            MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "median"]
        MyPKResults$tmax_last[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "min"]
        MyPKResults$tmax_last[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
            MyPKResults$tmax_last[MyPKResults$Stat == "max"]
        
        if(EffectorPresent){
            MyPKResults$tmax_last_withInhib[
                MyPKResults$Stat == switch(MeanType, "geometric" = "geomean", "arithmetic" = "mean")] <-
                MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "median"]
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per5", "CI95_low", "CI90_low")] <-
                MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "min"]
            MyPKResults$tmax_last_withInhib[MyPKResults$Stat %in% c("per95", "CI95_high", "CI90_high")] <-
                MyPKResults$tmax_last_withInhib[MyPKResults$Stat == "max"]
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
                          "Cmax_ratio_last_90CIL", "Cmax_ratio_last_90CIU",
                          "AUCtau_ratio_last_90CIL", "AUCtau_ratio_last_90CIU")
    }
    
    # observed data -----------------------------------------------------
    if(class(sectionInfo) != "logical" | "data.frame" %in% class(observed_PK) |
       class(observed_PK)[1] == "numeric"){
        if(class(sectionInfo) != "logical"){ # this is when the user has supplied a report form.
            MyObsPK <- sectionInfo$ObsData[names(sectionInfo$ObsData) %in% MyObsPKParam] %>%
                as.data.frame() %>% t() %>% as.data.frame() %>%
                rename("Obs" = V1) %>%
                mutate(PKParam = row.names(.),
                       Obs = as.numeric(Obs))
        }  else { # this is when the user has NOT supplied a report form
            # Converting named vector to data.frame b/c everything else is set
            # up as a data.frame.
            if(class(observed_PK)[1] == "numeric"){
                observed_PK <- as.data.frame(t(observed_PK))
            }
            
            # Making obs PK names match correct PK parameters regardless of case
            ObsNames <- data.frame(OrigName = names(observed_PK)) %>% 
                mutate(PKparameter_lower = sub("_first", "_dose1",
                                               tolower(OrigName)), 
                       PKparameter_lower = sub("_cv", "", PKparameter_lower)) %>% 
                left_join(AllPKParameters %>% select(PKparameter) %>% 
                              unique() %>% 
                              mutate(PKparameter_lower = tolower(PKparameter))) %>% 
                mutate(PKparameter = ifelse(str_detect(tolower(OrigName), "cv"), 
                                            paste0(PKparameter, "_CV"), 
                                            PKparameter), 
                       PKparameter = ifelse(OrigName == "File", "File", PKparameter))
            
            MyObsPK <- observed_PK
            names(MyObsPK) <- ObsNames$PKparameter
            
            # Making observed_PK that was supplied as a data.frame or file long
            # w/column for PKparameter.
            MyObsPK <- MyObsPK %>% 
                pivot_longer(cols = any_of(c(AllPKParameters$PKparameter, 
                                             paste0(AllPKParameters$PKparameter, "_CV"))), 
                             names_to = "PKParam", 
                             values_to = "Obs")
        }
        
        MyObsPK <- MyObsPK %>% 
            mutate(Stat = ifelse(str_detect(PKParam, "_CV"), 
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
        
        if(complete.cases(sheet_PKparameters)){
            MyObsPK$PKParam <- sub("_first|_dose1|_last", "", MyObsPK$PKParam)
        }
        
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
        
        suppressMessages(
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
    PKlevels <- AllPKParameters %>% select(PKparameter, SortOrder) %>% 
        unique() %>% arrange(SortOrder) %>% pull(PKparameter)
    
    # PKlevels must be changed if user specified a tab b/c then the parameters
    # won't have _last or _dose1 suffixes.
    if(complete.cases(sheet_PKparameters)){
        PKlevels <- unique(sub("_last|_dose1", "", PKlevels))
        # When the suffix is included, then we get an order with 1st dose and
        # then last dose, which is appropriate, but when the user specifies a
        # tab, we need to change the order to get any AUC parameters before any
        # Cmax parameters. 
        PKlevels <- fct_relevel(PKlevels, c(PKlevels[str_detect(PKlevels, "AUC")], 
                                            PKlevels[str_detect(PKlevels, "Cmax")]))
        
        PKlevels <- sort(PKlevels)
    } 
    
    # If the user wants to specify the order, allowing that here.
    if(PKorder == "user specified"){
        PKlevels <- PKparameters
    }
    
    PKToPull <- factor(PKToPull, levels = PKlevels)
    PKToPull <- sort(PKToPull)
    
    # Getting columns in a good order
    MyPKResults <- MyPKResults %>%
        select(any_of(c("Statistic", as.character(PKToPull))))
    
    PKToPull <- as.character(intersect(PKToPull, names(MyPKResults)))
    
    # Optionally adding final column names
    if(prettify_columns){
        
        # If user specified tab, then need to adjust PK parameters here, too.
        if(complete.cases(sheet_PKparameters)){
            AllPKParameters_mod <- 
                AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
                mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                       PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
                                                      PrettifiedNames))) %>% 
                unique()
            
            suppressMessages(
                PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
                    left_join(AllPKParameters_mod) %>% 
                    pull(PrettifiedNames)
            )
        } else {
            suppressMessages(
                PrettyCol <- data.frame(PKparameter = PKToPull) %>% 
                    left_join(AllPKParameters %>% 
                                  select(PKparameter, PrettifiedNames)) %>% 
                    unique() %>% 
                    pull(PrettifiedNames)
            )
        }
        
        # Adjusting units as needed.
        PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
        PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
        PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
        PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
        
        MyEffector <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
                        Deets$Inhibitor2)
        
        if(any(complete.cases(MyEffector))){
            MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
            
            if(class(prettify_compound_names) == "logical" &&
               prettify_compound_names){
                MyEffector <- prettify_compound_name(MyEffector)
            }
            
            if(class(prettify_compound_names) == "character"){
                MyEffector <- prettify_compound_names["inhibitor"]
            }
            
            PrettyCol <- sub("effector", MyEffector, PrettyCol)
        }
        
        names(MyPKResults) <- c("Statistic", PrettyCol)
        
    }
    
    if(checkDataSource){
        
        ColsToInclude <- c("PKparam", "File", "Tab", 
                           switch(MeanType,
                                  "arithmetic" = "mean",
                                  "geometric" = "geomean"))
        
        if(includeConfInt){
            ColsToInclude <- c(ColsToInclude, "CI90_low", "CI90_high")
        }
        
        if(includeCV){
            ColsToInclude <- c(ColsToInclude, 
                               switch(MeanType, 
                                      "arithmetic" = "CV", 
                                      "geometric" = "GCV"))
        }
        
        if(includePerc){
            ColsToInclude <- c(ColsToInclude, "per5", "per95")
        }
        
        OutQC <- MyPKResults_all$QC %>% 
            select(PKparam, File, matches(ColsToInclude))
    }
    
    if(complete.cases(save_table)){
        
        # Checking whether they have specified just "docx" or just "csv" for
        # output b/c then, we'll use sim_data_file as file name. This allows us
        # to determine what the path should be, too, for either sim_data_file or
        # for some specified file name.
        if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
            OutPath <- dirname(sim_data_file)
            save_table <- sub("xlsx", 
                              # If they included "." at the beginning of the
                              # file exension, need to remove that here.
                              sub("\\.", "", save_table),
                              basename(sim_data_file))
        } else {
            # If they supplied something other than just "docx" or just "csv",
            # then check whether that file name is formatted appropriately.
            
            if(str_detect(basename(save_table), "\\..*")){
                if(str_detect(basename(save_table), "\\.docx") == FALSE){
                    # If they specified a file extension that wasn't docx, make that
                    # file extension be .csv
                    save_table <- sub("\\..*", ".csv", save_table)
                }
            } else {
                # If they didn't specify a file extension at all, make it .csv. 
                save_table <- paste0(save_table, ".csv")
            }
            
            # Now that the file should have an appropriate extension, check what
            # the path and basename should be.
            OutPath <- dirname(save_table)
            save_table <- basename(save_table)
        }
        
        if(str_detect(save_table, "docx")){ 
            # This is when they want a Word file as output
            
            # May need to change the working directory temporarily, so
            # determining what it is now
            CurrDir <- getwd()
            
            OutPath <- dirname(save_table)
            if(OutPath == "."){
                OutPath <- getwd()
            }
            
            # Check for whether they're trying to save on SharePoint, which DOES
            # NOT WORK. If they're trying to save to SharePoint, instead, save
            # to their Documents folder.
            
            # Side regex note: The myriad \ in the "sub" call are necessary b/c
            # \ is an escape character, and often the SharePoint and Large File
            # Store directory paths start with \\\\.
            if(str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$SharePtDir)){
                
                OutPath <- paste0("C:/Users/", Sys.info()[["user"]], 
                                  "/Documents")
                warning(paste0("You have attempted to use this function to save a Word file to SharePoint, and Microsoft permissions do not allow this. We will attempt to save the ouptut to your Documents folder, which we think should be ", 
                               OutPath,
                               ". Please copy the output to the folder you originally requested or try saving locally or on the Large File Store."), 
                        call. = FALSE)
            }
            
            LFSPath <- str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$LgFileDir)
            
            if(LFSPath){
                # Create a temporary directory in the user's AppData/Local/Temp
                # folder.
                TempDir <- tempdir()
                
                # Upon exiting this function, delete that temporary directory.
                on.exit(unlink(TempDir))
                
            }
            
            FileName <- basename(save_table)
            
            rmarkdown::render(system.file("rmarkdown/templates/pk-summary-table/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_dir = switch(as.character(LFSPath), 
                                                  "TRUE" = TempDir,
                                                  "FALSE" = OutPath),
                              output_file = FileName, 
                              quiet = TRUE)
            # Note: The "system.file" part of the call means "go to where the
            # package is installed, search for the file listed, and return its
            # full path.
            
            if(LFSPath){
                file.copy(file.path(TempDir, FileName), OutPath, overwrite = TRUE)
            }
            
        } else {
            # This is when they want a .csv file as output
            write.csv(MyPKResults, paste0(OutPath, "/", save_table), row.names = F)
        }
    }
    
    if(checkDataSource){
        MyPKResults <- list("Table" = MyPKResults,
                            "QC" = OutQC)
        
        if(complete.cases(save_table)){ 
            write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
        }
        
    }
    
    return(MyPKResults)
}



