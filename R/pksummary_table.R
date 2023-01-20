#' Make summary PK tables for reports
#'
#' \code{pksummary_table} creates tables of PK parameters for reports and
#' presentations, including reporting means, CVs, and confidence intervals or
#' percentiles and, optionally, comparisons to observed data. This function
#' automatically finds the correct tab and the correct cells in a Simulator
#' output Excel file to obtain those data. \strong{Notes:} \itemize{\item{Please
#' see the notes at the bottom of this help file for how to supply observed data
#' in a standardized fashion that this function can read.} \item{If you would
#' like to make a single PK table for multiple files at once, please see the
#' function \code{\link{pksummary_mult}}.} \item{All parameters are for the
#' \emph{substrate} unless you specify that you would like the PK parameters to
#' be pulled from a specific tab in the Excel output file that is for some other
#' compound.} \item{ If the simulator output Excel file lives on SharePoint,
#' you'll need to close it or this function will just keep running and not
#' generate any output while it waits for access to the file.}}
#'
#' Because we need to have a standardized way to input observed data, setting up
#' the input for this function requires creating a data.frame or named vector of
#' the observed PK data, supplying a csv or Excel file with observed PK data, or
#' filling out an Excel form.
#'
#' \strong{OPTION A: Supply a data.frame or a named vector.} If you supply a
#' data.frame, the column names will indicate which PK parameter you want, and
#' if you supply a named numeric vector, the names of the vector will perform
#' the same function. If you have CV values for any observed data that you'd
#' like to include in the table, make the name be the PK parameter with a suffix
#' of "_CV".
#'
#' An example of specifying a data.frame: \code{observed_PK =
#' data.frame(AUCinf_dose1 = 60, AUCinf_dose1_CV = 0.38, Cmax_dose1 = 22,
#' Cmax_dose1_CV = 0.24)}
#'
#' An example of specifying a named vector: \code{observed_PK = c("AUCinf_dose1"
#' = 60, "AUCinf_dose1_CV" = 0.38, "Cmax_dose1" = 22, "Cmax_dose1_CV" = 0.24)}.
#'
#' \strong{OPTION B: Use an Excel or csv file of oserved PK data.} In Excel,
#' create a single-tab Excel file or a csv file where the 1st row lists the
#' names of the PK parameters and the 2nd row lists the values. Just as with
#' Option A, you can include observed CV values by adding "_CV" to the parameter
#' name. To see an example of how this should look, run this in the console and
#' then open the csv file:
#'
#' \code{write.csv(data.frame(AUCinf_dose1 = 60, AUCinf_dose1_CV = 0.38,
#' Cmax_dose1 = 22, Cmax_dose1_CV = 0.24), file = "Example observed PK
#' values.csv", row.names = FALSE)}
#'
#' When you call on \code{pksummary_table}, use the following syntax,
#' substituting your file name for the example: \code{observed_PK = "Example
#' observed PK values.csv"}
#'
#' \strong{OPTION C: Fill out an Excel form.} Here are the steps to take for
#' this option: \enumerate{
#'
#' \item{Use the function \code{\link{generateReportInputForm}} to create an
#' Excel file where you can enter information about your project. Example:
#' \code{generateReportInputForm("My report input form.xlsx")}}
#'
#' \item{Go to the tab "study info - DDI" or "study info - no DDI", whichever is
#' appropriate for your situation. Under the heading "Simulated data", enter the
#' name of the specific simulator output Excel file you want to compare.}
#'
#' \item{Under the heading "Observed data" on that same tab, enter details about
#' your observed data. It's ok if you don't have all the information; anything
#' that's missing won't be included in the final S/O table. It's also ok to
#' rename this tab or make copies of it within the same Excel file for making
#' other S/O tables.}
#'
#' \item{Save the report input form.}
#'
#' \item{Back in RStudio, run this function using the file name of that Excel
#' report form as input for \code{report_input_file} and the name of the "study
#' info - DDI/no DDI" tab as the input for \code{sheet_report}. Note: If the
#' Excel file lives on SharePoint, you'll need to close it or this function will
#' just keep running and not generate any output while it waits for access to
#' the file.} }
#'
#'
#' @param sim_data_file a simulator output file. If you supplied a file name in
#'   a data.frame of observed PK or a csv or Excel file of observed PK for
#'   \code{observed_PK}, that file name will be used preferentially and you can
#'   leave this blank. Similarly, if you supply a filled-out report input form
#'   to the argument \code{report_input_file}, the file name you supplied
#'   \emph{there} will be used preferentially, and you can leave this blank.
#' @param report_input_file (optional) This argument is an alternative way to
#'   specify both what simulator Excel file to use and also what the observed PK
#'   parameters were. Input is the name of the Excel file created by running
#'   \code{\link{generateReportInputForm}}, which you have now filled out,
#'   including the path if it's in any other directory than the current one.
#'   Please see the "Details" section at the bottom for more information on this
#'   option.
#' @param sheet_report the sheet in the Excel report file that contains
#'   information about the study, e.g., "study info - DDI" or "study info - no
#'   DDI" if you haven't renamed the tab. This only applies if you have supplied
#'   an Excel file name for \code{report_input_file}. If you're supplying a
#'   simulator output Excel file for \code{sim_data_file}, ignore this.
#' @param PKparameters (optional) the PK parameters to include as a character
#'   vector. \itemize{
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
#'   \code{view(PKParameterDefinitions)} into the console.}
#'
#'   \item{If you supply observed data using either the argument
#'   \code{report_input_file} or the argument \code{observed_PK}, the only PK
#'   parameters that will be included are those available for the observed
#'   data.}
#'
#'   \item{Parameters that don't make sense for your scenario -- such as asking
#'   for \code{AUCinf_dose1_withInhib} when your simulation did not include an
#'   inhibitor or effector -- will not be included.}
#'
#'   \item{tmax will be listed as median, min, and max rather than mean, lower
#'   and higher X\% confidence interval or X percentiles. Similarly, if you
#'   request trial means, the values for tmax will be the range of medians for
#'   the trials rather than the range of means.}}
#'
#'   An example of acceptable input here: \code{PKparameters = c("AUCtau_last",
#'   "AUCtau_last_withInhib", "Cmax_last", "Cmax_last_withInhib",
#'   "AUCtau_ratio_last", "Cmax_ratio_last")}.
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
#'   simulated-to-observed mean ratios will be calculated. If you supply an
#'   Excel file, it should have only one tab. We prefer supplying csv files here
#'   since they're faster to read in anyway. The supplied data.frame or file
#'   must include columns for each of the PK parameters you would like to
#'   compare, and those column names \emph{must} be among the PK parameter
#'   options listed in \code{PKParameterDefinitions}. If you would like the
#'   output table to include the observed data CV for any of the parameters, add
#'   "_CV" to the end of the parameter name, e.g., "AUCinf_dose1_CV". Please see
#'   the "Example" section of this help file for examples of how to set this up.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default). If you supplied a
#'   report input form, only specify this if you'd like to override the value
#'   listed there.
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
#' @param includeRange TRUE or FALSE (default) for whether to include the
#'   minimum and maximum values
#' @param concatVariability TRUE or FALSE (default) for whether to concatenate
#'   the variability. If "TRUE", the output will be formatted into a single row
#'   and listed as the lower confidence interval or percentile to the upper CI
#'   or percentile, e.g., "2400 to 2700". Please note that the current
#'   SimcypConsultancy template lists one row for each of the upper and lower
#'   values, so this should be set to FALSE for official reports.
#' @param adjust_conc_units Would you like to adjust the units to something
#'   other than what was used in the simulation? Default is NA to leave the
#'   units as is, but if you set the concentration units to something else, this
#'   will attempt to adjust the units to match that. This only adjusts AUC and
#'   Cmax values at present and, if you're switching between mass per volume and
#'   mole per volume units, it will assume you want to use the MW of the
#'   substrate to do that.
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
#'   different. For example, \code{prettify_compound_names = c("effector" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "effector"
#'   includes \emph{all} the effectors and effector metabolites present, so, if
#'   you're setting the effector name, you really should use something like this
#'   if you're including effector metabolites: \code{prettify_compound_names =
#'   c("effector" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
#'   "superstatin")}.
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file. (You can also save the table
#'   to a Word file later with the function \code{\link{formatTable_Simcyp}}.)
#'   If you supply only the file extension, e.g., \code{save_table = "docx"},
#'   the name of the file will be the file name plus "PK summary table" with
#'   that extension and output will be located in the same folder as
#'   \code{sim_data_file}. If you supply something other than just "docx" or
#'   just "csv" for the file name but you leave off the file extension, we'll
#'   assume you want it to be ".csv". While the main PK table data will be in
#'   whatever file format you requested, if you set \code{checkDataSource =
#'   TRUE}, the QC data will be in a csv file on its own and will have "- QC"
#'   added to the end of the file name. \strong{WARNING:} SAVING TO WORD DOES
#'   NOT WORK ON SHAREPOINT. This is a Microsoft permissions issue, not an R
#'   issue. If you try to save on SharePoint, you will get a warning that R will
#'   save your file to your local (not OneDrive) Documents folder instead.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return Returns a data.frame of PK summary data and, if observed data were
#'   provided, simulated-to-observed ratios. If \code{checkDataSource = TRUE},
#'   output will instead be a list of that data.frame (named "Table") and
#'   information on where the values came from for QCing (named "QC").
#' @export
#' @examples
#' pksummary_table("abc1a-5mg-qd.xlsx")
#'
#' pksummary_table(report_input_file = "My report input - project abc-1a.xlsx",
#'          sheet_report = "study info - Clinical study 001A",
#'          includeTrialMeans = TRUE)
#'
#' # An example of how to format observed data as a data.frame:
#' pksummary_table(sim_data_file = "My simulated data.xlsx",
#'                 observed_PK = data.frame(AUCinf_dose1 = 60,
#'                                          AUCinf_dose1_CV = 0.38,
#'                                          Cmax_dose1 = 22,
#'                                          Cmax_dose1_CV = 0.24))
#'
#' # Or you can supply a named numeric vector:
#' pksummary_table(sim_data_file = "My simulated data.xlsx",
#'                 observed_PK = c("AUCinf_dose1" = 60,
#'                                 "AUCinf_dose1_CV" = 0.38,
#'                                 "Cmax_dose1" = 22,
#'                                 "Cmax_dose1_CV" = 0.24))
#'
#' # Or an Excel or csv file:
#' pksummary_table(sim_data_file = "mdz-5mg-sd.xlsx",
#'                 observed_PK = "mdz observed PK.csv")
#' 

pksummary_table <- function(sim_data_file = NA, 
                            PKparameters = NA,
                            PKorder = "default", 
                            sheet_PKparameters = NA,
                            observed_PK = NA, 
                            report_input_file = NA,
                            sheet_report = NA,
                            mean_type = NA,
                            tissue = "plasma",
                            includeCV = TRUE,
                            includeConfInt = TRUE,
                            includeRange = FALSE,
                            includePerc = FALSE,
                            includeTrialMeans = FALSE,
                            concatVariability = FALSE,
                            adjust_conc_units = NA,
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
    
    # Check for appropriate input for arguments
    tissue <- tolower(tissue)
    if(tissue %in% c("plasma", "blood") == FALSE){
        warning("You have not supplied a permissible value for tissue. Options are `plasma` or `blood`. The PK parameters will be for plasma.", 
                call. = FALSE)
        tissue <- "plasma"
    }
    
    PKorder <- tolower(PKorder)
    if(PKorder %in% c("default", "user specified") == FALSE){
        warning("You have not supplied a permissible value for the order of PK parameters. Options are `default` or `user specified`. The default PK parameter order will be used.", 
                call. = FALSE)
        PKorder <- "default"
    }
    
    if(PKorder != "default" & is.na(PKparameters[1])){
        warning("You have requested `user specified` for the argument 'PKorder', which sets the order of columns in the table, but you have not specified what that order should be with the argument `PKparameters`. The order will be the default order from the Consultancy Report Template.", 
                call. = FALSE)
        PKorder <- "default"
    }
    
    if(class(prettify_compound_names) == "character" &&
       is.null(names(prettify_compound_names))){
        warning("You have supplied values for `prettify_compound_names` but not assigned them with compound IDs. That means we don't know which one is the substrate and which one is the effector(s). For now, we'll try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the effector.", 
                call. = FALSE)
        prettify_compound_names <- TRUE
    }
    
    if(class(prettify_compound_names) == "character"){
        if(any(str_detect(names(prettify_compound_names), "inhibitor"))){
            names(prettify_compound_names)[
                which(str_detect(names(prettify_compound_names), "inhibitor"))] <- "effector"
        }
        
        if("substrate" %in% names(prettify_compound_names) == FALSE){
            warning("The compound IDs you supplied for `prettify_compound_names` must include compound IDs of `substrate` and, if there are any effectors, `effector` for the compounds to be prettified as requested. For now, we'll just try our best to prettify the compound names, but if the result is not what you want, please supply a named character vector for what you want to use for the substrate and what you want to use for the effector.", 
                    call. = FALSE)
            prettify_compound_names <- TRUE
        }
    }
    
    # If user asked for a specific sheet and that sheet is the same thing as the
    # AUC tab, then set sheet_PKparameters to NA b/c a) it will automatically
    # look there 1st for the PK and b) it won't mess up anything for knowing
    # which dose the PK is for. The other sheets w/PK parameters are not obvious
    # as to which dose it is, so for those, we need to remove the "_dose1" or
    # "_last".
    if(complete.cases(sheet_PKparameters) &&
       sheet_PKparameters %in% c("AUC", "AUC_CI", "AUC_SD")){
        sheet_PKparameters <- NA
    }
    
    
    # Main body of function --------------------------------------------------
    
    ## Reading in all data and tidying ------------------------------------
    if(complete.cases(report_input_file)){
        
        # If they didn't include ".xlsx" at the end of whatever they supplied for
        # report_input_file, add that.
        report_input_file <- ifelse(str_detect(report_input_file, "xlsx$"), 
                                    report_input_file, paste0(report_input_file, ".xlsx"))
        
        if(is.na(sheet_report)){
            warning("You must supply a value for `sheet_report` if you supply a report input file.", 
                    call. = FALSE)
            return(list())
        }
        
        sectionInfo <- getSectionInfo(report_input_file = report_input_file,
                                      sheet_report = sheet_report)
        
        if(complete.cases(sim_data_file) & sim_data_file != sectionInfo$File){
            warning(paste0("The value supplied for `sim_data_file` was `", 
                           sim_data_file, 
                           "``, but the value you supplied in the report input file `",
                           report_input_file, "` was `", 
                           sectionInfo$File,
                           "`. The file listed in the report input file will be used."), 
                    call. = FALSE)
        }
        
        sim_data_file <- sectionInfo$sim_data_file
        # Should we add an error catch here for when user fills out
        # report_input_file but doesn't include any observed data to compare?
        # Maybe not. If the user doesn't want to include any obs data there,
        # just fill out sim_data_file.
        
        # If they supplied both a report_input_file and observed_PK, warn the
        # user that this will preferentially read the report_input_file.
        if(complete.cases(observed_PK[1])){
            warning("You have supplied both a report input file and, separately, observed data. The report input file will be used preferentially and the observed data will be ignored.", 
                    call. = FALSE)
        }
        
        observed_PK <- as.data.frame(sectionInfo$ObsData)
        
    } else {
        
        # Setting this for use later since it's easiest if sectionInfo is
        # logical when it doesn't apply. 
        sectionInfo <- FALSE
        
        # If they supplied observed_PK, get sim_data_file from that. 
        if(complete.cases(observed_PK[1]) && (class(observed_PK) == "character")){
            observed_PK <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                  "csv" = read.csv(observed_PK), 
                                  "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                           sheetIndex = 1))
            
        } else if(class(observed_PK)[1] == "numeric"){ # This is when user has supplied a named numeric vector
            
            # Converting named vector to data.frame b/c everything else is set
            # up as a data.frame.
            if(class(observed_PK)[1] == "numeric"){
                observed_PK <- as.data.frame(t(observed_PK))
            }
        }
    }
    
    # At this point, observed_PK, if it exists, should be a data.frame b/c it
    # either was a data.frame at the outset, it has been created by reading an
    # Excel or csv file for observed data, or it came from a report input form.
    if("data.frame" %in% class(observed_PK)){
        
        # There should be only 1 row in observed_PK, so removing any extras that
        # might have gotten included accidentally.
        observed_PK <- observed_PK[1, ]
        
        # Also only keeping columns with complete cases for PK values.
        observed_PK[, sapply(observed_PK, complete.cases)]
        
        # If they supplied a file name in the observed PK data, then use that
        # instead of anything they may have supplied for sim_data_file.
        if("File" %in% names(observed_PK) && complete.cases(observed_PK$File)){
            if(complete.cases(sim_data_file) & sim_data_file != observed_PK$File){
                warning(paste0("The value supplied for `sim_data_file` was `", 
                               sim_data_file, 
                               "``, but the value you supplied in the observed PK data was `", 
                               observed_PK$File,
                               "`. The file listed with the observed data will be used."), 
                        call. = FALSE)
            }
            
            sim_data_file <- observed_PK$File
        }
        
        # Cleaning up and harmonizing observed data
        MyObsPK <- observed_PK
        
        names(MyObsPK) <- sub("_first", "_dose1", names(MyObsPK))
        names(MyObsPK) <- sub("tau_dose1", "t_dose1", names(MyObsPK))
        names(MyObsPK) <- sub("AUCt_ratio_last", "AUCtau_ratio_last", names(MyObsPK))
        names(MyObsPK) <- sub("_last", "_last", names(MyObsPK))
        
        # Making obs PK names match correct PK parameters regardless of case
        suppressMessages(
            ObsNames <- data.frame(OrigName = names(MyObsPK)) %>% 
                mutate(PKparameter_lower = sub("_first", "_dose1",
                                               tolower(OrigName)), 
                       PKparameter_lower = sub("_ss", "_last", 
                                               PKparameter_lower),
                       PKparameter_lower = sub("_cv", "", PKparameter_lower)) %>% 
                left_join(AllPKParameters %>% select(PKparameter) %>% 
                              unique() %>% 
                              mutate(PKparameter_lower = tolower(PKparameter))) %>% 
                mutate(PKparameter = ifelse(str_detect(tolower(OrigName), "cv"), 
                                            paste0(PKparameter, "_CV"), 
                                            PKparameter), 
                       PKparameter = ifelse(OrigName == "File", "File", PKparameter), 
                       PKparameter = ifelse(is.na(PKparameter), OrigName, PKparameter))
        )
        names(MyObsPK) <- ObsNames$PKparameter
        
        # Having extra columns messes things up, so removing any extraneous
        # things the user might have included.
        
        # Getting the names w/out "File"
        NewObsNames <- names(MyObsPK)[names(MyObsPK) %in% 
                                          c(AllPKParameters$PKparameter, 
                                            paste0(AllPKParameters$PKparameter, "_CV"))]
        MyObsPK <- as.data.frame(MyObsPK[, NewObsNames])
        names(MyObsPK) <- NewObsNames
        
        # If user provided observed PK, then make sure those PK parameters are
        # included in the PK to extract.
        PKparameters <- sort(unique(c(PKparameters, names(MyObsPK))))
        
    }
    
    # At this point, we should have the sim_data_file. 
    if(is.na(sim_data_file)){
        warning("You must enter a simulator output file name for `sim_data_file`, include a simulator output file name with observed PK data, or include a simulator output file name within the Excel file you supplied for `report_input_file`. We don't know what file to use for your simulated data.", 
                call. = FALSE)
        return(list())
    }
    
    # If they didn't include ".xlsx" at the end, add that.
    sim_data_file <- ifelse(str_detect(sim_data_file, "xlsx$"), 
                            sim_data_file, paste0(sim_data_file, ".xlsx"))
    
    # Figuring out what kind of means user wants, experimental details, etc.
    
    # First, the scenarios where there are observed data to compare from a
    # filled-out report template (sectionInfo exists)
    if(class(sectionInfo) != "logical"){
        
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
        
        # And second, the scenario where user has not supplied a filled-out
        # report form.
        MeanType <- ifelse(is.na(mean_type), "geometric", mean_type)
        GMR_mean_type <- MeanType
        # NB re. GMR_mean_type: I originally had this set to "geometric" all the
        # time because that's nearly always what we report. However, the more I
        # thought about it, the more I realized that people will probably expect
        # this to be whatever mean type they set for the main mean type and it's
        # just going to be confusing to change it. If it turns out to be an
        # issue, revisit this. - LSh
        Deets <- extractExpDetails(sim_data_file = sim_data_file)
        
        # extractExpDetails will check whether the Excel file provided was, in
        # fact, a Simulator output file and return a list of length 0 if not.
        # Checking for that here.
        if(length(Deets) == 0){
            # warning(paste0("The file ", sim_data_file, 
            #                " is not a Simulator output file and will be skipped."))
            return(list())
        }
        
        EffectorPresent <- complete.cases(Deets$Inhibitor1)
        DoseRegimen <- Deets$Regimen_sub
    }
    
    if(Deets$PopRepSim == "Yes"){
        warning(paste0("The simulator file supplied, `", 
                       sim_data_file, 
                       "`, is for a population-representative simulation and thus doesn't have any aggregate data. This function only really works with aggregate data, so this file will be skipped."),
                call. = FALSE)
        return(list())
    }
    
    ## Determining which PK parameters to pull --------------------------------
    if(complete.cases(PKparameters[1])){
        # If user specified "_first" instead of "_dose1", make that work, too. 
        PKToPull <- sub("_first", "_dose1", PKparameters)
        
        # If the user supplied "XXXtau_dose1", change that to "XXXt_dose1". 
        PKToPull <- sub("tau_dose1", "t_dose1", PKToPull)
        
        # If the user supplied "XXX_ss", change that to "XXX_last".
        PKToPull <- sub("_last", "_last", PKToPull)
        
        # If the user used AUCt_last instead of AUCtau_last, fix that for them.
        PKToPull <- sub("AUCt_last", "AUCtau_last", PKToPull)
        PKToPull <- sub("AUCt_ratio_last", "AUCtau_ratio_last", PKToPull)
        
    } else {
        
        if(class(sectionInfo) == "logical"){ # sectionInfo is logical if they did not supply a report input form
            if("data.frame" %in% class(observed_PK)){
                # If user supplies an observed file, then pull the parameters
                # they want to match. If user specified "_first" instead of
                # "_dose1", make that work, too.
                PKToPull <- sub("_first", "_dose1", tolower(names(MyObsPK)))
                
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
        warning(paste0("None of the parameters you requested are available from the supplied simulator output file `",
                       sim_data_file, "`. Please check that the parameters requested make sense for the simulation. For example, did you request multiple-dose parameters for a single-dose regimen?"),
                call. = FALSE)
        return(list())
    }
    
    ## Getting PK parameters -------------------------------------------------
    suppressWarnings(
        MyPKResults_all <- extractPK(sim_data_file = sim_data_file,
                                     PKparameters = PKToPull,
                                     tissue = tissue,
                                     sheet = sheet_PKparameters, 
                                     returnAggregateOrIndiv =
                                         switch(as.character(includeTrialMeans),
                                                "TRUE" = c("aggregate", "individual"),
                                                "FALSE" = "aggregate")))
    
    # If there were no PK parameters to be pulled, MyPKResults_all will have
    # length 0 and we can't proceed.
    if(length(MyPKResults_all) == 0){
        warning("No PK results were found. Does your simulator output file include any sheets with names such as 'AUC', 'AUC_CI' or 'AUC_SD'? Something like that must be present for most of the typical PK parameters to get extracted.", 
                call. = FALSE)
        return()
    }
    
    # PKToPull must be changed if user specified a tab b/c then the parameters
    # won't have _last or _dose1 suffixes. HOWEVER, if the sheet matched the AUC
    # tab in terms of formatting, then we DO know which dose it was, so don't
    # remove suffixes in that case.
    if(complete.cases(sheet_PKparameters) &
       any(str_detect(names(MyPKResults_all[[1]]), "_dose1|_last")) == FALSE){
        PKToPull <- sub("_last|_dose1", "", PKToPull)
    }
    
    # Changing units if user wants. 
    if(complete.cases(adjust_conc_units)){
        # Only adjusting AUC and Cmax values and not adjusting time portion of
        # units -- only conc.
        if(Deets$Units_Cmax != adjust_conc_units){
            ColsToChange <- names(MyPKResults_all$aggregate)[
                str_detect(names(MyPKResults_all$aggregate), "AUC|Cmax")
            ]
            
            for(i in ColsToChange){
                TEMP <- match_units(
                    MyPKResults_all$aggregate %>% 
                        rename(Conc = i) %>% 
                        mutate(CompoundID = "substrate", # need a placeholder here. This will only work for substrate anyway.
                               Conc_units = Deets$Units_Cmax, 
                               Time = 1, Time_units = "hours"),
                    goodunits = list("Conc_units" = adjust_conc_units, 
                                     "Time_units" = "hours"), 
                    MW = c("substrate" = Deets$MW_sub))
                MyPKResults_all$aggregate[, i] <- TEMP$Conc
                rm(TEMP)
            }
            
            # Need to change units in Deets now to match.
            Deets$Units_AUC <- sub(Deets$Units_Cmax, adjust_conc_units, Deets$Units_AUC)
            Deets$Units_Cmax <- adjust_conc_units
        }
    }
    
    # If they requested AUCinf but there was trouble with that extrapolation,
    # AUCinf won't be present in the data but AUCt will be. Check for that and
    # change PKToPull to reflect that change.
    if(any(str_detect(PKToPull, "AUCinf")) & 
       (("data.frame" %in% class(MyPKResults_all[[1]]) & 
         any(str_detect(names(MyPKResults_all[[1]]), "AUCinf")) == FALSE) |
        ("data.frame" %in% class(MyPKResults_all[[1]]) == FALSE &
         !str_detect(names(MyPKResults_all)[1], "AUCinf")))){
        warning(paste0("AUCinf included NA values in the file `", 
                       sim_data_file, 
                       "`, meaning that the Simulator had trouble extrapolating to infinity and thus making the AUCinf summary data unreliable. AUCt will be returned to use in place of AUCinf as you deem appropriate."),
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
                    "MaxMean" = includeTrialMeans, 
                    "min" = includeRange, 
                    "max" = includeRange)
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
    
    if(exists("MyObsPK", inherits = FALSE)){
        # Making observed_PK that was supplied as a data.frame or file long
        # w/column for PKparameter.
        MyObsPK <- MyObsPK %>% 
            pivot_longer(cols = any_of(c(AllPKParameters$PKparameter, 
                                         paste0(AllPKParameters$PKparameter, "_CV"))), 
                         names_to = "PKParam", 
                         values_to = "Obs")
        
        MyObsPK <- MyObsPK %>% 
            mutate(Stat = ifelse(str_detect(PKParam, "_CV"), 
                                 ifelse({{MeanType}} == "geometric", "GCV", "CV"), 
                                 ifelse({{MeanType}} == "geometric", "geomean", "mean")))
        
        if(EffectorPresent){
            # Accounting for when the mean ratios for obs data are
            # actually geometric even though the other obs data means are
            # arithmetic. This will label observed data GMR values as
            # "mean" (for arithmetic means) rather than "geomean" so that
            # it will be easier to return only the correct mean types. I
            # know that's confusing, but I couldn't come up with a better
            # way to do that, so my apologies! -LSh
            MyObsPK <- MyObsPK %>% 
                mutate(Stat = ifelse({{MeanType}} == "arithmetic" &
                              {{GMR_mean_type}} == "geometric" &
                              str_detect(PKParam, "ratio") &
                              str_detect(Stat, "mean"), # detecting mean or geomean but not CV 
                          "mean", Stat))
        }
        
        MyObsPK <- MyObsPK %>% 
            mutate(PKParam = sub("_CV", "", PKParam))
        
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
        
        # If user supplied obs data and did NOT specify PK parameters that they
        # wanted, only keep PK parameters where there are values for the
        # observed mean data.
        if(is.na(PKparameters[1])){
            PKToPull <- MyObsPK %>% filter(complete.cases(Obs)) %>% 
                pull(PKParam) %>% unique()
        }
        
    } else {
        MyPKResults <- MyPKResults %>% 
            rename(Value = Sim) %>% 
            mutate(SorO = "Sim")
    }
    
    
    ## Putting everything together and formatting -------------------------
    
    # Formatting and selecting only rows where there are data
    MyPKResults <- MyPKResults %>%
        mutate(Value = if_else(str_detect(Stat, "CV"), 
                               round_consultancy(100*Value),
                               round_consultancy(Value))) %>%
        filter(Stat %in% c(ifelse(MeanType == "geometric", "geomean", "mean"),
                           "CI90_low", "CI90_high", "CI95_low", "CI95_high",
                           "min", "max", "per5", "per95", 
                           ifelse(MeanType == "geometric", "GCV", "CV"), 
                           "MinMean", "MaxMean", "S_O")) %>%
        pivot_wider(names_from = PKParam, values_from = Value) %>% 
        mutate(SorO = factor(SorO, levels = c("Sim", "Obs", "S_O")), 
               Stat = factor(Stat, levels = c("mean", "geomean", "CV", "GCV",
                                              "min", "max",
                                              "CI90_low", "CI90_high", "CI95_low", 
                                              "CI95_high", "per5", "per95",
                                              "MinMean", "MaxMean", "S_O"))) %>% 
        arrange(SorO, Stat) %>% 
        filter(if_any(.cols = -c(Stat, SorO), .fns = complete.cases)) %>% 
        mutate(across(.cols = everything(), .fns = as.character)) 
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
                        "Perc" = c("per5", "per95"), 
                        "Range" = c("min", "max"))
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
                                     "Range" = "Rangeconcat",
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
                   "min" = "Minimum", 
                   "max" = "Maximum",
                   "Rangeconcat" = "Range",
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
    if(complete.cases(sheet_PKparameters) & 
       any(str_detect(names(MyPKResults_all[[1]]), "_dose1|_last")) == FALSE){
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
    if(str_detect(PKorder, "user")){
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
        if(complete.cases(sheet_PKparameters) & 
           any(str_detect(names(MyPKResults_all[[1]]), "_dose1|_last")) == FALSE){
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
        
        if(complete.cases(adjust_conc_units)){
            PrettyCol <- gsub(Deets$Units_Cmax,  adjust_conc_units, PrettyCol)
        }
        
        # Adjusting units as needed.
        PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
        PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
        PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
        PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
        PrettyCol <- gsub("ug/mL", "µg/mL", PrettyCol)
        
        MyEffector <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
                        Deets$Inhibitor2)
        
        if(any(complete.cases(MyEffector))){
            MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
            
            if(class(prettify_compound_names) == "logical" &&
               prettify_compound_names){
                MyEffector <- prettify_compound_name(MyEffector)
            }
            
            if(class(prettify_compound_names) == "character" &
               "effector" %in% names(prettify_compound_names)){
                names(prettify_compound_names)[
                    str_detect(tolower(names(prettify_compound_names)), 
                               "effector")][1] <- "effector"
                MyEffector <- prettify_compound_names["effector"]
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
        
        if(any(str_detect(PKToPull, "tmax"))){
            ColsToInclude <- c(ColsToInclude, "min", "max", "median")
        }
        
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
        
        if(includeRange){
            ColsToInclude <- c(ColsToInclude, "max", "min")
        }
        
        OutQC <- MyPKResults_all$QC %>% 
            select(PKparam, File, matches(ColsToInclude))
    }
    
    
    # Saving --------------------------------------------------------------
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
            # This is when they want a .csv file as output. In this scenario,
            # changing the value "simulated" in the list of stats to include
            # whether it was arithmetic or geometric b/c that info is included
            # in the Word file but not in the table itself.
            MyPKResults <- MyPKResults %>% 
                mutate(Statistic = sub("Simulated", 
                                       paste("Simulated", MeanType, "mean"), Statistic))
            WarningDF <- data.frame(Col1 = "WARNING:",
                                    Col2 = "This table was saved to a csv file, and Excel automatically drops any trailing zeroes. Please check your sig figs to make sure you haven't inadvertently dropped a trailing zero.")
            names(WarningDF) <- names(MyPKResults)[1:2]
            
            write.csv(bind_rows(MyPKResults, WarningDF),
                      paste0(OutPath, "/", save_table), row.names = F)
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



