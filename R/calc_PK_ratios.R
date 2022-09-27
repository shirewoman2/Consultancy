#' Calculate the ratio of PK parameters between the two matched simulations --
#' UNDER CONSTRUCTION
#'
#' \code{calc_PK_ratios} matches PK data from two simulator output Excel files
#' by subject ID and trial and calculates the ratio of those parameters for each
#' individual. It then calculates either the geometric or arithmetic mean and
#' desired confidence interval for those data.
#'
#' @param sim_data_file_numerator a simulator output Excel file that will
#'   provide the numerator for the calculated ratios.
#' @param sim_data_file_denominator a simulator output Excel file that will
#'   provide the denominator for the calculated ratios
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are: \describe{
#'
#'   \item{"all"}{all possible parameters}
#'
#'   \item{"AUC tab"}{only those parameters on the "AUC" tab (default). The
#'   "AUC_CI" tab or "AUC_SD" tab will be used if "AUC" tab is not present.}
#'
#'   \item{"Absorption tab"}{only those parameters on the "Absorption" tab.
#'   Please note that we haven't developed this function for output in the
#'   "Overall Fa Fg" tab for ADAM-model simulations yet.}
#'
#'   \item{a vector of any combination of specific, individual parameters, each
#'   surrounded by quotes and encapsulated with \code{c(...)}}{An example:
#'   \code{c("Cmax_dose1", "AUCtau_last")}. To see the full set of possible
#'   parameters to extract, enter \code{view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}
#'
#'   \item{a vector of individual parameters with one parameter for the
#'   numerator and whatever parameter you want from the other file for the
#'   denominator, separated by "/"}{The previous options are all for when you
#'   want to take the ratio of the \emph{same} parameter for file 1 / file 2.
#'   However, if you want to compare one PK parameter from file 1 with a
#'   \emph{different} parameter for file 2, you can do that with this option.
#'   Here's an example of how to input the parameters so that you can calculate
#'   the dose 1 AUCinf with an inhibitor present for file 1 divided by the
#'   AUCinf for dose 1 with no inhibitor (baseline) for file 2:
#'   \code{PKparameters = c("AUCinf_dose1_withInhib / AUCinf_dose1")} Please
#'   note that the quotes are around \emph{both} of the two parameters!}}
#'
#'   Currently, the PK data are only for the substrate unless noted, although
#'   you can sometimes hack around this by supplying a specific sheet to extract
#'   for a compound other than the substrate, e.g. sheet = "AUC(Sub Pri Met1)".
#'   This has NOT been as well tested, though, so be sure to check that you're
#'   getting what you expected!
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default) or "blood" (possible but not as thoroughly
#'   tested).
#' @param mean_type What kind of means and confidence intervals do you want
#'   listed in the output table? Options are "arithmetic" or "geometric"
#'   (default).
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1". \emph{Note:} This does not
#'   currently do anything if the supplied PK parameters are ratios of one
#'   parameter for the numerator file and a different parameter for the
#'   denominator file; we haven't figured out how best to set that up yet.
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
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "inhibitor"
#'   includes \emph{all} the effectors and effector metabolites present, so, if
#'   you're setting the effector name, you really should use something like this
#'   if you're including effector metabolites: \code{prettify_compound_names =
#'   c("inhibitor" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
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
#'   the main PK table data will be in whatever file format you requested, if
#'   you set \code{checkDataSource = TRUE}, the QC data will be in a csv file on
#'   its own and will have "- QC" added to the end of the file name.
#'   \strong{WARNING:} SAVING TO WORD DOES NOT WORK ON SHAREPOINT. This is a
#'   Microsoft permissions issue, not an R issue. If you try to save on
#'   SharePoint, you will get a warning that R will save your file to your
#'   Documents folder instead.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return
#' @export
#'
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios <- function(sim_data_file_numerator,
                           sim_data_file_denominator, 
                           PKparameters = "AUC tab", 
                           tissue = "plasma",
                           mean_type = "geometric", 
                           conf_int = 0.9, 
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
    
    # Main body of function -------------------------------------------------
    
    # Checking whether the user wants to have different PK parameters for
    # numerator than for denominator.
    if(any(str_detect(PKparameters, "/"))){
        PKparam_split <- as.data.frame(str_split_fixed(
            PKparameters, pattern = "( )?/( )?", n = 2)) %>% 
            mutate(V2 = ifelse(V2 == "", V1, V2))
        
        PKnumerator <- PKparam_split$V1
        PKdenominator <- PKparam_split$V2
    } else {
        PKnumerator <- PKparameters
        PKdenominator <- PKparameters
    }
    
    
    PK1 <- extractPK(sim_data_file = sim_data_file_numerator, 
                     PKparameters = PKnumerator, 
                     tissue = tissue,
                     returnAggregateOrIndiv = "individual", 
                     returnExpDetails = TRUE)
    # RETURN TO THIS LATER. For now, I'm just getting the experimental details
    # for file 1. Later, we could add code to check and compare them for file 2
    # to make absolutely sure they match.
    
    PK2 <- extractPK(sim_data_file = sim_data_file_denominator, 
                     PKparameters = PKdenominator, 
                     tissue = tissue,
                     returnAggregateOrIndiv = "individual")
    
    # Making the order of columns match the order set by the user so that we can
    # match the appropriate things between files.
    PK1$individual <- PK1$individual[, c("Individual", "Trial", PKnumerator)]
    PK2$individual <- PK2$individual[, c("Individual", "Trial", PKdenominator)]
    
    # Because we need to match parameters when joining, renaming PK parameters
    # in PK2 to match the names in PK1 even though they're not *really* the same
    # PK parameters. We'll deal with that difference later.
    names(PK2$individual) <- names(PK1$individual)
    
    suppressMessages(
        MyPK <- PK1$individual %>% 
            pivot_longer(cols = -c(Individual, Trial), 
                         names_to = "Parameter", 
                         values_to = "Value1") %>% 
            left_join(PK2$individual %>% 
                          pivot_longer(cols = -c(Individual, Trial), 
                                       names_to = "Parameter", 
                                       values_to = "Value2")) %>% 
            mutate(Ratio = Value1 / Value2)
    )
    
    MyPKResults <- MyPK %>% group_by(Parameter) %>% 
        summarize(Ratio_mean = switch(mean_type, 
                                      "geometric" = round_consult(gm_mean(Ratio)), 
                                      "arithmetic" = round_consult(mean(Ratio, na.rm = T))), 
                  Ratio_CI_l = switch(mean_type, 
                                      "geometric" = round_consult(gm_conf(Ratio, CI = conf_int)[1]),
                                      "arithmetic" = round_consult(confInt(Ratio, CI = conf_int)[1])), 
                  Ratio_CI_u = switch(mean_type, 
                                      "geometric" = round_consult(gm_conf(Ratio, CI = conf_int)[2]),
                                      "arithmetic" = round_consult(confInt(Ratio, CI = conf_int)[2]))) %>% 
        pivot_longer(cols = matches("Ratio"), 
                     names_to = "Statistic", 
                     values_to = "Value") %>% 
        pivot_wider(names_from = Parameter, 
                    values_from = Value)
    
    StatNames_geo <- c(
        "Ratio_mean" = "Geometric Mean Ratio", 
        "Ratio_CI_l" = "90% confidence interval around the geometric mean ratio (lower limit)", 
        "Ratio_CI_u" = "90% confidence interval around the geometric mean ratio (lower limit)")
    names(StatNames_geo) <- sub("90", round(conf_int*100), names(StatNames_geo))
    
    StatNames_arith <- c(
        "Ratio_mean" = "Arithmetic Mean Ratio", 
        "Ratio_CI_l" = "90% confidence interval around the arithmetic mean ratio (lower limit)", 
        "Ratio_CI_u" = "90% confidence interval around the arithmetic mean ratio (lower limit)")
    names(StatNames_arith) <- sub("90", round(conf_int*100), names(StatNames_arith))
    
    if(mean_type == "geometric"){
        MyPKResults$Statistic <- StatNames_geo[MyPKResults$Statistic]
    } else {
        MyPKResults$Statistic <- StatNames_arith[MyPKResults$Statistic]
    }
    
    # Checking on possible effectors to prettify
    Deets <- PK1$ExpDetails
    MyEffector <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
                    Deets$Inhibitor2)
    MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
    MyEffector <- ifelse(MyEffector == "", NA, MyEffector)
    
    if(any(complete.cases(MyEffector))){
        
        if(class(prettify_compound_names) == "logical" &&
           prettify_compound_names){
            MyEffector <- prettify_compound_name(MyEffector)
        }
        
        if(class(prettify_compound_names) == "character"){
            names(prettify_compound_names)[
                str_detect(tolower(names(prettify_compound_names)), 
                           "inhibitor")][1] <- "inhibitor"
            MyEffector <- prettify_compound_names["inhibitor"]
        }
    }
    
    if(all(PKnumerator == PKdenominator)){
        
        if(prettify_columns){
            suppressMessages(
                PrettyCol <- data.frame(PKparameter = names(MyPKResults)[
                    !names(MyPKResults) == "Statistic"]) %>% 
                    left_join(AllPKParameters %>% 
                                  select(PKparameter, PrettifiedNames)) %>% 
                    unique()
            )
            
            if(any(duplicated(PrettyCol$PrettifiedNames))){
                # This happens when CLt and CLinf are included.
                PrettyCol <- PrettyCol %>% 
                    mutate(PrettifiedNames = 
                               ifelse(str_detect(PKparameter, "CLt"),
                                      sub("h\\)",
                                          "h, calculated using interval to time t)",
                                          PrettifiedNames), 
                                      PrettifiedNames))
            }
            
            PrettyCol <- PrettyCol %>% pull(PrettifiedNames)
            
            # Adjusting units as needed.
            PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
            PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
            PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
            PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
            
            # Prettifying effector names as necessary
            if(any(complete.cases(MyEffector))){
                PrettyCol <- sub("effector", MyEffector, PrettyCol)
            }
            
            # Just making absolutely sure that the order of columns matches
            MyPKResults <- MyPKResults[, c("Statistic", names(MyPKResults)[
                !names(MyPKResults) == "Statistic"])]
            
            # Setting prettified names.
            names(MyPKResults) <- c("Statistic", PrettyCol)
            
        }
        
    } else {
        # This is when column names must include some info about which PK
        # parameters were used. I'm not sure this can be easily prettified, so
        # not even trying for now.
        names(MyPKResults) <- c("Statistic", PKparameters)
        
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
            
            # Storing some objects so they'll work with the markdown file
            PKToPull <- PKparameters
            MeanType <- mean_type
            sim_data_file <- str_comma(c(basename(sim_data_file_numerator),
                                         basename(sim_data_file_denominator)))
            
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
            write.csv(MyPKResults, paste0(OutPath, "/", save_table), row.names = F)
        }
    }
    
    if(checkDataSource){
        MyPKResults <- list("Table" = MyPKResults,
                            "QC" = bind_rows(PK1$QC, PK2$QC))
        
        if(complete.cases(save_table)){ 
            write.csv(MyPKResults$QC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
        }
        
    }
    
    return(MyPKResults)
    
}

