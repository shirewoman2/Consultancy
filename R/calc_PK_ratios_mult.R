#' Calculate the ratio of PK parameters between multiple pairs of simulations
#'
#' \code{calc_PK_ratios_mult} matches PK data from pairs of simulator output
#' Excel files and calculates the mean and confidence intervals of the ratios of
#' the requested PK parameters.
#'
#' @param sim_data_file_pairs a data.frame or a csv file with at least two
#'   columns: "Numerator", listing the files to use for the numerator of each
#'   calculation, and "Denominator", listing the files to use for the
#'   denominator of each calculation. Each row comprises one pair to use for
#'   calculating ratios. This data.frame or csv file can also be used to specify
#'   the sheets to use for extracting the numerator or denominator data. (Please
#'   see the arguments \code{sheet_PKparameters_num} and
#'   \code{sheet_PKparameters_denom}.)
#' @param paired TRUE (default) or FALSE for whether the study design is paired,
#'   as in, the subjects are \emph{identical} between the two simulations.
#'   \strong{THIS IS AN IMPORTANT DISTINCTION AND WILL AFFECT HOW THE
#'   CALCULATIONS ARE PERFORMED!} An example of a paired study would be a DDI
#'   study where each subject has a measurement without the effector of interest
#'   and then has a second measurement \emph{with} the effector. The comparison
#'   is for repeated measurements of the \emph{same subject}. An example of an
#'   unpaired study design would be comparing healthy volunteers to subjects
#'   with hepatic impairment because those are measurements on \emph{different}
#'   subjects. For paired study designs, the order of operations is to calculate
#'   each subject's mean ratio and then to calculate the mean of those ratios.
#'   For unpaired study designs, the order of operations is to calculate the
#'   mean for the numerator simulation and then divide it by the mean for the
#'   denominator simulation. Would this be clearer if you could see the
#'   mathematical equations? We agree but can't easily include equations in the
#'   help file. However, if you run this and save the output to a Word file, the
#'   equations will be included in the output.
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
#' @param sheet_PKparameters_num (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_numerator}, list that tab here. Most of the time, this
#'   should be left as NA.
#' @param sheet_PKparameters_denom (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_denominator}, list that tab here. Most of the time,
#'   this should be left as NA.
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default) or "blood" (possible but not as thoroughly
#'   tested).
#' @param mean_type What kind of means and confidence intervals do you want
#'   listed in the output table? Options are "arithmetic" or "geometric"
#'   (default).
#' @param include_num_denom_columns TRUE (default) or FALSE for whether to
#'   include columns in the output table for the numerator data alone and
#'   columns for the denominator alone. For example, if you wanted to calculate
#'   the dose 1 AUC ratio for cancer patients compared to healthy volunteers,
#'   settting \code{include_num_denom_columns = TRUE} would give you that ratio
#'   and also a column with summary statistics on the AUC for cancer patients
#'   and a column with summary statistics on the AUC for healthy volunteers.
#'   Setting it to FALSE would give you only the ratios.
#' @param extract_forest_data TRUE or FALSE (default) to get forest-plot data at
#'   the same time. If set to TRUE, this will return a list that includes data
#'   formatted for use with the function \code{\link{forest_plot}}. This will
#'   assume that the denominator is the baseline or control scenario and the
#'   numerator is the comparison. In the output for this, the column "Dose_sub"
#'   will contain the dose of the substrate in the denominator simualtions, and
#'   the column "Dose_inhib" will contain the dose of the inhibitor (if there
#'   was one) in the numerator simulations or the dose of the substrate in the
#'   numerator simulations if there was not. If there was not an inhibitor, the
#'   column "Inhibitor 1" will contain the file names for the numerator sims.
#' @param conf_int confidence interval to use; default is 90\%
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1". We're still tweaking this to
#'   make it look just right!
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
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{\item{NA or "Consultancy"}{All output will be rounded according
#'   to Simcyp Consultancy Team standards: to three significant figures when the
#'   value is < 100 or to the ones place if the value is >= 100. Please see the
#'   function \code{\link{round_consultancy}}, which does the rounding here.}
#'   \item{"none"}{No rounding will be performed.} \item{"significant X" where
#'   "X" is a number}{Output will be rounded to X significant figures. "signif
#'   X" also works fine.} \item{"round X" where "X" is a number}{Output will be
#'   rounded to X digits} \item{"Word only"}{Output saved to Word or a csv file
#'   will be rounded using the function \code{\link{round_consultancy}}, but
#'   nothing will be rounded in the output R object. This can be useful when you
#'   want to have nicely rounded and formatted output in a Word file but you
#'   \emph{also} want to use the results from \code{calc_PK_ratios_mult} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
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
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from and data to use for making forest plots
#' @export
#'
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios_mult <- function(sim_data_file_pairs,  
                                paired = TRUE,
                                PKparameters = "AUC tab", 
                                sheet_PKparameters_num = NA,
                                sheet_PKparameters_denom = NA,
                                tissue = "plasma",
                                mean_type = "geometric", 
                                include_num_denom_columns = TRUE, 
                                conf_int = 0.9, 
                                includeCV = TRUE, 
                                includeConfInt = TRUE,
                                rounding = NA,
                                checkDataSource = TRUE, 
                                extract_forest_data = FALSE, 
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
    
    if(extract_forest_data & includeConfInt == FALSE){
        warning("To get forest-plot data, we need the confidence interval, but you have set `includeConfInt = FALSE`. We're going to change that to TRUE so that we can get what we need for forest-plot data.", 
                call. = FALSE)
        includeConfInt <- TRUE
    }
    
    # Main body of function -------------------------------------------------
    
    # Loading sim_data_file_pairs as needed
    if(class(sim_data_file_pairs)[1] == "character"){
        
        # Checking file name format and setting as csv
        sim_data_file_pairs <- ifelse(str_detect(sim_data_file_pairs, "\\."),
                                      sub("\\..*", ".csv", sim_data_file_pairs), 
                                      sim_data_file_pairs)
        
        sim_data_file_pairs <- read.csv(sim_data_file_pairs)
    }
    
    if(all(c("Numerator", "Denominator") %in% names(sim_data_file_pairs) == FALSE)){
        stop("You must have a column titled `Numerator` and a column titled `Denominator` in the csv file or data.frame `sim_data_file_pairs`, and you are missing at least one of those. We don't know which files are for the numerators and which are for the denominators and cannot proceed.", 
             call. = FALSE)
    }
    
    if(all(c("sheet_PKparameters_denom", "sheet_PKparameters_num") %in%
           names(sim_data_file_pairs)) == FALSE){
        sim_data_file_pairs$sheet_PKparameters_num <- sheet_PKparameters_num
        sim_data_file_pairs$sheet_PKparameters_denom <- sheet_PKparameters_denom
    }
    
    
    MyTable <- list()
    QC <- list()
    ForestInfo <- list()
    
    for(i in 1:nrow(sim_data_file_pairs)){
        # Including a progress message
        message("Extracting data for file pair #", i)
        
        TEMP <- calc_PK_ratios(
            sim_data_file_numerator = sim_data_file_pairs$Numerator[i], 
            sim_data_file_denominator = sim_data_file_pairs$Denominator[i], 
            paired = paired, 
            PKparameters = PKparameters, 
            sheet_PKparameters_num = sim_data_file_pairs$sheet_PKparameters_num[i], 
            sheet_PKparameters_denom = sim_data_file_pairs$sheet_PKparameters_denom[i], 
            tissue = tissue, 
            mean_type = mean_type, 
            include_num_denom_columns = include_num_denom_columns, 
            conf_int = conf_int, 
            includeCV = includeCV, 
            includeConfInt = includeConfInt, 
            prettify_columns = FALSE, 
            prettify_compound_names = FALSE, 
            rounding = "none", 
            checkDataSource = TRUE, 
            returnExpDetails = TRUE,
            save_table = NA)
        
        MyTable[[i]] <- TEMP$Table %>% 
            mutate(File = paste(sim_data_file_pairs$Numerator[i], "/", 
                                sim_data_file_pairs$Denominator[i]))
        QC[[i]] <- TEMP$QC
        
        ForestInfo[[i]] <- data.frame(
            File = paste(sim_data_file_pairs$Numerator[i], "/", 
                         sim_data_file_pairs$Denominator[i]), 
            Dose_sub = TEMP$ExpDetails_denom$Dose_sub, 
            Dose_inhib = switch(as.character(
                complete.cases(TEMP$ExpDetails_num$Inhibitor1)), 
                "TRUE" = TEMP$ExpDetails_num$Dose_inhib, 
                "FALSE" = TEMP$ExpDetails_num$Dose_sub), 
            Substrate = TEMP$ExpDetails_denom$Substrate, 
            Inhibitor1 = switch(as.character(
                complete.cases(TEMP$ExpDetails_num$Inhibitor1)), 
                "TRUE" = TEMP$ExpDetails_num$Inhibitor1, 
                "FALSE" = TEMP$ExpDetails_num$Substrate), 
            File_num = sim_data_file_pairs$Numerator[i], 
            File_denom = sim_data_file_pairs$Denominator[i]) %>% 
            left_join(MyTable[[i]], by = join_by(File))
        
        rm(TEMP)
    }
    
    MyPKResults <- bind_rows(MyTable)
    
    # Setting the rounding option
    round_opt <- function(x, round_fun){
        
        round_fun <- ifelse(is.na(round_fun), "consultancy", tolower(round_fun))
        round_fun <- ifelse(str_detect(tolower(round_fun), "word"), "none", round_fun)
        
        suppressWarnings(
            NumDig <- as.numeric(str_trim(sub("signif(icant)?|round", "", round_fun)))
        )
        
        if(str_detect(round_fun, "signif|round") & 
           !str_detect(round_fun, "[0-9]{1,}")){
            warning("You appear to want some rounding, but we're not sure how many digits. We'll use 3 for now, but please check the help file for appropriate input for the argument `rounding`.", 
                    call. = FALSE)
            NumDig <- 3
        }
        
        round_fun <- str_trim(sub("[0-9]{1,}", "", round_fun))
        round_fun <- ifelse(str_detect(round_fun, "signif"), "signif", round_fun)
        
        Out <- switch(round_fun, 
                      "round" = round(x, digits = NumDig),
                      "signif" = signif(x, digits = NumDig), 
                      "consultancy" = round_consultancy(x), 
                      "none" = x)
        
        return(Out)
    }
    
    # Rounding as requested and setting column order
    MyPKResults <- MyPKResults %>% 
        mutate(across(.cols = where(is.numeric), 
                      .fns = round_opt, 
                      round_fun = ifelse(complete.cases(rounding) & 
                                             rounding == "Word only", 
                                         "none", rounding))) %>% 
        select(-File, File)
    
    
    # Saving --------------------------------------------------------------
    
    Out <- list(Table = MyPKResults)
    
    
    if(complete.cases(save_table)){
        
        # Rounding as necessary
        if(complete.cases(rounding) && rounding == "Word only"){
            MyPKResults <- MyPKResults %>% 
                mutate(across(.cols = where(is.numeric), 
                              .fns = round_opt, round_fun = "Consultancy")) %>% 
                select(-File, File)
        } 
        
        # Checking whether they have specified just "docx" or just "csv" for
        # output b/c then, we'll use "PK ratios" as file name. This allows us to
        # determine what the path should be, too.
        if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
            OutPath <- getwd()
            save_table <- sub("xlsx", 
                              # If they included "." at the beginning of the
                              # file extension, need to remove that here.
                              sub("\\.", "", save_table),
                              "PK ratios")
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
            
            FileName <- basename(save_table)
            
            # Storing some objects so they'll work with the markdown file
            PKToPull <- PKparameters
            MeanType <- mean_type
            FromCalcPKRatios <- TRUE
            
            rmarkdown::render(system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd",
                                          package="SimcypConsultancy"), 
                              output_dir = switch(as.character(LFSPath), 
                                                  "TRUE" = TempDir,
                                                  "FALSE" = OutPath),
                              output_file = FileName, 
                              quiet = TRUE)
            # Note: The "system.file" part of the call means "go to where the
            # package is installed, search for the file listed, and return its
            # full path.
            
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
    
    if(extract_forest_data){
        
        StatNames <- unique(MyPKResults$Statistic[
            str_detect(MyPKResults$Statistic, "Mean Ratio|CI|^Simulated$")])
        names(StatNames) <- StatNames
        StatNames[which(str_detect(StatNames, "Ratio|^Simulated$"))] <- "GMR"
        StatNames[which(str_detect(StatNames, "CI - Lower"))] <- "CI90_lo"
        StatNames[which(str_detect(StatNames, "CI - Upper"))] <- "CI90_hi"
        
        FD <- bind_rows(ForestInfo) %>% 
            select(File, Statistic, Substrate, Dose_sub, Inhibitor1, Dose_inhib,
                   matches(" / ")) %>% 
            filter(str_detect(Statistic, "Ratio|^Simulated$|Lower|Upper")) %>% 
            pivot_longer(cols = -c("Statistic", "File", "Dose_sub", "Dose_inhib", 
                                   "Substrate", "Inhibitor1"), 
                         names_to = "Parameter1", 
                         values_to = "Value") %>% 
            mutate(
                Statistic = StatNames[Statistic],
                Parameter = str_extract(
                    Parameter1, 
                    str_c(AllPKParameters %>% filter(AppliesOnlyWhenEffectorPresent == FALSE) %>% 
                              pull(PKparameter) %>% unique(), 
                          collapse = "|")), 
                Parameter = paste(Parameter, Statistic, sep = "__"), 
                Parameter = sub("_dose1", "_ratio_dose1", Parameter), 
                Parameter = sub("_last", "_ratio_last", Parameter)) %>% 
            select(-Parameter1, -Statistic) %>% 
            filter(str_detect(Parameter, "AUCinf|AUCt|Cmax"))
        
        if(nrow(FD) == 0){
            warning("The PK parameters selected don't work for forest plots, which can only take PK parameters for AUCinf, AUCtau, and Cmax for dose 1 or the last dose. We cannot return any forest-plot data.", 
                    call. = FALSE)
            FD <- data.frame()
            
        } else {
            
            suppressMessages(
                FD <-  FD %>% 
                    pivot_wider(names_from = Parameter, values_from = Value) %>% 
                    select(File, Substrate, Dose_sub, Inhibitor1, Dose_inhib, 
                           everything())
            )
        }
        
        Out[["ForestData"]] <- FD
        
    }
    
    if(checkDataSource){
        Out[["QC"]] <- bind_rows(QC)
    }
    
    if(length(Out) == 1){
        return(Out[[1]])
    } else {
        return(Out)
    }
    
}

