#' Make PK summary tables from multiple simulator output files at once
#'
#' @param sim_data_files a character vector of simulator output files, e.g.,
#'   \code{sim_data_files = c("My file 1.xlsx", "My file 2.xlsx")} or, if you
#'   want all the Excel files in the current folder, \code{sim_data_files =
#'   list.files("xlsx")}. If you supply data for \code{observed_PK}, anything
#'   you enter here will be ignored because the file names will be pulled from
#'   there instead.
#' @param observed_PK (optional) If you have a data.frame or an Excel or csv
#'   file with observed PK parameters, supply the data.frame or the full file
#'   name in quotes here, and the simulated-to-observed mean ratios will be
#'   calculated. The supplied data.frame or file \emph{must} include columns for
#'   the simulator output Excel file (title this "File") and each of the PK
#'   parameters you would like to compare, and those column names \emph{must}
#'   match the PK parameter options listed in
#'   \code{data(PKParameterDefinitions)}. If you would like the output table to
#'   include the observed data CV for any of the parameters, add "_CV" to the
#'   end of the parameter name, e.g., "AUCinf_dose1_CV". Please see the example
#'   section at the bottom of the help file for example syntax for providing a
#'   data.frame here.
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
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the table saved as a Word or csv file. If you supply only the file
#'   extension, e.g., \code{save_table = "docx"}, the name of the file will be
#'   "PK summary table" with that extension. If you supply something other than
#'   just "docx" or just "csv" for the file name but you leave off the file
#'   extension, we'll assume you want it to be ".csv". All PK info will be
#'   included in a single Word or csv file, and, if \code{checkDataSource =
#'   TRUE}, that will be saved in a single csv file. \strong{WARNING:} SAVING TO
#'   WORD DOES NOT WORK ON SHAREPOINT. This is a Microsoft permissions issue,
#'   not an R issue. If you try to save on SharePoint, you will get a warning
#'   that R will save your file instead to your Documents folder.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return Returns a data.frame with summary PK parameters from multiple
#'   simulator output files
#' @export
#'
#' @examples
#'
#' # Create PK summary tables for all the Simulator output
#' # files in your working directory; get the default PK parameters
#' pksummary_mult(sim_data_files = NA)
#'
#' # Include a data.frame of observed data for S/O comparisons
#' pksummary_mult(
#'     sim_data_files = NA,
#'     observed_PK = data.frame(File = c("mdz-5mg-qd-keto-400mg-qd.xlsx",
#'                                       "mdz-5mg-qd-rif-600mg-qd.xlsx"),
#'                              AUCtau_last = c(55, 55),
#'                              Cmax_last = c(20, 20),
#'                              AUCtau_last_withInhib = c(800, 20),
#'                              Cmax_last_withInhib = c(100, 5)))
#'
#' # Include a data.frame of observed data for S/O comparisons
#' pksummary_mult(
#'     sim_data_files = NA,
#'     observed_PK = data.frame(File = c("mdz-5mg-qd-keto-400mg-qd.xlsx",
#'                                       "mdz-5mg-qd-rif-600mg-qd.xlsx"),
#'                              AUCtau_last = c(55, 55),
#'                              Cmax_last = c(20, 20),
#'                              AUCtau_last_withInhib = c(800, 20),
#'                              Cmax_last_withInhib = c(100, 5)))
#' 
pksummary_mult <- function(sim_data_files = NA, 
                           observed_PK = NA,
                           PKparameters = NA,
                           PKorder = "default", 
                           sheet_PKparameters = NA, 
                           mean_type = NA, 
                           tissue = "plasma", 
                           includeCV = TRUE, 
                           includeConfInt = TRUE, 
                           includePerc = FALSE, 
                           includeTrialMeans = FALSE, 
                           concatVariability = FALSE, 
                           prettify_columns = TRUE, 
                           checkDataSource = TRUE, 
                           save_table = NA, 
                           fontsize = 11){
    
    # Read in the observed_PK data if it's not already a data.frame. Note that
    # the class of observed_PK will be logical if left as NA.
    if(class(observed_PK) == "character"){
        observed_PKDF <- switch(str_extract(observed_PK, "csv|xlsx"), 
                                "csv" = read.csv(observed_PK), 
                                "xlsx" = xlsx::read.xlsx(observed_PK, 
                                                         sheetIndex = 1))
    } else {
        
        # If user did not supply specific files, then extract all the files in
        # the current folder that end in "xlsx".
        if(length(sim_data_files) == 1 && is.na(sim_data_files)){
            sim_data_files <- list.files(pattern = "xlsx$")
            sim_data_files <- sim_data_files[!str_detect(sim_data_files, "^~")]
        }
    }
    
    if("data.frame" %in% class(observed_PK)){
        observed_PKDF <- observed_PK
    }
    
    if(exists("observed_PKDF", inherits = FALSE) &&
       "File" %in% names(observed_PKDF) == FALSE){
        stop("You must include a column titled 'File' with the observed PK so that this function knows which simulator output file to pull simulated PK parameters from.", 
             call. = FALSE)
    }
    
    # Getting the data
    MyPKResults <- list()
    OutQC <- list()
    
    if(exists("observed_PKDF", inherits = FALSE)){
        
        for(i in 1:nrow(observed_PKDF)){
            
            print(paste("Extracting data from", observed_PKDF$File[i]))
            
            temp <- pksummary_table(
                sim_data_file = as.character(observed_PKDF[i, "File"]),
                observed_PK = observed_PKDF[i, ], 
                PKparameters = PKparameters, 
                PKorder = PKorder, 
                sheet_PKparameters = sheet_PKparameters, 
                mean_type = mean_type,
                tissue = tissue, 
                includeCV = includeCV,
                includeConfInt = includeConfInt, 
                includePerc = includePerc, 
                includeTrialMeans = includeTrialMeans,
                concatVariability = concatVariability,
                prettify_columns = prettify_columns, 
                checkDataSource = checkDataSource,
                prettify_compound_names = c("inhibitor" = "effector",
                                            "substrate" = "substrate"))
            
            if(checkDataSource){
                
                MyPKResults[[i]] <- temp$Table %>% 
                    mutate(File = as.character(observed_PKDF[i, "File"]))
                
                OutQC[[i]] <- temp$QC
                
            } else {
                
                MyPKResults[[i]] <- temp %>% 
                    mutate(File = as.character(observed_PKDF[i, "File"]))    
                
            }
            
            rm(temp)
        }
        
    } else {
        
        for(i in sim_data_files){
            
            print(paste("Extracting data from", i))
            
            temp <- pksummary_table(sim_data_file = i, 
                                    PKparameters = PKparameters, 
                                    PKorder = PKorder, 
                                    sheet_PKparameters = sheet_PKparameters, 
                                    mean_type = mean_type,
                                    tissue = tissue, 
                                    includeCV = includeCV,
                                    includeConfInt = includeConfInt, 
                                    includePerc = includePerc, 
                                    includeTrialMeans = includeTrialMeans,
                                    concatVariability = concatVariability,
                                    prettify_columns = prettify_columns, 
                                    checkDataSource = checkDataSource,
                                    prettify_compound_names = c("inhibitor" = "effector",
                                                                "substrate" = "substrate"))
            
            if(length(temp) == 0){
                rm(temp)
                next
            }
            
            if(checkDataSource){
                
                MyPKResults[[i]] <- temp$Table %>% 
                    mutate(File = i)
                
                OutQC[[i]] <- temp$QC
                
            } else {
                
                MyPKResults[[i]] <- temp %>% 
                    mutate(File = i)    
                
            }
            
            rm(temp)
            
        }
    }
    
    if(PKorder == "default"){
        
        MyPKResults <- bind_rows(MyPKResults)
        
        suppressMessages(
            MyPKResults <- MyPKResults %>% 
                select(Statistic, 
                       any_of(data.frame(PrettifiedNames = names(MyPKResults)) %>%
                                  left_join(AllPKParameters %>% select(PrettifiedNames, SortOrder)) %>% 
                                  filter(complete.cases(SortOrder)) %>% 
                                  arrange(SortOrder) %>% pull(PrettifiedNames) %>% unique()),
                       everything()) %>% 
                relocate(File, .after = last_col())
        )
        
    } else {
        
        MyPKResults <- bind_rows(MyPKResults) %>% 
            select(Statistic, 
                   any_of(PKparameters), everything()) %>% 
            relocate(File, .after = last_col())
    }
    
    OutQC <- bind_rows(OutQC)
    
    if(checkDataSource){
        Out <- list(MyPKResults, OutQC)
        names(Out) <- c("Table", "QC")
    } else {
        Out <- MyPKResults
    }
    
    if(complete.cases(save_table)){
        
        # Checking whether they have specified just "docx" or just "csv" for
        # output b/c then, we'll use "PK summary table" as file name.
        if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
            OutPath <- "."
            save_table <- paste0("PK summary table.", sub("\\.", "", save_table))
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
        
        if(str_detect(save_table, "\\.csv")){
            # This is when they want a csv file as output
            write.csv(MyPKResults, paste0(OutPath, "/", save_table), row.names = F)
        } else {
            # This is when they want a Word file as output
            
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
            
            rmarkdown::render(
                system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd", 
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
            
        }
        
        if(checkDataSource){
            write.csv(OutQC, sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
        }
    }
    
    return(Out)
}


