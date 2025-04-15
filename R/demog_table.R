#' Make a table of demographics for a set of simulations
#'
#' \code{demog_table} makes a table of the demographics of a set of simulations
#' and, if appliable, observed data. UNDER CONSTRUCTION.
#'
#' @param demog_dataframe the output from running \code{\link{extractDemog}}.
#'   Optionally (and we recommend) with added observed demographic data, perhaps
#'   from observed overlay XML files.
#' @param demog_parameters Which demographic parameters do you want to include?
#'   Options are NA to include all of the parameters in demog_dataframe or a
#'   chraacter vector of the columns in demog_dataframe that you want to
#'   include. No need to include any columns that are not a demographic
#'   parameter because we'll ignore them, e.g., you don't need to tell us to
#'   include the columns "File" or "Individual", etc.)
#' @param mean_type What kind of means and CVs or confidence intervals do you
#'   want listed in the output table? Options are "arithmetic" or "geometric"
#'   (default).
#' @param variability_type What statistic would you like to use for reporting
#'   the variability? Options are: \describe{\item{"90\% CI" (default)}{90\% confidence
#'   interval; this will be geometric or arithmetic based on your choice for
#'   \code{mean_type}} \item{"CV"}{coefficient of variation; this will be
#'   geometric or arithmetic based on your choice for \code{mean_type}}
#'   \item{"SD"}{arithmetic standard deviation} \item{"none"}{to get no 
#'   variability stats included in the table}}
#' @param variability_format formatting used to indicate the variability When
#'   the variability is concatenated. Options are "to" (default) to get output
#'   like "X to Y", "hyphen" to get output like "X - Y", "brackets" to get
#'   output like "[X, Y]", or "parentheses" for the eponymous symbol if you're
#'   an American and a bracket if you're British, e.g., "(X, Y)". (Sorry for the
#'   ambiguity; this was written by an American who didn't originally realize
#'   that there was another name for parentheses.)
#' @param break_down_by_sex TRUE (default) or FALSE for whether to break down
#'   the data by sex.
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
#'   \emph{also} want to use the results from \code{pksummary_mult} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the table saved as a Word or csv file.  Do not include any slashes, dollar
#'   signs, or periods in the file name. (You can also save the table to a Word
#'   file later with the function \code{\link{formatTable_Simcyp}}.) If you
#'   supply only the file extension, e.g., \code{save_table = "docx"}, the name
#'   of the file will be "PK summary table" with that extension. If you supply
#'   something other than just "docx" or just "csv" for the file name but you
#'   leave off the file extension, we'll assume you want it to be ".csv". All PK
#'   info will be included in a single Word or csv file, and, if
#'   \code{checkDataSource = TRUE}, that will be saved in a single csv file.
#' @param shading_column If you would like to alternate the shading of the rows
#'   in the output table, supply here the unquoted name of the column to check
#'   for when to change the shading; every time that column's value changes, the
#'   shading will alternate between white and light gray. For example, if you
#'   have a table with PK values for multiple files and you have more than one
#'   row per file (an example of this would be the output from the function
#'   \code{\link{pksummary_mult}}), setting \code{shading_column = File} will
#'   cause the shading of the rows to alternate between white and light gray
#'   whenever the file changes. Please see the examples at the bottom of this
#'   help file.
#' @param merge_shaded_cells TRUE (default) or FALSE for whether to merge the
#'   cells that have the same shade. This only applies when one of the columns
#'   in the input data.frame is used for deciding when to alternate shading,
#'   that is, \code{shading_column} has a value.
#' @param merge_columns a vector of quoted column names or of numeric column
#'   positions that should be merged vertically whenever the values are the
#'   same. For example, \code{merge_columns = c("File", "Tissue")} will cause
#'   the cells in the columns "File" and "Tissue" to merge vertically whenever
#'   the same value shows up in consecutive rows. Similarly, \code{merge_columns
#'   = c(1, 3, 5)} will merge vertically the 1st, 3rd, and 5th columns whenever
#'   the values are the same. Note: This is different from most other functions
#'   in the SimcypConsultancy package, which require unquoted column names.
#'   Honestly, we just don't know how code things for you to supply a variable
#'   number of unquoted column names for a single argument; we've just hit a
#'   coding knowledge limitation here!
#' @param sort_column optionally specify a column to sort by. If none are
#'   supplied, the table will not be sorted. If you would like to sort by more
#'   than one column, we recommend sorting \emph{before} using this function,
#'   e.g., \code{MyPKTable <- MyPKTable \%>\% arrange(Study, Dose)} to sort by
#'   the column "Study" and then by the column "Dose" and \emph{then} supply
#'   "MyPKTable" to \code{formatTable_Simcyp}. (This is just an example; your
#'   table must include those two columns for that to work.)
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # none yet
#' 
demog_table <- function(demog_dataframe, 
                        demog_parameters = NA, 
                        mean_type = "geometric", 
                        variability_type = "90% CI", 
                        variability_format = "to",
                        break_down_by_sex = TRUE, 
                        rounding = NA, 
                        save_table = NA, 
                        # shading_column, 
                        # merge_shaded_cells = TRUE,
                        # merge_columns = NA, 
                        sort_column, 
                        page_orientation = "landscape", 
                        fontsize = 11){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Checking mean type input syntax
   mean_type <- tolower(mean_type)[1]
   if(complete.cases(mean_type)){
      if(mean_type %in% c("geometric", "arithmetic", "median") == FALSE){
         if(mean_type == "mean"){
            warning(paste0(str_wrap("Technically, the input for mean_type should be `geometric` (default) or `arithmetic`. You specified a mean type of `mean`, so we think you want arithmetic means. If that's incorrect, please set mean_type to `geometric`."),
                           "\n"), call. = FALSE)
         }
         
         mean_type <- case_when(str_detect(mean_type, "geo") ~ "geometric", 
                                str_detect(mean_type, "arith") ~ "arithmetic", 
                                str_detect(mean_type, "med") ~ "median")
         
         if(mean_type %in% c("geometric", "arithmetic", "median") == FALSE){
            warning("You specified something other than `geometric` (default), `arithmetic`, or `median` for the mean type, so we're not sure what you would like. We'll use the default of geometric means.\n", 
                    call. = FALSE)
            
            mean_type <- "geometric"
         }
      }
      
   } else {
      mean_type <- "geometric"
   }
   
   # In other functions, for varous reasons, need to convert mean_type to
   # MeanType, so doing that here, too, for consistency.
   MeanType <- mean_type
   
   variability_type <- ifelse(variability_type == "GCV", "CV", variability_type)
   
   # Checking rounding
   rounding <- tolower(rounding[1])
   rounding <- sub("signif ", "significant ", rounding)
   rounding <- ifelse(is.na(rounding), "consultancy", rounding)
   if(str_detect(rounding, "consultancy|none|significant|round|word only") == FALSE){
      warning(paste0(str_wrap("You have entered something for the rounding argument other than the available options. We'll set this to the default, `Consultancy`. Please check the help file for details."), 
                     "\n"), call. = FALSE)
   }
   
   # Make sure that input to variability_format is ok
   if(variability_format %in% c("to", "hyphen", "brackets", "parentheses") == FALSE){
      warning("The input for variability_format is not among the acceptable options, which are `to`, `hyphen`, `brackets` for square brackets, or `parentheses` for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of `to`.\n",
              call. = FALSE)
      variability_format <- "to"
   }
   
   if(break_down_by_sex == FALSE){
      demog_dataframe$Sex <- "both"
   }
   
   
   # Tidying inputs ----------------------------------------------------------
   
   names(demog_dataframe)[
      !names(demog_dataframe) %in% c("File", "Trial", "Individual", "Population", 
                                     "Simulated")] <-
      tolower(names(demog_dataframe)[
         !names(demog_dataframe) %in% c("File", "Trial", "Individual", "Population", 
                                        "Simulated")])
   
   Harmonized <- harmonize_demog(demog_dataframe = demog_dataframe, 
                                 demog_parameters = demog_parameters)
   DemogParams <- Harmonized$DemogParams
   PossDemogParams <- Harmonized$PossDemogParams
   
   if(nrow(DemogParams) == 0){
      stop(wrapn("You have not supplied any valid demographic parameters. Please check your input and try again."), 
           call. = FALSE)
   }
   
   DemogLabs <- PossDemogParams$Label
   names(DemogLabs) <- tolower(PossDemogParams$Parameter)
   
   
   # Main body of function ---------------------------------------------------
   
   FT <- 
      suppressWarnings(suppressMessages(
         demog_dataframe %>% 
            select(any_of(c("File", "Trial", "Individual", "Population", "sex",
                            "Simulated", DemogParams$Parameter))) %>% 
            rename(Sex = sex) %>% # dealing w/inconsistencies between table and graph functions
            pivot_longer(cols = DemogParams$Parameter, 
                         names_to = "Parameter", 
                         values_to = "Value") %>% 
            filter(Parameter %in% DemogParams$Parameter) %>% 
            group_by(File, Simulated, Sex, Parameter) %>% 
            summarize(Mean = mean(Value, na.rm = T), 
                      SD = sd(Value, na.rm = T), 
                      Median = median(Value, na.rm = T),
                      Geomean = gm_mean(Value), 
                      CI90_l = switch(mean_type, 
                                      "geometric" = gm_conf(Value, CI = 0.9)[1], 
                                      "arithmetic" = confInt(Value, CI = 0.9)[1], 
                                      "median" = gm_conf(Value, CI = 0.9)[1]), 
                      CI90_u = switch(mean_type, 
                                      "geometric" = gm_conf(Value, CI = 0.9)[2], 
                                      "arithmetic" = confInt(Value, CI = 0.9)[2],
                                      "median" = gm_conf(Value, CI = 0.9)[2]), 
                      CV = switch(mean_type, 
                                  "geometric" = gm_CV(Value), 
                                  "arithmetic" = sd(Value, na.rm = T) / 
                                     mean(Value, na.rm = T),
                                  "median" = sd(Value, na.rm = T) / 
                                     mean(Value, na.rm = T)))
      ))
   
   FT <- FT %>% ungroup() %>% 
      mutate(across(.cols = -c(File, Simulated, Sex, Parameter), 
                    .fns = function(x) round_opt(x, round_fun = rounding)),  
             Var = switch(variability_type, 
                          "90% CI" = switch(variability_format, 
                                            "to" = paste(CI90_l, "to", CI90_u), 
                                            "hyphen" = paste(CI90_l, "-", CI90_u), 
                                            "brackets" = paste0("[", CI90_l, ", ", CI90_u, "]"),
                                            "parentheses" = paste0("(", CI90_l, ", ", CI90_u, ")")), 
                          "CV" = CV, 
                          "SD" = SD, 
                          # placeholder
                          "none" = SD), 
             Var = ifelse(Var == "NA to NA", NA, Var), 
             Value = switch(mean_type, 
                            "geometric" = Geomean, 
                            "arithmetic" = Mean, 
                            "median" = Median)) %>% 
      select(File, Simulated, Sex, Parameter, Value, Var) %>% 
      rename(SorO = Simulated) %>% 
      pivot_longer(cols = c(Value, Var), 
                   names_to = "Statistic", 
                   values_to = "Val") %>% 
      mutate(Statistic = case_when(Statistic == "Value" & 
                                      {{mean_type}} == "geometric" ~ "geometric mean", 
                                   Statistic == "Value" & 
                                      {{mean_type}} == "arithmetic" ~ "mean", 
                                   Statistic == "Value" & 
                                      {{mean_type}} == "median" ~ "median", 
                                   Statistic == "Var" & 
                                      {{variability_type}} == "90% CI" ~ "90% confidence interval", 
                                   Statistic == "Var" & 
                                      {{variability_type}} == "CV" ~ "coefficient of variation", 
                                   Statistic == "Var" & 
                                      {{variability_type}} == "SD" ~ "standard deviation", 
                                   Statistic == "Var" & 
                                      {{variability_type}} == "none" ~ "REMOVE THIS ROW"), 
             SorO = ifelse(SorO == TRUE, "simulated", "observed"), 
             Parameter = case_match(Parameter, 
                                    "Weight_kg" ~ "Weight (kg)", 
                                    "Height_cm" ~ "Height (cm)", 
                                    "BSA_m2" ~ "BSA (m^2^)", 
                                    "BrainWt_g" ~ "Brain weight (g)", 
                                    "KidneyWt_g" ~ "Kidney weight (g)", 
                                    "LiverWt_g" ~ "Liver weight (g)", 
                                    "BMI_kgm2" ~ "BMI (kg/m^2^)", 
                                    "CardiacOut" ~ "Cardiac output (L/h)", 
                                    "Haematocrit" ~ "Haematocrit (percent)", 
                                    "RenalFunction" ~ "Renal function", 
                                    "HSA_gL" ~ "HSA (g/L)", 
                                    "AGP_gL" ~ "AGP (g/L)", 
                                    "Other_uM" ~ "Other drug-binding protein (uM)", 
                                    "Creatinine_umolL" ~ "Creatinine (umol/L)", 
                                    "GFR_mLminm2" ~ "GFR (mL/min/m^2^)", 
                                    .default = Parameter)) %>% 
      pivot_wider(names_from = Parameter, values_from = Val)
   
   if(variability_type == "none"){
      FT <- FT %>% filter(!Statistic == "REMOVE THIS ROW")
   }
   
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Format the file name appropriately, including making the extension be
      # docx, even if they specified something else.
      save_table <- ifelse(str_detect(save_table, "\\..*$"), 
                           sub("\\..*", ".docx", save_table), 
                           paste0(save_table, ".docx"))
      
      # Now that the file should have an appropriate extension, check what
      # the path and basename should be.
      OutPath <- dirname(save_table)
      save_table <- basename(save_table)
      
      # May need to change the working directory temporarily, so
      # determining what it is now
      CurrDir <- getwd()
      
      OutPath <- dirname(save_table)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(save_table)
      
      SimFiles <- unique(demog_dataframe$File[demog_dataframe$Simulated])
      ObsFiles <- unique(demog_dataframe$File[demog_dataframe$Simulated == FALSE])
      Caption <- paste0("Source simulated data: ", str_comma(SimFiles))
      
      Caption <- ifelse(length(ObsFiles) > 0, 
                        paste0(Caption, ". Source observed data: ", 
                               str_comma(ObsFiles)), 
                        Caption)
      
      if(break_down_by_sex){
         FT <- FT %>% 
            formatTable_Simcyp(fontsize = fontsize, 
                               merge_shaded_cells = TRUE, 
                               merge_columns = c("File", "SorO", "Sex"), 
                               shading_column = Sex, 
                               page_orientation = page_orientation, 
                               save_table = save_table, 
                               title_document = "Demographics", 
                               table_caption = Caption)
         
      } else {
         
         FT <- FT %>% select(-Sex) %>% 
            formatTable_Simcyp(fontsize = fontsize, 
                               merge_shaded_cells = TRUE, 
                               merge_columns = c("File", "SorO"), 
                               shading_column = File, 
                               page_orientation = page_orientation, 
                               save_table = save_table, 
                               title_document = "Demographics", 
                               table_caption = Caption)
      }
   }
   
   return(FT)
   
}

