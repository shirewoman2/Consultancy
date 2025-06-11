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
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input:
#'
#'   \describe{\item{NA (default)}{get all the simulations included in
#'   \code{demog_dataframe}}
#'
#'   \item{a character vector of the file names you want}{The items in the character
#'   vector must \emph{exactly} match file names in the column "File" of the
#'   \code{demog_dataframe}, including the file extension}
#'
#'   \item{a regular expression}{This will include in the output only files
#'   that match the regular expression. This must have length = 1, and it IS
#'   case sensitive. For example, say you only want to look at development or
#'   verification simulations and you included "dev" or "ver" in those file
#'   names, respectively. Here is how you could specify that (the vertical pipe |
#'   means "or" for regular expressions): \code{sim_to_include = "dev|ver"}}}
#' @param sim_file_labels optionally specify labels to use in lieu of simulation
#'   file names in the table. This should be a named character vector where the
#'   names are the simulation file name and the values are what you'd like to
#'   have appear in the table instead. Be sure that the file name matches
#'   perfectly, including the file extension! The order you list here will be
#'   the order the simulations appear in your table. Example:
#'   \code{sim_file_labels = c("mdz-5mg-sd-hv.xlsx" = "Healthy subjects",
#'   "mdz-5mg-sd-cpa.xlsx" = "Child-Pugh A", "mdz-5mg-sd-cpb.xlsx" = "Child-Pugh B",
#'   "mdz-5mg-sd-cpc.xlsx" = "Child-Pugh C")}
#' @param include_SorO_column TRUE or FALSE (default) for whether to include a
#'   column indicating whether the data were simulated or observed. TRUE will
#'   always include it and FALSE will only include it when there were both
#'   simulated and observed data present.
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # none yet
#' 
demog_table <- function(demog_dataframe, 
                        demog_parameters = NA, 
                        sims_to_include = NA, 
                        sim_file_labels = NA, 
                        mean_type = "geometric", 
                        variability_type = "90% CI", 
                        variability_format = "to",
                        break_down_by_sex = TRUE, 
                        include_SorO_column = F, 
                        rounding = NA, 
                        save_table = NA, 
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
      warning(wrapn("The input for variability_format is not among the acceptable options, which are 'to', 'hyphen', 'brackets' for square brackets, or 'parentheses' for the eponymous symbol if you're an American and a bracket if you're British. We'll use the default of 'to'."),
              call. = FALSE)
      variability_format <- "to"
   }
   
   # If user has supplied regex for sims_to_include, that should have length 1.
   # If they supplied a character vector of files, that should probably have
   # length > 1. Even if they only supplied a single file name here, it should
   # still work to use regex instead of a perfect match.
   if(any(complete.cases(sims_to_include)) && 
      length(sims_to_include) == 1){
      sims_to_include <- 
         demog_dataframe$File[str_detect(demog_dataframe$File, 
                                         sims_to_include)]
      # At this point, sims_to_include should be a character vector of file
      # names.
   }
   
   # Keeping only the requested sims for sims_to_include
   if(any(complete.cases(sims_to_include))){
      demog_dataframe <- filter_sims(which_object = demog_dataframe, 
                                     which_sims = sims_to_include,
                                     include_or_omit = "include")
   }
   
   if(any(complete.cases(sim_file_labels))){
      
      ExtraFiles <- setdiff(names(sim_file_labels), 
                            unique(demog_dataframe$File))
      if(length(ExtraFiles) > 0){
         warning(paste0(wrapn("The following files were included in 'sim_file_labels' but are not present in 'demog_dataframe', so we will ignore them:"), 
                        str_c(paste0("   '", ExtraFiles, "'"), 
                              collapse = "\n"), 
                        "\n"), 
                 call. = FALSE)
         
      }
      
      FileLevels <- intersect(names(sim_file_labels),
                              unique(demog_dataframe$File))
      MissingFiles <- setdiff(unique(demog_dataframe$File), FileLevels)
      if(length(MissingFiles) > 0){
         warning(paste0(wrapn("The following files were included in 'demog_dataframe' but not in 'sim_file_labels', so we will list them after the ones that *were* included in 'sim_file_labels':"), 
                        str_c(paste0("   '", MissingFiles, "'"), 
                              collapse = "\n"), 
                        "\n", 
                        wrapn("If you meant for those files to be omitted entirely, please specify which simulation files you wanted with the argument 'sims_to_include'.")), 
                 call. = FALSE)
         FileLevels <- c(FileLevels, MissingFiles)
         names(MissingFiles) <- MissingFiles
         sim_file_labels <- c(sim_file_labels, MissingFiles)
      }
   } else {
      FileLevels <- sort(unique(demog_dataframe$File))
   }
   
   demog_dataframe$File <- factor(demog_dataframe$File, 
                                  levels = FileLevels)
   
   # Tidying inputs ----------------------------------------------------------
   
   names(demog_dataframe)[
      !names(demog_dataframe) %in% c("File", "Trial", "Individual", "Population", 
                                     "Simulated")] <-
      tolower(names(demog_dataframe)[
         !names(demog_dataframe) %in% c("File", "Trial", "Individual", "Population", 
                                        "Simulated")])
   
   Harmonized <- harmonize_demog(demog_dataframe = demog_dataframe, 
                                 demog_parameters = demog_parameters, 
                                 table_or_graph = "table")
   DemogParams <- Harmonized$DemogParams
   PossDemogParams <- Harmonized$PossDemogParams
   
   if(nrow(DemogParams) == 0){
      stop(wrapn("You have not supplied any valid demographic parameters. Please check your input and try again."), 
           call. = FALSE)
   }
   
   DemogLabs <- PossDemogParams$Label
   names(DemogLabs) <- tolower(PossDemogParams$Parameter)
   
   # If they didn't specifically ask for allometric scalar initially and the
   # values in that column are all 1, omit that parameter b/c not interesting.
   if(all(is.na(demog_parameters)) &
      "allometricscalar" %in% names(demog_dataframe) &&
      (all(demog_dataframe$allometricscalar == 1) | 
       all(is.na(demog_dataframe$allometricscalar)))){
      demog_dataframe$allometricscalar <- NULL
      DemogParams <- DemogParams %>% 
         filter(Parameter != "allometricscalar")
   }
   
   
   # Main body of function ---------------------------------------------------
   
   GroupCols <- switch(as.character(break_down_by_sex),
                       "TRUE" = c("File", "Simulated", "Parameter", "sex"), 
                       "FALSE" = c("File", "Simulated", "Parameter"))
   ParamCols <- switch(as.character(break_down_by_sex),
                       "TRUE" = c(DemogParams$Parameter, "sex"), 
                       "FALSE" = DemogParams$Parameter)
   
   suppressWarnings(suppressMessages(
      Out <- demog_dataframe %>% 
         select(any_of(c("File", "Trial", "Individual", "Population", "Simulated",
                         ParamCols))) %>% 
         # NB: Can't pivot with sex b/c not numeric data and the other
         # parameters are.
         pivot_longer(cols = any_of(setdiff(tolower(PossDemogParams$Parameter),
                                            "sex")), 
                      names_to = "Parameter", 
                      values_to = "Value") %>% 
         filter(Parameter %in% DemogParams$Parameter) %>% 
         group_by(across(.cols = any_of(GroupCols))) %>% 
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
   
   Out <- Out %>% ungroup() %>% 
      mutate(across(.cols = -GroupCols, 
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
      select(any_of(c(GroupCols, "Value", "Var"))) %>% 
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
             `Simulated or observed` = ifelse(Simulated == TRUE, "simulated", "observed"), 
             Parameter = DemogLabs[Parameter]) %>% 
      select(-Simulated)
   
   # Adjusting capitalization of column for sex if present
   names(Out)[which(names(Out) == "sex")] <- "Sex"
   
   Out <- Out %>% 
      pivot_wider(names_from = Parameter, values_from = Val)
   
   if(any(complete.cases(sim_file_labels))){
      Out <- Out %>% 
         mutate(Population = sim_file_labels[File]) %>% 
         select(-File) %>% 
         select(Population, everything())
   }
   
   if(variability_type == "none"){
      Out <- Out %>% filter(!Statistic == "REMOVE THIS ROW")
   }
   
   if(include_SorO_column == FALSE & 
      length(sort(unique(demog_dataframe$Simulated))) == 1){
      Out <- Out %>% select(-`Simulated or observed`)
   }
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Format the file name appropriately, including making the extension be
      # docx, even if they specified something else.
      save_table <- ifelse(str_detect(save_table, "\\..*$"), 
                           sub("\\..*", ".docx", save_table), 
                           paste0(save_table, ".docx"))
      
      FileName <- save_table
      if(str_detect(FileName, "\\.docx") == FALSE){
         # Making sure they've got a good extension
         FileName <- paste0(sub(".*$", "", FileName), ".docx")
      }
      
      SimFiles <- unique(as.character(demog_dataframe$File[demog_dataframe$Simulated]))
      ObsFiles <- unique(as.character(demog_dataframe$File[demog_dataframe$Simulated == FALSE]))
      Caption <- paste0("Source simulated data: ", str_comma(SimFiles))
      
      Caption <- ifelse(length(ObsFiles) > 0, 
                        paste0(Caption, ". Source observed data: ", 
                               str_comma(ObsFiles)), 
                        Caption)
      
      MergeCols <- intersect(c("File", "Population", 
                               "Simulated or observed", "Sex"), 
                             names(Out))
      
      if(break_down_by_sex){
         
         FT <- Out %>% 
            formatTable_Simcyp(
               fontsize = fontsize, 
               merge_shaded_cells = TRUE, 
               merge_columns = MergeCols, 
               shading_column = Sex, 
               center_1st_column = T, 
               bold_cells = list(c(0, NA)), 
               page_orientation = page_orientation, 
               save_table = FileName, 
               title_document = "Demographics", 
               table_caption = Caption)
         
      } else {
         
         if("File" %in% names(Out)){
            
            FT <- Out %>% 
               formatTable_Simcyp(
                  fontsize = fontsize, 
                  merge_shaded_cells = TRUE, 
                  merge_columns = MergeCols, 
                  shading_column = File, 
                  center_1st_column = T, 
                  bold_cells = list(c(0, NA)), 
                  page_orientation = page_orientation, 
                  save_table = FileName, 
                  title_document = "Demographics", 
                  table_caption = Caption)
         } else {
            FT <- Out %>% 
               formatTable_Simcyp(
                  fontsize = fontsize, 
                  merge_shaded_cells = TRUE, 
                  merge_columns = MergeCols, 
                  shading_column = Population, 
                  center_1st_column = T, 
                  bold_cells = list(c(0, NA)), 
                  page_orientation = page_orientation, 
                  save_table = FileName, 
                  title_document = "Demographics", 
                  table_caption = Caption)
         }
      }
   }
   
   return(Out)
   
}

