#' Create a table describing a simulation trial design.
#'
#' @param existing_exp_details output from running
#'   \code{\link{extractExpDetails_mult}}
#' @param sims_to_include optionally specify which simulation files you want to
#'   include.
#' @param prettify_sim_data_file optionally specify what to use for column names
#'   instead of, e.g., "abc1a-5mg-sd.xlsx". For example, maybe that particular
#'   simulation was from Clinical Study 101, 5 mg cohort. Here's how you could
#'   specify that you'd rather see the clinical study name over the simulation
#'   file name: \code{prettify_sim_data_file = c("abc1a-5mg-sd.xlsx" =
#'   "Clinical Study 101, 5 mg single dose", "abc1a-200mg-qd.xlsx" =
#'   "Clinical Study 102, 200 mg QD")} This can be a named character vector like
#'   that or it can be a data.frame with a column called "File" and a second
#'   column called "Annotation". \strong{WARNING}: This will fail if the names
#'   are not unique!
#' @param detail_set optionally specify which details you want. Current options
#'   are "default" for a handful of standard columns or "all trial design" to
#'   get all possible trial design columns.
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, perpetrators, or perpetrator metabolites are among the
#'   standard options for the simulator, and leaving \code{prettify_compound_names =
#'   TRUE} will make the name of those compounds something more human readable.
#'   For example, "SV-Rifampicin-MD" will become "rifampicin", and
#'   "Sim-Midazolam" will become "midazolam". Alternatively, set each compound
#'   to the name you'd prefer to see in your table if you would like something
#'   different. For example, \code{prettify_compound_names = c("Ketoconazole-400 mg QD" =
#'   "ketoconazole PO", "StatinX with updated ka" = "superstatin")}.
#' @param font font to use. Default is "Arial" and any fonts available on your
#'   machine in either Word or PowerPoint should be acceptable. If you get Times
#'   New Roman in your table when you asked for something else, it means that
#'   that font isn't available or maybe wasn't spelled the way R is expecting
#'   it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
#'   the latter is listed in PowerPoint and Word.
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
#' @param column_widths optionally specify what the widths of the columns should
#'   be with a numeric vector of the widths in inches, e.g., \code{column_widths
#'   = c(1.5, 2, 0.5, 3)}
#' @param output_type specify what type of output to get. Options:
#'   \describe{\item{"flextable" (default)}{a nicely formatted table that you can view or
#'   insert into an Rmarkdown document}
#'
#'   \item{"data.frame"}{a data.frame that you can manipulate further in R} 
#'   
#'   \item{"both"}{both the nicely formatted flextable and the data.frame}}
#' @param include_shading TRUE (default) or FALSE for whether to add shading to
#'   rows to make things easier to read. If this is set to TRUE, then the rows
#'   will be shaded every other row when there's no DDI or by which compound the
#'   parameters apply to if there is.
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
#'   slashes, dollar signs, or periods in the file name. If you leave off the
#'   file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#'
#' @return a formatted table and optionally, a saved Word file with that table
#' @export
#'
#' @examples
#' make_trial_design_table(existing_exp_details = MDZdetails,
#'                         save_table = "MDZ trial design info.docx")
#' 
make_trial_design_table <- function(existing_exp_details, 
                                    sims_to_include = NA, 
                                    prettify_sim_data_file = NA, 
                                    detail_set = "default", 
                                    prettify_compound_names = TRUE, 
                                    output_type = "flextable", 
                                    include_shading = TRUE, 
                                    font = "Palatino Linotype", 
                                    fontsize = 11, 
                                    column_widths = NA, 
                                    save_table = NA,
                                    page_orientation = "portrait"){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(length(detail_set) > 1 ||
      detail_set %in% c("default", "all trial design") == FALSE){
      stop(wrapn("You have specified something other than 'default' or 'all trial design' for the possible set of details to show in your table, but those are the only possible options for detail_set. Please try again."), 
           call. = FALSE)
   }
   
   if(any(c("logical", "character", "data.frame") %in%
          class(prettify_sim_data_file)) == FALSE){
      warning(wrapn("You have supplied something other than a character vector or a data.frame for the argument prettify_sim_data_file, so we don't know what to do with your input and we'll have to ignore it."), 
              call. = FALSE)
      prettify_sim_data_file <- NA
   }
   
   if("data.frame" %in% class(prettify_sim_data_file)){
      names(prettify_sim_data_file) <- tolower(names(prettify_sim_data_file))
      if(all(c("file", "annotation") %in% names(prettify_sim_data_file)) == FALSE){
         warning(wrapn("You have supplied a data.frame for the argument prettify_sim_data_file, but the column names are not 'File' and 'Annotation', which are the only column names we know how to deal with. We'll have to ignore your input for prettify_sim_data_file."), 
                 call. = FALSE)
         prettify_sim_data_file <- NA
      }
   }
   
   if(class(prettify_compound_names) == "character" &&
      is.null(names(prettify_compound_names))){
      warning(wrapn("You have supplied values for `prettify_compound_names` but not assigned them with compound names, so we're not sure what values to replace."), 
              call. = FALSE)
      prettify_compound_names <- FALSE
   }
   
   output_type <- tolower(output_type)
   if(output_type %in% c("flextable", "data.frame", "both") == FALSE){
      warning(wrapn("You requested something for the output type other than a flextable, a data.frame, or both, so we'll give you the default, a flextable."), 
              call. = FALSE)
      output_type <- "flextable"
   }
   
   
   # Main function ------------------------------------------------------------
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(any(complete.cases(sims_to_include))){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          sims_to_include, 
                                          "include")
   }
   
   # Prettifying as requested
   if("logical" %in% class(prettify_compound_names)){
      if(prettify_compound_names){
         existing_exp_details$MainDetails <- 
            existing_exp_details$MainDetails %>% 
            mutate(across(.cols = any_of(AllRegCompounds$DetailNames), 
                          .fns = prettify_compound_name))
      }
   } else {
      # this is when they have supplied a character vector for what to use for
      # each compound name
      for(d in AllRegCompounds$DetailNames){
         for(cmpd in names(prettify_compound_names)){
            existing_exp_details$MainDetails[, d][
               which(existing_exp_details$MainDetails[, d] == cmpd)] <- 
               prettify_compound_names[cmpd]
         }
      }
   }
   
   if(detail_set == "default"){
      MyDetails <- c("File", "Population", "Age_min", "Age_max", "PercFemale", 
                     "Substrate", "Inhibitor1", "Inhibitor2",
                     paste0(rep(c("DoseRoute", "Dose", "DoseInt",
                                  "StartDayTime", "PrandialSt"), 
                                each = 3), 
                            c("_sub", "_inhib", "_inhib2")), 
                     "SimDuration", "NumSubjTrial", "NumTrials", 
                     "SimStartDayTime", "SimEndDayTime") 
   } else {
      MyDetails <- c(AllExpDetails$Detail[
         AllExpDetails$SimulatorSection == "Trial Design"], "Population")
      MyDetails <- setdiff(MyDetails, c("SimulatorVersion"))
   }
   
   # Dealing w/sim file names
   if(any(complete.cases(prettify_sim_data_file))){
      
      if("data.frame" %in% class(prettify_sim_data_file)){
         prettify_sim_data_file <- unique(prettify_sim_data_file)
         
         FileAnnotations <- prettify_sim_data_file$annotation
         names(FileAnnotations) <- prettify_sim_data_file$file
      } else {
         FileAnnotations <- prettify_sim_data_file
      }
   } else {
      # This is just so things will pass through rename_with function. 
      FileAnnotations <- existing_exp_details$MainDetails$File
      names(FileAnnotations) <- existing_exp_details$MainDetails$File
   }
   
   Inhib1Present <- any(complete.cases(existing_exp_details$MainDetails$Inhibitor1))
   Inhib2Present <- any(complete.cases(existing_exp_details$MainDetails$Inhibitor2))
   
   suppressWarnings(
      DF <- annotateDetails(existing_exp_details = existing_exp_details, 
                            detail_set = MyDetails, 
                            show_compound_col = "concatenate") %>% 
         left_join(AllExpDetails %>% select(Detail, SortOrder), 
                   by = "Detail") %>% unique() %>% 
         mutate(SortOrder = ifelse(Notes == "Compound name", 
                                   # hacking this to make compound name come 1st
                                   1, SortOrder), 
                Notes = case_when(CompoundID == "substrate" & 
                                     Notes == "Compound name" ~ "Substrate name", 
                                  CompoundID == "inhibitor 1" & 
                                     Inhib2Present == FALSE & 
                                     Notes == "Compound name" ~ "Perpetrator name", 
                                  CompoundID == "inhibitor 1" & 
                                     Inhib2Present == TRUE & 
                                     Notes == "Compound name" ~ "Inhibitor 1 name", 
                                  CompoundID == "inhibitor 2" & 
                                     Notes == "Compound name" ~ "Inhibitor 2 name", 
                                  .default = Notes), 
                CompoundID = ifelse(is.na(CompoundID), 
                                    "none", as.character(CompoundID)), 
                CompoundID = factor(CompoundID,
                                    levels = c("none", AllRegCompounds$CompoundID))) %>% 
         arrange(CompoundID, SortOrder) %>%
         select(-SortOrder) %>% unique() %>% 
         rename(Parameter = Notes) %>% 
         mutate(across(.cols = matches("\\.xlsx|\\.db"), 
                       .fns = \(Value) ifelse(complete.cases(as.numeric(Value)), 
                                              as.character(round(as.numeric(Value), 5)), Value))) %>% 
         select(-SimulatorSection, -DataSource, -Compound, -Detail,
                -matches("All files have")) %>% 
         rename_with(~ str_replace_all(., FileAnnotations)) %>% 
         select(Parameter, everything())
   )
   
   # Error catching column_widths now that we have DF
   if("logical" %in% class(column_widths) == FALSE){
      if("numeric" %in% class(column_widths) == FALSE){
         warning(wrapn("You have supplied something other than numeric data for the column widths, so we don't know what you want and will ignore this."), 
                 call. = FALSE)
         column_widths <- NA
      } else {
         # Making sure we have more than enough values
         column_widths <- rep(column_widths, ncol(DF))
      }
   } 
   
   # Document title
   title_document  <- paste("Trial design for", 
                            ifelse(length(existing_exp_details$MainDetails$File) == 1, 
                                   paste("the simulation", 
                                         existing_exp_details$MainDetails$File), 
                                   paste("the simulations", 
                                         str_comma(existing_exp_details$MainDetails$File))))
   table_caption <- NA
   
   if(include_shading){
      if(Inhib1Present){
         FT <- 
            format_table_simple(DF = DF, 
                                fontsize = fontsize, 
                                font = font, 
                                shading_column = CompoundID) %>% 
            flextable::delete_columns(j = which(names(DF) == "CompoundID"))
         
      } else {
         # This is when they want shading and sim only includes substrate
         FT <- 
            format_table_simple(DF = DF, 
                                fontsize = fontsize, 
                                shading_column = Parameter, 
                                font = font) %>% 
            flextable::delete_columns(j = which(names(DF) == "CompoundID"))
      }
   } else {
      # This is when they don't want shading 
      FT <- 
         format_table_simple(DF = DF, 
                             fontsize = fontsize, 
                             font = font) %>% 
         flextable::delete_columns(j = which(names(DF) == "CompoundID"))
   }
   
   # Setting columnn widths. Note that there is one fewer column now b/c
   # CompoundID is not included.
   if("logical" %in% class(column_widths) == FALSE){
      for(col in 1:(ncol(DF)-1)){
         FT <- FT %>% 
            flextable::width(j = col, width = column_widths[col])
      }
   }
   
   
   # Saving and returning -----------------------------------------------------
   if(complete.cases(save_table)){
      
      formatTable_Simcyp(DF = FT, 
                         save_table = save_table, 
                         page_orientation = page_orientation, 
                         title_document = title_document, 
                         table_caption = table_caption)
   }
   
   if(output_type == "flextable"){
      return(FT)
   } else if(output_type == "data.frame"){
      return(DF)
   } else if(output_type == "both"){
      return(list("flextable" = FT, 
                  "data.frame" = DF))
   }
   
}


