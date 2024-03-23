#' Create a table describing your file design.
#' 
#' UNDER CONSTRUCTION. Proof of concept only for now. 
#'
#' @param existing_exp_details output from running extractExpDetails_mult
#' @param sim_data_files optionally specify which simulation files you want to
#'   inlude.
#' @param cols_to_include optionally specify which columns you want. Current
#'   options are "default" for a handful of standard columns or "all trial
#'   design" to get all possible trial design columns. You can also specify a
#'   character vector of columns you want from the "MainDetails" item from
#'   existing_exp_details.
#' @param ... Arguments that pass through to formatTable_Simcyp. 
#'
#' @return a formatted table and optionally, a saved Word file with that table
#' @export
#'
#' @examples
#' make_trial_design_table(existing_exp_details = MDZdetails, 
#'                         shading_column = File, 
#'                         save_table = "MDZ trial design info.docx")

#' 
make_trial_design_table <- function(existing_exp_details, 
                                    sim_data_files = NA, 
                                    cols_to_include = "default", 
                                    fontsize = 11, 
                                    shading_column, 
                                    merge_shaded_cells = TRUE,
                                    merge_columns = NA, 
                                    sort_column, 
                                    bold_cells = list(c(0, NA), c(NA, 1)),
                                    center_1st_column = FALSE,
                                    highlight_cells = NA, 
                                    highlight_color = "yellow",
                                    save_table = NA, 
                                    title_document = NA, 
                                    table_caption = NA){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Setting up for NSE --------------------------------------------------------
   
   shading_column <- rlang::enquo(shading_column)
   sort_column <- rlang::enquo(sort_column)
   
   # Main function ------------------------------------------------------------
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(any(complete.cases(sim_data_files))){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          sim_data_files, 
                                          "include")
   }
   
   if(all(cols_to_include == "default")){
      Cols <- c("File", "Age_min", "Age_max", "PercFemale", 
                "Substrate", "Inhibitor1", 
                "DoseRoute_sub", "Dose_sub", "DoseInt_sub", "StartDayTime_sub", 
                "DoseRoute_inhib", "Dose_inhib", "DoseInt_inhib", "StartDayTime_inhib", 
                "SimDuration") 
      cols_to_include <- NA
   } else {
      Cols <- c()
   }
   
   if("all trial design" %in% cols_to_include){
      Cols <- c(Cols, 
                AllExpDetails$Detail[AllExpDetails$SimulatorSection == "Trial Design"])
   }
   
   # Getting any additional columns people want
   Cols <- sort(unique(c(Cols, cols_to_include)))
   
   Out <- existing_exp_details$MainDetails %>% 
      select(any_of(Cols)) %>% 
      # Need to set the order still. The code below would be for sorting rows but this is columns. 
      
      # left_join(AllExpDetails %>% select(Detail, SortOrder)) %>% 
      # arrange(SortOrder) %>% 
      # select(-SortOrder) %>% 
      select(File, everything()) %>% 
      purrr::discard(~all(is.na(.)))
   
   formatTable_Simcyp(DF = Out, 
                      fontsize = fontsize, 
                      shading_column = !!shading_column, 
                      merge_shaded_cells = merge_shaded_cells,
                      merge_columns = merge_columns, 
                      sort_column = !! sort_column, 
                      bold_cells = bold_cells,
                      center_1st_column = center_1st_column,
                      highlight_cells = highlight_cells, 
                      highlight_color = highlight_color,
                      save_table = save_table, 
                      title_document = title_document, 
                      table_caption = table_caption)
   
}


