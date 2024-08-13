#' Summarize a model pretty simply - UNDER CONSTRUCTION
#'
#' \code{make_model_summary_table} is meant for use with writing compound
#' summary pdfs. It includes minimal error catching at this point. NB: This
#' rounds numeric values to the 5th digit.
#'
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}}
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input: "all" to get
#'   everything or else a character vector of the sim results files you want. If
#'   these are not present in existing_exp_details, they will be ignored.
#' @param compoundID For which compound do you want to summarize the model
#'   parameters? Options are "substrate", "primary metabolite 1", "primary
#'   metabolite 2", "secondary metabolite", "inhibitor 1", "inhibitor 2", or
#'   "inhibitor 1 metabolite". 
#' @param font font to use. Default is "Arial" and any fonts available on your
#'   machine in either Word or PowerPoint should be acceptable. If you get Times
#'   New Roman in your table when you asked for something else, it means that
#'   that font isn't available or maybe wasn't spelled the way R is expecting
#'   it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
#'   the latter is listed in PowerPoint and Word.
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
#'   slashes, dollar signs, or periods in the file name. If you leave off the
#'   file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # none yet
make_model_summary_table <- function(existing_exp_details,
                                     sims_to_include = "all", 
                                     compoundID = "substrate", 
                                     font = "Palatino Linotype", 
                                     fontsize = 11, 
                                     save_table = NA,
                                     page_orientation = "portrait"){
   
   if(any(sims_to_include != "all")){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          sims_to_include, 
                                          "include")
   }
   
   suppressWarnings(
      FT <- 
         annotateDetails(existing_exp_details = existing_exp_details, 
                         compoundID = compoundID, 
                         detail_set = "Simcyp inputs") %>% 
         rename("Section of model" = SimulatorSection, 
                Value = "All files have this value for this compound ID and compound") %>% 
         filter(complete.cases(Value) & 
                   !Detail %in% c("SimulatorVersion", AllCompounds$DetailNames)) %>% 
         left_join(AllExpDetails %>% select(Detail, ReportTableText) %>% unique(), 
                   by = "Detail") %>% 
         mutate(Parameter = ifelse(is.na(ReportTableText), 
                                   Detail, ReportTableText), 
                Value = ifelse(complete.cases(as.numeric(Value)), 
                               as.character(round(as.numeric(Value), 5)), Value)) %>% 
         select("Section of model", Parameter, Value) %>% 
         format_table_simple(shading_column = "Section of model") %>% 
         flextable::width(j = 1, width = 2) %>% 
         flextable::width(j = 2, width = 3) %>% 
         flextable::width(j = 3, width = 1.5) %>% 
         flextable::merge_v(j = 1) 
   )
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      title_document <- "Model Summary"
      table_caption <- NA
      
      formatTable_Simcyp(DF = FT, 
                         save_table = save_table, 
                         page_orientation = page_orientation, 
                         title_document = title_document, 
                         table_caption = table_caption)
   }
   
   
   return(FT)
   
}


