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
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # none yet
make_model_summary_table <- function(existing_exp_details,
                                     sims_to_include = "all", 
                                     compoundID = "substrate"){
   
   if(any(sims_to_include != "all")){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          sims_to_include, 
                                          "include")
   }
   
   suppressWarnings(
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
         width(j = 1, width = 1.5) %>% 
         width(j = 2, width = 3) %>% 
         width(j = 3, width = 1.5) %>% 
         merge_v(j = 1) 
   )
}


