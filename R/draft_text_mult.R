#' Create some draft text to insert into the methods and results sections of a
#' report describing the trial design of several simulations at once
#'
#' @param existing_exp_details the output from running either
#'   \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} --
#'   either is fine as long as it contains details for \code{sim_data_file}.
#' @param prettify_compound_names TRUE (default) or FALSE for whether to make
#'   compound names prettier in legend entries and in any Word output files.
#'   This was designed for simulations where the substrate and any metabolites,
#'   perpetrators, or perpetrator metabolites are among the standard options for
#'   the simulator, and leaving \code{prettify_compound_names = TRUE} will make
#'   the name of those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam".
#' @param default_cmpd_file Was one of the default compound files used for the
#'   substrate (if this was a perpetrator simulation) or the perpetrator (if
#'   this was a victim simulation)? TRUE (default) or FALSE. The only thing this
#'   affects is the sentence in the template report text, "The default compound
#'   library file for XXX was used."
#' @param mean_type "arithmetic" or "geometric" (default) means in PK tables
#' @param save_text optionally save the output as a Word document, which is what
#'   we recommend, by providing a Word file name here.
#' @param sim_data_files name of the Excel file containing the simulator output,
#'   in quotes or leave as the default "use existing_exp_details" to get all the
#'   files included in the object provided for \code{existing_exp_details}.
#' @param client_drug_regex fill in a string of text that will match your client
#'   drug, and that will be used to check whether the client drug is in the
#'   substrate position or inhibitor 1 position in the simulation. Which
#'   position it is affects the wording of the output text.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape" 
#'
#' @return list of study design info for a report and, optionall, a Word
#'   document with that info
#' @export
#'
#' @examples
#' none yet
draft_text_mult <- function(sim_data_files = "use existing_exp_details", 
                            existing_exp_details, 
                            prettify_compound_names = TRUE, 
                            default_cmpd_file = TRUE, 
                            client_drug_regex = NA, 
                            mean_type = "geometric", 
                            save_text = NA, 
                            page_orientation = "portrait"){
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning("You must specify `portrait` or `landscape` for the argument page_orientation, and you've specified something else. We'll use the default of `portrait`.\n", 
              call. = FALSE)
   }
   
   if("character" %in% class(sim_data_files) &&
      all(sim_data_files == "use existing_exp_details")){
      sim_data_files <- existing_exp_details$MainDetails$File
   }
   
   DraftText <- list()
   
   for(sim in sim_data_files){
      
      if(is.na(client_drug_regex)){
         victim_sim <- TRUE
      } else {
         victim_sim <- str_detect(existing_exp_details$MainDetails$Substrate, 
                                  client_drug_regex)
      }
      
      DraftText[[sim]][["Methods"]] <- 
         draft_methods_text(sim_data_file = sim, 
                            existing_exp_details = existing_exp_details, 
                            prettify_compound_names = prettify_compound_names, 
                            default_cmpd_file = default_cmpd_file, 
                            victim_sim = victim_sim)
      
      DraftText[[sim]][["Results"]] <- 
         draft_results_text(sim_data_file = sim, 
                            existing_exp_details = existing_exp_details, 
                            prettify_compound_names = prettify_compound_names, 
                            mean_type = mean_type)
      
      rm(sim, victim_sim)
      
   }
   
   ## Saving --------------------------------------------------------------
   if(complete.cases(save_text)){
      
      # Checking whether they have specified just "docx" for output b/c then,
      # we'll use "Draft methods and results text" as file name.
      if(str_detect(sub("\\.", "", save_text), "^docx$")){
         OutPath <- "."
         save_text <- paste0("Draft methods and results text.", sub("\\.", "", save_text))
      } else {
         # If they supplied something other than just "docx", then check whether
         # that file name is formatted appropriately.
         
         if(str_detect(basename(save_text), "\\..*")){
            if(str_detect(basename(save_text), "\\.docx") == FALSE){
               # If they specified a file extension that wasn't docx, make that
               # file extension be .docx
               save_text <- sub("\\..*", ".docx", save_text)
            }
         } else {
            # If they didn't specify a file extension at all, make it .docx. 
            save_text <- paste0(save_text, ".docx")
         }
         
         # Now that the file should have an appropriate extension, check what
         # the path and basename should be.
         OutPath <- dirname(save_text)
         save_text <- basename(save_text)
      }
      
      OutPath <- dirname(save_text)
      
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(save_text)
      
      TemplatePath <- switch(page_orientation, 
                             "landscape" = system.file("Word/landscape_report_template.dotx",
                                                       package="SimcypConsultancy"), 
                             "portrait" = system.file("Word/report_template.dotx",
                                                      package="SimcypConsultancy"))
      
      rmarkdown::render(
         system.file("rmarkdown/templates/draftsections/skeleton/skeleton.Rmd", 
                     package="SimcypConsultancy"),
         output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
         output_dir = OutPath, 
         output_file = FileName, 
         quiet = TRUE)
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
   }
   
   return(DraftText)
   
}

