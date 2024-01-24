#' Title
#'
#' @param sim_data_files files
#' @param existing_exp_details details
#' @param prettify_compound_names T or F
#' @param default_cmpd_file T or F
#' @param victim_sim T or F
#' @param mean_type geometric or arithmetic
#' @param save_text word doc file name
#'
#' @return list of study design info for a report and, optionall, a Word
#'   document with that info
#' @export
#'
#' @examples
#' none yet
draft_text_mult <- function(sim_data_files, 
                            existing_exp_details, 
                            prettify_compound_names = TRUE, 
                            default_cmpd_file = TRUE, 
                            client_drug_regex = NA, 
                            mean_type = "geometric", 
                            save_text = NA){
   
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
      
      rmarkdown::render(
         system.file("rmarkdown/templates/draftsections/skeleton/skeleton.Rmd", 
                     package="SimcypConsultancy"),
         output_dir = OutPath, 
         output_file = FileName, 
         quiet = TRUE)
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
   }
   
   return(DraftText)
   
}

