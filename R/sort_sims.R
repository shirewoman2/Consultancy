#' Sort the simulation file order in an object
#'
#' @param which_object the R object for which you want to sort simulation files
#' @param sim_order what should the simulation file order be? Must be a
#'   character vector of the file names \emph{exactly} as they appear in the
#'   column "File" if the object is a data.frame or in the data.frames inside
#'   the list if the object is a list. Note that this means that "File" must be
#'   a column in the data.frames. If it is not, the object will be returned
#'   without any sorting. If any simulation files were omitted from
#'   \code{sort_order}, they will be included at the end in the same order they
#'   originally appeared.
#'
#' @return the R object with all simulations sorted.
#' @export
#'
#' @examples
#' Details <- sort_sims(which_object = Details,
#'                      sim_order = c("file 1.xlsx", "file 2.xlsx"))
#'
#' 
sort_sims <- function(which_object, 
                      sim_order){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   # Dealing w/possible mess-ups in specifying sim_order
   sim_order <- sub("\\.wksz", ".xlsx", sim_order)
   if(any(str_detect(sim_order, "\\.xlsx")) == FALSE){
      sim_order <- paste0(sim_order, ".xlsx")
   }
   sim_order <- sub("\\.xlsx\\.xlsx", ".xlsx", sim_order)
   
   
   # Main body of function ----------------------------------------------------
   
   if("list" %in% class(which_object)){
      # This is when it's probably the output from running extractExpDetails_mult.
      for(i in names(which_object)){
         
         if(length(which_object[[i]]) == 0){ next }
         
         if("File" %in% names(which_object[[i]]) == FALSE){ next }
         
         if(ncol(which_object[[i]]) > 0){
            
            # Need to account for any missing or replicated sim files.
            sim_order_temp <- union(intersect(sim_order,
                                              unique(which_object[[i]]$File)), 
                                    unique(which_object[[i]]$File))
            
            which_object[[i]] <- which_object[[i]] %>% 
               mutate(File = factor(File, levels = sim_order_temp)) %>% 
               arrange(File) %>% 
               mutate(File = as.character(File))
            
            rm(sim_order_temp)
         }
      }
   } else {
      
      if(length(which_object) == 0){ return(which_object) }
      
      if("File" %in% names(which_object) == FALSE){ return(which_object) }
      
      # Need to account for any missing or replicated sim files.
      sim_order_temp <- union(intersect(sim_order,
                                        unique(which_object$File)), 
                              unique(which_object$File))
      
      which_object <- which_object %>%
         mutate(File = factor(File, levels = sim_order_temp)) %>% 
         arrange(File) %>% 
         mutate(File = as.character(File))
   }
   
   return(which_object)
}


