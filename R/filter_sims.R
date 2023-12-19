#' Selectively include or omit specific simulations from an R object
#'
#' @param which_object the R object for which you want to filter to remove or
#'   retain only certain simulation files
#' @param which_sims which simulations are affected? Must be a character vector
#'   of the file names \emph{exactly} as they appear in the column "File" if the
#'   object is a data.frame or in the data.frames inside the list if the object
#'   is a list
#' @param include_or_omit Do you want to "include" or "omit" these simulations?
#'
#' @return the R object with only those simulations if \code{include_or_omit} is
#'   set to "include" or that R object with those simulations omitted if
#'   \code{include_or_omit} is set to "omit".
#' @export
#'
#' @examples
#' Details <- filter_sims(which_object = Details, 
#'                        which_sims = c("file 1.xlsx", "file 2.xlsx"), 
#'                        include_or_omit = "omit")
#' 
#' 
filter_sims <- function(which_object, 
                        which_sims, 
                        include_or_omit){
   
   if("list" %in% class(which_object)){
      # This is when it's probably the output from running extractExpDetails_mult.
      for(i in names(which_object)){
         
         if(ncol(which_object[[i]]) > 0){
            which_object[[i]] <- which_object[[i]] %>% 
               filter(File %in% which_sims == switch(include_or_omit, 
                                                     "include" = TRUE, 
                                                     "omit" = FALSE))
         }
      }
   } else {
      which_object <- which_object %>%
         filter(File %in% which_sims == switch(include_or_omit, 
                                               "include" = TRUE, 
                                               "omit" = FALSE))
   }
   
   return(which_object)
}


