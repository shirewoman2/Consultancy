#' Add simulations to an R object
#'
#' @description \code{add_sims} was designed with the idea of adding togethet
#'   information from two objects that were created from two separate calls to
#'   \code{\link{extractExpDetails_mult}}, but you can also use it with other R
#'   objects.
#'
#'   For example, say you ran \code{\link{extractExpDetails_mult}} with some
#'   initial development simulations and then, later, you ran it with some other
#'   development simulations another time and now, you want both of those sets
#'   of results be combined, maybe because you want to run
#'   \code{\link{annotateDetails}} to compare them. Run \code{add_sims} on these
#'   two objects to combine them into a single set of details with the standard
#'   structure for objects created by \code{\link{extractExpDetails_mult}}.
#'   Similarly, you can run this from an object created from one call to
#'   \code{\link{extractConcTime}} and a second object created the same way to
#'   obtain a single object with the same structure as if you'd gotten all the
#'   data extracted at once.
#'
#' @param object_A one R object
#' @param object_B a second R object whose results you want to combine with the
#'   first R object. Both object_A and object_B must have the same data class,
#'   i.e., both must be lists or both data.frames.
#'
#' @return the combined R object
#' @export
#'
#' @examples
#' Details <- add_sims(object_A = Details_dev,
#'                     object_B = Details_final)
#'
#' 
add_sims <- function(object_A, 
                     object_B){
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   if(("list" %in% class(object_A) & 
       "list" %in% class(object_B) == FALSE) |
      ("data.frame" %in% class(object_A) & 
       "data.frame" %in% class(object_B) == FALSE)){
      stop(wrapn("Both object_A and object_B must be lists or both must be data.frames, and you've got a mix."), 
           call. = FALSE)
   }
   
   if("list" %in% class(object_A)){
      # This is when it's probably the output from running extractExpDetails_mult.
      if(all(c(names(object_A), names(object_B)) %in% ExpDetailListItems) == FALSE){
         stop(wrapn("When the object_A and object_B are lists, they must both be output from running extractExpDetails_mult, and it appears that something is not."), 
              call. = FALSE)
      }
      
      for(i in names(object_A)){
         
         ClassesA <- sapply(object_A[[i]], class)
         ClassesB <- sapply(object_B[[i]], class)
         
         ClassesA <- data.frame(Column = names(ClassesA), 
                                ClassA = as.character(ClassesA))
         ClassesB <- data.frame(Column = names(ClassesB), 
                                ClassB = as.character(ClassesB))
         Classes <- full_join(ClassesA, ClassesB, by = "Column") %>% 
            mutate(ClassA = ifelse(ClassA == "logical" & 
                                      ClassB != "logical", 
                                   ClassB, ClassA), 
                   ClassB = ifelse(ClassB == "logical" & 
                                      ClassA != "logical", 
                                   ClassA, ClassB)) %>% 
            filter(ClassA != ClassB)
         
         object_A[[i]] <- object_A[[i]] %>% 
            mutate(across(.cols = any_of(Classes$Column), 
                          .fns = as.character))
         
         object_B[[i]] <- object_B[[i]] %>% 
            mutate(across(.cols = any_of(Classes$Column), 
                          .fns = as.character))
         
         object_A[[i]] <- bind_rows(object_A[[i]], 
                                    object_B[[i]])
      }
      
   } else {
      object_A <- object_A %>% bind_rows(object_B)
   }
   
   return(object_A)
}


