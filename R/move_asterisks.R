#' INTERNAL - Move asterisks around so that you can put asterisks around that
#' string and things will be italicized appropriately
#'
#' @description \code{move_asterisks} finds all locations of an asterisk plus a
#'   space or a space plus an asterisk and swaps the positions of the spaces and
#'   the asterisks. It also makes sure that your string doesn't end in two
#'   asterisks by removing the terminal asterisk.
#'
#'   \strong{How to use this:} Say you've got a string that is a figure caption
#'   with two italicized references included inside the string. If you try to
#'   put the whole caption in italics in a .Rmd file with \code{*`r
#'   MyCaption`*}, the italics will be messed up and you will likely have an
#'   extra asterisk at the end of the final output in the knitted document. This
#'   fixes that.
#'
#' @param string a string of text, e.g., a figure or table caption for use in an
#'   Rmarkdown document that will be italicized
#' @param num_asterisks_to_match number of asterisks to match and move. If left as NA,
#'   this will move sets of 3, 2, and then 1 asterisks. If set to 3, 2, or 1,
#'   though, it will \emph{only} look for that number to match and move. 
#'
#' @return a string
#' @export
#' 
move_asterisks <- function(string, 
                           num_asterisks_to_match = NA){
   
   # Error catching and setting up --------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   
   # Main body of function -------------------------------------------------
   
   move_subfun <- function(string, 
                           numstars = 1, 
                           ignore = NA){
      
      starstring <- case_match(numstars, 
                               1 ~ "\\*", 
                               2 ~ "\\*\\*", 
                               3 ~ "\\*\\*\\*")
      starstring_noesc <- case_match(numstars, 
                                     1 ~ "*", 
                                     2 ~ "**", 
                                     3 ~ "***")
      
      # Check for more than 1 set of asterisks
      AsteriskLocs <- str_locate_all(string, starstring)[[1]][, 1]
      NumAsterisks <- length(AsteriskLocs)
      
      # Pass through if there are no asterisks
      if(str_detect(string, starstring) == FALSE){
         return(string)
      }
      
      if(NumAsterisks %% 2 != 0){
         warning(wrapn("You have an uneven number of asterisks, so we don't know what you're trying to italicize and can't move your asterisks around."), 
                 call. = FALSE)
         return(string)
      } 
      
      # NB: ORDER IS IMPORTANT! Find all locations and THEN replace. 
      AsteriskLocs_space_star <- 
         str_locate_all(string, paste0(" ", starstring))[[1]] %>%
         as.data.frame() %>% 
         filter(complete.cases(start))
      space_star_check <- map2(.x = AsteriskLocs_space_star$start,
                               .y = AsteriskLocs_space_star$end, 
                               .f = `:`) %>% 
         map(.x = ., .f = \(x) any(x %in% ignore)) %>% 
         unlist()
      AsteriskLocs_space_star <- 
         AsteriskLocs_space_star[which(space_star_check == FALSE), ]
      
      AsteriskLocs_star_space <-
         str_locate_all(string, paste0(starstring, " "))[[1]] %>% 
         as.data.frame() %>% 
         filter(complete.cases(start))
      star_space_check <- map2(.x = AsteriskLocs_star_space$start,
                               .y = AsteriskLocs_star_space$end, 
                               .f = `:`) %>% 
         map(.x = ., .f = \(x) any(x %in% ignore)) %>% 
         unlist()
      AsteriskLocs_star_space <- 
         AsteriskLocs_star_space[which(star_space_check == FALSE), ]
      
      if(nrow(AsteriskLocs_space_star) > 0){
         for(i in 1:nrow(AsteriskLocs_space_star)){
            str_sub(string, 
                    start = AsteriskLocs_space_star$start[i], 
                    end = AsteriskLocs_space_star$end[i]) <- 
               paste0(starstring_noesc, " ")
         }
      }
      
      if(nrow(AsteriskLocs_star_space) > 0){
         for(i in 1:nrow(AsteriskLocs_star_space)){
            str_sub(string, 
                    start = AsteriskLocs_star_space$start[i], 
                    end = AsteriskLocs_star_space$end[i]) <- 
               paste0(" ", starstring_noesc)
         }
      }
      
      # Remove any * at the end to avoid having a ** in the .Rmd 
      TerminalAsterisk <- str_locate(string, paste0(starstring, "$"))
      if(complete.cases(TerminalAsterisk[1, 1])){
         str_sub(string, 
                 start = TerminalAsterisk[, 1], 
                 end = TerminalAsterisk[, 2]) <- ""
      }
      
      return(string)
   }
   
   for(j in 1:length(string)){
      
      # ORDER IS IMPORTANT. Need to find locations of all pairs of 3, 2, and then 1
      # asterisks and THEN replace them as sets.
      
      star3_loc <- str_locate_all(string[j], "\\*\\*\\*")[[1]] %>% 
         as.data.frame()
      star3_loc_v <- map2(.x = star3_loc$start,
                          .y = star3_loc$end, 
                          .f = `:`) %>% 
         unlist()
      
      star2_loc <- str_locate_all(string[j], "\\*\\*")[[1]] %>% 
         as.data.frame() %>% 
         filter(!start %in% star3_loc_v)
      star2_loc_v <- map2(.x = star2_loc$start,
                          .y = star2_loc$end, 
                          .f = `:`) %>% 
         unlist()
      
      star1_loc <- str_locate_all(string[j], "\\*")[[1]] %>% 
         as.data.frame() %>% 
         filter(!start %in% c(star3_loc_v, star2_loc_v))
      
      if(is.na(num_asterisks_to_match)){
         num_asterisks_to_match <- 3:1
      }
      
      for(k in num_asterisks_to_match){
         string[j] <- move_subfun(string[j], 
                                  numstars = k, 
                                  ignore = switch(as.character(k), 
                                                  "3" = NA, 
                                                  "2" = star3_loc_v, 
                                                  "1" = c(star3_loc_v, 
                                                          star2_loc_v)))
      }
   }
   
   return(string)
   
}


