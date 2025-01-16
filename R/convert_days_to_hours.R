#' Convert times in days to times in hours 
#'
#' @param days the days you want to convert to hours, which can be either a
#'   character or numeric vector. This can include "day" or "days" and can be
#'   from one day to a different day. For example, all of these are acceptable:
#'   "day 1", "2", "days 5-6", "day 3 to day 4".
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' convert_days_to_hours("day 1")
#' # Note that this will give you the full 24-hour period, so "day 1" will 
#' # give you the numeric vector of 0 and 24. 
#'
#' # You don't need to include the "day" bit.
#' convert_days_to_hours(2)
#'
#' # If you include days in a format anything like "day X to day Y" or
#' # "day X-Y", we'll give you a numeric vector that includes the full last day.
#' convert_days_to_hours("day 3 to 4")
#'
#' 
convert_days_to_hours <- function(days){
   
   if(length(days) != 1){
      warning(wrapn("You have supplied more than 1 item for converting days into hours, and we can only accept 1 item. We'll only use the 1st item provided."), 
              call. = FALSE)
   }
   
   days <- gsub("day(s)?( )", "", tolower(days[1]))
   days <- as.numeric(str_split_1(days, pattern = "( )?to( )?|( )?-( )?"))
   
   if(length(days) > 2){
      warning(wrapn("You have supplied something other than 'day X' or 'day X to day Y', which is all this function can handle. We'll only use the 1st two days you provided."), 
              call. = FALSE)
      days <- days[1:2]
   }
   
   if(length(days) == 1){
      days <- c(days, days + 1)
   } else {
      days[2] <- days[2] + 1
   }
   
   hours <- (days - 1) * 24
   
   return(hours)
   
}

