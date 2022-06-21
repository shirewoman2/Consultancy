#' Calculate the time difference between two times formatted as in simulator
#' output
#'
#' @param time1 the earlier time in the simulator output, e.g., "Day 1, 09:00"
#' @param time2 the later time in the simulator output, e.g., "Day 2, 09:00"
#' @param units units for the time. Options are "hours" (default), "minutes", or
#'   "days".
#'
#' @return Returns a number
#' @export
#'
#' @examples
#' difftime_sim(time1 = "Day 1, 09:00", time2 = "Day 14, 21:00")
#' difftime_sim(time1 = "Day 1, 09:00", time2 = "Day 1, 21:00",
#'              units = "minutes")
#' difftime_sim(time1 = "Day 1, 09:00", time2 = "Day 42, 21:00",
#'              units = "days")
#'
difftime_sim <- function(time1, time2, units = "hours"){
    
    Days_t1 <- as.numeric(sub("Day ", "",
                              stringr::str_extract(time1, "Day [0-9]{1,3}"))) - 1
    Hrs_t1 <- as.numeric(sub(":[0-9]{2}", "",
                             stringr::str_extract(time1, "[0-9]{2}:[0-9]{2}")))
    Mins_t1 <- as.numeric(sub("[0-9]{2}:", "",
                              stringr::str_extract(time1, "[0-9]{2}:[0-9]{2}")))
    
    Days_t2 <- as.numeric(sub("Day ", "",
                              stringr::str_extract(time2, "Day [0-9]{1,3}"))) - 1
    Hrs_t2 <- as.numeric(sub(":[0-9]{2}", "",
                             stringr::str_extract(time2, "[0-9]{2}:[0-9]{2}")))
    Mins_t2 <- as.numeric(sub("[0-9]{2}:", "",
                              stringr::str_extract(time2, "[0-9]{2}:[0-9]{2}")))
    
    Out_hr <- (Days_t2 - Days_t1) * 24 + (Hrs_t2 - Hrs_t1) +
        (Mins_t2 - Mins_t1)/60
    
    Out <- Out_hr * switch(units,
                           "hours" = 1,
                           "minutes" = 60,
                           "days" = 1/24)
    
    return(Out)
    
}


