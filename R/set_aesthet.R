#' Set up certain aesthetics a conc-time plot
#'
#' This function is specifically for setting certain aesthetic options for a
#' concentration-time graph and is NOT meant to be called on its own.
#'
#'
#' @param line_type user-specified line type
#' @param figure_type user-specified figure type
#' @param MyEffector concatenated string of all effectors involved
#' @param compoundToExtract compound user wants
#' @param obs_shape user-specified observed data shape
#' @param line_color user-specified line color 
#' @param obs_color user-specified observed-data color
#'
#' @return

set_aesthet <- function(line_type, figure_type, MyEffector, compoundToExtract, 
                        obs_shape, line_color, obs_color){
    
    # Setting user specifications for shape, linetype, and color where
    # applicable.
    if(is.na(line_type[1])){
        if(str_detect(figure_type, "ribbon") & 
           length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
           MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){
            line_type <- c("solid", "solid")
        } else {
            line_type <- c("solid", "dashed")
        }
    }
    
    if(is.na(obs_shape[1])){
        obs_shape <- c(21, 24)
    }
    
    if(complete.cases(line_color[1]) & is.na(obs_color[1])){
        obs_color <- line_color
    }
    
    if(is.na(line_color[1])){
        if(str_detect(figure_type, "ribbon") & 
           length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
           MyEffector[1] != "none" & compoundToExtract != "inhibitor 1"){
            line_color <- c("#377EB8", "#E41A1C")
        } else {
            line_color <- c("black", "black")
        }
    }
    
    if(complete.cases(line_color[1]) && figure_type == "Freddy" &&
       length(line_color) == 1){
        line_color <- rep(line_color, 2)
    }
    
    if(length(obs_color) == 1 &&
       (complete.cases(obs_color[1]) & obs_color == "default") |
       (is.na(obs_color[1]) & figure_type == "Freddy")){
        obs_color <- "#3030FE"
    }    
    
    # Assigning the variables created or changed here to the environment one
    # level up, e.g., probably the environment within the function that's
    # calling on *this* function.
    assign("line_type", line_type, envir = parent.frame())
    assign("line_color", line_color, envir = parent.frame())
    assign("obs_shape", obs_shape, envir = parent.frame())
    assign("obs_color", obs_color, envir = parent.frame())
    
}
