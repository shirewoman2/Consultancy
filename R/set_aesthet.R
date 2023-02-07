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
#' @return several objects to use with setting graph aesthetics

set_aesthet <- function(line_type, figure_type, MyEffector, compoundToExtract, 
                        obs_shape, line_color, obs_color, 
                        obs_line_trans, obs_fill_trans){
    
    # Setting user specifications for shape, linetype, and color where
    # applicable.
    if(is.na(line_type[1])){
        if(str_detect(figure_type, "ribbon") & 
           length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
           MyEffector[1] != "none" & 
           ("inhibitor 1" %in% compoundToExtract == FALSE)){
            line_type <- c("solid", "solid")
        } else {
            line_type <- c("solid", "dashed")
        }
    }
    
    if(is.na(obs_shape[1])){
        obs_shape <- c(1, 2)
    }
    
    if(is.na(line_color[1])){
        if(str_detect(figure_type, "ribbon") & 
           length(MyEffector) > 0 && complete.cases(MyEffector[1]) &&
           MyEffector[1] != "none" & 
           ("inhibitor 1" %in% compoundToExtract == FALSE)){
            line_color <- c("#377EB8", "#E41A1C")
        } else {
            line_color <- c("black", "black")
        }
    }
    
    if(complete.cases(line_color[1]) && figure_type == "freddy" &&
       length(line_color) == 1){
        line_color <- rep(line_color, 2)
    }
    
    if(length(obs_color) == 1 &&
       (complete.cases(obs_color[1]) & obs_color == "default") |
       (is.na(obs_color[1]) & figure_type == "freddy")){
        obs_color <- "#3030FE"
    } else {
        obs_color = "black"
    }
    
    obs_fill_trans <- ifelse(is.na(obs_fill_trans), 
                             0.5, obs_fill_trans)
    
    obs_line_trans <- ifelse(is.na(obs_line_trans), 
                             1, obs_line_trans)
    
    # Assigning the variables created or changed here to the environment one
    # level up, e.g., probably the environment within the function that's
    # calling on *this* function.
    assign("line_type", line_type, envir = parent.frame())
    assign("line_color", line_color, envir = parent.frame())
    assign("obs_shape", obs_shape, envir = parent.frame())
    assign("obs_color", obs_color, envir = parent.frame())
    assign("obs_fill_trans", obs_fill_trans, envir = parent.frame())
    assign("obs_line_trans", obs_line_trans, envir = parent.frame())
    
}
