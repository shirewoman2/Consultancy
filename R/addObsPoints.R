#' INTERNAL PACKAGE USE: Add observed data points to a concentration-time plot
#'
#' \code{addObsPoints} adds observed data to \code{\link{ct_plot}} or
#' \code{\link{ct_plot_overlay}} and sets the color and shape of those points
#' appropriately. This is NOT meant to be used as a stand-alone function.
#'
#' @param obs_dataframe observed data as a data.frame
#' @param A the existing ggplot2 graph to which the observed data will be added
#' @param LegCheck TRUE or FALSE for whether to include the legend. From
#'   ct_plot_overlay originally and should always be TRUE for ct_plot.
#' @param connect_obs_points TRUE or FALSE (default) for whether to add
#'   connecting lines between observed data points from the same individual
#' @param obs_shape shape
#' @param obs_size size
#' @param obs_color color
#' @param obs_line_trans line transparency
#' @param obs_fill_trans fill transparency
#' @param figure_type figure type
#' @param line_width line width
#' @param AESCols which aesthetics are mapped to which columns
#' @param line_type line types used for simulated data b/c obs shape needs to be
#'   named the same for ct_plot legends
#' @param line_color line colors used for simulated data b/c obs color needs to
#'   be named the same for ct_plot legends
#'
#' @return a ggplot2 layer with observed data

addObsPoints <- function(obs_dataframe, 
                         A, 
                         figure_type,
                         AESCols, 
                         obs_shape,
                         line_type, 
                         obs_color,
                         line_color,
                         obs_size, 
                         obs_line_trans,
                         obs_fill_trans, 
                         connect_obs_points,
                         line_width, 
                         LegCheck){
   
   # NB: colorBy_column and linetype_column MUST BE factor by this point.
   
   # Dealing with idiosyncracies of ribbon figure type
   if(str_detect(figure_type, "ribbon")){
      obs_dataframe <- obs_dataframe %>% 
         mutate(MyMean = Conc, 
                per5 = Conc, 
                per95 = Conc)
   }
   
   obs_size <- ifelse(is.na(obs_size), 2, obs_size)
   
   if(connect_obs_points){
      A <- A +
         geom_line(data = obs_dataframe,
                   alpha = obs_line_trans,
                   show.legend = FALSE, 
                   linewidth = ifelse(is.na(line_width), 0.5, line_width * 0.5))
   }
   
   # The set_aesthet function made sure that all the shapes are either 21:25
   # (where you can map fill) or be any shape except those (color is mapped
   # instead). I just could NOT get this to work with a mix where you'd need to
   # sometimes map fill and sometimes map color.
   
   if("colorBy_column" %in% names(obs_dataframe) &&
      "factor" %in% class(obs_dataframe$colorBy_column)){
      obs_color <- rep(obs_color, length(levels(obs_dataframe$colorBy_column)))[
         1:length(levels(obs_dataframe$colorBy_column))]
      names(obs_color) <- levels(obs_dataframe$colorBy_column)
   }
   
   if("linetype_column" %in% names(obs_dataframe) &&
      "factor" %in% class(obs_dataframe$linetype_column)){
      obs_shape <- rep(obs_shape, length(levels(obs_dataframe$linetype_column)))[
         1:length(levels(obs_dataframe$linetype_column))]
      names(obs_shape) <- levels(obs_dataframe$linetype_column)
   }
   
   
   if(all(c(AESCols["color"], AESCols["linetype"]) != "<empty>")){
      
      # Scenario: linetype and color are mapped ---------------------------------
      
      if(all(obs_shape %in% 21:25)){
         # solid fill w/black outline
         
         if(all(obs_color == line_color) | 
            figure_type == "compound summary"){
            A <- A + 
               # filled shapes added here
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column,
                              fill = colorBy_column),
                          alpha = obs_fill_trans,
                          # color = NA,
                          size = obs_size,
                          show.legend = LegCheck) +
               
               # only black outlines added here
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column),
                          alpha = obs_line_trans,
                          size = obs_size,
                          fill = NA,
                          color = "black",
                          show.legend = LegCheck)
            
         } else {
            # e.g., Freddy figure types or other instances where all the
            # observed points should have the same single color
            A <- A + 
               # filled shapes added here
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column),
                          fill = unique(obs_color), 
                          alpha = obs_fill_trans,
                          # color = NA,
                          size = obs_size,
                          show.legend = LegCheck) +
               
               # only black outlines added here
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column),
                          alpha = obs_line_trans,
                          size = obs_size,
                          fill = NA,
                          color = "black",
                          show.legend = LegCheck)
         }
         
      } else {
         # any other point shape other than solid fill w/black outline
         if(all(obs_color == line_color) | 
            figure_type == "compound summary"){
            A <- A + 
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column,
                              color = colorBy_column),
                          alpha = obs_fill_trans,
                          size = obs_size,
                          show.legend = LegCheck) 
            
         } else {
            # e.g., Freddy figure types or other instances where all the
            # observed points should have the same single color
            A <- A + 
               geom_point(data = obs_dataframe,
                          aes(shape = linetype_column),
                          color = unique(obs_color), 
                          alpha = obs_fill_trans,
                          size = obs_size,
                          show.legend = LegCheck) 
         }
      }
      
      A <- A  +
         labs(fill = as.character(AESCols["color"]), 
              shape = as.character(AESCols["linetype"]))
      
      return(A)
      
   } else if(AESCols["color"] =="<empty>" &
             AESCols["linetype"] != "<empty>" & length(unique(obs_shape)) != 1){
      
      # Scenario: only linetype is mapped ---------------------------------------
      
      if(all(obs_shape %in% 21:25)){
         A <- A + 
            # filled shapes added here
            geom_point(data = obs_dataframe,
                       aes(shape = linetype_column),
                       alpha = obs_fill_trans,
                       fill = unique(obs_color), 
                       size = obs_size,
                       show.legend = LegCheck) +
            
            # only black outlines added here
            geom_point(data = obs_dataframe,
                       aes(shape = linetype_column),
                       alpha = obs_line_trans,
                       size = obs_size,
                       fill = NA,
                       color = "black",
                       show.legend = LegCheck)
         
      } else {
         # any other point shape other than solid fill w/black outline
         A <- A + 
            geom_point(data = obs_dataframe,
                       aes(shape = linetype_column),
                       color = unique(obs_color), 
                       alpha = obs_fill_trans,
                       size = obs_size,
                       show.legend = LegCheck) 
      }
      
      A <- A  +
         labs(shape = as.character(AESCols["linetype"]))
      
      return(A)
      
   } else if(AESCols["color"] !="<empty>" & length(unique(obs_color)) != 1 &
             AESCols["linetype"] == "<empty>"){
      
      # Scenario: only color is mapped -------------------------------------------
      
      if(all(obs_shape %in% 21:25)){
         A <- A + 
            # filled shapes added here
            geom_point(data = obs_dataframe,
                       aes(fill = colorBy_column),
                       alpha = obs_fill_trans,
                       shape = unique(obs_shape), 
                       size = obs_size,
                       show.legend = LegCheck) +
            
            # only black outlines added here
            geom_point(data = obs_dataframe,
                       fill = NA,
                       color = "black",
                       alpha = obs_line_trans,
                       shape = unique(obs_shape), 
                       size = obs_size,
                       show.legend = LegCheck)
         
      } else {
         # any other point shape other than solid fill w/black outline
         A <- A + 
            geom_point(data = obs_dataframe,
                       aes(color = colorBy_column),
                       shape = unique(obs_shape), 
                       alpha = obs_fill_trans,
                       size = obs_size,
                       show.legend = LegCheck) 
      }
      
      A <- A  +
         labs(fill = as.character(AESCols["color"]))
      
      return(A)
      
   } else {
      
      # No color or shape mapping --------------------------------------------
      
      # Really, this is when neither color nor linetype are mapped or when color
      # is mapped but obs_color has length 1 (same for all datasets) or when
      # linetype (really, shape) is mapped but obs_shape has length 1 (same for
      # all datasets).
      
      if(all(obs_shape %in% 21:25)){
         A <- A + 
            # filled shapes added here
            geom_point(data = obs_dataframe,
                       fill = unique(obs_color),
                       alpha = obs_fill_trans,
                       shape = unique(obs_shape), 
                       size = obs_size,
                       show.legend = LegCheck) +
            
            # only black outlines added here
            geom_point(data = obs_dataframe,
                       fill = NA,
                       color = "black",
                       alpha = obs_line_trans,
                       shape = unique(obs_shape), 
                       size = obs_size,
                       show.legend = LegCheck)
         
      } else {
         # any other point shape other than solid fill w/black outline
         A <- A + 
            geom_point(data = obs_dataframe,
                       color = unique(obs_color), 
                       shape = unique(obs_shape), 
                       alpha = obs_fill_trans,
                       size = obs_size,
                       show.legend = LegCheck) 
      }
      
      return(A)
      
   }
   
}


