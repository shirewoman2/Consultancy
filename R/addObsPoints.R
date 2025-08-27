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
   
   obs_color <- obs_color[1:length(levels(obs_dataframe$colorBy_column))]
   names(obs_color) <- levels(obs_dataframe$colorBy_column)
   
   obs_shape <- obs_shape[1:length(levels(obs_dataframe$linetype_column))]
   names(obs_shape) <- levels(obs_dataframe$linetype_column)
   
   if(all(obs_shape %in% 21:25)){
      # solid fill w/black outline
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
                    show.legend = LegCheck) + 
         scale_fill_manual(values = obs_color) 
      
   } else {
      # any other point shape but solid fill w/black outline
      A <- A + 
         geom_point(data = obs_dataframe,
                    aes(shape = linetype_column,
                        color = colorBy_column),
                    alpha = obs_fill_trans,
                    size = obs_size,
                    show.legend = LegCheck)
   }
   
   A <- A  +
      scale_color_manual(values = obs_color) + 
      scale_shape_manual(values = obs_shape) +
      labs(fill = as.character(AESCols["color"]), 
           shape = as.character(AESCols["linetype"]))
   
   return(A)
   
}


