#' INTERNAL PACKAGE USE: Add observed data points to a concentration-time plot
#'
#' \code{addObsPoints} adds observed data to \code{\link{ct_plot}} or
#' \code{\link{ct_plot_overlay}} and sets the color and shape of those points
#' appropriately. This is NOT meant to be used as a stand-alone function.
#'
#' @param obs_dataframe observed data as a data.frame
#' @param A the existing ggplot2 graph to which the observed data will be added
#' @param map_obs_color TRUE or FALSE for whether to map the observed data color
#'   to specific columns.
#' @param map_obs_shape TRUE or FALSE for whether to map the observed data shape
#'   to specific columns.
#' @param LegCheck TRUE or FALSE for whether to include the legend. From
#'   ct_plot_overlay originally and should always be TRUE for ct_plot.
#' @param connect_obs_points TRUE or FALSE (default) for whether to add
#'   connecting lines between observed data points from the same individual
#' @param AES aesthetics
#' @param obs_shape shape
#' @param obs_shape_user original input for shape
#' @param obs_size size
#' @param obs_color color
#' @param obs_color_user original input for color
#' @param obs_line_trans line transparency
#' @param obs_line_trans_user original input for line transparency
#' @param obs_fill_trans fill transparency
#' @param obs_fill_trans_user original input for fill transparency
#' @param figure_type figure type
#' @param line_width line width
#'
#' @return a ggplot2 layer with observed data

addObsPoints <- function(obs_dataframe, 
                         A, 
                         AES,
                         obs_shape,
                         obs_shape_user,
                         obs_size, 
                         obs_color,
                         obs_color_user,
                         obs_line_trans,
                         obs_line_trans_user,
                         obs_fill_trans, 
                         obs_fill_trans_user, 
                         figure_type,
                         map_obs_color,
                         map_obs_shape,
                         connect_obs_points,
                         line_width, 
                         LegCheck){
   
   # Some notes on how things are set up by the time this is called:
   
   # For figure_type of compound summary, main aes have NOT mapped anything to
   # color. For all other figure types from ct_plot function, the color has been
   # mapped to Inhibitor column.
   
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
   # (where you can map fill) or be any shape but those (color is mapped
   # instead). I just could NOT get this to work with a mix.
   
   # Outline only --> Use obs_color to determine outline color. This also
   # applies to nonstandard shapes s/a if they want a letter for the point
   # shape.
   
   # Solid shapes --> use obs_color to determine fill color.
   
   # Outline plus solid --> use obs_color to determine fill color, but outline
   # color will be black.
   
   # Determining the number of shapes and colors required and specifying shape
   # values in obs_dataframe. Note that Inhibitor and Study columns are factor
   # at this point, as are any other columns that observed data aesthetics are
   # mapped to.
   
   # FIXME: I think I need to just have a single column that I have mapped to
   # color/fill and just use that here. That would work for both ct_plot (any
   # figure type) and ct_plot_overlay (where they could have mapped ANYTHING to
   # color/fill).
   
   if(figure_type == "compound summary"){
      obs_color <- obs_color[1:length(levels(obs_dataframe$Study))]
      names(obs_color) <- levels(obs_dataframe$Study)
      
      if(any(obs_dataframe$Inhibitor != "none")){
         obs_shape <- obs_shape[1:2]
         names(obs_shape) <- levels(obs_dataframe$Inhibitor)
         
      } else {
         obs_shape <- obs_shape[1:length(levels(obs_dataframe$Study))]
         names(obs_shape) <- levels(obs_dataframe$Study)
      }
      
   } else {
      obs_color <- obs_color[1:length(levels(obs_dataframe$Inhibitor))]
      names(obs_color) <- levels(obs_dataframe$Inhibitor)
      
      obs_shape <- obs_shape[1:length(levels(obs_dataframe$Inhibitor))]
      names(obs_shape) <- levels(obs_dataframe$Inhibitor)
      
      # Assigning shape and color to data 
      obs_dataframe <- obs_dataframe %>% 
         mutate(Shape = obs_shape[as.character(Inhibitor)],
                Color = obs_color[as.character(Inhibitor)],
                OutlineColor = case_when(Shape %in% 21:25 ~ "black",
                                         Shape %in% 15:20 ~ NA,
                                         .default = Color),
                FillColor = case_when(Shape %in% 21:25 ~ Color,
                                      Shape %in% 15:20 ~ Color,
                                      .default = NA))
      
      ObsMap <- obs_dataframe %>% 
         select(Inhibitor, Shape, Color, OutlineColor, FillColor) %>%
         unique() 
      
      obs_outline_color <- ObsMap$OutlineColor
      names(obs_outline_color) <- ObsMap$Inhibitor
      
      obs_fill_color <- ObsMap$FillColor
      names(obs_fill_color) <- ObsMap$Inhibitor
      
   }
   
   # Adding 1) solids and then 2) outlines for all of these scenarios
   if(figure_type == "compound summary" & 
      any(obs_dataframe$Inhibitor != "none")){
      
      A <- A + 
         A <- A + 
            # filled shapes added here
            geom_point(data = obs_dataframe,
                       aes(shape = Inhibitor, 
                           fill = Study), 
                       alpha = obs_fill_trans,
                       size = obs_size,
                       # color = "purple",
                       show.legend = LegCheck) + 
            
            # only colored outlines added here
            geom_point(data = obs_dataframe %>%
                          filter(!Shape %in% 15:25),
                       aes(shape = Inhibitor,
                           color = Study),
                       alpha = obs_line_trans,
                       size = obs_size,
                       fill = NA,
                       show.legend = LegCheck) +
            
            # only black outlines added here
            geom_point(data = obs_dataframe %>%
                          filter(Shape %in% 21:25),
                       aes(shape = Inhibitor),
                       alpha = obs_line_trans,
                       size = obs_size,
                       fill = NA,
                       color = "black",
                       show.legend = LegCheck)
         
      
   } else if(figure_type == "compound summary" & 
             all(obs_dataframe$Inhibitor == "none")){
      
      A <- A + 
      # filled shapes added here (part 1)
      geom_point(data = obs_dataframe %>% 
                    filter(Shape %in% 15:20),
                 aes(shape = Study, 
                     color = Study), 
                 alpha = obs_fill_trans,
                 size = obs_size,
                 show.legend = LegCheck) + 
         
         # filled shapes added here (part 2)
         geom_point(data = obs_dataframe %>% 
                       filter(Shape %in% 21:25),
                    aes(shape = Study, 
                        fill = Study), 
                    alpha = obs_fill_trans,
                    size = obs_size,
                    show.legend = LegCheck) + 
         
         # only colored outlines added here
         geom_point(data = obs_dataframe %>%
                       filter(!Shape %in% 15:25),
                    aes(shape = Study,
                        color = Study),
                    alpha = obs_line_trans,
                    size = obs_size,
                    fill = NA,
                    show.legend = LegCheck) +
         
         # only black outlines added here
         geom_point(data = obs_dataframe %>%
                       filter(Shape %in% 21:25),
                    aes(shape = Study),
                    alpha = obs_line_trans,
                    size = obs_size,
                    fill = NA,
                    color = "black",
                    show.legend = LegCheck)
      
   } else {
      
      A <- A +
         # filled shapes added here
         geom_point(data = obs_dataframe,
                    aes(shape = Inhibitor, 
                        fill = Inhibitor), 
                    alpha = obs_fill_trans,
                    size = obs_size,
                    # color = "purple",
                    show.legend = LegCheck) + 
         
         # only colored outlines added here
         geom_point(data = obs_dataframe %>%
                       filter(!Shape %in% 15:25),
                    aes(shape = Inhibitor,
                        color = Inhibitor),
                    alpha = obs_line_trans,
                    size = obs_size,
                    fill = NA,
                    show.legend = LegCheck) +

         # only black outlines added here
         geom_point(data = obs_dataframe %>%
                       filter(Shape %in% 21:25),
                    aes(shape = Inhibitor),
                    alpha = obs_line_trans,
                    size = obs_size,
                    fill = NA,
                    color = "black",
                    show.legend = LegCheck)
      
   }
   
   A <- A +
      scale_shape_manual(values = obs_shape) +
      scale_fill_manual(values = obs_fill_color)
   
   return(A)
   
}


