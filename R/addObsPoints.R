#' INTERNAL PACKAGE USE: Add observed data points to a concentration-time plot
#'
#' \code{addObsPoints} adds observed data to \code{\link{ct_plot}} or
#' \code{\link{ct_plot_overlay}} and sets the color and shape of those points
#' appropriately. This is NOT meant to be used as a stand-alone function.
#'
#' @param obs_dataframe observed data as a data.frame
#' @param A the existing ggplot2 graph to which the observed data will be added
#' @param map_obs_color TRUE or FALSE for whether to map the observed data
#'   color to specific columns. 
#' @param map_obs_shape TRUE or FALSE for whether to map the observed data
#'   shape to specific columns. 
#' @param LegCheck TRUE or FALSE for whether to include the legend. From
#'   ct_plot_overlay originally and should always be TRUE for ct_plot.
#' @param connect_obs_points TRUE or FALSE (default) for whether to add
#'   connecting lines between observed data points from the same individual
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
                         # map_obs_color, 
                         connect_obs_points,
                         line_width, 
                         LegCheck){
   
   # Dealing with idiosyncracies of ribbon figure type
   if(str_detect(figure_type, "ribbon")){
      obs_dataframe <- obs_dataframe %>% 
         mutate(MyMean = Conc, 
                per5 = Conc, 
                per95 = Conc)
   }
   
   obs_size <- ifelse(is.na(obs_size), 2, obs_size)
   
   # Need to adjust some code based on whether the obs_shape was solid, outline
   # only, or a mix. 
   SolidShapes <- which(obs_shape %in% 15:20)
   OutlineSolid <- which(obs_shape %in% 21:25)
   OutlineShapes <- setdiff(1:length(obs_shape), c(SolidShapes, OutlineSolid))
   
   if(connect_obs_points){
      A <- A +
         geom_line(data = obs_dataframe,
                   alpha = obs_line_trans,
                   show.legend = FALSE, 
                   linewidth = ifelse(is.na(line_width), 0.5, line_width * 0.5))
   }
   
   # Plan: 
   
   # Outline only --> Use obs_color to determine outline color. 
   
   # SolidShapes --> use obs_color to determine fill color.
   
   # OutlineSolid --> use obs_color to determine fill color, but outline color
   # will be black.
   
   # MixShape --> Use the above options specifically for each of those shapes,
   # i.e., when it is a solid shape, use obs_color to determine fill color. When
   # it is an OutlineSolid shape, use obs_color to determine fill but make
   # outline balck. When it is an outline only or any other shape (b/c they
   # *could* use something nonstandard s/a a letter), then use obs_color to
   # determine outline color.
   
   # Really, since we're going to deal w/each shape type differently, add the
   # obs points in layers: 1) add any filled shapes, 2) add any outlines.
   
   # Determining the number of shapes required
   
   obs_color_4realsies <- X
   
   if(figure_type == "compound summary"){
      A <-  A +
         # making obs point outlines
         geom_point(data = obs_dataframe,
                    aes(x = Time, y = Conc, fill = Study, shape = Study), 
                    # inherit.aes = F, 
                    color = "black", 
                    alpha = obs_line_trans,
                    size = obs_size,
                    show.legend = LegCheck)
      
   } else if(str_detect(AES, "linetype")){
      A <- A +
         # making obs point outlines
         switch(as.character(map_obs_color),
                # "TRUE" is when there are multiple sets of
                # observed data that are mapped to color or
                # linetype, etc.
                "TRUE" = geom_point(data = obs_dataframe,
                                    alpha = obs_line_trans,
                                    fill = NA,
                                    size = obs_size,
                                    show.legend = LegCheck),
                # "FALSE" is when the user has specified what
                # color they want the observed data to be.
                "FALSE" = geom_point(data = obs_dataframe, 
                                     alpha = obs_line_trans, 
                                     color = "black", 
                                     fill = NA, 
                                     size = obs_size,
                                     show.legend = LegCheck)) +
         # making obs point fill
         switch(as.character(map_obs_color),
                "TRUE" =  geom_point(data = obs_dataframe,
                                     alpha = obs_fill_trans,
                                     size = obs_size,
                                     show.legend = LegCheck),
                "FALSE" = geom_point(data = obs_dataframe, 
                                     alpha = obs_fill_trans, 
                                     color = "black", 
                                     fill = obs_color, 
                                     size = obs_size,
                                     show.legend = LegCheck)) +
         scale_shape_manual(values = obs_shape) 
      
   } else {
      
      A <- A +
         # making obs point outlines
         switch(as.character(map_obs_color),
                # "TRUE" is when there are multiple sets of
                # observed data that are mapped to color or
                # linetype, etc.
                "TRUE" = geom_point(data = obs_dataframe,
                                    alpha = obs_line_trans,
                                    fill = NA,
                                    size = obs_size,
                                    show.legend = LegCheck, 
                                    shape = obs_shape[1]),
                # "FALSE" is when the user has specified what
                # color they want the observed data to be.
                "FALSE" = geom_point(data = obs_dataframe, 
                                     alpha = obs_line_trans, 
                                     color = "black", 
                                     fill = NA, 
                                     size = obs_size,
                                     show.legend = LegCheck, 
                                     shape = obs_shape[1])) +
         # making obs point fill
         switch(as.character(map_obs_color),
                "TRUE" =  geom_point(data = obs_dataframe,
                                     alpha = obs_fill_trans,
                                     size = obs_size,
                                     show.legend = LegCheck, 
                                     shape = obs_shape[1]),
                "FALSE" = geom_point(data = obs_dataframe, 
                                     alpha = obs_fill_trans, 
                                     color = "black", 
                                     fill = obs_color, 
                                     size = obs_size,
                                     show.legend = LegCheck, 
                                     shape = obs_shape[1]))   
   }
   
   # } else if(OutlineShapes){
   
   # If user specified only obs_fill_trans but they've got an
   # outline-only shape, then assume that they meant to specify
   # obs_line_trans instead.
   if(complete.cases(obs_fill_trans_user) &  is.na(obs_line_trans_user)){
      obs_line_trans <- obs_fill_trans
   }
   
   if(figure_type == "compound summary"){
      A <- A +
         # making obs point outlines
         geom_point(data = obs_dataframe,
                    aes(color = Study, shape = Study), 
                    alpha = obs_line_trans,
                    size = obs_size,
                    show.legend = LegCheck)
      
   } else if(str_detect(AES, "linetype")){
      A <- A +
         # making obs point outlines
         switch(as.character(map_obs_color),
                "TRUE" = geom_point(data = obs_dataframe,
                                    alpha = obs_line_trans,
                                    size = obs_size,
                                    show.legend = LegCheck), 
                "FALSE" = geom_point(data = obs_dataframe,
                                     alpha = obs_line_trans,
                                     color = obs_color,
                                     size = obs_size,
                                     fill = obs_color,
                                     show.legend = LegCheck)) +
         scale_shape_manual(values = obs_shape) 
   } else {
      A <- A +
         # making obs point outlines
         switch(as.character(map_obs_color),
                "TRUE" = geom_point(data = obs_dataframe,
                                    alpha = obs_line_trans,
                                    size = obs_size,
                                    show.legend = LegCheck, 
                                    shape = obs_shape[1]), 
                "FALSE" = geom_point(data = obs_dataframe,
                                     alpha = obs_line_trans,
                                     color = obs_color,
                                     size = obs_size,
                                     fill = obs_color,
                                     show.legend = LegCheck, 
                                     shape = obs_shape[1])) 
   }
   
   # } else {
   # This is when all shapes are solid only OR there is some mixture of
   # outline-only, solid-only, and outline-with-fill. In the latter
   # circumstance, there isn't a good way to control alpha for outline and
   # fill separately, so we'll use only obs_fill_trans to determine alpha.
   
   if(figure_type == "compound summary"){
      
      if(OutlineSolid){
         suppressMessages(
            A <- A +
               # Adding outlines b/c otherwise they will be whatever
               # transparency obs_fill_trans is.
               geom_point(data = obs_dataframe,
                          aes(shape = Study),
                          color = "black",
                          fill = NA,
                          alpha = obs_line_trans,
                          size = obs_size,
                          show.legend = LegCheck) +
               geom_point(data = obs_dataframe,
                          aes(shape = Study, fill = Study), 
                          alpha = obs_fill_trans,
                          size = obs_size,
                          show.legend = LegCheck) + 
               scale_shape_manual(values = obs_shape) +
               scale_fill_manual(values = obs_color)
         )
         
      } else {
         
         suppressMessages(
            A <- A +
               # making obs point outlines
               geom_point(data = obs_dataframe,
                          aes(shape = Study, 
                              fill = Study, 
                              color = Study), 
                          alpha = obs_fill_trans,
                          size = obs_size,
                          show.legend = LegCheck) +
               scale_color_manual(values = obs_color) +
               scale_shape_manual(values = obs_shape) +
               scale_fill_manual(values = obs_color)
         )
      }
      
   } else if(str_detect(AES, "linetype")){
      
      if(OutlineSolid){
         if(map_obs_color){
            A <- A +
               # Adding outlines b/c otherwise they will be whatever
               # transparency obs_fill_trans is.
               geom_point(data = obs_dataframe,
                          aes(shape = Inhibitor), 
                          # FIXME
                          alpha = obs_line_trans,
                          color = "black",
                          fill = NA, 
                          size = obs_size,
                          show.legend = LegCheck) +
               geom_point(data = obs_dataframe,
                          aes(shape = Inhibitor), 
                          alpha = obs_fill_trans,
                          # color = NA,
                          fill = obs_color, 
                          size = obs_size,
                          show.legend = LegCheck) + 
               scale_shape_manual(values = obs_shape)
         } else {
            A <- A + 
               # Adding outlines b/c otherwise they will be whatever
               # transparency obs_fill_trans is.
               geom_point(data = obs_dataframe,
                          alpha = obs_line_trans,
                          color = "black",
                          fill = NA,
                          shape = obs_shape, 
                          size = obs_size,
                          show.legend = LegCheck) +
               geom_point(data = obs_dataframe,
                          alpha = obs_fill_trans,
                          # color = NA, 
                          fill = obs_color,
                          shape = obs_shape, 
                          size = obs_size,
                          show.legend = LegCheck)
         }
         
      } else {
         A <- A +
            # making obs point fill
            switch(as.character(map_obs_color),
                   "TRUE" = geom_point(data = obs_dataframe,
                                       alpha = obs_fill_trans,
                                       size = obs_size,
                                       show.legend = LegCheck),
                   "FALSE" =  geom_point(data = obs_dataframe,
                                         alpha = obs_fill_trans,
                                         color = obs_color,
                                         size = obs_size,
                                         fill = obs_color,
                                         show.legend = LegCheck)) 
      }
      
   } else {
      if(OutlineSolid){
         A <- A +
            # Adding outlines b/c otherwise they will be whatever
            # transparency obs_fill_trans is.
            geom_point(data = obs_dataframe,
                       alpha = obs_line_trans,
                       color = "black",
                       fill = NA, 
                       size = obs_size,
                       show.legend = LegCheck, 
                       shape = obs_shape[1]) + 
            geom_point(data = obs_dataframe,
                       alpha = obs_fill_trans,
                       color = "black", 
                       fill = obs_color, 
                       size = obs_size,
                       show.legend = LegCheck, 
                       shape = obs_shape[1])
         
      } else {
         A <- A +
            # making obs point fill
            switch(as.character(map_obs_color),
                   "TRUE" = geom_point(data = obs_dataframe,
                                       alpha = obs_fill_trans,
                                       size = obs_size,
                                       show.legend = LegCheck, 
                                       shape = obs_shape[1]),
                   "FALSE" =  geom_point(data = obs_dataframe,
                                         alpha = obs_fill_trans,
                                         color = obs_color,
                                         size = obs_size,
                                         fill = obs_color,
                                         show.legend = LegCheck, 
                                         shape = obs_shape[1]))
      }
   }
   # }
   
   return(A)
   
}


