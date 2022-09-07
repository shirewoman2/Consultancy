#' Add observed data points to a concentration-time plot
#'
#' \code{addObsPoints} addes observed data to \code{\link{ct_plot}} or
#' \code{\link{ct_plot_overlay}} and sets the color and shape of those points
#' appropriately. This is NOT meant to be used as a stand-alone function.
#'
#' @param obs_data observed data as a data.frame 
#' @param A the existing ggplot2 graph to which the observed data will be added
#' @param MapObsData TRUE or FALSE for whether to map the observed data to
#'   specific columns. Originally from ct_plot_overlay.
#' @param LegCheck TRUE or FALSE for whether to include the legend. From
#'   ct_plot_overlay originally and should always be TRUE for ct_plot.
#'
#' @return

addObsPoints <- function(obs_data, 
                         A, 
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
                         MapObsData, 
                         LegCheck){
    
    # Dealing with idiosyncracies of ribbon figure type
    if(str_detect(figure_type, "ribbon")){
        obs_data <- obs_data %>% 
            mutate(MyMean  = Conc, per5 = Conc, per95 = Conc)
    }
    
    obs_size <- ifelse(is.na(obs_size), 2, obs_size)
    
    # Need to adjust some code based on whether the obs_shape was solid, outline
    # only, or a mix. 
    OutlineOnly <- all(obs_shape %in% c(0:14))
    SolidOnly <- all(obs_shape %in% c(15:20))
    MixShape <- all(obs_shape %in% c(0:14) == FALSE) & 
        all(obs_shape %in% c(15:20) == FALSE)
    
    # Adjusting shapes for when user wants only outlines and no fill for
    # observed data points but may not have set the correct shape for that
    if(complete.cases(obs_fill_trans_user) &&
       obs_fill_trans_user == 0 & (SolidOnly | MixShape)){
        
        SolidToOutline <- c("15" = 0, 
                            "16" = 1, 
                            "17" = 2, 
                            "18" = 5, 
                            "19" = 1, 
                            "20" = 1)
        obs_shape <- as.numeric(SolidToOutline[as.character(obs_shape)])
        OutlineOnly <- TRUE
        obs_color <- "black"
        
    }
    
    # Adjusting shapes for when user has left obs_color as NA, figure type is
    # not "Freddy", and shape is a mixed shape b/c they probably just want
    # outlines in that situation.
    if(is.na(obs_color_user) & figure_type != "freddy" & 
       all(is.na(obs_shape_user)) & MixShape){
        MixToOutline <- 0:14
        names(MixToOutline) <- as.character(MixToOutline)
        MixToOutline <- c(MixToOutline, 
                          "15" = 0, 
                          "16" = 1, 
                          "17" = 2, 
                          "18" = 5, 
                          "19" = 1, 
                          "20" = 1, 
                          "21" = 1, 
                          "22" = 0, 
                          "23" = 5, 
                          "24" = 2, 
                          "25" = 6)
        obs_shape <- as.numeric(MixToOutline[as.character(obs_shape)])
        OutlineOnly <- TRUE
        obs_color <- "black"
        
    }
    
    if(MixShape){
        
        A <- A +
            # making obs point outlines
            switch(as.character(MapObsData),
                   # "TRUE" is when there are multiple sets of
                   # observed data that are mapped to color or
                   # linetype, etc.
                   "TRUE" = geom_point(data = obs_data,
                                       alpha = obs_line_trans,
                                       fill = NA,
                                       size = obs_size,
                                       show.legend = LegCheck),
                   # "FALSE" is when the user has specified what
                   # color they want the observed data to be.
                   "FALSE" = geom_point(data = obs_data, 
                                        alpha = obs_line_trans, 
                                        color = "black", 
                                        fill = NA, 
                                        size = obs_size,
                                        show.legend = LegCheck)) +
            # making obs point fill
            switch(as.character(MapObsData),
                   "TRUE" =  geom_point(data = obs_data,
                                        alpha = obs_fill_trans,
                                        size = obs_size,
                                        show.legend = LegCheck),
                   "FALSE" = geom_point(data = obs_data, 
                                        alpha = obs_fill_trans, 
                                        color = "black", 
                                        fill = obs_color, 
                                        size = obs_size,
                                        show.legend = LegCheck)) +
            scale_shape_manual(values = obs_shape) 
        
    } else if(OutlineOnly){
        
        # If user specified only obs_fill_trans but they've got an
        # outline-only shape, then assume that they meant to specify
        # obs_line_trans instead.
        if(complete.cases(obs_fill_trans_user) &  is.na(obs_line_trans_user)){
            obs_line_trans <- obs_fill_trans
        }
        
        A <- A +
            # making obs point outlines
            switch(as.character(MapObsData),
                   "TRUE" = geom_point(data = obs_data,
                                       alpha = obs_line_trans,
                                       size = obs_size,
                                       show.legend = LegCheck), 
                   "FALSE" = geom_point(data = obs_data,
                                        alpha = obs_line_trans,
                                        color = obs_color,
                                        size = obs_size,
                                        fill = obs_color,
                                        show.legend = LegCheck)) +
            scale_shape_manual(values = obs_shape) 
        
    } else {
        # This is when all shapes are solid only OR there is some
        # mixture of outline-only, solid-only, and mixed shapes. In
        # the latter circumstance, there isn't a good way to control
        # alpha for outline and fill separately, so we'll use only
        # obs_fill_trans to determine alpha.
        
        # If user specified only obs_line_trans but they've got an
        # solid shape, then assume that they meant to specify
        # obs_fill_trans instead.
        if(SolidOnly & complete.cases(obs_line_trans_user) & 
           is.na(obs_fill_trans_user)){
            obs_fill_trans <- obs_line_trans
        }
        
        A <- A +
            # making obs point fill
            switch(as.character(MapObsData),
                   "TRUE" = geom_point(data = obs_data,
                                       alpha = obs_fill_trans,
                                       size = obs_size,
                                       show.legend = LegCheck),
                   "FALSE" =  geom_point(data = obs_data,
                                         alpha = obs_fill_trans,
                                         color = obs_color,
                                         size = obs_size,
                                         fill = obs_color,
                                         show.legend = LegCheck)) +
            scale_shape_manual(values = obs_shape) 
    } 
    
    return(A)
    
}


