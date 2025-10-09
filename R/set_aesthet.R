#' INTERNAL PACKAGE USE: Set up certain aesthetics for a conc-time plot
#'
#' Determine line colors, line types, point shapes, and point colors.
#'
#' @param figure_type user-specified figure type
#' @param DDI T or F - only applies to ct_plot; might not even need this
#' @param line_type user-specified line type
#' @param obs_shape user-specified observed data shape
#' @param line_color user-specified line color
#' @param obs_color user-specified observed-data color
#' @param n_line_type number of line types needed
#' @param obs_line_trans obs line alpha
#' @param obs_fill_trans obs fill alpha
#' @param AESCols AESCols
#' @param n_line_color number line colors needed
#' @param n_obs_shape number obs shapes needed
#' @param n_obs_color number obs colors needed
#' @param from_ct_plot T or F on whether this is from ct_plot (alternative is
#'   ct_plot_overlay). Using this to determine whether line color is black or
#'   colors.
#' @param linetype_levels levels in linetype_column
#' @param color_levels levels in colorBy_column
#'
#' @return several objects to use with setting graph aesthetics

set_aesthet <- function(figure_type, 
                        from_ct_plot, 
                        DDI, 
                        AESCols, 
                        line_type,
                        n_line_type, 
                        linetype_levels = NA, 
                        
                        line_color,
                        n_line_color, 
                        color_levels = NA, 
                        
                        obs_shape, 
                        n_obs_shape, 
                        obs_color, 
                        n_obs_color, 
                        obs_line_trans, 
                        obs_fill_trans){
   
   # Notes on things that should be before calling this function: 
   
   # The column Inhibitor and the column Study, if it exists, are factor. 
   
   # Setting user specifications for shape, linetype, and color where
   # applicable.
   
   # Noting original preferences
   line_color_orig <- line_color
   line_type_orig <- line_type
   obs_color_orig <- obs_color
   obs_shape_orig <- obs_shape
   
   
   # line_type ---------------------------------------------------------------
   
   if(is.na(line_type[1])){
      line_type <- rep(c("solid", "dashed", "dotted"), n_line_type)
   }
   
   line_type <- rep(line_type, n_line_type)[1:n_line_type]
   
   if(any(complete.cases(linetype_levels)) & 
      figure_type %in% c("compound summary") == FALSE){
      names(line_type) <- linetype_levels
   }
   
   
   # obs_shape --------------------------------------------------------------
   
   if(is.na(obs_shape[1])){
      if(figure_type == "compound summary"){
         obs_shape <- rep(c(16, 17, 15, 18, 1, 2, 0, 5, 6, 8, 7, 9, 10, 
                            12, 13, 3, 4, 14), n_obs_shape)
      } else if(figure_type == "freddy"){
         obs_shape <- c(1, 2)
      } else {
         obs_shape <- rep(c(1, 2, 0, 5, 6, 8, 7, 9, 10, 
                            12, 13, 3, 4, 14), n_obs_shape)
      }
   }
   
   obs_shape <- rep(obs_shape, n_obs_shape)[1:n_obs_shape]
   
   if(any(complete.cases(linetype_levels))){
      names(obs_shape) <- linetype_levels
   }
   
   # It's really hard to have a mix of shapes with solid plus outline and then
   # also shapes that are just outline or just solid. If people have requested a
   # mix, give them a warning and set all the shapes to their solid version.
   SolidShapes <- which(obs_shape %in% 15:20)
   MixShapes <- which(obs_shape %in% 21:25)
   OutlineShapes <- setdiff(1:length(obs_shape), c(SolidShapes, MixShapes))
   
   if(length(MixShapes) > 1 & length(c(SolidShapes, OutlineShapes)) > 1){
      warning(wrapn("You have requested a mixture of shapes with a fill color and then a black outline plus other shapes that have just an outline or just a solid fill color. This is hard to get to look right on your graph, so we're only going to use the solid versions of those mixed shapes."), 
              call. = FALSE)
      
      obs_shape <- case_match(obs_shape, 
                              21 ~ 19, 
                              22 ~ 15, 
                              23 ~ 18, 
                              24 ~ 17, 
                              25 ~ 6, # no filled upside down triangle available so using outline version here
                              .default = obs_shape)
      
   }
   
   
   # line_color --------------------------------------------------------------
   
   if(is.na(line_color[1])){
      if(AESCols["color"] == "<empty>" | from_ct_plot == TRUE){
         line_color <- rep("black", n_line_color)
         
      } else {
         line_color <- make_color_set(color_set = "default", 
                                      num_colors = n_line_color)
         
         if(DDI & AESCols["color"] == "Inhibitor" & n_line_color == 2){
            # Making baseline blue, DDI red
            line_color <- c("#377EB8", "#E41A1C")
         }
      }
      
   } else {
      # If they did specify any line colors, still use make_color_set with those
      # colors to check that they have specified legit colors and to make the
      # exact colors needed if they specified a color set.
      line_color <- make_color_set(color_set = line_color, 
                                   num_colors = n_line_color)
   }
   
   # Just making absolutely sure we have enough colors
   line_color <- rep(line_color, n_line_color)[1:n_line_color]
   
   # To ensure that simulated and observed data colors match, naming the values
   # in line_color.
   if(any(complete.cases(color_levels)) & 
      figure_type %in% c("compound summary") == FALSE){
      names(line_color) <- color_levels
   }
   
   
   # obs_color ----------------------------------------------------------------
   
   if(from_ct_plot){
      # If it's from ct_plot and they did not specify obs_color, then the line
      # color should match line_color UNLESS the figure type is compound summary
      # or Freddy.
      
      if(all(is.na(obs_color)) & 
         figure_type %in% c("freddy", "compound summary") == FALSE){
         obs_color <- line_color[1:n_obs_color]
         
      } else if(all(is.na(obs_color)) & figure_type == "freddy"){
         obs_color <- "#3030FE"
         
      } else if(figure_type == "compound summary"){
         if(all(is.na(obs_color))){
            obs_color <- make_color_set(color_set = "default", 
                                        num_colors = n_obs_color)
         } else {
            obs_color <- make_color_set(color_set = obs_color, 
                                        num_colors = n_obs_color)
         }
         
      } else if((length(unique(line_color)) == 1 &&
                 line_color[1] == "black") &
                all(complete.cases(obs_color))){
         
         # This is when it's from ct_plot and line_color doesn't change based on
         # Inhibitor column.
         obs_color <- make_color_set(color_set = obs_color, 
                                     num_colors = 1)
         
      } else if((length(unique(line_color)) > 1)){
         
         if(length(unique(obs_color)) == 1){
            # nothing should happen here. Obs color will remain the same.
         } else {
            # This is when it's from ct_plot and line_color DOES change based on
            # Inhibitor column.
            obs_color <- line_color
         }
      }
      
   } else {
      # from ct_plot_overlay here
      if(all(is.na(obs_color))){
         obs_color <- line_color
      } else {
         obs_color <- make_color_set(color_set = obs_color[1], 
                                     num_colors = 1)
      } 
   }
   
   if(any(complete.cases(line_color_orig)) & 
      any(complete.cases(obs_color_orig)) && 
      length(obs_color) > 1){
      warning(wrapn(paste0("You have requested one set of colors for the lines in your graph, which are mapped to the values in the column '", 
                           AESCols["color"], 
                           "' and a different set of colors for your observed data points, which are mapped to the same column. We can only do one set of colors per mapped column, so we will use the values you specified for line color for the observed data points as well.")), 
              call. = FALSE)
   }
   
   obs_fill_trans <- ifelse(is.na(as.numeric(obs_fill_trans)), 
                            0.5, as.numeric(obs_fill_trans))
   
   obs_line_trans <- ifelse(is.na(as.numeric(obs_line_trans)), 
                            1, as.numeric(obs_line_trans))
   
   # returning
   Out <- list("line_type" = line_type, 
               "line_color" = line_color,
               "obs_shape" = obs_shape,
               "obs_color" = obs_color, 
               "obs_fill_trans" = obs_fill_trans,
               "obs_line_trans" = obs_line_trans)
   
   return(Out)
   
}
