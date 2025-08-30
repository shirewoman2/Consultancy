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
#'
#' @return several objects to use with setting graph aesthetics

set_aesthet <- function(figure_type, 
                        from_ct_plot, 
                        DDI, 
                        AESCols, 
                        line_type,
                        n_line_type, 
                        line_color,
                        n_line_color, 
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
   if(is.na(line_type[1])){
      line_type <- rep(c("solid", "dashed"), n_line_type)
   }
   
   line_type <- rep(line_type, n_line_type)[1:n_line_type]
   
   if(is.na(obs_shape[1])){
      if(figure_type == "compound summary"){
         obs_shape <- rep(c(16, 17, 15, 18, 1, 2, 0, 5, 6, 8, 7, 9, 10, 
                            12, 13, 3, 4, 14), n_obs_shape)
      } else {
         obs_shape <- rep(c(1, 2, 0, 5, 6, 8, 7, 9, 10, 
                            12, 13, 3, 4, 14), n_obs_shape)
      }
   }
   
   obs_shape <- rep(obs_shape, n_obs_shape)[1:n_obs_shape]
   
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
   
   # Noting original preferences 
   line_color_orig <- line_color
   line_type_orig <- line_type
   obs_color_orig <- obs_color
   obs_shape_orig <- obs_shape
   
   if(is.na(line_color[1])){
      if(from_ct_plot | AESCols["color"] == "<empty>"){
         line_color <- rep("black", n_line_color)
      } else {
         line_color <- make_color_set(color_set = "default", num_colors = n_line_color)
      }
      
      if(DDI & AESCols["color"] == "Inhibitor" & n_line_color == 2){
         # Making baseline blue, DDI red
         line_color <- c("#377EB8", "#E41A1C")
      }
   }
   
   line_color <- rep(line_color, n_line_color)[1:n_line_color]
   
   if(is.na(obs_color[1]) & n_obs_color == 1){
      if(figure_type %in% c("freddy", "compound summary")){
         obs_color <- "#3030FE"
      } else {
         obs_color <- "black"
      }
   } else if(is.na(obs_color[1]) & n_obs_color > 1){
      obs_color <- line_color
   } else if(any(complete.cases(line_color_orig)) & 
             any(complete.cases(obs_color_orig))){
      warning(wrapn(paste0("You have requested one set of colors for the lines in your graph, which are mapped to the values in the column '", 
                           AESCols["color"], 
                           "' and a different set of colors for your observed data points, which are mapped to the same column. We can only do one set of colors per mapped column, so we will use the values you specified for line color for the observed data points as well.")), 
              call. = FALSE)
      obs_color <- line_color
   }
   
   obs_color <- rep(obs_color, n_obs_color)[1:n_obs_color]
   
   obs_fill_trans <- ifelse(is.na(obs_fill_trans), 
                            0.5, obs_fill_trans)
   
   obs_line_trans <- ifelse(is.na(obs_line_trans), 
                            1, obs_line_trans)
   
   # returning
   Out <- list("line_type" = line_type, 
               "line_color" = line_color,
               "obs_shape" = obs_shape,
               "obs_color" = obs_color, 
               "obs_fill_trans" = obs_fill_trans,
               "obs_line_trans" = obs_line_trans)
   
}
