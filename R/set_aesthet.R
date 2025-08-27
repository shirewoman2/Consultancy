#' INTERNAL PACKAGE USE: Set up certain aesthetics for a conc-time plot
#'
#' First (not last) step in determining line colors, line types, point shapes,
#' and point colors. Subsequent steps will adjust the exact number of all
#' shapes, linetypes, colors, etc. in the graph.
#'
#' @param line_type user-specified line type
#' @param figure_type user-specified figure type
#' @param MyPerpetrator concatenated string of all perpetrators involved
#' @param MyCompoundID compound user wants
#' @param obs_shape user-specified observed data shape
#' @param line_color user-specified line color
#' @param obs_color user-specified observed-data color
#'
#' @return several objects to use with setting graph aesthetics

set_aesthet <- function(line_type, figure_type, MyPerpetrator, MyCompoundID, 
                        obs_shape, line_color, obs_color, 
                        obs_line_trans, obs_fill_trans){
   
   # Notes on things that should be going into this function: 
   
   # There should be only one obs_color for ct_plot graphs that are not compound
   # summary figure types.
   
   # The column Inhibitor and the column Study, if it exists, are factor. 
   
   
   # Setting user specifications for shape, linetype, and color where
   # applicable.
   if(is.na(line_type[1])){
      if(str_detect(figure_type, "ribbon") & 
         length(MyPerpetrator) > 0 && complete.cases(MyPerpetrator[1]) &&
         MyPerpetrator[1] != "none" & 
         ("inhibitor 1" %in% MyCompoundID == FALSE)){
         line_type <- c("solid", "solid")
      } else {
         line_type <- c("solid", "dashed")
      }
   }
   
   if(is.na(obs_shape[1])){
      if(figure_type == "compound summary"){
         # Not sure how many we'll need, and the main ct_plot function will
         # replicate this as needed until we have enough shapes, so just picking
         # several good-ish ones to use.
         obs_shape <- c(16, 17, 15, 18, 1, 2, 0, 5, 6, 8, 7, 9, 10, 12, 13, 3, 4, 14)
      } else {
         obs_shape <- c(1, 2)
      }
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
   
   if(is.na(line_color[1])){
      if(str_detect(figure_type, "ribbon") & 
         length(MyPerpetrator) > 0 && complete.cases(MyPerpetrator[1]) &&
         ("inhibitor 1" %in% MyCompoundID == FALSE)){
         line_color <- c("#377EB8", "#E41A1C")
      } else {
         line_color <- c("black", "black")
      }
   }
   
   if(complete.cases(line_color[1]) &&
      figure_type %in% c("freddy", "compound summary") &&
      length(line_color) == 1){
      line_color <- rep(line_color, 2)
   }
   
   if(length(obs_color) == 1 &&
      (complete.cases(obs_color) && obs_color == "default") | 
      (is.na(obs_color[1]) & 
       figure_type %in% c("freddy", "compound summary"))){
      obs_color <- "#3030FE"
   } else if(length(obs_color) == 1 && is.na(obs_color)){
      obs_color = "black"
   }
   
   if(all(is.na(obs_color))){
      obs_color <- line_color
   }
   
   obs_fill_trans <- ifelse(is.na(obs_fill_trans), 
                            0.5, obs_fill_trans)
   
   obs_line_trans <- ifelse(is.na(obs_line_trans), 
                            1, obs_line_trans)
   
   # Assigning the variables created or changed here to the environment one
   # level up, e.g., probably the environment within the function that's
   # calling on *this* function.
   Out <- list("line_type" = line_type, 
               "line_color" = line_color,
               "obs_shape" = obs_shape,
               "obs_color" = obs_color, 
               "obs_fill_trans" = obs_fill_trans,
               "obs_line_trans" = obs_line_trans)
   
}
