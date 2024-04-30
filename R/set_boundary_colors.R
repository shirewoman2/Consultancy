#' INTERNAL: Create a set of boundary colors based on the color set and the
#' boundaries the user needs ---- CONSIDERING WHETHER IT'S WORTH IT TO MAKE
#' THIS. It doesn't actually save me much copying and pasting since I only use
#' it once in formatTable_Simcyp, where it has one set of color_set options and
#' boundaries and number of breaks, and only twice in so_graph, where it has a
#' different set of color_set options and boundaries and number of breaks.
#'
#' @param color_set either a single value, e.g., "red green" or a character
#'   vector of colors. Acceptable single-value color set names:\itemize{
#'   \item{"yellow to red" - for highlighting in PK tables}
#'
#'   \item{"traffic" - for highlighting in PK tables}
#'
#'   \item{"red black lines" or "red black fill" - for S/O boundary lines or fill}
#'
#'   \item{"red green lines" or "red green fill" - for S/O boundary lines or fill}
#'
#'   \item{"muted red green lines" or "muted red green fill" - for S/O boundary lines or fill}}
#'
#' @param boundaries numeric vector of boundaries
#' @param max_num_breaks maximum number of breaks. For so_graph, this should be
#'   3 and for formatTable_Simcyp, this should be 4.
#'
#' @return a character vector of colors, one for each numeric boundary
#'
#' @examples
#' # None. Internal use only.
#' 
set_boundary_colors <- function(color_set,
                                boundaries, 
                                break_type = "GMR"){
   
   # NOTE: Not doing any error catching here b/c internal and I'm assuming that
   # I'll have done the appropriate error catching in the parent function.
   
   # Upper number of breaks not including 1
   MaxBound <- length(boundaries)
   
   # Tidying inputs
   if(1 %in% boundaries & 
      any(color_set %in% c("red black", "yellow to red", "green to red", 
                           "muted red green", "red green", "traffic"))){
      color_set <- paste(color_set, "1")
      
   }
   
   # Boundaries are only set up with *specific* colors when there are <= 3
   # boundaries, not including the middle. If there are more boundaries than
   # that, then we'll use colorRampPalette to pick the colors. Because of how
   # this is set up in the boundary_color_set switch, we want the breaks to be
   # no larger than 4.
   ColorChoices <- paste(
      color_set, 
      cut(length(boundaries), breaks = c(0:min(c(4, MaxBound)), Inf)))
   
   boundary_color_set <- 
      switch(ColorChoices, 
             # red black -----------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "red black (0,1]" = "black", 
             
             # 2 boundaries
             "red black (1,2]" = c("black", "black"), 
             
             # 3 boundaries
             "red black (2,3]" = c("black", "black", "red"), 
             
             # 4 boundaries
             "red black (3,4]" = c("black",
                                     colorRampPalette(c("black", "#FFC000", "red"))(
                                        length(boundaries) - 1)), 
             
             # > 4 boundaries (this is same as above intentionally)
             "red black (4,Inf]" = c("black",
                                     colorRampPalette(c("black", "#FFC000", "red"))(
                                        length(boundaries) - 1)), 
             
             # 1 boundary, e.g., it's only unity
             "red black 1 (0,1]" = "black", 
             
             # 2 boundaries
             "red black 1 (1,2]" = c("black", "black"), 
             
             # 3 boundaries
             "red black 1 (2,3]" = c("black", "black", "red"), 
             
             # 4 boundaries
             "red black 1 (3,4]" = c("black",
                                       colorRampPalette(c("black", "#FFC000", "red"))(
                                          length(boundaries) - 1)), 
             
             # > 4 boundaries (this is same as above intentionally)
             "red black 1 (4,Inf]" = c("black",
                                       colorRampPalette(c("black", "#FFC000", "red"))(
                                          length(boundaries) - 1)), 
             
             # red green ------------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "red green (0,1]" = "#17A142", 
             
             # 2 boundaries
             "red green (1,2]" = c("#17A142", "#17A142"), 
             
             # 3 boundaries
             "red green (2,3]" = c("#17A142", "#17A142", "red"), 
             
             # 4 boundaries
             "red green (3,4]" = c("#17A142", 
                                     colorRampPalette(c("#17A142", "red"))(
                                        length(boundaries) - 1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "red green (4,Inf]" = c("#17A142", 
                                     colorRampPalette(c("#17A142", "red"))(
                                        length(boundaries) - 1)), 
             
             
             # 1 boundary, e.g., it's only unity
             "red green 1 (0,1]" = "#17A142", 
             
             # 2 boundaries
             "red green 1 (1,2]" = c("#17A142", "red"), 
             
             # 3 boundaries
             "red green 1 (2,3]" = c("#17A142", "orange", "red"), 
             
             # 4 boundaries
             "red green 1 (3,4]" = c("#17A142", 
                                       colorRampPalette(c("#17A142", "red"))(
                                          length(boundaries) - 1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "red green 1 (4,Inf]" = c("#17A142", 
                                       colorRampPalette(c("#17A142", "red"))(
                                          length(boundaries) - 1)), 
             
             
             # muted red green ------------------------------------------------
             
             # 1 boundary, e.g., it's only unity
             "muted red green (0,1]" = "#A4E4AF", 
             
             # 2 boundaries
             "muted red green (1,2]" = c("#A4E4AF", "#A4E4AF"), 
             
             # 3 boundaries
             "muted red green (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
             
             # 4 boundaries
             "muted red green (3,4]" = c("#A4E4AF", 
                                           colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                              length(boundaries)-1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "muted red green (4,Inf]" = c("#A4E4AF", 
                                           colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                              length(boundaries)-1)), 
             
             # 1 boundary, e.g., it's only unity
             "muted red green 1 (0,1]" = "#A4E4AF", 
             
             # 2 boundaries
             "muted red green 1 (1,2]" = c("#A4E4AF", "#A4E4AF"), 
             
             # 3 boundaries
             "muted red green 1 (2,3]" = c("#A4E4AF", "#A4E4AF", "#E6A2A2"), 
             
             # 4 boundaries
             "muted red green 1 (3,4]" = c("#A4E4AF", 
                                             colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                length(boundaries)-1)), 
             
             # >4 boundaries (this is same as above intentionally)
             "muted red green 1 (4,Inf]" = c("#A4E4AF", 
                                             colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                                                length(boundaries)-1)), 
             
             
             # yellow to red ---------------------------------------------------
             
             # no middle, 1 boundary
             "yellow to red (0,1]" = "#FF9595",
             
             # no middle, 2 boundaries
             "yellow to red (1,2]" = c("#FFFF95", "#FF9595"),
             
             # no middle, 3 boundaries
             "yellow to red (2,3]" = c("#FFFF95", "#FFDA95", "#FF9595"),
             
             # no middle, >3 boundaries
             "yellow to red (3,4]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                length(boundaries)),
             
             # This is the same as the above on purpose.
             "yellow to red (4,Inf]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                length(boundaries)),
             
             # Just highlight everything green. This would be weird and
             # probably not what the user wants, but is among the possible
             # choices for inputs.
             "yellow to red 1 (0,1]" = c("#C7FEAC"),
             
             # highlight middle, 1 boundary other than middle
             "yellow to red 1 (1,2]" = c("#C7FEAC", "#FF9595"),
             
             # highlight middle, 2 boundaries other than middle
             "yellow to red 1 (2,3]" = c("#C7FEAC", "#FFFF95", "#FF9595"),
             
             # highlight middle, 3 boundaries other than middle
             "yellow to red 1 (3,4]" = c("#C7FEAC", "#FFFF95", "#FFDA95", "#FF9595"),
             
             # highlight middle, >3 boundaries other than middle
             "yellow to red 1 (4,Inf]" =
                c("#C7FEAC",
                  colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                     length(boundaries))),
             
             
             # traffic --------------------------------------------------------
             
             # no middle, 1 boundary
             "traffic (0,1]" = "#FF0000",
             
             # no middle, 2 boundaries
             "traffic (1,2]" = c("#FFC000", "#FF0000"),
             
             # no middle, 3 boundaries
             "traffic (2,3]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             
             # no middle, >3 boundaries
             "traffic (3,4]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             # This is the same as the above on purpose.
             
             "traffic (4,Inf]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                length(boundaries)),
             # This is the same as the above on purpose.
             
             # Just highlight everything green. This would be weird and
             # probably not what the user wants, but is among the possible
             # choices for inputs.
             "traffic 1 (0,1]" = c("#00B050"),
             
             # highlight middle, 1 boundary other than middle
             "traffic 1 (1,2]" = c("#00B050", "#FF0000"),
             
             # highlight middle, 2 boundaries other than middle
             "traffic 1 (2,3]" = c("#00B050", "#92D050", "#FF0000"),
             
             # highlight middle, 3 boundaries other than middle
             "traffic 1 (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000"),
             
             # highlight middle, >3 boundaries other than middle
             "traffic 1 (4,Inf]" =
                c("#00B050", "#92D050", 
                  colorRampPalette(c("#FFC000", "#FF0000"))(
                     length(boundaries)-1))
      ) 
   
   if(break_type == "GMR" & color_set %in% c("yellow to red 1", 
                                             "green to red 1", 
                                             "traffic 1")){
      boundary_color_set <- 
         switch(ColorChoices, 
                
                # yellow to red for GMRs -----------------------------------------
                
                "yellow to red 1 (3,4]" = c("white", "#FFFF95", "#FFDA95", "#FF9595"), 
                
                "green to red 1 (3,4]" = c("#C7FEAC", "#FFFF95", "#FFDA95", "#FF9595"), 
                
                "traffic 1 (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000")
         )
   }
   
   if(break_type == "GMR"){
      names(boundary_color_set) <- c("negligible", 
                                     "weak", 
                                     "moderate", 
                                     "strong")
   }
   
   return(boundary_color_set)
   
}


