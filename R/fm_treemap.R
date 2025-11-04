#' For comparing fm values, make a treemap as opposed to the standard and
#' arguably inferior pie chart
#'
#' @description Create a treemap -- filled rectangles -- to indicate the
#'   fraction of metabolism due to various pathways. Rectangles are proportional
#'   in size to the fm values and include labels for the drug-metabolizing
#'   enzyme involved and the percentage that enzyme is responsible for. Any
#'   rectangles that would be smaller than some user-specified cutoff (see
#'   argument "label_fm_cutoff") will be pulled out underneath the graph for
#'   readability.
#'
#'   \strong{Why bother with a treemap as opposed to a pie chart?} Rectangles
#'   are easier to compare visually than the sizes of pie slices, making a
#'   treemap easier to understand and interpret (reference: "The Visual Display
#'   of Quantitative Information" by Edward Tufte, Professor Emeritus of
#'   Political Science, Statistics, and Computer Science at Yale University).
#'
#'
#' @param fm_dataframe a data.frame containing columns for the drug-metabolizing
#'   enzyme and the fm, the fraction metabolized. If you ran
#'   \code{\link{extractFmFe}} to get these data, be sure to \emph{only} get the
#'   data you want. For example, if this was a DDI simulation, you would want to
#'   use only the data in the absense of a perpetrator or only in the presence
#'   of one; summing up both numbers wouldn't make sense.
#' @param mean_type If there is a column called "statistic" in your data, which
#'   mean type would you like to display? Acceptable values are any statistics
#'   present in that column, but you can only show one.
#' @param DDI_option If the fm_dataframe included fm and fe values for when any
#'   DDI perpetrator drugs are present, then you've got some options for how to
#'   display this. Acceptable options are: "baseline only" (default) to show
#'   only fm values at baseline, "DDI only", to show only fm values in the
#'   presence of the perpetrator drug(s), or "facet by DDI" to show two graphs,
#'   broken up by whether there is a DDI.
#' @param pathway_column the name of the column that contains the names of the
#'   pathways. If you ran \code{\link{extractFmFe}} to get these data, set this
#'   to \code{pathway_column = Enzyme}
#' @param fm_column the name of the column that contains the fm values. If you
#'   ran \code{\link{extractFmFe}} to get these data, set this to
#'   \code{fm_column = Max} if you want the maximum fm value for each. Note that
#'   this should be the values as decimals, e.g., 0.8 for 80\%. The total area
#'   occupied by boxes in your treemap will sum to 100\%, even if these numbers
#'   do not, so pay attention to what data you're graphing and make sure things
#'   sum to 1.
#' @param show_numbers_on_graph TRUE (default) or FALSE for whether to list the
#'   numerical fm value for each pathway on the graph
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{
#'
#'   \item{"none"}{No rounding will be performed.}
#'
#'   \item{"significant X" where "X" is a number}{Output will be rounded to X
#'   significant figures. \code{rounding = "signif X"} also works fine. If you
#'   leave rounding as NA, the default is "significant 3".}
#'
#'   \item{"round X" where "X" is a number}{Output will be rounded to X digits}}
#' @param color_set the set of colors to use. Options are "default", which
#'   matches Simcyp colors in PowerPoint presentations and is rather nice, "set
#'   1" for Brewer set 1, "set 2" for Brewer set 2 (colorblind friendly),
#'   "rainbow", which works well when there are a LOT of fm's to visualize,
#'   "blue-green", which makes a pretty gradient of blues and greens, "blues",
#'   which makes a graident of blues, or "viridis", which is from the eponymous
#'   package by Simon Garnier and ranges colors from purple to blue to green to
#'   yellow in a manner that is "printer-friendly, perceptually uniform and easy
#'   to read by those with colorblindness", according to the package author.
#' @param label_fm_cutoff cutoff to use for having a label show up underneath
#'   the graph; default is 0.05. You may want to play around with this a bit in
#'   the final graphic file because how this graph looks is extremely dependent
#'   on its dimensions.
#' @param legend_nrow optionally specify the number of rows to use for the
#'   legend. If left as NA, there will be 1 row.
#' @param biggest_box_position where to put the biggest box. Defaults to "top
#'   left", and other options are "bottom left", "bottom right", and "top
#'   right".
#' @param graph_title optionally specify a title that will be centered across
#'   your graph
#' @param graph_title_size the font size for the graph title if it's included
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you do not designate a
#'   file extension, it will be saved as a png file, but if you specify a
#'   different file extension, it will be saved as that file format. Acceptable
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Do not include any slashes, dollar signs, or periods in the file name.
#'   Leaving this as NA means the file will not be automatically saved to disk.
#' @param fig_height figure height in inches; default is 3
#' @param fig_width figure width in inches; default is 4
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#'
#' Lenv_fm <- data.frame(DME = c("CYP3A4", "other CYPs", "aldehyde oxidase"),
#'                       fm = c(0.79, 0.20, 0.01))
#' fm_treemap(fm_dataframe = Lenv_fm,
#'            pathway_column = DME,
#'            fm_column = fm)
#'
#' fm_treemap(fm_dataframe = Lenv_fm,
#'            pathway_column = DME,
#'            fm_column = fm,
#'            color_set = "blue-green",
#'            label_fm_cutoff = 0.01)
#' 
fm_treemap <- function(fm_dataframe,
                       pathway_column, 
                       fm_column, 
                       mean_type = NA, 
                       DDI_option = "baseline only", 
                       show_numbers_on_graph = TRUE, 
                       rounding = NA, 
                       color_set = "default", 
                       label_fm_cutoff = 0.05,
                       legend_nrow = NA,
                       biggest_box_position = "top left",
                       graph_title = NA,
                       graph_title_size = 14, 
                       save_graph = NA, 
                       fig_height = 3, 
                       fig_width = 4){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   if(nrow(fm_dataframe) == 0){
      stop("Please check your input. The data.frame you supplied for fm_dataframe doesn't have any rows.", 
           call. = FALSE)
   }
   
   # Check whether treemapify has been installed. 
   if("treemapify" %in% installed.packages() == FALSE){
      stop("The function fm_treemap requires the package treemapify. Please run `install.packages('treemapify')`.", 
           call. = FALSE)
   }
   
   if("Time" %in% names(fm_dataframe)){
      stop(wrapn("You have supplied fm data that change with time, and this function only works with static fm data or with the maximum or minimum fm data from the 'Time variant %fm and fe' tab of a Simulator Excel file."), 
           call. = FALSE)
   }
   
   # Setting things up for nonstandard evaluation
   fm_column <- rlang::enquo(fm_column)
   pathway_column <- rlang::enquo(pathway_column)
   
   if(rlang::as_label(fm_column) %in% names(fm_dataframe) == FALSE){
      stop(wrapn("The column you have listed for the fm values is not present in your data.frame. Please enter a valid column for fm data."),
           call. = FALSE)
   }
   
   if(rlang::as_label(pathway_column) %in% names(fm_dataframe) == FALSE){
      stop(wrapn("The column you have listed for the pathways is not present in your data.frame. Please enter a valid column for pathway data."),
           call. = FALSE)
   }
   
   if(biggest_box_position %in% c("bottom left", "top left", "bottom right", 
                                  "top right") == FALSE){
      warning(wrapn(paste0("The only options for `biggest_box_position` are `bottom left`, `top left`, `bottom right`, or `top right`, and you supplied a value of `", 
                           biggest_box_position,
                           "``, which is not one of the options. We'll use the default of `bottom left`.")), 
              call. = FALSE)
      biggest_box_position <- "top left"
   }
   
   rounding <- ifelse(is.na(rounding), "significant 3", rounding)
   
   if(DDI_option %in% c("baseline only", "DDI only", "facet by DDI") == FALSE){
      warning(wrapn("The value you supplied for DDI_option is not one of `baseline only`, `DDI only`, or `facet by DDI`, which are the only acceptable options. We'll set this to the default of `baseline only`."), 
              call. = FALSE)
      DDI_option <- "baseline only"
   }
   
   # If Parameter column not present, assume that it should be fm.
   if("Parameter" %in% names(fm_dataframe) == FALSE){
      fm_dataframe$Parameter <- "fm"
   }
   
   # If Tissue column not present, assume that it should be plasma.
   if("Tissue" %in% names(fm_dataframe) == FALSE){
      fm_dataframe$Tissue <- "plasma"
   }
   
   mean_type <- mean_type[1]
   if("Statistic" %in% names(fm_dataframe)){
      if(complete.cases(mean_type) & 
         mean_type %in% fm_dataframe$Statistic == FALSE){
         stop(wrapn(paste0("You requested a mean_type of '", 
                           mean_type, 
                           "', but that is not present in the data. Please supply a value for mean_type that is present in the column 'Statistic'.")), 
              call. = FALSE)
      } else {
         fm_dataframe <- fm_dataframe %>% 
            filter(Statistic == mean_type)
      }
   }
   
   
   # Main function ----------------------------------------------------------
   
   # Adding labels 
   fm_dataframe <- fm_dataframe %>% 
      rename(DME = !!pathway_column,
             fm = !!fm_column)
   
   if("PerpPresent" %in% names(fm_dataframe)){
      if(DDI_option == "baseline only"){
         fm_dataframe <- fm_dataframe %>% filter(PerpPresent == FALSE)
      } else if(DDI_option == "DDI only"){
         fm_dataframe <- fm_dataframe %>% filter(PerpPresent == TRUE)
      }
   } else (
      fm_dataframe$PerpPresent <- FALSE
   )
   
   # Making a prettier label for any faceting by DDI
   fm_dataframe <- fm_dataframe %>% 
      mutate(Perpetrator = ifelse(PerpPresent, "DDI", "baseline"), 
             Perpetrator = factor(Perpetrator, levels = c("baseline", "DDI")))
   
   # If any of the DME are replicated in multiple tissues, we'll need to note
   # that in the labels. I don't think we want labels that say, e.g., "liver
   # CYP3A4", "liver CYP1A2", etc. when they're ALL liver, though.
   suppressMessages(
      TissueDMECheck <- fm_dataframe %>% 
         group_by(DME, PerpPresent) %>% 
         summarize(N = n()) %>% 
         filter(N > 1)
   )
   
   if(nrow(TissueDMECheck) > 0){
      fm_dataframe <- fm_dataframe %>% 
         mutate(DME = case_when(DME %in% TissueDMECheck$DME ~ paste(Tissue, DME), 
                                .default = DME))
   }
   
   fm_dataframe <- fm_dataframe %>% 
      # Rounding as requested
      mutate(fm = round_opt(fm*100, round_fun = rounding, is_this_for_Word = FALSE), 
             # Dealing with possible renal elimination
             DME = case_when(is.na(DME) & Parameter == "fe" ~
                                paste(Tissue, "elimination"), 
                             .default = DME), 
             # adding labels
             Label = switch(as.character(show_numbers_on_graph), 
                            "TRUE" = paste0(DME, "\n", fm, "%"), 
                            "FALSE" = DME), 
             LabelLegend = paste0(DME, " ", fm, "%"), 
             Label = fct_reorder(Label, fm, .desc = TRUE), 
             LabelLegend = fct_reorder(LabelLegend, fm, .desc = TRUE)) %>% 
      arrange(Label)
   
   
   # subfunction so that we can create multiple sets of graphs as needed
   tree_subfun <- function(DDI){
      fm_dataframe <- fm_dataframe %>% filter(PerpPresent == DDI)
      
      # Adding options for colors
      NumColors <- length(unique(fm_dataframe$Label))
      
      MyColors <- make_color_set(color_set = color_set, 
                                 num_colors = NumColors)
      names(MyColors) <- unique(fm_dataframe$LabelLegend)
      
      # Putting into the legend any fm's that are below the threshold
      G <- ggplot(fm_dataframe, aes(label = Label, area = fm, fill = LabelLegend)) +
         treemapify::geom_treemap(start = sub(" ", "", biggest_box_position)) +
         treemapify::geom_treemap_text(fontface = "bold", 
                                       colour = "white", 
                                       place = "centre", 
                                       min.size = 6, 
                                       start = sub(" ", "", biggest_box_position)) +
         scale_fill_manual(
            # FIXME - Fiddle with this to get only the legend entries that are not
            # already labeled in the graph
            # breaks = fm_dataframe$LabelLegend[which(fm_dataframe$fm < label_fm_cutoff)],
            values = MyColors) +
         theme(legend.title = element_blank(),
               legend.position = "bottom")
      
      if(complete.cases(legend_nrow)){
         G <- G + guides(fill = guide_legend(nrow = legend_nrow))
      }
      
      if(DDI_option == "facet by DDI"){
         G <- G + ggtitle(ifelse(DDI, "DDI", "baseline")) +
            theme(plot.title = element_text(hjust = 0.5))
      }
      
      return(G)
   }
   
   if(DDI_option == "facet by DDI"){
      A <- tree_subfun(FALSE)
      B <- tree_subfun(TRUE)
      G <- ggpubr::ggarrange(A, B, nrow = 1, common.legend = FALSE, align = "hv")
      
      if(("character" %in% class(graph_title) && complete.cases(graph_title)) |
         "expression" %in% class(graph_title)){
         G <- ggpubr::annotate_figure(G, 
                                      top = ggpubr::text_grob(graph_title, 
                                                              size = graph_title_size))
      }
   } else {
      
      G <- tree_subfun(DDI = DDI_option == "DDI only") 
      
      if(("character" %in% class(graph_title) && complete.cases(graph_title)) |
         "expression" %in% class(graph_title)){
         G <- G + ggtitle(graph_title) +
            theme(plot.title = element_text(hjust = 0.5, size = graph_title_size))
      }
   }
   
   if(complete.cases(save_graph)){
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg"), 
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
      }
      
      ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
             plot = G)
   }
   
   return(G)
   
}


