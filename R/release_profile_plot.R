#' Make a graph of the release profiles of a compound
#'
#' \code{release_profile_plot} is meant to be used in conjunction with
#' \code{\link{extractExpDetails}} to create graphs with release-profile data,
#' possibly for multiple simulations or for multiple compounds.
#'
#' @param figure_type the type of figure to plot. \describe{
#'
#'   \item{"means only"}{(default) show only the mean release profile}
#'
#'   \item{"percentile ribbon"}{show an opaque line for the mean data and
#'   transparent shading for the standard deviation.}}
#' @param linear_or_log the type of graph to be returned. Options: \describe{
#'   \item{"semi-log"}{y axis is log transformed; this is the default}
#'
#'   \item{"linear"}{no axis transformation}
#'
#'   \item{"both vertical"}{both the linear and the semi-log graphs will be
#'   returned, and graphs are stacked vertically}
#'
#'   \item{"both horizontal"}{both the linear and the semi-log graphs will be
#'   returned, and graphs are stacked horizontally}}
#' @param colorBy_column (optional) the column in
#'   \code{existing_exp_details$ReleaseProfiles} that should be used for
#'   determining which color lines and/or points will be. This should be
#'   unquoted, e.g., \code{colorBy_column = Tissue}.
#' @param color_labels optionally specify a character vector for how you'd like
#'   the labels for whatever you choose for \code{colorBy_column} to show up in
#'   the legend. For example, use \code{color_labels = c("file 1.xlsx" = "fa
#'   0.5", "file 2.xlsx" = "fa 0.2")} to indicate that "file 1.xlsx" is for an
#'   fa of 0.5 and "file 2.xlsx" is for an fa of 0.2. The order in the legend
#'   will match the order designated here.
#' @param legend_label_color optionally indicate on the legend something
#'   explanatory about what the colors represent. For example, if
#'   \code{colorBy_column = File} and \code{legend_label_color = "Simulations
#'   with various fa values"}, that will make the label above the file names in
#'   the legend more explanatory than just "File". The default is to use
#'   whatever the column name is for \code{colorBy_column}. If you don't want a
#'   label for this legend item, set this to "none".
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"default"}{a set of colors from Cynthia Brewer et al. from Penn State
#'   that are friendly to those with red-green colorblindness. The first three
#'   colors are green, orange, and purple. This can also be referred to as
#'   "Brewer set 2". If there are only two unique values in the colorBy_column,
#'   then Brewer set 1 will be used since red and blue are still easily
#'   distinguishable but also more aesthetically pleasing than green and
#'   orange.}
#'
#'   \item{"Brewer set 1"}{colors selected from the Brewer palette "set 1". The
#'   first three colors are red, blue, and green.}
#'
#'   \item{"ggplot2 default"}{the default set of colors used in ggplot2 graphs
#'   (ggplot2 is an R package for graphing.)}
#'
#'   \item{"rainbow"}{colors selected from a rainbow palette. The default
#'   palette is limited to something like 6 colors, so if you have more than
#'   that, that's when this palette is most useful. It's \emph{not} very useful
#'   when you only need a couple of colors.}
#'
#'   \item{"blue-green"}{a set of blues fading into greens. This palette can be
#'   especially useful if you are comparing a systematic change in some
#'   continuous variable -- for example, increasing dose or predicting how a
#'   change in intrinsic solubility will affect concentration-time profiles --
#'   because the direction of the trend will be clear.}
#'
#'   \item{"blues"}{a set of blues fading light blue to dark blue. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"Tableau"}{uses the standard Tableau palette; requires the "ggthemes"
#'   package}
#'
#'   \item{"viridis"}{from the eponymous package by Simon Garnier and ranges
#'   colors from purple to blue to green to yellow in a manner that is
#'   "printer-friendly, perceptually uniform and easy to read by those with
#'   colorblindness", according to the package author}
#'
#'   \item{a character vector of colors}{If you'd prefer to set all the colors
#'   yourself to \emph{exactly} the colors you want, you can specify those
#'   colors here. An example of how the syntax should look: \code{color_set =
#'   c("dodgerblue3", "purple", "#D8212D")} or, if you want to specify exactly
#'   which item in \code{colorBy_column} gets which color, you can supply a
#'   named vector. For example, if you're coloring the lines by the compound ID,
#'   you could do this: \code{color_set = c("substrate" = "dodgerblue3",
#'   "inhibitor 1" = "purple", "primary metabolite 1" = "#D8212D")}. If you'd
#'   like help creating a specific gradation of colors, please talk to a member
#'   of the R Working Group about how to do that using
#'   \link{colorRampPalette}.}}
#'
#' @param include_errorbars TRUE or FALSE (default) for whether to include error
#'   bars for the standard deviation.
#' @param errorbar_width width of error bars to use in hours (or, if you've used
#'   some other time unit, in whatever units are in your data). Default is 0.5.
#' @param linetype_column the column in
#'   \code{existing_exp_details$ReleaseProfiles} that should be used for
#'   determining the line types
#' @param linetype_labels optionally specify a character vector for how you'd
#'   like the labels for whatever you choose for \code{linetype_column} to show
#'   up in the legend. For example, use \code{linetype_labels = c("file 1.xlsx"
#'   = "fa 0.5", "file 2.xlsx" = "fa 0.2")} to indicate that "file 1.xlsx" is
#'   for an fa of 0.5 and "file 2.xlsx" is for an fa of 0.2. The order in the
#'   legend will match the order designated here.
#' @param linetypes the line types to use. Default is "solid" for all lines.
#'   You'll need one line type for each possible value in the column you
#'   specified for \code{linetype_column}.
#' @param line_width optionally specify how thick to make the lines. Acceptable
#'   input is a number; the default is 1 for most lines and 0.8 for some, to
#'   give you an idea of where to start.
#' @param legend_label_linetype optionally indicate on the legend something
#'   explanatory about what the line types represent. For example, if
#'   \code{linetype_column = Inhibitor} and \code{legend_label_linetype =
#'   "Inhibitor present"}, that will make the label in the legend above, e.g.,
#'   "none", and whatever perpetrator was present more explanatory than just
#'   "Inhibitor". The default is to use whatever the column name is for
#'   \code{linetype_column}. If you don't want a label for this legend item, set
#'   this to "none".
#' @param facet1_column optionally break up the graph into small multiples; this
#'   specifies the first of up to two columns to break up the data by, and the
#'   designated column name should be unquoted, e.g., \code{facet1_column =
#'   Tissue}. If \code{floating_facet_scale} is FALSE and you haven't specified
#'   \code{facet_ncol} or  \code{facet_nrow}, then \code{facet1_column} will
#'   designate the rows of the output graphs.
#' @param facet2_column optionally break up the graph into small multiples; this
#'   specifies the second of up to two columns to break up the data by, and the
#'   designated column name should be unquoted, e.g., \code{facet2_column =
#'   CompoundID}. If \code{floating_facet_scale} is FALSE and you haven't
#'   specified \code{facet_ncol} or  \code{facet_nrow}, then
#'   \code{facet2_column} will designate the columns of the output graphs.
#' @param facet_ncol optionally specify the number of columns of facetted graphs
#'   you would like to have. This only applies when you have specified a column
#'   for \code{facet1_column} and/or \code{facet2_column}.
#' @param facet_nrow optionally specify the number of rows of facetted graphs
#'   you would like to have. This only applies when you have specified a column
#'   for \code{facet1_column} and/or \code{facet2_column}.
#' @param floating_facet_scale TRUE or FALSE (default) for whether to allow the
#'   axes for each facet of a multi-facetted graph to scale freely to best fit
#'   whatever data are present. Default is FALSE, which means that all data will
#'   be on the same scale for easy comparison. However, this could mean that
#'   some graphs have lines that are hard to see, so you can set this to TRUE to
#'   allow the axes to shrink or expand according to what data are present for
#'   that facet. Floating axes comes with a trade-off for the looks of the
#'   graphs, though: Setting this to TRUE does mean that your x axis won't
#'   automatically have pretty breaks that are sensible for times in hours.
#' @param facet_spacing Optionally set the spacing between facets. If left as
#'   NA, a best-guess as to a reasonable amount of space will be used. Units are
#'   "lines", so try, e.g. \code{facet_spacing = 2}. (Reminder: Numeric data
#'   should not be in quotes.)
#' @param time_range time range to display. Options: \describe{
#'
#'   \item{NA}{entire time range of data; default}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}}
#'
#' @param x_axis_interval set the x-axis major tick-mark interval. Acceptable
#'   input: any number or leave as NA to accept default values, which are
#'   generally reasonable guesses as to aesthetically pleasing and PK-relevant
#'   intervals.
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param pad_x_axis optionally add a smidge of padding to the x axis (default
#'   is TRUE, which includes some generally reasonable padding). If changed to
#'   FALSE, the y axis will be placed right at the beginning of your time range
#'   and all data will end \emph{exactly} at the end of the time range
#'   specified. If you want a \emph{specific} amount of x-axis padding, set this
#'   to a number; the default is \code{c(0.02, 0.04)}, which adds 2\% more space
#'   to the left side and 4\% more space to the right side of the x axis. If you
#'   only specify one number, padding is added to the left side.
#' @param pad_y_axis optionally add a smidge of padding to the y axis (default
#'   is TRUE, which includes some generally reasonable padding). As with
#'   \code{pad_x_axis}, if changed to FALSE, the x axis will be placed right at
#'   the bottom of your data, possibly cutting a point in half. If you want a
#'   \emph{specific} amount of y-axis padding, set this to a number; the default
#'   is \code{c(0.02, 0)}, which adds 2\% more space to the bottom and nothing
#'   to the top of the y axis. If you only specify one number, padding is added
#'   to the bottom.
#' @param y_axis_limits_lin Optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as NA, the Y axis limits for the
#'   linear plot will be automatically selected. This only applies when you have
#'   requested a linear plot with \code{linear_or_log}.
#' @param y_axis_limits_log Optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to the nearest order of magnitude. If left as NA, the Y axis
#'   limits for the semi-log plot will be automatically selected. This only
#'   applies when you have requested a semi-log plot with \code{linear_or_log}.
#' @param y_axis_interval set the y-axis major tick-mark interval. Acceptable
#'   input: any number or leave as NA to accept default values, which are
#'   generally reasonable guesses as to aesthetically pleasing intervals.
#' @param y_axis_label optionally supply a character vector or an expression to
#'   use for the y axis label
#' @param hline_position numerical position(s) of any horizontal lines to add to
#'   the graph. The default is NA to have no lines, and good syntax if you
#'   \emph{do} want lines would be, for example, \code{hline_position = 10} to
#'   have a horizontal line at 10 ng/mL (or whatever your concentration units
#'   are) or \code{hline_position = c(10, 100, 1000)} to have horizontal lines
#'   at each of those y values. Examples of where this might be useful would be
#'   to indicate a toxicity threshold, a target Cmin, or the lower limit of
#'   quantification for the assay used to generate the concentration-time data.
#' @param hline_style the line color and type to use for any horizontal lines
#'   that you add to the graph with \code{hline_position}. Default is "red
#'   dotted", but any combination of 1) a color in R and 2) a named linetype is
#'   acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
#'   To see all the possible linetypes, type \code{ggpubr::show_line_types()}
#'   into the console.
#' @param vline_position numerical position(s) of any vertical lines to add to
#'   the graph. The default is NA to have no lines, and good syntax if you
#'   \emph{do} want lines would be, for example, \code{vline_position = 12} to
#'   have a vertical line at 12 h or \code{vline_position = seq(from = 0, to =
#'   168, by = 24)} to have horizontal lines every 24 hours for one week.
#'   Examples of where this might be useful would be indicating dosing times or
#'   the time at which some other drug was started or stopped.
#' @param vline_style the line color and type to use for any vertical lines that
#'   you add to the graph with \code{vline_position}. Default is "red dotted",
#'   but any combination of 1) a color in R and 2) a named linetype is
#'   acceptable. Examples: "red dotted", "blue dashed", or "#FFBE33 longdash".
#'   To see all the possible linetypes, type \code{ggpubr::show_line_types()}
#'   into the console.
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs. (Not applicable if only outputting
#'   linear or only semi-log graphs.)
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14. This also determines the font size of the graph labels.
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right" (default in most scenarios), "bottom", "top", or "none" if
#'   you don't want one at all.
#' @param prettify_compound_names set this to a) TRUE (default) or FALSE for
#'   whether to make the compound names in the legend prettier or b) supply a
#'   named character vector to set it to the exact name you'd prefer to see in
#'   your legend. For example, \code{prettify_compound_names =
#'   c("Sim-Ketoconazole-400 mg QD" = "ketoconazole", "Wks-Drug ABC-low_ka" =
#'   "Drug ABC")} will make those compounds "ketoconazole" and "Drug ABC" in a
#'   legend, and \code{prettify_compound_names = TRUE} will make some reasonable
#'   guesses about what a prettier compound name should be. An example of
#'   setting this to TRUE: "SV-Rifampicin-MD" would become "rifampicin", and
#'   "Sim-Ketoconazole-200 mg BID" would become "ketoconazole".
#' @param existing_exp_details output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}. This must be from package version >=
#'   2.8.0.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png"or "My conc time graph.docx".
#'   The nice thing about saving to Word is that the figure title and caption
#'   text will be partly filled in automatically, although you should check that
#'   the text makes sense in light of your exact graph. If you leave off ".png"
#'   or ".docx", it will be saved as a png file, but if you specify a different
#'   graphical file extension, it will be saved as that file format. Acceptable
#'   graphical file extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png",
#'   "bmp", or "svg". Do not include any slashes, dollar signs, or periods in
#'   the file name. Leaving this as NA means the file will not be automatically
#'   saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @return a ggplot2 graphs or a set of arranged ggplot2 graphs
#' @export
#'
#' @examples
#' Details <- extractExpDetails_mult(sim_data_files = NA)
#' 
#' release_profile_plot(existing_exp_details = Details)
#' 
#' # If you have multiple simulations or multiple compounds and you only 
#' # want to graph one, here is an example of how to filter your data to do
#' # that. 
#' 
#'  Details_subset <- Details
#'  Details_subset$ReleaseProfiles <- Details_subset$ReleaseProfiles %>% 
#'       filter(CompoundID == "substrate" & 
#'              File == "simulation A.xlsx")
#' 
#' release_profile_plot(existing_exp_details = Details_subset)
#' 
#' 
release_profile_plot <- function(existing_exp_details, 
                                 sims_to_include = NA, 
                                 compoundsToExtract = NA, 
                                 figure_type = "percentile ribbon", 
                                 linear_or_log = "linear",
                                 colorBy_column,
                                 color_labels = NA, 
                                 legend_label_color = NA,
                                 color_set = "default",
                                 include_errorbars = FALSE, 
                                 errorbar_width = 0.5,
                                 linetype_column, 
                                 linetype_labels = NA, 
                                 linetypes = c("solid", "dashed"),
                                 line_width = NA,
                                 line_transparency = NA,
                                 legend_label_linetype = NA,
                                 facet1_column,
                                 facet2_column, 
                                 facet_ncol = NA, 
                                 facet_nrow = NA,
                                 floating_facet_scale = FALSE,
                                 facet_spacing = NA,
                                 time_range = NA, 
                                 x_axis_interval = NA,
                                 x_axis_label = NA,
                                 pad_x_axis = TRUE,
                                 pad_y_axis = TRUE,
                                 y_axis_limits_lin = NA,
                                 y_axis_limits_log = NA, 
                                 y_axis_interval = NA,
                                 y_axis_label = "Percent released",
                                 hline_position = NA, 
                                 hline_style = "red dotted", 
                                 vline_position = NA, 
                                 vline_style = "red dotted",
                                 graph_labels = TRUE,
                                 graph_title = NA,
                                 graph_title_size = 14, 
                                 legend_position = NA,
                                 prettify_compound_names = TRUE,
                                 qc_graph = FALSE,
                                 save_graph = NA,
                                 fig_height = 6,
                                 fig_width = 5){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   if(figure_type %in% c("means only", "percentile ribbon") == FALSE){
      warning("The only acceptable options for `figure_type` are `means_only` or `percentile ribbon`. We'll set this to the default, `percentile ribbon`.\n", 
              call. = FALSE)
      figure_type <- "percentile ribbon"
   }
   
   if(is.na(y_axis_label)){
      warning("You must specify a value for `y_axis_label`. We'll use the default value of `Percent released`.\n", 
              call. = FALSE)
      y_axis_label <- "Percent released"
   }
   
   if(any(complete.cases(time_range)) && class(time_range) != "numeric"){
      warning("For the `release_profile_plot` function, the time range may only be numeric or NA. We'll set this to NA.\n", 
              call. = FALSE)
      time_range <- NA
   }
   
   # Need to harmonize input to check some of these other bits
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   # If user has supplied regex, that should have length 1. If they supplied a
   # character vector of files, that should probably have length > 1. Even if
   # they only supplied a single file name here, it should still work to use
   # regex instead of a perfect match.
   if(any(complete.cases(sims_to_include)) && 
      length(sims_to_include) == 1){
      sims_to_include <- existing_exp_details$MainDetails$File[
         str_detect(existing_exp_details$MainDetails$File, 
                    sims_to_include)]
      # At this point, sims_to_include should be a character vector of file
      # names.
   }
   
   # Keeping only the requested sims for sims_to_include
   if(any(complete.cases(sims_to_include))){
      existing_exp_details <- filter_sims(which_object = existing_exp_details, 
                                          which_sims = sims_to_include,
                                          include_or_omit = "include")
      
      # existing_exp_details should now only have the sims they requested.
      # Noting if any sims are missing.
      MissingSims <- setdiff(sims_to_include, 
                             existing_exp_details$MainDetails$File)
      if(length(MissingSims) > 0){
         warning(paste0("The following simulation files were not found in the ReleaseProfiles data for existing_exp_details:\n", 
                        str_c(MissingSims, collapse = "\n"), "They will be ignored.\n"), 
                 call. = FALSE)
      }
   } else {
      sims_to_include <- unique(existing_exp_details$ReleaseProfiles$File)
   }
   
   
   if(all(is.na(compoundsToExtract))){
      compoundsToExtract <- AllRegCompounds$CompoundID
   }
   
   # Checking for bad inputs on compounds
   BadCmpd <- tolower(compoundsToExtract)[
      which(tolower(compoundsToExtract) %in% AllRegCompounds$CompoundID == FALSE)]
   GoodCmpd <- intersect(AllRegCompounds$CompoundID, 
                         tolower(compoundsToExtract))
   
   if(length(BadCmpd) > 0){
      if(length(GoodCmpd) == 0){
         stop(wrapn(paste0("None of the compounds you entered are among the possible options for compoundsToExtract: ", 
                           str_comma(paste0("'", BadCmpd, "'")), 
                           ". Please check your inputs and try again.")), 
              call. = FALSE)
      } else {
         warning(wrapn(paste0("The following compounds are not among the possible options for compoundsToExtract: ", 
                              str_comma(paste0("'", BadCmpd, "'")), 
                              ". They will be ignored.")), 
                 call. = FALSE)
      }
   }
   compoundsToExtract <- GoodCmpd
   
   
   # Main body of function -------------------------------------------------
   
   Release <- list()
   
   for(ff in sims_to_include){
      
      Release[[ff]] <- list()
      Deets <- filter_sims(existing_exp_details, 
                           which_sims = ff, 
                           include_or_omit = "include")
      
      for(cmpd in compoundsToExtract){
         
         # Checking on whether compound was included in sim
         if(as.logical(is.na(Deets$MainDetails[
            AllRegCompounds$DetailNames[AllRegCompounds$CompoundID == cmpd]]))){
            next
         }
         
         Suffix <- AllRegCompounds$Suffix[AllRegCompounds$CompoundID == cmpd]
         
         # Different input data depending on the CR/MR input
         if(complete.cases(Deets$MainDetails[[paste0("CR_MR_Input", Suffix)]]) &&
            Deets$MainDetails[[paste0("CR_MR_Input", Suffix)]] ==
            "Weibull"){
            
            Release[[ff]][[cmpd]] <- 
               data.frame(CompoundID = cmpd, 
                          Time = seq(from = 0, 
                                     to = Deets$MainDetails$SimDuration, 
                                     length.out = 500))
            
            suppressWarnings(
               Release[[ff]][[cmpd]]$Release_mean <- 
                  pweibull(q =  Release[[ff]][[cmpd]]$Time, 
                           
                           scale = Deets$MainDetails[[paste0("ReleaseProfile_alpha", 
                                                             Suffix)]], 
                           
                           shape = Deets$MainDetails[[paste0("ReleaseProfile_beta", 
                                                             Suffix)]]) * 
                  Deets$MainDetails[[paste0("ReleaseProfile_Fmax", Suffix)]]
            )
            
            # I'm not clear on how to calculate the variation surrounding a
            # weibull function, and this is giving me errors sometimes. Not
            # showing anything but mean for Weibull release functions.
            Release[[ff]][[cmpd]] <- Release[[ff]][[cmpd]] %>% 
               mutate(ReleaseUpper = Release_mean, 
                      ReleaseLower = Release_mean)
            
            # suppressWarnings(
            #    Release[[ff]][[cmpd]]$ReleaseUpper <- 
            #       pweibull(q =  Release[[ff]][[cmpd]]$Time, 
            #                
            #                scale = Deets$MainDetails[[paste0("ReleaseProfile_alpha", 
            #                                                  Suffix)]] + 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_alpha", 
            #                                             Suffix)]] * 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_alpha_CV", 
            #                                             Suffix)]], 
            #                
            #                shape = Deets$MainDetails[[paste0("ReleaseProfile_beta", 
            #                                                  Suffix)]] +
            #                   Deets$MainDetails[[paste0("ReleaseProfile_beta", 
            #                                             Suffix)]] * 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_beta_CV", 
            #                                             Suffix)]]) * 
            #       (Deets$MainDetails[[paste0("ReleaseProfile_Fmax", Suffix)]] +
            #           Deets$MainDetails[[paste0("ReleaseProfile_Fmax", Suffix)]] * 
            #           Deets$MainDetails[[paste0("ReleaseProfile_Fmax_CV", Suffix)]])
            # )
            # 
            # suppressWarnings(
            #    Release[[ff]][[cmpd]]$ReleaseLower <- 
            #       pweibull(q =  Release[[ff]][[cmpd]]$Time, 
            #                
            #                scale = Deets$MainDetails[[paste0("ReleaseProfile_alpha", 
            #                                                  Suffix)]] - 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_alpha", 
            #                                             Suffix)]] * 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_alpha_CV", 
            #                                             Suffix)]], 
            #                
            #                shape = Deets$MainDetails[[paste0("ReleaseProfile_beta", 
            #                                                  Suffix)]] -
            #                   Deets$MainDetails[[paste0("ReleaseProfile_beta", 
            #                                             Suffix)]] * 
            #                   Deets$MainDetails[[paste0("ReleaseProfile_beta_CV", 
            #                                             Suffix)]]) * 
            #       (Deets$MainDetails[[paste0("ReleaseProfile_Fmax", Suffix)]] -
            #           Deets$MainDetails[[paste0("ReleaseProfile_Fmax", Suffix)]] * 
            #           Deets$MainDetails[[paste0("ReleaseProfile_Fmax_CV", Suffix)]])
            # )
            
            Release[[ff]][[cmpd]] <- Release[[ff]][[cmpd]] %>% 
               mutate(Release_mean = Release_mean / 100, 
                      ReleaseUpper = ReleaseUpper / 100, 
                      ReleaseLower = ReleaseLower / 100, 
                      ReleaseSD = NA, # FIXME - return to this later. This isn't going to have conventional SD, so I want to use ReleaseUpper and ReleaseLower instead. 
                      Tissue_subtype = NA, 
                      Simulated = TRUE, 
                      # placeholders only
                      Inhibitor = "none",
                      Trial = "mean",
                      Tissue = "plasma",
                      Conc_units = "ng/mL",  
                      Time_units = "hours", 
                      DoseNum = 1)
            
         } else {
            
            if(nrow(Deets$ReleaseProfiles %>% filter(CompoundID == cmpd)) == 0){next}
            
            Release[[ff]][[cmpd]] <- Deets$ReleaseProfiles %>% 
               filter(CompoundID == cmpd) %>% 
               mutate(Release_mean = Release_mean / 100,
                      ReleaseUpper = Release_mean + Release_mean * Release_CV, 
                      ReleaseLower = Release_mean - Release_mean * Release_CV, 
                      ReleaseSD = Release_mean * Release_CV, 
                      Tissue_subtype = NA, 
                      Simulated = TRUE, 
                      # placeholders only
                      Inhibitor = "none",
                      Trial = "mean",
                      Tissue = "plasma",
                      Conc_units = "ng/mL",  
                      Time_units = "hours", 
                      DoseNum = 1)
         }
      }
      
      Release[[ff]] <- bind_rows(Release[[ff]])
      
   }
   
   Release <- bind_rows(Release)
   
   
   # Adding compounds names
   suppressMessages(
      MyCompounds <- existing_exp_details$MainDetails %>% 
         select(any_of(c("File", "Substrate", "PrimaryMetabolite1", 
                         "PrimaryMetabolite2", "SecondaryMetabolite", 
                         "Inhibitor1", "Inhibitor1Metabolite", 
                         "Inhibitor2"))) %>% 
         pivot_longer(cols = -File, 
                      names_to = "DetailNames", 
                      values_to = "Compound") %>% 
         left_join(AllRegCompounds) %>% 
         select(File, CompoundID, Compound) %>% 
         filter(complete.cases(Compound))
   )
   
   suppressMessages(
      Release <- Release %>% left_join(MyCompounds)
   )
   
   # ggplot(Release, aes(x = Time, y = Release_mean, 
   #                     ymax = ReleaseUpper, ymin = ReleaseLower)) +
   #    geom_point() + geom_line() + 
   #    geom_ribbon(alpha = 0.5) + 
   #    scale_x_time() +
   #    theme_consultancy()
   
   facet1_column <- rlang::enquo(facet1_column)
   facet2_column <- rlang::enquo(facet2_column)
   colorBy_column <- rlang::enquo(colorBy_column)
   linetype_column <- rlang::enquo(linetype_column)
   
   # Including hacks to make this work
   ct_plot_overlay(
      ct_dataframe = Release, 
      figure_type = figure_type, 
      # NSE trouble: not enquo alone, not quo, not
      # substitute, but enquo plus !! here
      colorBy_column = !!colorBy_column,
      linetype_column = !!linetype_column,
      facet1_column = !!facet1_column,
      facet2_column = !!facet2_column,
      obs_to_sim_assignment = NA,
      mean_type = "arithmetic",
      linear_or_log = linear_or_log,
      color_labels = color_labels, 
      legend_label_color = legend_label_color,
      color_set = color_set,
      include_errorbars = include_errorbars, 
      errorbar_width = errorbar_width,
      linetype_labels = linetype_labels, 
      linetypes = linetypes,
      line_width = line_width,
      line_transparency = line_transparency,
      legend_label_linetype = legend_label_linetype,
      facet_ncol = facet_ncol, 
      facet_nrow = facet_nrow,
      floating_facet_scale = floating_facet_scale,
      facet_spacing = facet_spacing,
      time_range = time_range, 
      x_axis_interval = x_axis_interval,
      x_axis_label = x_axis_label,
      pad_x_axis = pad_x_axis,
      pad_y_axis = pad_y_axis,
      y_axis_limits_lin = y_axis_limits_lin,
      y_axis_limits_log = y_axis_limits_log, 
      y_axis_interval = y_axis_interval,
      y_axis_label = y_axis_label,
      hline_position = hline_position, 
      hline_style = hline_style, 
      vline_position = vline_position, 
      vline_style = vline_style,
      graph_labels = graph_labels,
      graph_title = graph_title,
      graph_title_size = graph_title_size, 
      legend_position = legend_position,
      prettify_compound_names = prettify_compound_names,
      qc_graph = FALSE,
      existing_exp_details = existing_exp_details,
      save_graph = save_graph,
      fig_height = fig_height,
      fig_width = fig_width)  
   
}
