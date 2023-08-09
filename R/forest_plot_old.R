#' Create a forest plot --  WARNING! TO BE DEPRECATED!
#'
#' Please see the newer version of the forest plot function:
#' \code{\link{forest_plot}}. \code{forest_plot_old} creates a forest plot of
#' AUC and Cmax ratios. Please use the function \code{\link{extractForestData}}
#' or \code{\link{pksummary_mult}} (set the argument "extract_forest_data" to
#' TRUE) to generate the input data for \code{forest_dataframe}. In the graph,
#' the data will be broken up by: 1) the file name, which will be graphed on
#' either the main y axis or, if you would like, a secondary y axis and 2)
#' anything you specify for \code{facet_column_x}, which will be graphed in the
#' horizontal direction.
#' \strong{If you're a little confused here or you're just the kind of person
#' who prefers to wing it rather than reading the instructions when assembling
#' furniture, we recommend skipping to the end of this help file and trying out
#' the examples to see what we mean.}
#'
#' @param forest_dataframe a data.frame with extracted forest-plot data,
#'   generated from running \code{\link{extractForestData}} on Simulator output
#'   files or a csv or Excel file with the same data. (If it's an Excel file, it
#'   must have only one tab.) If this is a forest plot of perpetrator drug-drug
#'   interactions (DDIs), the column "VictimCompound" will be what is used to
#'   label the y axis. If it is a forest plot of victim DDIs, it will be the
#'   column "PerpCompound". If, instead of the substrate or effector names, you
#'   would like some other label to appear on the y axis, please see the
#'   argument \code{y_axis_labels}.
#' @param perp_or_victim specify whether the drug of interest is a "victim" or
#'   "perpetrator". This will determine whether the graphs will be labeled on
#'   the y axis by the substrate name (for perpetrator forest plots) or by the
#'   effector name (for victim forest plots).
#' @param PKparameters optionally specify which PK parameters in
#'   \code{forest_dataframe} to use as input. If left as NA, all the PK
#'   parameters you extracted with \code{\link{extractForestData}} will be
#'   included. If you try to include a parameter that's not already present in
#'   forest_dataframe, it will be ignored. Enclose the parameters with
#'   \code{c(...)}.
#' @param include_dose_num TRUE (default) or FALSE on whether to include the
#'   dose number when listing the PK parameter. By default, the parameter will
#'   be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", but,
#'   especially if you only have a single-dose regimen, you might not want that.
#'   To make the parameters be only, e.g., "Cmax ratio" or "AUCtau ratio", set
#'   this to FALSE.
#' @param mean_type type of mean to extract; options are "geometric" (default),
#'   "arithmetic", or "median"
#' @param variability_type type of variability to show as whiskers; options are
#'   "90\% CI" (default), "95\% CI", "5th to 95th percentiles", "range",
#'   "geometric CV", "arithmetic CV", or "standard deviation" ("sd" is also ok
#'   for that last one).
#' @param observed_PK observed PK data, laid out just like
#'   \code{forest_dataframe} with the same columns except that you can omit the
#'   "File" column. Only observed data with the same perpetrator compound(s),
#'   the same victim compound(s), and the same summary statistics as in the
#'   simulated data will be included in the graph.
#' @param y_axis_column the column by which you would like to break up the y
#'   axis, e.g., \code{y_axis_column = File} (default). You must specify
#'   something here; it cannot be left blank. This will automatically replace
#'   the file names listed in \code{forest_dataframe} with the effector for
#'   victim forest plots and with the substrate for perpetrator forest plots.
#' @param y_order optionally set the order of items on the y axis. If
#'   \code{y_order} is left as NA, the y axis will be sorted according to the
#'   geometric mean AUC ratio with inhibitors on top and inducers on the bottom.
#'   If you would like to use some other order, there are four possible ways to
#'   specify this: \describe{
#'
#'   \item{"as is"}{If you're already happy with the order of things in the
#'   input data \code{forest_dataframe}, then setting \code{y_order = "as is"}
#'   will keep things in exactly the same order.}
#'
#'
#'   \item{a character vector of file names}{e.g., \code{y_order =
#'   c("myfile1.xlsx", "myfile2.xlsx")}. The file names will automatically be
#'   replaced with the compounds of interest, so, if this were a victim forest
#'   plot, what will appear on the y axis will be, e.g., "itraconazole" and
#'   "efavirenz" rather than the actual file names.}
#'
#'   \item{a character vector of the compounds of interest, named by the
#'   file}{e.g., \code{y_order = c("myfile1.xlsx" = "itraconazole",
#'   "myfile2.xlsx" = "efavirenz")}. As with the second option, what appears on
#'   the y axis is not the file name but "itraconazole" and "efavirenz". This is
#'   also a good way to specify that that compound appear \emph{exactly} the way
#'   you want, e.g., \code{y_order = c("myfile1.xlsx" = "itraconazole (strong
#'   CYP3A inhibitor)", "myfile2.xlsx" = "efavirenz (moderate CYP3A inducer)")}}
#'
#'   \item{a character vector of the compounds of interest}{e.g., \code{y_order
#'   = c("itraconazole", "efavirenz")} The compound names must be spelled
#'   perfectly to match!}} Please see the bottom of this help file for examples.
#' @param y_axis_column_secondary optionally break up the graphs along the y
#'   axis by an additional column. For example, say your drug of interest is a
#'   perpetrator and you've administered each of the substrates on different
#'   days. Here's one way you could approach that: \enumerate{\item{Make a
#'   column "DoseDay" in your data.frame that specifies which day the substrate
#'   was administered. (Please ask a member of the R Working Group for
#'   assistance if you're unsure how to do that.) Let's say you have values in
#'   that column of "Day 1", "Day 5", and "Day 14".} \item{Set
#'   \code{y_axis_column = DoseDay}} \item{Set the order of the dose days:
#'   \code{y_order = c("Day 1", "Day 5", "Day 14")}. Please note that these must
#'   perfectly match the actual values in that DoseDay column you made!}
#'   \item{Set \code{y_axis_column_secondary = File} (Nota bene: At least one of
#'   \code{y_axis_column} or \code{y_axis_column_secondary} MUST be set to
#'   File.)} \item{Last, set \code{y_order_secondary = c("buprenorphine",
#'   "repaglinide", "midazolam")}, where those are the substrates simulated in
#'   the order you want.}} This will break up the graphs first by the dose day
#'   and next by the substrate and do so in the order you specified. Unclear on
#'   how this works? Please check the examples at the bottom of this help file.
#' @param y_order_secondary the order to use for the secondary y axis
#'   (optional). Just like with the argument \code{y_order}, this may be
#'   \code{y_order_secondary = "as is"}, a character vector of the unique items
#'   in \code{y_axis_column_secondary}, or, if \code{y_axis_column_secondary} is
#'   File, the compounds that each file represents or a named character vector
#'   of the compounds. Please see the examples at the bottom of this file.
#' @param y_axis_title optionally specify a vertical title to be displayed to
#'   the left of the y axis. Example: \code{y_axis_title = "Effector
#'   co-administered with Drug X"}. Default ("none") leaves off any y-axis title.
#'   This does NOT work with a secondary y axis and will be ignored.
#' @param x_axis_limits the x axis limits to use; default is 0.06 to 12.
#' @param x_axis_number_type set the x axis number type to be "ratios"
#'   (default), "percents" (converts the ratios to a percent), or "keep trailing
#'   zeroes" (still uses ratios but, unlike the default, guesses at a reasonable
#'   number of digits to include based on the range of the data and includes
#'   trailing zeroes as necessary)
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param facet_column_x optionally break up the graph horizontally into small
#'   multiples. The designated column name should be unquoted, e.g.,
#'   \code{facet_column_x = Dose_sub}
#' @param x_order optionally specify the order in which the x-axis facets should
#'   appear. For example, if you \code{facet_column_x} is the dosing regimen and
#'   the values there are "QD" and "BID", the default will be to show them in
#'   alphabetical order. If you want "QD" to show up first, though, set the
#'   order with \code{x_order = c("QD", "BID")}
#' @param dose_units the units used in dosing. If you set \code{facet_column_x},
#'   \code{y_axis_column}, or \code{y_axis_column_secondary} to Dose_sub or
#'   Dose_inhib, setting the dose units here will automatically add those units
#'   to the appropriate graph labels. This way, the graph label will be, e.g.,
#'   "50 mg" and "100 mg" instead of just "50" and "100". This just helps make
#'   it clearer what the numbers represent. If you specify anything other than
#'   Dose_sub or Dose_inhib for \code{facet_column_x}, \code{y_axis_column}, or
#'   \code{y_axis_column_secondary}, this will be ignored.
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier. This was designed for simulations where the
#'   substrates or effectors are among the standard options for the simulator,
#'   and leaving \code{prettify_compound_names = TRUE} will make the name of
#'   those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam".
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all. \emph{Note:} We're still working on the legend position when
#'   there's something specified for \code{y_axis_column_secondary}; choosing
#'   that option requires us to lay out the graphs differently, and when we do
#'   that, the legend position doesn't work well for anything other than "none"
#'   or "right".
#' @param color_set the set of colors to use for shading the graph background to
#'   indicate the level of interaction depicted. Options are "grays" (default),
#'   "yellow to red" (makes graphs like Figure 1 of
#'   \href{https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.12864}{Chen
#'   Jones 2022 CPT, doi 10.1002/psp4.12864}), "none" for no shading at all, or
#'   a named character vector of the colors you want for each interaction level,
#'   e.g., \code{color_set = c("insignificant" = "white", "weak" = "gray90",
#'   "moderate" = "gray75", strong = "gray50")}. The cutoff values listed match
#'   those in "Clinical Drug Interaction Studies -- Cytochrome P450 Enzyme- and
#'   Transporter-Mediated Drug Interactions: Guidance for industry", US Food and
#'   Drug Administration Center for Drug Evaluation and Research, 2020, p. 19.
#' @param point_shape the shape of the points to show for the center statistic
#'   (e.g., the geometric mean). The default is to show a white triangle for
#'   observed data and a white circle for simulated data, so \code{point_shape =
#'   c(24, 21)}. Any shapes used in R graphs will work; see options by typing
#'   \code{ggpubr::show_point_shapes()} into the console. If you have both
#'   simulated and observed data, enter two shapes with the first being the
#'   shape for the observed data and the second being the shape for the
#'   simulated data. If you have only simulated data, enter only one. If you
#'   enter more shapes than your graph requires, the extras will be ignored.
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14.
#' @param pad_y_axis optionally add a smidge of padding to the y axis (default
#'   is TRUE, which includes some generally reasonable padding). If set to
#'   FALSE, the y axis tick marks will be placed closer to the top and bottom of
#'   your data. If you want a \emph{specific} amount of y-axis padding, set this
#'   to a number; the default is \code{c(0.2, 0)}, which adds 20\% more space to
#'   the bottom and nothing to the top of the y axis. If you only specify one
#'   number, we'll assume that's the percent you want added to the bottom.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". If you leave off ".png" or ".docx" from the file name, it will
#'   be saved as a png file, but if you specify a different graphical file
#'   extension, it will be saved as that file format. Acceptable graphical file
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Do not include any slashes, dollar signs, or periods in the file name.
#'   Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @return Output is a graph.
#' @export
#' @examples
#'
#' # We'll use some example forest-plot data for the substrate bufuralol
#' # with various effectors. To start, we'll just look at one dose level.
#' Buf_lowdose <- ForestData %>% filter(Dose_sub == 20)
#' forest_plot_old(forest_dataframe = Buf_lowdose,
#'             perp_or_victim = "victim")
#'
#' # If there were multiple dosing levels of your drug, though, it might be
#' # nice to break up the graph by the substrate dose like this:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub)
#'
#' # Maybe you want just one longer graph  with all the low-dose simulations
#' # on the top and the high-dose simulations on the bottom. Here's one
#' # way to do that:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = Dose_sub,
#'             y_axis_column_secondary = File)
#'
#' # Or you could break things up first by the file and then by the dose:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = File,
#'             y_axis_column_secondary = Dose_sub)
#'
#' # By default, the order of compounds will put inhibitors on top and
#' # inducers on the bottom, sorted by their AUC GMR. If you already liked
#' # the order you had things in whatever you supplied for forest_dataframe,
#' # you can tell the forest_plot_old function not to change that by setting
#' # y_order or y_order_secondary to "as is".
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = Dose_sub,
#'             y_axis_column_secondary = File,
#'             y_order_secondary = "as is")
#'
#' # Here's another way you could set the order in which the compounds appear:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = Dose_sub,
#'             y_axis_column_secondary = File,
#'             y_order_secondary = c("itraconazole", "fluvoxamine",
#'                                   "ticlopidine", "quinidine"))
#'
#' # Perhaps it would be less opaque to just list the full file names
#' # in the order you want:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = Dose_sub,
#'             y_axis_column_secondary = File,
#'             y_order_secondary = c("buf-20mg-sd-fluv-36mg-qd.xlsx",
#'                                   "buf-20mg-sd-itra-200mg-qd.xlsx",
#'                                   "buf-20mg-sd-quin-200mg-qd.xlsx",
#'                                   "buf-20mg-sd-tic-219mg-bid.xlsx",
#'                                   "buf-50mg-sd-fluv-36mg-qd.xlsx",
#'                                   "buf-50mg-sd-itra-200mg-qd.xlsx",
#'                                   "buf-50mg-sd-quin-200mg-qd.xlsx",
#'                                   "buf-50mg-sd-tic-219mg-bid.xlsx"))
#'
#' # You could also change the way the compound names appear when
#' # you list the file names as a named character vector like this:
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = Dose_sub,
#'             y_axis_column_secondary = File,
#'             y_order_secondary = c("buf-20mg-sd-fluv-36mg-qd.xlsx" = "fluvoxamine (SSRI)",
#'                                   "buf-20mg-sd-itra-200mg-qd.xlsx" = "itraonazole\n(strong CYP3A inhibitor)",
#'                                   "buf-20mg-sd-quin-200mg-qd.xlsx" = "quinidine\n(CYP2D6 inhibitor)",
#'                                   "buf-20mg-sd-tic-219mg-bid.xlsx" = "ticlodipine",
#'                                   "buf-50mg-sd-fluv-36mg-qd.xlsx" = "fluvoxamine\nwith higher dose substrate",
#'                                   "buf-50mg-sd-itra-200mg-qd.xlsx" = "itraconazole\nwith higher dose substrate",
#'                                   "buf-50mg-sd-quin-200mg-qd.xlsx" = "quinidine\n(antimalarial)",
#'                                   "buf-50mg-sd-tic-219mg-bid.xlsx" = "ticlodipine\nwith higher dose substrate"))
#' # (The "\n" means "new line".)
#'
#' # As long as there are unique values for each of the files in that named
#' # character vector, you don't need to break up your data in any other way.
#' # Here's basically the same function call as above but without
#' # y_axis_column_secondary.
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             y_axis_column = File,
#'             y_order = c("buf-20mg-sd-fluv-36mg-qd.xlsx" = "fluvoxamine (SSRI)",
#'                         "buf-20mg-sd-itra-200mg-qd.xlsx" = "itraonazole\n(strong CYP3A inhibitor)",
#'                         "buf-20mg-sd-quin-200mg-qd.xlsx" = "quinidine\n(CYP2D6 inhibitor)",
#'                         "buf-20mg-sd-tic-219mg-bid.xlsx" = "ticlodipine",
#'                         "buf-50mg-sd-fluv-36mg-qd.xlsx" = "fluvoxamine\nwith higher dose substrate",
#'                         "buf-50mg-sd-itra-200mg-qd.xlsx" = "itraconazole\nwith higher dose substrate",
#'                         "buf-50mg-sd-quin-200mg-qd.xlsx" = "quinidine\n(antimalarial)",
#'                         "buf-50mg-sd-tic-219mg-bid.xlsx" = "ticlodipine\nwith higher dose substrate"))
#'
#' # Here are some options for modifying the aesthetics of your graph:
#' # -- Adjust the x axis limits with x_axis_limits
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             x_axis_limits = c(0.9, 5))
#'
#' # -- Include a legend for the shading
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom")
#'
#' # -- Change the shading to be like in Chen Jones 2022 CPT
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = "yellow to red")
#'
#' # -- Or make the shading disappear
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = "none")
#'
#' # -- Or specify exactly which colors you want for which interaction level
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = c("negligible" = "white", "weak" = "gray90",
#'                           "moderate" = "gray75", strong = "gray50"))
#'
#' # -- Make the compound names match *exactly* what was in the simulator file
#' # rather than being automatically prettified
#' forest_plot_old(forest_dataframe = ForestData,
#'             perp_or_victim = "victim",
#'             facet_column_x = Dose_sub,
#'             prettify_compound_names = FALSE)


forest_plot_old <- function(forest_dataframe, 
                        perp_or_victim, 
                        PKparameters = NA, 
                        include_dose_num = TRUE,
                        mean_type = "geometric",
                        variability_type = "90% CI", 
                        observed_PK = NA,
                        y_axis_column = File, 
                        y_order = NA, 
                        y_axis_column_secondary,
                        y_order_secondary = NA,
                        y_axis_title = "none", 
                        x_axis_limits = NA, 
                        x_axis_label = NA,
                        x_axis_number_type = "ratios",
                        facet_column_x, 
                        x_order = NA,
                        dose_units = "mg",
                        prettify_compound_names = TRUE, 
                        legend_position = "none", 
                        color_set = "grays",
                        point_shape = c(24, 21),
                        graph_title = NA,
                        graph_title_size = 14, 
                        pad_y_axis = TRUE, 
                        save_graph = NA,
                        fig_height = 6,
                        fig_width = 5){
   
   # Error catching and data tidying ------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   if("perp_or_victim" %in% names(match.call()) == FALSE){
      stop("You must use the argument `perp_or_victim` to specify whether your compound of interest is a DDI perpetrator or a DDI victim. Otherwise, we don't know whether to break of your graph by the substrate (when it's a perpetrator) or the inhibitor (when it's a victim).")
   }
   
   
   if("character" %in% class(forest_dataframe)){
      if(str_detect(forest_dataframe, "csv$")){
         forest_dataframe <- read.csv(forest_dataframe)
      } else if(str_detect(forest_dataframe, "xlsx$")){
         forest_dataframe <- readxl::read_excel(forest_dataframe)
      } else {
         stop("It looks like you are supplying a file to be read for `forest_dataframe`, but we're not sure whether it's a csv or Excel file, so we can't read it. Please add the file extension to the file name and try again.", 
              call. = FALSE)
      }
   }
   
   if(nrow(forest_dataframe) == 0){
      stop("Please check your input. The data.frame you supplied for `forest_dataframe` doesn't have any rows.", 
           call. = FALSE)
   }
   
   if((all(c("File", "VictimCompound", "PerpCompound") %in%
           names(forest_dataframe)) |
       all(c("File", "Substrate", "Inhibitor1") %in%
           names(forest_dataframe))) == FALSE){
      stop("Please check your input for `forest_dataframe` because it does not appear to match the output from running the function `extractForestData`, and it must match that for this function to work.", 
           call. = FALSE)
   }
   
   # Cleaning up inputs
   perp_or_victim <- ifelse(str_detect(perp_or_victim, "perp"), 
                            "perpetrator", "victim")
   
   x_axis_number_type <- ifelse(str_detect(x_axis_number_type, "perc"), 
                                "percents", x_axis_number_type)
   if(length(x_axis_number_type) != 1 |
      x_axis_number_type %in% c("ratios", "percents", "keep trailing zeroes") == FALSE){
      warning("The value for `x_axis_number_type` must be `ratios`, `percents`, or `keep trailing zeroes` and you've specified something else. Please check your input. For now, we'll assume you want ratios, the default.", 
              call. = FALSE)
      x_axis_number_type <- "ratios"
   }
   
   # Noting whether user supplied observed data
   ObsIncluded <- "data.frame" %in% class(observed_PK)
   
   # If they didn't supply dose info, that's ok as long as they also didn't
   # break up graphs by that variable. Warn them if they did.
   if("Dose_sub" %in% names(forest_dataframe) == FALSE){
      if(as_label(rlang::enquo(y_axis_column_secondary)) == "Dose_sub" |
         as_label(rlang::enquo(y_axis_column)) == "Dose_sub"){
         warning("You asked to break up the graphs by the column `Dose_sub`, but that column wasn't included in your data. This is going to cause some issues with your graph.", 
                 call. = FALSE)
      } 
      forest_dataframe$Dose_sub <- NA
   }
   
   if("Dose_inhib" %in% names(forest_dataframe) == FALSE){
      if(as_label(rlang::enquo(y_axis_column_secondary)) == "Dose_inhib" |
         as_label(rlang::enquo(y_axis_column)) == "Dose_inhib"){
         warning("You asked to break up the graphs by the column `Dose_inhib`, but that column wasn't included in your data. This is going to cause some issues with your graph.", 
                 call. = FALSE)
      } 
      forest_dataframe$Dose_inhib <- NA
   }
   
   # Setting things up for nonstandard evaluation 
   facet_column_x <- rlang::enquo(facet_column_x)
   y_axis_column <- rlang::enquo(y_axis_column)
   y_axis_column_secondary <- rlang::enquo(y_axis_column_secondary)
   
   
   # Reshaping data as needed ------------------------------------------------
   # Getting data into the shape needed. Earlier versions of the
   # SimcypConsultancy package (< 1.19.1) use a wide version of
   # forest_dataframe where the names of the columns included BOTH the PK
   # parameter AND the stat, separated by two underscores. This allows for
   # either the wide or the long format for forest_dataframe to work.
   if(any(str_detect(names(forest_dataframe), "(AUC|Cmax).*?__"))){
      # This is when they supplied a wide format of the data.
      forest_dataframe <- forest_dataframe %>% 
         pivot_longer(cols = matches("^AUC.*ratio|Cmax.*ratio"), 
                      names_to = "PKParam", values_to = "Value") %>% 
         separate(PKParam, into = c("PKparameter", "Statistic"), sep = "__") %>% 
         mutate(Statistic = case_match(Statistic, 
                                       "GMR" ~ "GeoMean", 
                                       "CI90_lo"~ "CI90_Lower",
                                       "CI90_hi" ~ "CI90_Upper", 
                                       .default = Statistic)) %>% 
         pivot_wider(names_from = "Statistic", values_from = "Value")
      
      # Also reshaping observed data
      if(ObsIncluded){
         
         # Check whether obs data in long format already and reshape as needed
         if("PKparameter" %in% names(observed_PK) == FALSE){ 
            observed_PK <- observed_PK %>% 
               pivot_longer(cols = matches("^AUC.*ratio|Cmax.*ratio"), 
                            names_to = "PKParam", values_to = "Value") %>% 
               separate(PKParam, into = c("PKparameter", "Statistic"), sep = "__") %>% 
               mutate(Statistic = case_match(Statistic, 
                                             "GMR" ~ "GeoMean", 
                                             "CI90_lo"~ "CI90_Lower",
                                             "CI90_hi" ~ "CI90_Upper", 
                                             .default = Statistic)) %>% 
               pivot_wider(names_from = "Statistic", values_from = "Value")
         }
         
         # Check that observed data are matched to a specific simulation file
         ObsMismatch <- setdiff(unique(observed_PK$File), forest_dataframe$File)
         if(length(ObsMismatch) > 0){
            warning(paste0("In the provided observed PK data, the simulated file listed as matching that set of observed PK is not present in your simulated data for these files: ", 
                           str_comma(paste0("`", ObsMismatch, "`")), 
                           ". These observed data will be omitted from your graph."), 
                    call. = FALSE)
            observed_PK <- observed_PK %>% 
               filter(!File %in% ObsMismatch)
            
            if(nrow(observed_PK) == 0){
               observed_PK <- NA
            }
         }
         
         # PerpCompound and VictimCompound should be the same in the observed
         # data as in the simulated data. Checking that, warning user if they
         # differ, and using version in simulated data if there's a mismatch
         # or the compounds weren't provided. Ignoring any doses listed in
         # the observed data and just using the doses listed for those files
         # in the simulated data.
         # suppressMessages(
         #    ObsMatch <- forest_dataframe %>% 
         #       select(any_of(c("File", "PerpCompound", "VictimCompound", 
         #                       "Dose_sub", "Dose_inhib", "Tissue"))) %>% 
         #       full_join(observed_PK %>% 
         #                    select(any_of(c("File", "PerpCompound", 
         #                                    "VictimCompound", 
         #                                    "Dose_sub", "Dose_inhib", 
         #                                    "Tissue"))) %>% 
         #                    rename_with(.fn = ~ paste0(.x, "_obs")) %>% 
         #                    rename(File = File_obs))
         # )
         
         # Nope. Not going to issue a warning here. Need to come up with a
         # way to check for mismatches that also accounts for these columns
         # possibly not being present. Will need to return to this at some
         # point. For now, ignoring anything in obs data that doesn't match
         # because I'm just out of time for doing any further error catching
         # here for now.
         
         # suppressMessages(
         #    observed_PK <- observed_PK %>% 
         #       select(-any_of(c("PerpCompound", "VictimCompound",
         #                        "Dose_sub", "Dose_inhib", 
         #                        "Tissue"))) %>% 
         #       left_join(ObsMatch)
         # )
      }
   }
   
   # Dropping dose number when requested by user
   if(include_dose_num == FALSE){
      PKparameters <- sub("_dose1|_last", "", PKparameters)
      
      forest_dataframe <- forest_dataframe %>% 
         mutate(PKparameter = sub("_dose1|_last", "", PKparameter))
      
      if(ObsIncluded){
         observed_PK <- observed_PK %>% 
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter))
      }
   }
   
   # Renaming columns as needed for back-compatibility with previous package
   # versions and also noting which data was simulated. If they already have
   # PerpCompound or VictimCompound in the data, though, no need.
   if("PerpCompound" %in% names(forest_dataframe) == FALSE){
      forest_dataframe <- forest_dataframe %>% 
         rename(PerpCompound = any_of("Inhibitor1"))
   }
   
   if("VictimCompound" %in% names(forest_dataframe) == FALSE){
      forest_dataframe <- forest_dataframe %>% 
         rename(VictimCompound = any_of("Substrate"))
   }
   
   forest_dataframe <- forest_dataframe %>% 
      mutate(SimOrObs = "simulated")
   
   # If the column Tissue isn't included, then assume that the tissue was
   # plasma.
   if("Tissue" %in% names(forest_dataframe) == FALSE){
      forest_dataframe$Tissue <- "plasma"
   }
   if(ObsIncluded && "Tissue" %in% names(observed_PK) == FALSE){
      observed_PK$Tissue <- "plasma"
   }
   
   # Checking that the stats requested are available
   FDnames <- factor(names(forest_dataframe), 
                     levels = c("GeoMean", "Mean", "Median", 
                                "CI90_Lower", "CI90_Upper", 
                                "CI95_Lower", "CI95_Upper", 
                                "Centile5th_Lower","Centile95th_Upper", 
                                "Min", "Max", "SD", "GeoCV", "ArithCV"))
   FDnames <- sort(FDnames)
   CenterStat <- as.character(
      FDnames[which(FDnames %in% switch(mean_type, 
                                        "geometric" = "GeoMean", 
                                        "arithmetic" = "Mean", 
                                        "median" = "Median"))])
   if(length(CenterStat) == 0){
      CenterStat <- FDnames[which(FDnames %in% c("GeoMean", "Mean", "Median"))][1]
      
      if(length(CenterStat) == 0){
         stop("You must have geometric mean, arithmetic mean, or median data included in forest_dataframe to be able to make a forest plot.",
              call. = FALSE)
      } else {
         warning(paste0("You requested a mean_type of ", 
                        mean_type, ", but that was not available in your data. Instead, ", 
                        CenterStat, " will be used."), 
                 call. = FALSE)
      }
   }
   
   VarStat <- as.character(
      FDnames[which(FDnames %in% switch(
         variability_type, 
         "90% CI" = c("CI90_Lower", "CI90_Upper"), 
         "95% CI" = c("CI95_Lower", "CI95_Upper"), 
         "5th to 95th percentiles" = c("Centile5th_Lower", "Centile95th_Upper"), 
         "range" = c("Min", "Max"), 
         "arithmetic CV" = "ArithCV", 
         "geometric CV" = "GeoCV", 
         "sd" = "SD",
         "SD" = "SD", 
         "standard deviation" = "SD"))]
   )
   
   if(length(VarStat) == 0){
      VarStat <- FDnames[which(FDnames %in% c("CI90_Lower", "CI90_Upper", 
                                              "CI95_Lower", "CI95_Upper", 
                                              "Centile5th_Lower","Centile95th_Upper", 
                                              "Min", "Max", "SD", "GeoCV", "ArithCV"))][1]
      
      if(length(VarStat) == 0){
         stop("You must have variability data included in forest_dataframe to be able to make a forest plot. Please see the help file for acceptable types of variability input.",
              call. = FALSE)
      } else {
         warning(paste0("You requested a variability_type of ", 
                        variability_type, ", but that was not available in your data. Instead, ", 
                        switch(VarStat[1], 
                               "CI90_Lower" = "the geometric 90% confidence interval", 
                               "CI95_Lower" = "the geometric 90% confidence interval",  
                               "Centile5th_Lower" = "5th to 95th percentiles",
                               "range" = "the range",
                               "ArithCV" = "the arithmetic CV",
                               "GeoCV" = "the geometric CV",
                               "SD" = "the standard deviation", 
                               "standard deviation" = "SD"), 
                        " will be used."), 
                 call. = FALSE)
      }
   }
   
   if(any(VarStat %in% c("GeoCV"))){
      forest_dataframe <- forest_dataframe %>% 
         mutate(GeoCV_Lower = GeoMean + GeoMean * GeoCV, 
                GeoCV_Upper = GeoMean - GeoMean * GeoCV)
      
      if(ObsIncluded){
         observed_PK <- observed_PK %>% 
            mutate(GeoCV_Lower = GeoMean + GeoMean * GeoCV, 
                   GeoCV_Upper = GeoMean - GeoMean * GeoCV)
      }
      
      VarStat <- c("GeoCV_Lower", "GeoCV_Upper")
   }
   
   if(any(VarStat %in% c("ArithCV"))){
      forest_dataframe <- forest_dataframe %>% 
         mutate(ArithCV_Lower = Mean + Mean * ArithCV, 
                ArithCV_Upper = Mean - Mean * ArithCV)
      
      if(ObsIncluded){
         observed_PK <- observed_PK %>% 
            mutate(ArithCV_Lower = Mean + Mean * ArithCV, 
                   ArithCV_Upper = Mean - Mean * ArithCV)
      }
      
      VarStat <- c("ArithCV_Lower", "ArithCV_Upper")
   }
   
   if(any(VarStat %in% c("SD", "standard deviation"))){
      forest_dataframe <- forest_dataframe %>% 
         mutate(SD_Lower = Mean + SD, 
                SD_Upper = Mean - SD)
      
      if(ObsIncluded){
         observed_PK <- observed_PK %>% 
            mutate(SD_Lower = Mean + SD, 
                   SD_Upper = Mean - SD)
      }
      
      VarStat <- c("SD_Lower", "SD_Upper")
   }
   
   # Checking whether sim and obs stats match
   if(ObsIncluded){
      
      observed_PK <- observed_PK %>% 
         mutate(SimOrObs = "observed") # %>% 
      # # Checking whether the compounds match between simulated and observed
      # # and dropping anything that doesn't match.
      # filter(VictimCompound %in% forest_dataframe$VictimCompound &
      #           PerpCompound %in% forest_dataframe$PerpCompound) 
      
      # Nope. Not doing that. If they have altered the names of the compounds,
      # which is very reasonable and possible, then this won't match. They'll be
      # able to figure out that things are incorrect if they get a row in the
      # graph where there are obs data and no sim, and that will be easier to
      # troubleshoot than invisibly removing obs data here.
      
      CenterStat_obs <- names(observed_PK)[
         which(names(observed_PK) %in% c("GeoMean", "Mean", "Median"))]
      VarStat_obs <- names(observed_PK)[
         which(names(observed_PK) %in% c("CI90_Lower", "CI95_Lower", "SD_Lower",
                                         "Centile5th_Lower", "Min"))]
      
      if(any(CenterStat_obs %in% CenterStat) == FALSE | 
         any(VarStat_obs %in% VarStat) == FALSE){
         warning("You have different statistics in your simulated and observed data, so we cannot compare them. We are dropping the observed data for now. Please check your input.", 
                 call. = FALSE)
         ObsIncluded <- FALSE
      } else {
         forest_dataframe <- forest_dataframe %>% bind_rows(observed_PK)
      }
   } 
   
   # y_axis_column must not be empty.
   if(as_label(rlang::enquo(y_axis_column)) == "<empty>"){
      stop("Something must be specified for `y_axis_column` for the forest_plot_old function to work.", 
           call. = FALSE)
   }
   
   # Either y_axis_column or y_axis_column_secondary must be File. Checking
   # for that.
   if(as_label(rlang::enquo(y_axis_column_secondary)) != "File" &
      as_label(rlang::enquo(y_axis_column)) != "File"){
      stop("Either `y_axis_column` or `y_axis_column_secondary` must be set to the simulator file for the forest_plot_old function to work.", 
           call. = FALSE)
   }
   
   # Checking for when there might be multiple files for the same substrate or
   # inhibitor names
   FacetSec <- paste(as_label(rlang::enquo(facet_column_x)) == "<empty>", 
                     as_label(rlang::enquo(y_axis_column_secondary)) == "<empty>")
   
   Check <- forest_dataframe %>%
      mutate(Compound = switch(perp_or_victim,
                               "perpetrator" = VictimCompound,
                               "victim" = PerpCompound)) %>% 
      select(File, Compound, VictimCompound, PerpCompound, 
             Dose_sub, Dose_inhib, Tissue) %>% unique()
   
   if(FacetSec == "TRUE TRUE"){
      # user is not faceting and no sec y
      Check <- Check %>%
         group_by(File, Compound)
   } else if(FacetSec == "FALSE TRUE"){
      # user *is* faceting, no sec y
      Check <- Check %>% 
         group_by(across(.cols = any_of(c("File", "Compound",
                                          as_label(facet_column_x)))))
   } else if(FacetSec == "FALSE FALSE"){
      # user is faceting, there is a sec y
      Check <- Check %>% 
         group_by(across(.cols = any_of(c("File", "Compound",
                                          as_label(facet_column_x),
                                          as_label(y_axis_column_secondary)))))
   } else {
      # user is not faceting, but there is a sec y: "TRUE FALSE"
      Check <- Check %>% 
         group_by(across(.cols = any_of(c("File", "Compound",
                                          as_label(y_axis_column_secondary)))))
   }
   
   Check <- Check %>% summarize(N = n())
   
   if(any(complete.cases(y_order)) && y_order[1] == "as is"){
      y_order <- unique(forest_dataframe$File)
   }
   
   if(any(complete.cases(y_order_secondary)) && y_order_secondary[1] == "as is"){
      y_order_secondary <- unique(forest_dataframe$File)
   }
   
   if(is.na(y_order[1]) & any(Check$N > 1)){
      stop(paste0("You have more than one file per " ,
                  switch(perp_or_victim, 
                         "perpetrator" = "substrate", 
                         "victim" = "effector"),
                  " for the compounds ",
                  str_comma(Check$Compound[which(Check$N > 1)]),
                  ". Did you specify `perp_or_victim` correctly? You *can* have more than one file per ",
                  switch(perp_or_victim, 
                         "perpetrator" = "substrate", 
                         "victim" = "effector"),
                  ", but only if you break up your graphs using `facet_column_x` or `y_axis_secondary` or if you specify something for `y_order` so that it's clear what y axis labels you want to use for each of those files."),
           call. = FALSE)
   }
   
   if(complete.cases(y_order[1])){
      if(is.null(names(y_order)) == FALSE && 
         any(forest_dataframe$File %in% names(y_order) == FALSE)){
         warning(paste0("The file(s) ", 
                        str_comma(setdiff(forest_dataframe$File, 
                                          names(y_order))), 
                        " are not included in `y_order`, so they will be labeled as NA on the y axis unless that's fixed."), 
                 call. = FALSE)
      }
      
      if(is.null(names(y_order)) == FALSE & any(str_detect(y_order, "xlsx"))){
         warning("If you supply a named character vector for `y_order` to indicate what order the files should be and what compound they represent, then the files should be used as the names of the vector and the compound should be used as the values, e.g., `c('MyFile1.xlsx' = 'Compound A', 'MyFile2.xlsx' = 'Compound B')`. We will swap those for you.", 
                 call. = FALSE)
         temp <- y_order
         y_order <- names(temp)
         names(y_order) <- temp
         rm(temp)
      }
   }
   
   if(class(prettify_compound_names) != "logical"){
      warning("You appear to have supplied something to the argument `prettify_compound_names` other than TRUE or FALSE. Unfortunately, those are the only permissible values. We'll set this to TRUE.", 
              call. = FALSE)
      prettify_compound_names <- TRUE
   }
   
   # Making legend argument lower case to avoid case sensitivity
   legend_position <- tolower(legend_position)
   
   # If color_set is "none" and no observed data are included, then remove the
   # legend.
   if(length(color_set) == 1 && color_set == "none" & ObsIncluded == FALSE){
      legend_position <- "none"
   }
   
   if((length(color_set) == 1 &&
       color_set %in% c("none", "grays", "yellow to red") == FALSE) |
      (length(color_set) > 1 && length(color_set) != 4)){
      warning("Acceptable input for `color_set` is `grays`, `yellow to red`, `none`, or a named character vector of the colors you want for each interaction level (see examples in help file), and your input was not among those options. We'll use the default, `grays`, for now.", 
              call. = FALSE)
      
      color_set <- "grays"
   }
   
   if(legend_position %in% c("none", "bottom", "left", "right", "top") == FALSE){
      warning(paste0("You listed `", legend_position, 
                     "` for the legend position, which is not among the permissible options. The default of no legend (legend_position = `none`) will be used."),
              call. = FALSE)
      legend_position <- "none"
   }
   
   
   # Main body of function -------------------------------------------------
   # Adjusting forest_dataframe to get only the data we want
   forest_dataframe <- forest_dataframe %>% 
      select(File, VictimCompound, Dose_sub, PerpCompound, 
             Dose_inhib, Tissue, PKparameter, SimOrObs,
             any_of(as_label(facet_column_x)), 
             any_of(as_label(y_axis_column)),
             any_of(as_label(y_axis_column_secondary)),
             any_of(CenterStat), any_of(VarStat))
   
   if(any(complete.cases(PKparameters)) &&
      any(PKparameters %in% forest_dataframe$PKparameter) == FALSE){
      stop("None of the PK parameters requested are present in the data.frame supplied for `forest_dataframe`. No graph can be made.", 
           call. = FALSE)
   }
   
   # Making the data.frame work with all the stat types we included in
   # extractForestData.
   names(forest_dataframe)[names(forest_dataframe) == CenterStat] <- "Centre"
   names(forest_dataframe)[names(forest_dataframe) == VarStat[1]] <- "Lower"
   names(forest_dataframe)[names(forest_dataframe) == VarStat[2]] <- "Upper"
   
   # Setting up a column in the DF for the compound of interest and prettifying
   # as desired by user.
   forest_dataframe <- forest_dataframe %>% 
      mutate(MyCompound_orig = switch(perp_or_victim,
                                      "perpetrator" = VictimCompound,
                                      "victim" = PerpCompound),
             MyCompound = switch(as.character({{prettify_compound_names}}), 
                                 "TRUE" = prettify_compound_name(MyCompound_orig), 
                                 "FALSE" = MyCompound_orig))
   
   # Making sure that the data classes are correct
   suppressWarnings(
      forest_dataframe <- forest_dataframe %>% 
         mutate(across(.cols = c(Centre, Lower, Upper), 
                       .fns = as.numeric))
   )
   
   # Only including the PK parameters they requested when that's specified
   if(complete.cases(PKparameters[1])){
      forest_dataframe <- forest_dataframe %>% 
         filter(PKparameter %in% {{PKparameters}})
   }
   
   forest_dataframe <- forest_dataframe %>% 
      # Graphing this is easiest if the levels start with the item we want on
      # the bottom of the y axis and work upwards.
      mutate(PKparameter = factor(PKparameter, levels = c("Cmax_ratio",
                                                          "Cmax_ratio_last", 
                                                          "AUCtau_ratio",
                                                          "AUCtau_ratio_last", 
                                                          "Cmax_ratio_dose1", 
                                                          "AUCt_ratio",
                                                          "AUCt_ratio_dose1", 
                                                          "AUCinf_ratio",
                                                          "AUCinf_ratio_dose1")))
   
   # Only use PK parameters where there are all complete cases. 
   ParamToUse <- forest_dataframe %>% select(PKparameter, Centre) %>% 
      group_by(PKparameter) %>% 
      summarize(Centre = any(complete.cases(Centre))) %>% 
      filter(Centre == TRUE) %>% pull(PKparameter) %>% as.character()
   
   if(all(complete.cases(PKparameters)) && 
      all(PKparameters %in% ParamToUse == FALSE)){
      warning(paste0("Not all of your supplied PK parameters had complete values, and only parameters with all complete values can be included here. The PK parameters with missing values, which will not be included in the graph, were: ", 
                     str_comma(setdiff(PKparameters, ParamToUse))), 
              call. = FALSE)
      
      forest_dataframe <- forest_dataframe %>% 
         filter(PKparameter %in% intersect(PKparameters, ParamToUse))
   }
   
   
   ## Figuring out y axes and y axis orders ---------------------------------
   # Things don't work if more than one thing is mapped to File.
   if(as_label(y_axis_column) == "File" & 
      as_label(y_axis_column_secondary) == "File"){
      warning("You can only have one of `y_axis_column` or `y_axis_column_secondary` set to File, and you have both. We're going to ignore your settings for `y_axis_column_secondary`.", 
              call. = FALSE)
      y_axis_column_secondary <- as_quosure(NULL)
   }
   
   # Making a vector generally and then a column in forest_dataframe that
   # specifies the order of files. This could be the y_axis_column and y_order
   # or it could be y_axis_column_secondary and y_order_secondary, so we need
   # it to be flexible here.
   if(as_label(y_axis_column) == "File"){
      if(any(complete.cases(y_order))){
         YFileOrder <- y_order
      } else {
         YFileOrder <- "tbd"
      }
   } else {
      # y_axis_column_secondary is File.
      if(any(complete.cases(y_order_secondary))){
         YFileOrder <- y_order_secondary
      } else {
         YFileOrder <- "tbd"
      }
   }
   
   if(length(YFileOrder) == 1 && YFileOrder == "tbd"){
      # Determining order of files/compounds if unspecified
      YCmpdOrder <- forest_dataframe %>%
         select(MyCompound, PKparameter, Centre) %>% 
         filter(PKparameter %in% ParamToUse) %>% 
         group_by(MyCompound, PKparameter) %>% 
         summarize(Centre = max(Centre, na.rm = T)) %>% 
         pivot_wider(names_from = PKparameter, values_from = Centre) %>% 
         arrange(across(any_of(c("AUCinf_ratio_dose1", "AUCinf_ratio",
                                 "AUCt_ratio_dose1", "AUCtau_ratio_last",
                                 "AUCtau_ratio", "Cmax_ratio",
                                 "Cmax_ratio_dose1", "Cmax_ratio_last")))) %>% 
         pull(MyCompound) %>% rev() %>% unique()
      
      forest_dataframe <- forest_dataframe %>%
         mutate(MyCompound = factor(MyCompound, levels = YCmpdOrder)) %>% 
         arrange(MyCompound) %>% 
         mutate(File = factor(File, levels = unique(File)))
      # File and MyCompound are both now factor.
   } else {
      # this is when the user *has* specified y_order or y_order_secondary, so
      # checking on *how* user specified order. They may have specified file
      # names or specified the compounds that each file represents.
      # Furthermore, they may have specified files using the names of a
      # character vector. Dealing with each of those three scenarios.
      if(any(str_detect(names(YFileOrder), "xlsx"))){
         # When the file names are specified
         suppressMessages(
            forest_dataframe <- forest_dataframe %>% 
               mutate(File = factor(File, levels = names(YFileOrder))) %>% 
               arrange(File) %>% 
               left_join(data.frame(File = names(YFileOrder),
                                    MyCompound_rev = YFileOrder)) %>% 
               mutate(MyCompound = factor(MyCompound_rev, levels = unique(MyCompound_rev))))
         # File and MyCompound are both now factor. 
         
      } else if(any(str_detect(YFileOrder, "xlsx"))){
         # When the file names are not specified but the files
         # themselves are
         forest_dataframe <- forest_dataframe %>% 
            mutate(File = factor(File, levels = YFileOrder)) %>% 
            arrange(File) %>% 
            mutate(MyCompound = factor(MyCompound, levels = unique(MyCompound)))
         # File and MyCompound are both now factor. 
         
      } else {
         # This is when they have gone straight to the compounds that
         # each file represents and just listed them.
         
         # Check whether the items they listed are among the compounds or
         # the prettified compounds.
         if(any(YFileOrder %in% forest_dataframe$MyCompound)){
            forest_dataframe <- forest_dataframe %>% 
               mutate(MyCompound = factor(MyCompound, 
                                          levels = YFileOrder)) %>% 
               arrange(MyCompound) %>% 
               mutate(File = factor(File, levels = unique(File)))
            # File and MyCompound are both now factor. 
            
         } else {
            # compound names listed are not prettified
            forest_dataframe <- forest_dataframe %>% 
               mutate(MyCompound_orig = factor(MyCompound_orig, 
                                               levels = YFileOrder)) %>% 
               arrange(MyCompound_orig) %>% 
               mutate(MyCompound = factor(MyCompound, 
                                          levels = unique(MyCompound)), 
                      File = factor(File, levels = unique(File)))
            # File and MyCompound are both now factor.
         }
      }
   }
   
   # Now that we know what the y axis columns should be, setting them for each
   # possible scenario.
   if(as_label(y_axis_column) == "File"){
      # This is when the primary y axis grouping is by File.
      forest_dataframe <- forest_dataframe %>% 
         mutate(YCol = MyCompound)
      # Note that YCol is MyCompound, which is already a factor and thus in order.
      if(as_label(y_axis_column_secondary) != "<empty>"){
         forest_dataframe <- forest_dataframe %>% 
            mutate(Y2Col = !!y_axis_column_secondary)
         if(any(complete.cases(y_order_secondary))){
            forest_dataframe <- forest_dataframe %>% 
               mutate(Y2Col = factor(Y2Col, levels = y_order_secondary))
         }
      }
   } else {
      # This is when the primary y axis grouping is NOT by File, which means
      # that the secondary grouping MUST be by File. The primary y axis
      # grouping will be by whatever the user specified.
      forest_dataframe <- forest_dataframe %>% 
         mutate(Y2Col = MyCompound, 
                YCol = !!y_axis_column)
      # Note that Y2Col is File, which is already a factor and thus in order.
      if(any(complete.cases(y_order))){
         forest_dataframe <- forest_dataframe %>% 
            mutate(YCol = factor(YCol, levels = y_order))
      }
   }
   
   
   ##  Dealing with possible facet_column_x --------------------------------
   if(as_label(facet_column_x) != "<empty>"){
      forest_dataframe <- forest_dataframe %>%
         mutate(FCX = !!facet_column_x)
      
      if(any(complete.cases(x_order))){
         forest_dataframe <- forest_dataframe %>% 
            mutate(FCX = factor(FCX, levels = x_order))
      } else if(as_label(facet_column_x) %in% c("Dose_sub", "Dose_inhib")){
         forest_dataframe <- forest_dataframe %>% 
            mutate(FCX = paste(FCX, {{dose_units}}), 
                   FCX = forcats::fct_reorder(.f = FCX, 
                                              .x = !!facet_column_x,
                                              .fun = min))
      }
   }
   
   ## Setting up other stuff for graphing ----------------------------------
   
   # Yes, the set of numbers below will introduce infinite values in a
   # continuous x axis, and no, I can't seem to get ggplot2 to stop telling me
   # that, but this works to get the right shading. 
   Rect <- data.frame(Xmin = c(1.25, 2,   5,   0.5, 0.2, 0,   0.8), 
                      Xmax = c(2,    5, Inf,   0.8, 0.5, 0.2, 1.25), 
                      Ymin = -Inf, Ymax = Inf, 
                      IntLevel = c("weak", "moderate", "strong", 
                                   "weak", "moderate", "strong", "negligible")) %>% 
      mutate(IntLevel = factor(IntLevel, 
                               levels = c("negligible", "weak", "moderate", 
                                          "strong")))
   if(length(color_set) == 1){
      FillColor <- switch(color_set, 
                          "grays" = c("negligible" = "white",
                                      "weak" = "gray95", 
                                      "moderate" = "gray90",
                                      "strong" = "gray75"), 
                          "yellow to red" = c("negligible" = "white", 
                                              "weak" = "#FFFF95",
                                              "moderate" = "#FFDA95",
                                              "strong" = "#FF9595"), 
                          "none" = c("negligible" = "white", 
                                     "weak" = "white",
                                     "moderate" = "white",
                                     "strong" = "white"))
   } else {
      FillColor <- color_set
      
      # If the user did not properly name the vector, fix that.
      if(any(names(color_set) != c("negligible", "weak", "moderate", 
                                   "strong"))){
         names(color_set) <- c("negligible", "weak", "moderate", 
                               "strong")
      }
   }
   
   if(is.na(x_axis_limits[1])){
      GraphRange <- forest_dataframe %>% ungroup() %>% 
         select(Centre, Lower, Upper) %>% stack()
      GraphRange <- range(GraphRange$values, na.rm = T)
      
      x_axis_limits <- c(round_down(x = GraphRange[1]), 
                         round_up(x = GraphRange[2]))
      
   }
   
   XBreaks <- c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 
                1.25, 2, 5, 10, 
                50, 100, 500, 1000)
   names(XBreaks) <- c("0.001", "0.01", "0.05", "0.1", "0.2", "0.5", "0.8", 
                       "1.25", "2", "5", "10", 
                     "50", "100", "500", "1000")
   XBreaks <- XBreaks[XBreaks >= x_axis_limits[1] & 
                         XBreaks <= x_axis_limits[2]]
   
   if(x_axis_limits[1] %in% XBreaks == FALSE){
      XBreaks <- c(XBreaks, x_axis_limits[1])
      names(XBreaks)[length(XBreaks)] <- as.character(x_axis_limits[1])
      XBreaks <- sort(XBreaks)
   }
   
   if(x_axis_limits[2] %in% XBreaks == FALSE){
      XBreaks <- c(XBreaks, x_axis_limits[2])
      names(XBreaks)[length(XBreaks)] <- as.character(x_axis_limits[2])
      XBreaks <- sort(XBreaks)
   }
   
   # Tweaking y axis positions based on whether obs data included
   forest_dataframe <- forest_dataframe %>%
      mutate(PKparameter = droplevels(PKparameter),
             # Multiplying to add a little more space between parameters
             # so there's more room for errorbars w/out overlapping
             PKParam_num = as.numeric(PKparameter)* 1.5) 
   
   if(ObsIncluded){
      if(as_label(facet_column_x) == "PKparameter"){
         
         # Adjusting y position if there are any obs data.
         ObsCheck <- forest_dataframe %>% #filter(SimOrObs == "observed") %>% 
            group_by(YCol) %>% 
            summarize(Ylen = length(unique(SimOrObs)))
         
         forest_dataframe <- forest_dataframe %>%
            left_join(ObsCheck, by = "YCol") %>% 
            mutate(YCol_num = as.numeric(YCol),
                   YCol_num = case_when(Ylen == 1 ~ YCol_num, 
                                        Ylen == 2 & SimOrObs == "simulated" ~ YCol_num - 0.2, 
                                        Ylen == 2 & SimOrObs == "observed" ~ YCol_num + 0.2))
         
      } else {
         forest_dataframe <- forest_dataframe %>% 
            mutate(PKParam_num = ifelse(SimOrObs == "simulated", 
                                        PKParam_num - 0.2, PKParam_num + 0.2))
      }
   }
   
   # Adding padding if user requests it or trying to come up with a reasonable
   # amount if not
   if(class(pad_y_axis) == "logical"){ # class is logical if pad_y_axis unspecified
      if(pad_y_axis){
         pad_y_num <-  switch(paste(ObsIncluded, length(ParamToUse)), 
                              "FALSE 1" = 0,
                              "FALSE 2" = 0.5, 
                              "FALSE 3" = 0.25, 
                              "FALSE 4" = 0.2, 
                              "FALSE 5" = 0.2,
                              "TRUE 1" = 0.2, 
                              "TRUE 2" = 0.3, 
                              "TRUE 3" = 0.2, 
                              "TRUE 4" = 0.15, 
                              "TRUE 5" = 0.2)
      } else {
         pad_y_num <- 0
      }
   } else {
      # User has provided specific values to expand by
      pad_y_num <- pad_y_axis
      
      # Assuming that they want the expansion to be symmetric unless they
      # specify a 2nd value
      if(length(pad_y_axis) > 1){
         pad_y_num <- pad_y_axis[1:2]
      }
   }
   
   # Figuring out what x axis title should be
   XTitle <- paste(switch(CenterStat, 
                          "GeoMean" = "Geometric Mean Ratio", 
                          "Mean" = "Arithmetic Mean Ratio", 
                          "Median" = "Ratio of Medians"), 
                   switch(VarStat[1], 
                          "CI90_Lower" = "(90% Confidence Interval)", 
                          "CI95_Lower" = "(95% Confidence Interval)", 
                          "Centile5th_Lower" = "(5th to 95th Percentiles)", 
                          "Min" = "(Range)",
                          "SD_Lower" = "(Standard Deviation)", 
                          "GeoCV" = "GeoCV NOT YET SET UP",
                          "ArithCV" = "ArithCV NOT YET SET UP"))
   
   # Assigning shapes
   if(length(point_shape) == 1){
      MyShapes <- c(point_shape, point_shape)
   } else if(length(point_shape > 1)){
      MyShapes <- point_shape[1:2]
   }
   
   names(MyShapes) <- c("observed", "simulated")
   
   
   # Graph w/y_order_secondary column specified --------------------------
   if(as_label(y_axis_column_secondary) != "<empty>"){
      # This is when the user wants to have an additional y axis grouping s/a
      # day of dosing or prandial state or something like that. 
      
      if(as_label(facet_column_x) == "PKparameter"){
         warning("You have requested that we break up the graphs by PK parameter (facet_column_x = PKparameter) and also have requested a secondary y axis. We're sorry, but the forest_plot_old function is not currently set up to do both. We will not be able to use anything for facet_column_x here.", 
                 call. = FALSE)
         facet_column_x <- as_quosure(NULL)
      }
      
      if(as_label(y_axis_column_secondary) %in% c("Dose_sub", "Dose_inhib")){
         forest_dataframe <- forest_dataframe %>% 
            mutate(Y2Col = paste(Y2Col, {{dose_units}}), 
                   Y2Col = forcats::fct_reorder(.f = Y2Col, 
                                                .x = !!y_axis_column_secondary,
                                                .fun = min))
      }
      
      if(as_label(y_axis_column) %in% c("Dose_sub", "Dose_inhib")){
         forest_dataframe <- forest_dataframe %>% 
            mutate(YCol = paste(YCol, {{dose_units}}), 
                   YCol = forcats::fct_reorder(.f = YCol, 
                                               .x = !!y_axis_column,
                                               .fun = min))
      }
      
      G <- list()
      
      for(g in unique(forest_dataframe$YCol)){
         
         Forest_subset <- forest_dataframe %>% filter(YCol == g)
         
         G[[g]] <- suppressWarnings(
            ggplot(Forest_subset, aes(x = Centre, xmin = Lower, xmax = Upper, 
                                      y = PKParam_num, shape = SimOrObs)) +
               geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax, ymin = Ymin, 
                                          ymax = Ymax, fill = IntLevel), 
                         inherit.aes = FALSE) +
               scale_fill_manual(values = FillColor) +
               geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
               geom_errorbar(width = 0.3) +
               geom_point(size = 2.5, fill = "white") +
               scale_shape_manual(values = MyShapes) +
               scale_y_continuous(breaks = as.numeric(sort(unique(forest_dataframe$PKparameter))) * 1.5,
                                  labels = sapply(PKexpressions[levels(forest_dataframe$PKparameter)], FUN = `[`), 
                                  expand = expansion(mult = pad_y_num)) +
               labs(fill = "Interaction level", shape = NULL) +
               # scale_x_log10(breaks = XBreaks) + # this results in warnings I can't suppress
               scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 0.01, base = 10),
                                  breaks =  XBreaks, 
                                  labels = switch(x_axis_number_type, 
                                                  "ratios" = names(XBreaks),
                                                  "keep trailing zeroes" = scales::label_comma(), 
                                                  "percents" = scales::percent)) + 
               coord_cartesian(xlim = x_axis_limits) +
               xlab(ifelse(is.na(x_axis_label), XTitle, x_axis_label)) + 
               ylab(unique(Forest_subset$YCol)) +
               theme_consultancy() +
               theme(plot.margin = margin(0, 1, 0, 1),
                     legend.position = legend_position,
                     panel.background = element_rect(fill = "white", color = NA),
                     plot.background = element_rect(fill = "white", color = NA),
                     strip.background = element_rect(color=NA, fill="white"),
                     # strip.text = element_text(face = "bold"),
                     strip.text.y.left = element_text(angle = 0),
                     strip.placement = "outside",
                     panel.border = element_rect(colour = "grey70", fill = NA),
                     panel.spacing.y = unit(0, "cm"),
                     panel.spacing.x = unit(0.5, "cm"), 
                     axis.line.x.bottom = element_blank(),
                     axis.line.y.left = element_blank())
         )
         
         if(g != unique(forest_dataframe$YCol)[
            length(unique(forest_dataframe$YCol))]){
            
            G[[g]] <- G[[g]] +
               theme(axis.text.x = element_blank(), 
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank())
         }
         
         if(as_label(facet_column_x) != "<empty>"){
            G[[g]] <- G[[g]] + facet_grid(Y2Col ~ FCX, switch = "y") 
         } else {
            G[[g]] <- G[[g]] + facet_grid(Y2Col ~ ., switch = "y") 
         }
         
         if(ObsIncluded == FALSE){
            G[[g]] <- G[[g]] + guides(shape = "none")
         }
         
         rm(Forest_subset)
      }
      
      G <- patchwork::wrap_plots(G, ncol = 1, guides = "collect")
      
      if(complete.cases(graph_title)){
         G <- G + patchwork::plot_annotation(
            title = graph_title, 
            theme = theme(plot.title = element_text(hjust = 0.5, 
                                                    size = graph_title_size)))
      }
      
      
   } else {
      
      # Graph w/out y order secondary y axis -------------------------------
      if(as_label(y_axis_column) == "File"){
         forest_dataframe <- forest_dataframe %>% 
            mutate(YCol = MyCompound)
      }
      
      if(as_label(facet_column_x) == "PKparameter"){
         # If user wants to facet by the PK parameter, that's a special case
         # b/c we need to change what we're using for the y axis. NB:
         # Function is NOT set up for allowing for a secondary y axis when
         # facet_column_x is PKparameter, so y must be mapped to
         # y_axis_column at this point in the function. -LSh
         
         Param_exp <- c("Cmax_ratio_last" = PKexpressions[["Cmax_ratio_last"]], 
                        "Cmax_ratio" = PKexpressions[["Cmax_ratio"]],
                        "AUCtau_ratio_last" = PKexpressions[["AUCtau_ratio_last"]], 
                        "AUCtau_ratio" = PKexpressions[["AUCtau_ratio"]],
                        "Cmax_ratio_dose1" = PKexpressions[["Cmax_ratio_dose1"]],
                        "AUCt_ratio_dose1" = PKexpressions[["AUCt_ratio_dose1"]], 
                        "AUCt_ratio" = PKexpressions[["AUCt_ratio"]],
                        "AUCinf_ratio_dose1" = PKexpressions[["AUCinf_ratio_dose1"]], 
                        "AUCinf_ratio" = PKexpressions[["AUCinf_ratio"]])
         
         forest_dataframe <- forest_dataframe %>% 
            mutate(PKparameter_exp = factor(PKparameter, 
                                            levels = names({{Param_exp}}), 
                                            labels = {{Param_exp}}))
         
         G <- ggplot(forest_dataframe, aes(x = Centre, xmin = Lower, xmax = Upper, 
                                           y = YCol_num, shape = SimOrObs)) +
            geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax, 
                                       ymin = Ymin, ymax = Ymax, fill = IntLevel), 
                      inherit.aes = FALSE) +
            geom_hline(yintercept = seq(from = 1.5, by = 1, 
                                        to = length(levels(forest_dataframe$YCol)) - 0.5), 
                       color = "gray70", linewidth = 0.5) +
            scale_fill_manual(values = FillColor) +
            geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
            geom_errorbar(width = 0.3) +
            geom_point(size = 2.5, fill = "white") +
            facet_grid(. ~ PKparameter_exp, labeller = label_parsed) +
            scale_shape_manual(values = MyShapes) +
            scale_y_reverse(breaks = unique(as.numeric(forest_dataframe$YCol)), 
                            labels = levels(forest_dataframe$YCol),
                            expand = expansion(mult = pad_y_num/10)) + # pad_y_num should be smaller here
            labs(fill = "Interaction level", shape = NULL) +
            theme(axis.text.y = element_text(hjust = 0.5, face = "bold"), 
                  axis.ticks.y = element_blank())
         
      } else {
         
         G <- ggplot(forest_dataframe, aes(x = Centre, xmin = Lower, xmax = Upper, 
                                           y = PKParam_num, shape = SimOrObs)) +
            geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax, 
                                       ymin = Ymin, ymax = Ymax, fill = IntLevel), 
                      inherit.aes = FALSE) +
            scale_fill_manual(values = FillColor) +
            geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
            geom_errorbar(width = 0.3) +
            geom_point(size = 2.5, fill = "white") +
            scale_shape_manual(values = MyShapes) +
            scale_y_continuous(breaks = as.numeric(sort(unique(forest_dataframe$PKparameter))) * 1.5,
                               labels = sapply(PKexpressions[levels(forest_dataframe$PKparameter)], FUN = `[`), 
                               expand = expansion(mult = pad_y_num)) +
            labs(fill = "Interaction level", shape = NULL)
         
         if(as_label(facet_column_x) != "<empty>"){
            G <- G + facet_grid(YCol ~ FCX, switch = "y") 
         } else {
            G <- G + facet_grid(YCol ~ ., switch = "y") 
         }
      }
      
      if(ObsIncluded == FALSE){
         G <- G + guides(shape = "none")
      }
      
      G <- G +
         # scale_x_log10(breaks = XBreaks) + # this results in warnings I can't suppress
         scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 0.01, base = 10),
                            breaks =  XBreaks, 
                            labels = switch(x_axis_number_type, 
                                            "ratios" = names(XBreaks),
                                            "keep trailing zeroes" = scales::label_comma(),  
                                            "percents" = scales::label_percent())) + 
         coord_cartesian(xlim = x_axis_limits) +
         xlab(ifelse(is.na(x_axis_label), XTitle, x_axis_label)) + 
         ylab(switch(as.character(y_axis_title == "none"), 
                     "TRUE" = NULL, 
                     "FALSE" = y_axis_title)) +
         theme_consultancy() +
         theme(
            axis.line.x.bottom = element_blank(), 
            axis.line.y.left = element_blank(),
            legend.position = legend_position, ### KEEP THIS
            strip.text = element_text(face = "bold"),
            strip.text.y.left = element_text(angle = 0),
            strip.placement = "outside",
            panel.border = element_rect(colour = "grey70", fill = NA),
            panel.spacing.y = unit(0, "cm"),
            panel.spacing.x = unit(0.5, "cm"))
      
      if(complete.cases(graph_title)){
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
         Ext <- "png"
      }
      
      suppressWarnings(
         ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                plot = G)
      )
   }
   
   return(G)
}


