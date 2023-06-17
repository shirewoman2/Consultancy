#' Create a forest plot
#'
#' \code{forest_plot} creates a forest plot of AUC and Cmax ratios. Please use
#' the function \code{\link{extractForestData}} or \code{\link{pksummary_mult}}
#' (set the argument "extract_forest_data" to TRUE) to generate the input data
#' for \code{forest_dataframe}. In the graph, data will be broken up on the y
#' axis by the simulation file name and, optionally, anything you specify for
#' \code{facet_column_x}, which will be broken up along the x axis. Since file
#' names do not generally make the most lovely of y axis labels, please use the
#' argument \code{y_axis_labels} to specify how you'd like your y
#' axis to look. \strong{If you're a little confused here or you're just the
#' kind of person who prefers to wing it rather than reading the instructions
#' when assembling furniture, we recommend skipping to the end of this help file
#' and trying out the examples to see what we mean.}
#'
#' @param forest_dataframe a data.frame with extracted forest-plot data,
#'   generated from running \code{\link{extractForestData}} on Simulator output
#'   files or a csv or Excel file with the same data. (If it's an Excel file, it
#'   must have only one tab.)
#' @param y_axis_labels a column in \code{forest_dataframe} (unquoted) or a
#'   named character vector (each item in quotes) to use for labeling the
#'   simulations on the y axis. In all forest plots, the y axis will be broken
#'   up by the simulation file name behind the scenes, but that's likely not the
#'   prettiest way to label the y axis, which is where this argument comes in.
#'   If you already have a column in \code{forest_dataframe} with the label you
#'   want -- for example the column PerpCompound when your drug of interest is a
#'   victim or VictimCompound when it's the effector -- use that by specifying,
#'   for example, \code{y_axis_labels = PerpCompound}. If you would like to
#'   manually specify which simulation file should be labeled what here, do so
#'   with a named character vector, e.g.,
#'   \code{y_axis_labels = c("myfile1.xlsx" = "itraconazole", "myfile2.xlsx" =
#'   "efavirenz")}. If left as NA, we'll use the simulation file names. You can
#'   optionally make the compound names prettier with the argument
#'   \code{prettify_compound_names}.
#' @param y_order optionally set the order of simulation files on the y axis. If
#'   \code{y_order} is left as NA, the y axis will be sorted according to the
#'   AUC ratio with inhibitors on top and inducers on the bottom. If you would
#'   like to use some other order, there are four possible ways to
#'   specify this: \describe{
#'
#'   \item{"as is"}{If you're already happy with the order of things in the
#'   input data \code{forest_dataframe}, then setting \code{y_order = "as is"}
#'   will keep things in exactly the same order.}
#'
#'   \item{a character vector of file names}{e.g., \code{y_order =
#'   c("myfile1.xlsx", "myfile2.xlsx")}. The file names will be automatically
#'   replaced with whatever you specified for \code{y_axis_labels}.}
#'
#'   \item{"strongest inhibitor to strongest inducer"}{Sort the simulations
#'   from top to bottom by AUC ratio from the strongest inhibitor (largest AUC
#'   ratio) to the strongest inducer (smallest AUC ratio). (The Cmax ratio will
#'   be used if you didn't include AUC.) This is the default option and what you
#'   get when \code{y_order} is left as NA.}
#'
#'   \item{"strongest inducer to strongest inhibitor"}{Sort the simulations
#'   from top to bottom by AUC ratio from the strongest inducer (smallest AUC
#'   ratio) to the strongest inhibitor (largest AUC ratio). (The Cmax ratio will
#'   be used if you didn't include AUC.)}
#'
#'   }
#'
#' @param PKparameters optionally specify which PK parameters in
#'   \code{forest_dataframe} to use as input. If left as NA, all the PK
#'   parameters included in \code{forest_dataframe} will be included. If you try
#'   to include a parameter that's not already present in
#'   \code{forest_dataframe}, it will be ignored. User a character vector here,
#'   e.g., \code{PKparameters = c("AUCinf_dose1", "Cmax_dose1")}
#' @param observed_PK observed PK data, with the following columns: \describe{
#'
#'   \item{File}{the file you'd like the observed data to be graphed
#'   next to. Note that this file name must exist in the simulated data
#'   (forest_dataframe).}
#'
#'   \item{PKparameter}{the specific PK parameters to graph. Acceptable values
#'   are: "AUCinf_ratio_dose1", "AUCt_ratio_dose1", "AUCinf_ratio",
#'   "Cmax_ratio_dose1", "Cmax_ratio_last", or "Cmax_ratio". Whatever you use
#'   must match what was listed in the simulated data to be included in the
#'   graph.}
#'
#'   \item{at least one column named "Mean", "GeoMean", or "Median"}{Must be
#'   specified depending on your choice for \code{mean_type}, this column will
#'   be used for determining where to place the point. Whatever statistic you
#'   use \strong{must} be the same for both observed and simulated data.}
#'
#'   \item{(optional) at least one column or set of columns named "CI_Lower", "CI_Upper",
#'   "Centile_Lower", "Centile_Upper", "GeoCV", "ArithCV", "Min", "Max",
#'   "Std Dev"}{these will be used for the error bars and are optional.}
#'
#'   \item{whatever column you used for facet_column_x}{If you broke up the
#'   main graphs into two smaller graphs along the x axis, then whatever column you used for that
#'   must be present in the observed data, too.}
#'
#'   }
#'
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
#' @param mean_type type of mean to graph; options are "geometric" (default),
#'   "arithmetic", or "median", but this only works when those data are included
#'   in \code{forest_dataframe}. If you list the mean type as "mean", we'll
#'   assume you want arithmetic means.
#' @param variability_type type of variability to show as whiskers; options are
#'   "90\% CI" (default), "95\% CI", "5th to 95th percentiles", "range",
#'   "geometric CV", "arithmetic CV", or "standard deviation" ("sd" is also ok
#'   for that last one).
#' @param y_axis_title optionally specify a vertical title to be displayed to
#'   the left of the y axis. Example: \code{y_axis_title = "Effector
#'   co-administered with Drug X"}. Default ("none") leaves off any y-axis title.
#' @param x_axis_limits the x axis limits to use; default is 0.06 to 12.
#' @param x_axis_number_type set the x axis number type to be "ratios"
#'   (default), "percents" (converts the ratios to a percent), or "keep trailing
#'   zeroes" (still uses ratios but, unlike the default, guesses at a reasonable
#'   number of digits to include based on the range of the data and includes
#'   trailing zeroes as necessary)
#' @param x_axis_title optionally supply a character vector or an expression to
#'   use for the x axis title
#' @param facet_column_x optionally break up the graph horizontally into small
#'   multiples. The designated column name should be unquoted, e.g.,
#'   \code{facet_column_x = Dose_sub}. This would also allow you to potentially
#'   use the same value for \code{y_axis_labels} for multiple simulations. For
#'   example, say you have one simulation -- "SimA.xlsx" -- where the substrate
#'   was dosed QD and another simulation -- "SimB.xlsx" -- where it was dosed
#'   BID, and both of them were co-administered with the inhibitor itraconazole.
#'   You want the y axis labels to show what effector was used in each
#'   simulation, and both of these used the same inhibitor. If you break up your
#'   graphs by setting \code{facet_column_x} to whatever column you used to
#'   indicate the dosing regimen, then "SimA.xlsx" and "SimB.xlsx" won't overlap
#'   on the graph even though they both had the same effector. That's the reason
#'   it's ok here. Unclear? Please check out the examples at the bottom,
#'   particularly the ones that employ \code{facet_column_x}.
#' @param x_order optionally specify the order in which the x-axis facets should
#'   appear. For example, if you \code{facet_column_x} is the dosing regimen and
#'   the values there are "QD" and "BID", the default will be to show them in
#'   alphabetical order. If you want "QD" to show up first, though, set the
#'   order with \code{x_order = c("QD", "BID")}
#' @param dose_units the units used in dosing, which only applies if you set
#'   \code{facet_column_x}, to Dose_sub or Dose_inhib. In that situation,
#'   setting the dose units here will automatically add those units to the
#'   appropriate graph labels. This way, the graph label will be, e.g., "50 mg"
#'   and "100 mg" instead of just "50" and "100". This just helps make it
#'   clearer what the numbers represent. If you specify anything other than
#'   Dose_sub or Dose_inhib for \code{facet_column_x}, this will be ignored.
#' @param prettify_compound_names NA (default), TRUE, or FALSE on whether to
#'   make any compound names included in \code{y_axis_labels} prettier. This was
#'   designed for simulations where the substrates or effectors are among the
#'   standard options for the simulator, and leaving
#'   \code{prettify_compound_names = TRUE} will make the name of those compounds
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Midazolam" will become "midazolam". If you don't
#'   specify this, we'll prettify if you supply a column name for
#'   \code{y_axis_labels} and we *won't* prettify if you supply a named
#'   character vector there.
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all.
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
#'   the graph
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
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 6
#' @param fig_width figure width in inches; default is 5
#'
#' @return Output is a graph.
#' @export
#' @examples
#'
#' # We'll use some example forest-plot data for the substrate bufuralol
#' # with various effectors. To start, we'll use all the default settings.
#' forest_plot(forest_dataframe = BufForestData_20mg)
#'
#' # You can used the argument y_axis_labels to specify what to use for the
#' # y axis labels instead of the simulation file names. One option: Use a
#' # named character vector to list the file names and what you want to
#' # show instead. This can be as verbose as you like, and it's ok to use
#' # "\n" for a new line if you want.
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = c("buf-20mg-sd-fluv-36mg-qd.xlsx" = "fluvoxamine",
#'                               "buf-20mg-sd-itra-200mg-qd.xlsx" = "itraconazole",
#'                               "buf-20mg-sd-quin-200mg-qd.xlsx" = "quinidine",
#'                               "buf-20mg-sd-tic-219mg-bid.xlsx" = "ticlopidine\nSingh et al. 2017 study"))
#'
#' # Or use a different column in forest_dataframe to specify y_axis_labels.
#' # Please note that there must be one unique value in y_axis_labels for
#' # each simulation file.
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound)
#'
#' # By default, the graph will show the strongest inhibitors on top and
#' # the strongest inducers on the bottom, sorted by their AUC GMR. However,
#' # if you liked the order you already had things in whatever you supplied
#' # for forest_dataframe, you can tell the forest_plot function not to change
#' # that by setting y_order to "as is".
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             y_order = "as is")
#'
#' # If it would be sensible to break up your graph by a column in
#' # forest_dataframe, you can do that with the argument facet_column_x. We'll
#' # switch to some example data with two dose levels here.
#' forest_plot(forest_dataframe = BufForestData,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub)
#'
#' # Or break up your graph by the PK parameter shown.
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = PKparameter)
#'
#' # If what you supplied for forest_dataframe includes other statistics,
#' # you can graph those instead of the default, which is the geometric
#' # mean (point) and geometric 90 percent confidence interval (error bars).
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             mean_type = "median",
#'             variability_type = "range")
#'
#' # You can compare observed PK data as long as they are formatted the same
#' # way as the simulated data. Here's an example.
#' view(BufObsForestData_20mg)
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             observed_PK = BufObsForestData_20mg)
#'
#' # Here are some options for modifying the aesthetics of your graph:
#'
#' # -- Don't include "Dose 1" or "Last dose" in the PK parameter names.
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             include_dose_num = FALSE)
#'
#' # -- Add an overall graph title and a y axis title to make it clear that
#' # we're looking at the effects of various perpetrators on bufuralol PK (at
#' # least, that's what we're doing in this example).
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             include_dose_num = FALSE,
#'             y_axis_title = "Perpetrator",
#'             graph_title = "Effects of various DDI perpetrator\ndrugs on bufuralol PK")
#'
#' # -- Adjust the x axis limits with x_axis_limits
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             x_axis_limits = c(0.9, 5))
#'
#' # -- Include a legend for the shading
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom")
#'
#' # -- Change the shading to be like in Chen Jones 2022 CPT
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = "yellow to red")
#'
#' # -- Or make the shading disappear
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = "none")
#'
#' # -- Or specify exactly which colors you want for which interaction level
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub,
#'             legend_position = "bottom",
#'             color_set = c("negligible" = "white", "weak" = "#C6CDF7",
#'                           "moderate" = "#7294D4", strong = "#E6A0C4"))
#'
#' # -- Make the compound names match *exactly* what was in the simulator file
#' # rather than being automatically prettified
#' forest_plot(forest_dataframe = BufForestData_20mg,
#'             y_axis_labels = PerpCompound,
#'             facet_column_x = Dose_sub,
#'             prettify_compound_names = FALSE)


forest_plot <- function(forest_dataframe, 
                        y_axis_labels,
                        y_order = NA,
                        PKparameters = NA, 
                        observed_PK = NA,
                        facet_column_x, 
                        mean_type = "geometric",
                        variability_type = "90% CI", 
                        include_dose_num = TRUE,
                        y_axis_title = "none", 
                        prettify_compound_names = NA, 
                        x_axis_limits = NA, 
                        x_axis_title = NA,
                        x_axis_number_type = "ratios",
                        x_order = NA,
                        dose_units = "mg",
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
   
   # Setting things up for nonstandard evaluation 
   facet_column_x <- rlang::enquo(facet_column_x)
   
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
   
   # Cleaning up inputs
   if(class(prettify_compound_names) != "logical"){
      warning("You appear to have supplied something to the argument `prettify_compound_names` other than TRUE, FALSE, or NA. Unfortunately, those are the only permissible values. We'll set this to NA.", 
              call. = FALSE)
      prettify_compound_names <- NA
   }
   
   # Changing prettify_compound_names to "character" and setting NA to "not
   # set". Using this lower down in the script.
   prettify_compound_names <- ifelse(is.na(prettify_compound_names), 
                                     "not set",
                                     as.character(prettify_compound_names))
   
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
   
   # Making legend argument lower case to avoid case sensitivity
   legend_position <- tolower(legend_position)
   
   
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
                                       "CI90_lo"~ "CI_Lower",
                                       "CI90_hi" ~ "CI_Upper", 
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
                                             "CI90_lo"~ "CI_Lower",
                                             "CI90_hi" ~ "CI_Upper", 
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
      }
   }
   
   if(ObsIncluded){
      # If any of the column names had older stat names, fix that.
      observed_PK <- observed_PK %>% 
         rename_with(.fn = ~ gsub("CI90_lo|CI90_Lower", "CI_Lower", .x)) %>% 
         rename_with(.fn = ~ gsub("CI90_hi|CI90_Upper", "CI_Upper", .x))
   }
   
   # If any of the column names had older stat names, fix that.
   forest_dataframe <- forest_dataframe %>% 
      rename_with(.fn = ~ gsub("CI90_lo|CI90_Lower", "CI_Lower", .x)) %>% 
      rename_with(.fn = ~ gsub("CI90_hi|CI90_Upper", "CI_Upper", .x))
   
   if(all(c("File", "PKparameter") %in% names(forest_dataframe) == FALSE)){
      stop(paste0("There must be a column titled `File` in what you supply for forest_dataframe, and there also must be some PK data. We can't find that in your input. For an example of acceptable data, please run\n", 
                  "view(BufForestData_20mg)\n", 
                  "in the console."),
           call. = FALSE)
   }
   
   if(is.na(include_dose_num)){
      # Dropping dose number depending on input. First, checking whether they have
      # both dose 1 and last-dose data.
      DoseCheck <- c("first" = any(str_detect(forest_dataframe$PKparameter, "dose1")), 
                     "last" = any(str_detect(forest_dataframe$PKparameter, "last")))
      include_dose_num <- all(DoseCheck) == FALSE
   }
   
   # include_dose_num now should be either T or F no matter what, so checking
   # that.
   if(is.logical(include_dose_num) == FALSE){
      warning("Something is amiss with your input for `include_dose_num`, which should be NA, TRUE, or FALSE. We'll assume you meant for it to be TRUE.", 
              call. = FALSE)
   }
   
   if(include_dose_num == FALSE){
      PKparameters <- sub("_dose1|_last", "", PKparameters)
      
      forest_dataframe <- forest_dataframe %>% 
         mutate(PKparameter = sub("_dose1|_last", "", PKparameter))
      
      if(ObsIncluded){
         observed_PK <- observed_PK %>% 
            mutate(PKparameter = sub("_dose1|_last", "", PKparameter))
      }
   }
   
   forest_dataframe <- forest_dataframe %>% 
      mutate(SimOrObs = "predicted")
   
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
                                "CI_Lower", "CI_Upper", 
                                "Centile_Lower","Centile_Upper", 
                                "Min", "Max", "SD", "GeoCV", "ArithCV"))
   FDnames <- sort(FDnames)
   CenterStat <- as.character(
      FDnames[which(FDnames %in% switch(mean_type, 
                                        "geometric" = "GeoMean", 
                                        "mean" = "Mean",
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
         "90% CI" = c("CI_Lower", "CI_Upper"), 
         "95% CI" = c("CI_Lower", "CI_Upper"), 
         "5th to 95th percentiles" = c("Centile_Lower", "Centile_Upper"), 
         "range" = c("Min", "Max"), 
         "arithmetic CV" = "ArithCV", 
         "geometric CV" = "GeoCV", 
         "sd" = "SD",
         "SD" = "SD", 
         "standard deviation" = "SD"))]
   )
   
   if(length(VarStat) == 0){
      VarStat <- FDnames[which(FDnames %in% c("CI_Lower", "CI_Upper", 
                                              "CI_Lower", "CI_Upper", 
                                              "Centile_Lower","Centile_Upper", 
                                              "Min", "Max", "SD", "GeoCV", "ArithCV"))][1]
      
      if(length(VarStat) == 0){
         stop("You must have variability data included in forest_dataframe to be able to make a forest plot. Please see the help file for acceptable types of variability input.",
              call. = FALSE)
      } else {
         warning(paste0("You requested a variability_type of ", 
                        variability_type, ", but that was not available in your data. Instead, ", 
                        switch(VarStat[1], 
                               "CI_Lower" = "the geometric X% confidence interval", 
                               "Centile_Lower" = "Xth to Yth percentiles",
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
         mutate(SimOrObs = "observed") 
      
      CenterStat_obs <- names(observed_PK)[
         which(names(observed_PK) %in% c("GeoMean", "Mean", "Median"))]
      VarStat_obs <- names(observed_PK)[
         which(names(observed_PK) %in% c("CI_Lower", "CI_Lower", "SD_Lower",
                                         "Centile_Lower", "Min"))]
      
      if(any(CenterStat_obs %in% CenterStat) == FALSE | 
         any(VarStat_obs %in% VarStat) == FALSE){
         warning("You have different statistics in your simulated and observed data, so we cannot compare them. We are dropping the observed data for now. Please check your input.", 
                 call. = FALSE)
         ObsIncluded <- FALSE
      } else {
         forest_dataframe <- forest_dataframe %>% bind_rows(observed_PK)
      }
   } 
   
   # Setting up y_axis_labels and checking for possible problems. This has to
   # come AFTER rbinding the observed data.
   YLabClass <- tryCatch(class(y_axis_labels), 
                         error = function(x) "quosure")
   
   if(YLabClass == "quosure"){
      # This is when y_axis_labels is a column in forest_dataframe. 
      y_axis_labels <- rlang::enquo(y_axis_labels)
      
      # If they didn't specify anything for y_axis_labels, this should actually
      # be character data.
      if(as_label(y_axis_labels) == "<empty>"){
         y_axis_labels <- as.character(NA)
         YLabClass <- "character"
      } else {
         forest_dataframe <- forest_dataframe %>% 
            mutate(YCol = !!y_axis_labels, 
                   YCol = switch(prettify_compound_names, 
                                 # If they didn't set prettify_compound_names
                                 # but they did set a column, it's likely a
                                 # column with substrate or inhibitor 1 names,
                                 # so prettify unless they request not to.
                                 "not set" = prettify_compound_name(YCol), 
                                 "TRUE" = prettify_compound_name(YCol), 
                                 "FALSE" = YCol))
      } 
   }
   
   if(YLabClass == "character"){
      
      if(any(complete.cases(y_axis_labels)) && 
         is.null(names(y_axis_labels))){
         warning("You must supply names for `y_axis_labels` for us to break up the y axis with those labels. We will use the file names to break up the y axis instead.", 
                 call. = FALSE)
         y_axis_labels <- NA
         
      } else if(any(complete.cases(y_axis_labels)) && 
                any(duplicated(y_axis_labels))){
         warning(paste0("These values are duplicated in `y_axis_labels`:",
                        str_c(y_axis_labels[duplicated(y_axis_labels)], 
                              collapse = "\n"),
                        "and we need unique values to break up the y axis. We will use the file names to break up the y axis instead."), 
                 call. = FALSE)
         y_axis_labels <- NA
         
      } else if(any(complete.cases(y_axis_labels)) && 
                any(duplicated(names(y_axis_labels)))){
         warning(paste0("These names are duplicated in `y_axis_labels`:",
                        str_c(names(y_axis_labels)[duplicated(names(y_axis_labels))], 
                              collapse = "\n"),
                        "and we need unique names to break up the y  axis. We will use the file names to break up the y axis instead."), 
                 call. = FALSE)
         y_axis_labels <- NA
         
      } else if(any(complete.cases(y_axis_labels)) &&
                all(names(y_axis_labels) %in% forest_dataframe$File) == FALSE){
         warning(paste0("It looks like you're trying to provide a named character vector for `y_axis_labels`, but these file names are not present in forest_dataframe:\n", 
                        str_c(setdiff(names(y_axis_labels), forest_dataframe$File), 
                              collapse = "\n"), 
                        "\nFor now, we'll list the file names along the y axis rather than using what you specified with `y_axis_labels`."),
                 call. = FALSE)
         y_axis_labels <- NA
      } 
      
      if(any(complete.cases(y_axis_labels))){
         
         forest_dataframe <- forest_dataframe %>% 
            mutate(YCol = switch(prettify_compound_names, 
                                 # If user didn't specify whether they wanted
                                 # prettification but did use specific values
                                 # for y_axis_labels, they probably don't want
                                 # those to change. If not set, then here, DON'T
                                 # prettify.
                                 "not set" = y_axis_labels[File],
                                 "TRUE" = prettify_compound_name(y_axis_labels[File]),
                                 "FALSE" = y_axis_labels[File]))
      } else {
         forest_dataframe$YCol <- forest_dataframe$File
      }
   }
   
   # Checking for any replicates that would make points overlap, e.g., need only
   # 1 file per YCol value.
   CheckFile <- forest_dataframe %>% 
      select(any_of(c("File", "PKparameter", "YCol",
                      as_label(facet_column_x)))) %>% unique() %>% 
      group_by(across(.cols = any_of(c("YCol", "PKparameter", as_label(facet_column_x))))) %>% 
      summarize(N = n())
   if(any(CheckFile$N > 1)){
      CheckFile <- CheckFile %>% filter(N > 1)
      stop("You have more than one simulation file associated with the same y axis label, which would result in a confusing graph. Please check your input for `y_axis_labels`.", 
           call. = FALSE)
   }
   
   if(any(complete.cases(y_order))){
      if(length(y_order) == 1 && 
         !y_order %in% c("strongest inducer to strongest inhibitor", 
                         "strongest inhibitor to strongest inducer", 
                         "as is")){
         warning(paste0("Acceptable values for `y_order` are a character vector of file names or else `as is`, `strongest inhibitor to strongest inducer`, or `strongest inducer to strongest inhibitor`, and you entered:\n",
                        str_c(y_order, collapse = "\n"),
                        "\nWe will use the default order."),
                 call. = FALSE)
         y_order <- "strongest inhibitor to strongest inducer"
      } else if(length(y_order) > 1){
         # FIXME
      }
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
      select(File, YCol, PKparameter, SimOrObs, 
             any_of(c(as_label(facet_column_x), 
                      CenterStat, VarStat)))
   
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
   
   
   ## Figuring out y axis order ----------------------------------------------
   
   # Setting the order of the simulations. 
   StInhib_StInd <- forest_dataframe %>% 
      filter(SimOrObs == "predicted") %>% 
      select(YCol, PKparameter, Centre) %>% unique() %>% 
      arrange(desc(PKparameter), desc(Centre)) %>% 
      pull(YCol) %>% unique()
   
   YFileOrder <- ifelse(length(y_order) > 1, 
                        "user", ifelse(is.na(y_order), 
                                       "strongest inhibitor to strongest inducer", 
                                       y_order))
   
   forest_dataframe <- forest_dataframe %>% 
      mutate(YCol = factor(YCol, levels = switch(
         YFileOrder, 
         "strongest inhibitor to strongest inducer" = StInhib_StInd, 
         "strongest inducer to strongest inhibitor" = rev(StInhib_StInd), 
         "as is" = unique(forest_dataframe$YCol),
         "user" = forest_dataframe %>% 
            mutate(File = factor(File, levels = y_order)) %>% 
            arrange(File) %>% pull(YCol) %>% unique())))
   
   
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
   
   if(as_label(facet_column_x) == "PKparameter"){
      
      # Adjusting y position if there are any obs data.
      ObsCheck <- forest_dataframe %>% 
         group_by(YCol) %>% 
         summarize(Ylen = length(unique(SimOrObs)))
      
      forest_dataframe <- forest_dataframe %>%
         left_join(ObsCheck, by = "YCol") %>% 
         mutate(YCol_num = as.numeric(YCol),
                YCol_num = case_when(Ylen == 1 ~ YCol_num, 
                                     Ylen == 2 & SimOrObs == "predicted" ~ YCol_num - 0.2, 
                                     Ylen == 2 & SimOrObs == "observed" ~ YCol_num + 0.2))
      
   } else {
      forest_dataframe <- forest_dataframe %>% 
         mutate(PKParam_num = ifelse(SimOrObs == "predicted", 
                                     PKParam_num - 0.2, PKParam_num + 0.2))
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
                          "CI_Lower" = "(90% Confidence Interval)", 
                          "CI_Lower" = "(95% Confidence Interval)", 
                          "Centile_Lower" = "(5th to 95th Percentiles)", 
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
   
   names(MyShapes) <- c("observed", "predicted")
   
   
   # Graph ----------------------------------------------------------------
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
      
      # If there are any \n in YCol, labeller doesn't work. In that case, make
      # PKparameter_exp just be the name w/out subscripts or symbols.
      if(any(str_detect(forest_dataframe$YCol, "\\n"))){ 
         
         Param_ch <- c("Cmax_ratio_last" = "Last dose Cmax ratio", 
                       "Cmax_ratio" = "Cmax ratio", 
                       "AUCtau_ratio_last" = "Last dose AUCtau ratio", 
                       "AUCtau_ratio" = "AUCtau ratio", 
                       "Cmax_ratio_dose1" = "Dose 1 Cmax ratio", 
                       "AUCt_ratio_dose1" = "Dose 1 AUCt ratio", 
                       "AUCt_ratio" = "AUCt ratio", 
                       "AUCinf_ratio_dose1" = "Dose 1 AUCinf ratio")
         
         forest_dataframe <- forest_dataframe %>% 
            mutate(PKparameter_exp = 
                      factor(Param_ch[as.character(PKparameter)], 
                             levels = {{Param_ch}}))
      }
      
      G <- ggplot(forest_dataframe, aes(x = Centre, xmin = Lower, xmax = Upper, 
                                        y = SimOrObs, shape = SimOrObs)) +
         geom_rect(data = Rect, aes(xmin = Xmin, xmax = Xmax,
                                    ymin = Ymin, ymax = Ymax, fill = IntLevel),
                   inherit.aes = FALSE) +
         scale_fill_manual(values = FillColor) +
         geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
         geom_errorbar(width = 0.3) +
         geom_point(size = 2.5, fill = "white") +
         facet_grid(YCol ~ PKparameter_exp, 
                    labeller = switch(as.character(any(str_detect(forest_dataframe$YCol, "\\n"))), 
                                      "TRUE" = label_value, 
                                      "FALSE" = label_parsed), 
                    switch = "y") +
         scale_shape_manual(values = MyShapes) +
         scale_y_discrete(expand = expansion(mult = pad_y_num * 3)) + # Often needs a little extra
         labs(fill = "Interaction level", shape = NULL)
      
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
      xlab(ifelse(is.na(x_axis_title), XTitle, x_axis_title)) + 
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
   
   if(as_label(facet_column_x) == "PKparameter"){
      G <- G +
         theme(axis.ticks.y = element_blank(), 
               axis.text.y = element_blank())
   }
   
   # If color_set is "none", don't show fill for the legend. 
   if(length(color_set) == 1 && color_set == "none"){
      G <- G +
         guides(fill = "none")
   }
   
   if(complete.cases(graph_title)){
      G <- G + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5, size = graph_title_size))
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


