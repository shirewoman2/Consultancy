#' Make graphs comparing the center statistic and variability in PK across
#' trials and, optionally, against observed PK
#'
#' @description \code{trial_means_plot} creates a graph comparing variability
#'   across simulated trials and, when supplied, across observed trials as well
#'   for a given PK parameter. The central statistic is shown as a dot and the
#'   variability statistic is shown as error bars for each of the trials, and
#'   you can choose which statistics you want for each, e.g., geometric means
#'   for the central statistic and the geometric 90\% confidence interval for
#'   the variability statistic. UNDER CONSTRUCTION.
#'
#'   \strong{IMPORTANT:} If the summary statistics used for your observed data
#'   don't match the summary statistics you choose for \code{mean_type} and
#'   \code{variability_type}, you will get a misleading graph, so be careful!
#'
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#' @param sheet optionally specify the name of the sheet where you'd like to
#'   pull the PK data, in quotes. For example, if you have a user-defined AUC
#'   interval, specify the tab where those data are. \strong{NOTE:} If you want
#'   typical first- or last-dose PK, this should be left as NA. This \emph{only}
#'   applies for user-defined AUC intervals.
#' @param PKparameter PK parameter you want to extract from the simulator output
#'   file. To see the full set of possible parameters to extract, enter
#'   \code{view(PKParameterDefinitions)} into the console. Not case sensitive.
#' @param compoundToExtract For which compound do you want to extract
#'   PK data? Options are: \itemize{\item{"substrate"
#'   (default),} \item{"primary metabolite 1",} \item{"primary metabolite 2",}
#'   \item{"secondary metabolite",} \item{"inhibitor 1" -- this can be an
#'   inducer, inhibitor, activator, or suppresesor, but it's labeled as
#'   "Inhibitor 1" in the simulator,} \item{"inhibitor 2" for the 2nd inhibitor
#'   listed in the simulation,} \item{"inhibitor 1 metabolite" for the primary
#'   metabolite of inhibitor 1}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default), "unbound plasma", "blood", "unbound blood",
#'   "peripheral plasma", or "peripheral blood". \strong{NOTE: PK for peripheral
#'   sampling is not as well tested as for other tissues and is only set up for
#'   V21+. Please check your results carefully.}
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} or \code{\link{extractExpDetails}} to
#'   get all the details from the "Input Sheet" (e.g., when you ran
#'   extractExpDetails you said \code{exp_details = "Input Sheet"} or
#'   \code{exp_details = "all"}), you can save some processing time by supplying
#'   that object here, unquoted. If left as NA, this function will run
#'   \code{extractExpDetails} behind the scenes to figure out some information
#'   about your experimental set up.
#' @param mean_type What kind of means do you want to use for the center point
#'   on the graph? Options are "geometric" (default), "arithmetic", or "median".
#' @param variability_type What variability statistic to you want to use for the
#'   error bars? Options are "percentiles" for the 5th to 95th percentiles
#'   (default), "90\% CI", "SD", "CV", "GCV" (for geometric CV), or "range".
#' @param observed_PK a data.frame of observed PK that you would like to
#'   compare. This must include the columns "ObsValue" or "Value" for the center
#'   value for the observed data and "ObsVariability" or "Variability" for the
#'   error bars for the observed data. Some filtering this will do
#'   automatically:
#'
#'   \itemize{\item{If you include a column titled "File", we will only use PK
#'   data that have the same value in that column as what you supply for
#'   \code{sim_data_file}.}
#'
#'   \item{If you include a column titled "PKparameter", we will
#'   only use PK data that match what you supply for \code{PKparameter}.}
#'
#'   \item{If you want to include comparisons to more than one clinical study, add a column
#'   titled "Study" to your data. The value listed in that column will be used
#'   for labeling the study on your graph. If there is not a column titled
#'   "Study" in your observed PK, we will label the data in the graph as "observed".}
#'
#'   \item{If you include a column titled "CompoundID", we will only include PK
#'   data that match what you supplied for \code{compoundToExtract}.}
#'
#'   \item{If you include a column titled "Tissue", we will only include PK
#'   data that match what you supplied for \code{tissue}.}
#'
#'   }
#'
#' @param lines_for_population_stats optionally include horizontal lines for the
#'   overall simulated population statistics by specifying the desired line
#'   color and type. Set this as a single text string of 1) the color for the
#'   lines (any R-friendly color specification will work), 2) the linetype for
#'   the central statistic (any R-friendly linetype will work), 3) the linetype
#'   for the variability statistic, and, optional, 4) the linewidth to use. If
#'   you omit the linewidth, we'll use a linewidth of 0.5 by default. The
#'   default of "gray80 solid dotted" will make a light gray solid line at the
#'   central statistic and a light gray dotted line at whatever you choose for
#'   the variability statistic. If you set this to "none", no lines will be
#'   included.
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
#'   \item{"black and white" ("BW" also works)}{black and white only. "white"
#'   means a black circle with a white center. If you set
#'   \code{color_option = "S or O"}, the simulated trials will be white and the
#'   observed will be black. This looks like the trial-means
#'   plots in the Excel outputs from the Simulator.}
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
#'   \item{"blues"}{a set of blues fading from sky to navy. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"greens"}{a set of greens fading from chartreuse to forest. Like
#'   "blue-green", this palette can be especially useful if you are comparing a
#'   systematic change in some continuous variable.}
#'
#'   \item{"purples"}{a set of purples fading from lavender to aubergine. Like
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
#' @param color_option How do you want to color information in the graph?
#'   Options are: 
#'   
#'   \describe{\item{"by study" (default)}{all simulated trials
#'   will be the first color from \code{color_set} and then each set of
#'   observed data will be a different color from that set}
#'
#'   \item{"by trial"}{uses a different color from \code{color_set} for every
#'   trial, whether simulated or observed}
#'   
#'   \item{"simulated or observed" ("S or O" and "S vs O" will also work)}{color
#'   points based on whether they were simulated or observed}}
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected. (Setting up
#'   semi-log plot y axis intervals manually is a bit tricky and is not
#'   currently supported.)
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right" (default), "bottom", "top", or "none" if you don't want one
#'   at all.
#' @param graph_title optionally specify a title that will be centered across
#'   your graph or set of graphs
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14. This also determines the font size of the graph labels.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Summary and Input"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes anyway to figure out
#'   some information about your experimental set up.
#' @param return_caption TRUE or FALSE (default) for whether to return any
#'   caption text to use with the graph. This works best if you supply something
#'   for the argument \code{existing_exp_details}. If set to TRUE, you'll get as
#'   output a list of the graph, the figure heading, and the figure caption.
#' @param prettify_compound_names TRUE (default), FALSE or a character vector:
#'   This is asking whether to make compound names prettier in the figure
#'   heading and caption. This was designed for simulations where the substrate
#'   and any metabolites, perpetrators, or perpetrator metabolites are among the
#'   standard options for the simulator, and leaving
#'   \code{prettify_compound_names = TRUE} will make the name of those compounds
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Midazolam" will become "midazolam". Setting this to
#'   FALSE will leave the compound names as is. For an approach with more
#'   control over what the compound names will look like in legends and Word
#'   output, set each compound to the exact name you  want with a named
#'   character vector where the names are "substrate" and, as applicable,
#'   "inhibitor 1", "primary metabolite 1", etc. and the values are the names
#'   you want, e.g.,
#'   \code{prettify_compound_names = c("inhibitor 1" = "teeswiftavir",
#'   "substrate" = "superstatin")}.
#' @param name_clinical_study optionally specify the name(s) of the clinical
#'   study or studies for any observed data. This only affects the caption of
#'   the graph. For example, specifying \code{name_clinical_study = "101, fed
#'   cohort"} will result in a figure caption that reads in part "clinical study
#'   101, fed cohort". If you have more than one study, that's fine; we'll take
#'   care of stringing them together appropriately. Just list them as a
#'   character vector, e.g., \code{name_clinical_study = c("101",
#'   "102", "103")} will become "clinical studies 101, 102, and 103."
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". The nice thing about saving to Word is that the figure title
#'   and caption text will be filled in automatically. If you leave off ".png"
#'   or ".docx", the graph will be saved as a png file, but if you specify a
#'   different graphical file extension, it will be saved as that file format.
#'   Acceptable graphical file extensions are "eps", "ps", "jpeg", "jpg",
#'   "tiff", "png", "bmp", or "svg". Do not include any slashes, dollar signs,
#'   or periods in the file name. Leaving this as NA means the file will not be
#'   saved to disk.
#' @param fig_height figure height in inches
#' @param fig_width figure width in inches
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' # None yet
#' 
trial_means_plot <- function(sim_data_file, 
                             PKparameter = "Cmax_dose1", 
                             compoundToExtract = "substrate", 
                             tissue = "plasma", 
                             sheet = NA, 
                             mean_type = "geometric", 
                             variability_type = "percentiles", 
                             observed_PK = NA, 
                             lines_for_population_stats = "gray80 solid dotted", 
                             color_set = "default",
                             color_option = "by study", 
                             point_size = NA, 
                             bar_width = NA, 
                             y_axis_limits_lin = NA, 
                             legend_position = "right", 
                             include_dose_num = FALSE, 
                             graph_title = NA,
                             graph_title_size = 14,
                             existing_exp_details = NA, 
                             return_caption = FALSE, 
                             prettify_compound_names = TRUE,
                             name_clinical_study = NA, 
                             save_graph = NA,
                             fig_height = NA,
                             fig_width = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   if(length(PKparameter) > 1){
      warning(wrapn("You have supplied more than 1 PK parameter, and we can only plot one at a time. We'll only plot the 1st one."), 
              call. = FALSE)
      PKparameter <- PKparameter[1]
   }
   
   if(is.na(PKparameter)){
      warning(wrapn("You must supply a PK parameter to plot. We'll plot the default of Cmax for dose 1."), 
              call. = F)
      PKparameter <- "Cmax_dose1"
   }
   
   if(PKparameter %in% c(AllPKParameters$PKparameter, 
                         AllPKParameters$PKparameter_nodosenum) == FALSE){
      warning(wrapn("You must supply a PK parameter to plot that is among the possible options, which you can see by running view(PKParameterDefinitions). We'll plot the default of Cmax for dose 1."), 
              call. = F)
      PKparameter <- "Cmax_dose1"
   }
   
   if(length(sim_data_file) > 1){
      warning(wrapn("You have supplied more than 1 simulation results file, and we can only plot one at a time. We'll only plot the 1st one."), 
              call. = FALSE)
      sim_data_file <- sim_data_file[1]
   }
   
   mean_type <- tolower(mean_type)[1]
   if(mean_type %in% c("arithmetic", "geometric", "median") == FALSE){
      warning(wrapn("The only possibilities for the mean type are 'geometric', 'arithmetic', or 'median', and you have entered something else. We'll use the default of 'geometric'."), 
              call. = FALSE)
      mean_type <- "geometric"
   }
   
   variability_type <- toupper(variability_type)[1]
   # Checking for either GCV or CV b/c people likely will only use CV and we'll
   # straighten things out and set it as GCV when appropriate later.
   variability_type <- ifelse(str_detect(variability_type, "CV"), 
                              "CV", variability_type)
   if(variability_type %in% c("90% CI", "SD", "CV", "RANGE", "PERCENTILES") == FALSE){
      warning(wrapn("The only possible variability types are 'percentiles', '90% CI', 'SD', 'CV', or 'range', and you have entered something else. We'll use the default of percentiles."), 
              call. = FALSE)
      variability_type <- "PERCENTILES"
   }
   
   if(mean_type != "median" & str_detect(PKparameter, "tmax")){
      warning(wrapn(paste("You requested tmax for the PK parameter and then requested", 
                          mean_type, "means. Are you sure you didn't mean to request a mean_type of 'median' and a variability_type of 'range'?")), 
              call. = FALSE)
   }
   
   # Fixing mismatched stats
   if(mean_type == "geometric" & variability_type == "SD"){
      warning(wrapn("You have requested a geometric mean type but then an arithmetic variability type (SD), which does not make for a clear graph. We'll set the variability type to the geometric CV instead."), 
              call. = FALSE)
      variability_type <- "GCV"
   }
   if(mean_type == "geometric" & variability_type == "CV"){
      variability_type <- "GCV"
   }
   
   # If user wanted lines_for_population_stats added, check that they have
   # specified argument correctly and set up the character vector of preferences.
   LineAES <- str_split(lines_for_population_stats, pattern = " ")[[1]]
   if(length(LineAES) < 3 & any(complete.cases(lines_for_population_stats))){
      warning(wrapn("You requested that lines for the overall simulated population statistics be added to the graph, but you've supplied input that doesn't work for `lines_for_population_stats`. We'll set this to `gray solid dashed` for now, but please check the help file to get what you want."), 
              call. = FALSE)
      LineAES <- c("gray", "solid", "dashed", "0.5")
   }
   # This doesn't check that they've specified legit colors or linetypes, but
   # I'm hoping that ggplot2 errors will cover that.
   
   # Adding a linewidth to LineAES if user didn't specify one.
   if(length(LineAES) < 4){
      LineAES[4] <- "0.5"
   }
   
   color_option <- tolower(color_option)[1]
   color_option <- case_when(
      color_option %in% c("s or o", "by study", "by trial") ~ color_option, 
      str_detect(color_option, "simulated") &
         str_detect(color_option, "observed") ~ "s or o", 
      color_option %in% c("s vs o", "s vs. o", "s v o", "s v. o") ~ "s or o", 
      str_detect(color_option, "study") ~ "by study", 
      str_detect(color_option, "trial") ~ "by trial", 
      .default = color_option)
   
   if(color_option %in% c("by study", "by trial", "s or o") == FALSE){
      warning(wrapn("You have specified something for the argument 'color_option' that is not among the possible options. We will color the data by whether the data were simulated or observed, which is the default."), 
              call. = FALSE)
      color_option <- "by study"
   }
   
   if(all(color_set == "BW")){color_set = "black and white"}
   
   legend_position <- tolower(legend_position)[1]
   if(legend_position %in% c("left", "right", "bottom", "top", "none") == FALSE){
      warning(wrapn("You have specified something for the legend position that is not among the possible options. We'll set it to 'right', the default."), 
              call. = FALSE)
      legend_position <- "right"
   }
   
   suppressWarnings(point_size <- as.numeric(point_size)[1])
   point_size <- ifelse(is.na(point_size), 3, point_size)
   suppressWarnings(bar_width <- as.numeric(bar_width)[1])
   bar_width <- ifelse(is.na(bar_width), 0.25, bar_width)
   
   # Main body of function -----------------------------------------------------
   
   ## Getting simulated data --------------------------------------------------
   PKdata <- extractPK(sim_data_file = sim_data_file, 
                       PKparameters = PKparameter, 
                       tissue = tissue, 
                       compoundToExtract = compoundToExtract, 
                       existing_exp_details = existing_exp_details, 
                       sheet = sheet, 
                       returnAggregateOrIndiv = "individual", 
                       returnExpDetails = TRUE)
   
   PK_long <- PKdata$individual %>% 
      filter(PKparameter == {{PKparameter}}) %>% 
      group_by(Trial) %>% 
      summarize(Mean = mean(Value, na.rm = T), 
                SD = sd(Value, na.rm = T), 
                Min = min(Value, na.rm = T), 
                Max = max(Value, na.rm = T), 
                Geomean = gm_mean(Value, na.rm = T), 
                GCV = gm_CV(Value, na.rm = T), 
                CI90_lower = switch(mean_type, 
                                    "arithmetic" = confInt(Value, CI = 0.9, distribution_type = "t")[[1]], 
                                    "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[1]], 
                                    "median" = NA), 
                CI90_upper = switch(mean_type, 
                                    "arithmetic" = confInt(Value, CI = 0.9, distribution_type = "t")[[2]], 
                                    "geometric" = gm_conf(Value, CI = 0.9, distribution_type = "t")[[2]], 
                                    "median" = NA), 
                Median = median(Value, na.rm = T), 
                Per5 = quantile(Value, 0.05), 
                Per95 = quantile(Value, 0.95) ) %>% 
      mutate(SorO = "simulated", 
             Study = "simulated", 
             Center = case_when(mean_type == "arithmetic" ~ Mean, 
                                mean_type == "geometric" ~ Geomean, 
                                mean_type == "median" ~ Median), 
             
             Lower = case_when(
                variability_type == "PERCENTILES" ~ Per5, 
                mean_type == "arithmetic" & 
                   variability_type %in% c("SD", "CV") ~ Mean - SD, 
                
                mean_type == "geometric" & 
                   variability_type == "GCV" ~ Geomean  - Geomean * GCV, 
                
                variability_type == "90% CI" ~ CI90_lower, 
                
                variability_type == "RANGE" ~ Min), 
             
             Upper = case_when(
                variability_type == "PERCENTILES" ~ Per95, 
                mean_type == "arithmetic" & 
                   variability_type %in% c("SD", "CV") ~ Mean + SD, 
                
                mean_type == "geometric" & 
                   variability_type == "GCV" ~ Geomean  + Geomean * GCV, 
                
                variability_type == "90% CI" ~ CI90_upper, 
                
                variability_type == "RANGE" ~ Max))
   
   
   ## Getting observed PK for comparison ---------------------------------------
   
   if("logical" %in% class(observed_PK) == FALSE){
      
      if("character" %in% class(observed_PK) && 
         str_detect(observed_PK, "csv$")){
         observed_PK <- read.csv(observed_PK)
      }
      
      # Tidying column names. 
      observed_PK <- tidy_PKparameters_names(observed_PK)
      
      if("File" %in% names(observed_PK) &&
         any(complete.cases(observed_PK$File))){
         observed_PK <- observed_PK %>% 
            filter(basename(File) == basename(sim_data_file))
      }
      
      if("CompoundID" %in% names(observed_PK) &&
         any(complete.cases(observed_PK$CompoundID))){
         observed_PK <- observed_PK %>% 
            filter(CompoundID == {{compoundToExtract}})
      }
      
      if("Tissue" %in% names(observed_PK) &&
         any(complete.cases(observed_PK$Tissue))){
         observed_PK <- observed_PK %>% 
            filter(Tissue == {{tissue}})
      }
      
      if("PKparameter" %in% names(observed_PK) && 
         any(complete.cases(observed_PK$PKparameter))){
         observed_PK <- observed_PK %>% 
            filter(PKparameter == {{PKparameter}})
      } else {
         observed_PK$PKparameter <- PKparameter
      }
      
      if("ObsVariability" %in% names(observed_PK) == FALSE){
         observed_PK$ObsVariability <- NA
      }
      
      suppressWarnings(
         observed_PK <- observed_PK %>%
            separate(ObsVariability, into = c("Lower", "Upper"), 
                     sep = "( )?to( )?|( )?-( )?", remove = FALSE) %>% 
            rename(Center = ObsValue) %>% 
            mutate(SorO = "observed", 
                   Trial = "observed", 
                   across(.cols = c("Lower", "Upper"), .fns = as.numeric), 
                   SD = ifelse(is.na(Upper) & mean_type == "arithmetic" & 
                                  variability_type == "SD", 
                               Lower, NA), 
                   CV = ifelse(is.na(Upper) & mean_type == "arithmetic" & 
                                  variability_type == "CV", 
                               Lower, NA), 
                   GCV = ifelse(is.na(Upper) & mean_type == "geometric" & 
                                   variability_type == "GCV", 
                                Lower, NA), 
                   
                   Lower = case_when(is.na(Upper) & 
                                        mean_type == "arithmetic" & 
                                        variability_type == "SD" ~ Center - SD, 
                                     
                                     is.na(Upper) & mean_type == "arithmetic" & 
                                        variability_type == "CV" ~ Center - Center * CV, 
                                     
                                     is.na(Upper) & mean_type == "geometric" & 
                                        variability_type == "GCV" ~ Center - Center * GCV, 
                                     
                                     .default = Lower), 
                   
                   Upper = case_when(is.na(Upper) & 
                                        mean_type == "arithmetic" & 
                                        variability_type == "SD" ~ Center + SD, 
                                     
                                     is.na(Upper) & mean_type == "arithmetic" & 
                                        variability_type == "CV" ~ Center + Center * CV, 
                                     
                                     is.na(Upper) & mean_type == "geometric" & 
                                        variability_type == "GCV" ~ Center + Center * GCV, 
                                     
                                     .default = Upper))
      )
      
      names(observed_PK)[tolower(names(observed_PK)) == "study"] <- "Study"
      if("Study" %in% names(observed_PK)){
         
         # We need the trial itself to be just, e.g., "study 1", "study 2"
         # because the study labels are invariably much too long otherwise. It's
         # just really hard to get things to look good when all of the simulated
         # trials are a digit or 2 and then the observed data are all, e.g.,
         # "Clinical study 101, fasted". Instead, adding a legend when coloring
         # by study and there are multiple observed data sets.
         suppressWarnings(
            observed_PK <- observed_PK %>% 
               mutate(Trial = factor(Study, 
                                     levels = unique(Study)), 
                      Trial = paste("study", as.numeric(Trial)), 
                      Study = paste0(Trial, ": ", Study))
         )
         
      } else {
         observed_PK$Trial <- "observed"
      }
      
      ObsTrialLevels <- unique(observed_PK$Trial)
      ObsStudyLevels <- unique(observed_PK$Study)
      
   } 
   
   if("logical" %in% class(observed_PK) == FALSE){
      PK_long <- PK_long %>% 
         bind_rows(observed_PK) %>% 
         mutate(Trial = factor(Trial, 
                               levels = c(as.character(1:length(
                                  unique(PKdata$individual$Trial))), 
                                  ObsTrialLevels)), 
                Study = factor(Study, 
                               levels = c("simulated", ObsStudyLevels)))
   } else {
      PK_long <- PK_long %>% 
         mutate(Trial = factor(Trial, 
                               levels = c(as.character(1:length(
                                  unique(PKdata$individual$Trial))))))
   }
   
   
   # Making graph -------------------------------------------------------------
   
   # Setting colors
   if(color_option == "by study"){
      if(all(color_set == "black and white")){
         color_set <- c("black", "white")
      }
      
      MyColors <- make_color_set(color_set = color_set, 
                                 num_colors = length(unique(PK_long$Study)))
      MyFillColors <- MyColors
      
      if(length(unique(PK_long$Study)) == 1){
         legend_position <- "none"
      }
      
      G <- ggplot(PK_long, aes(x = Trial, color = Study, 
                               y = Center, ymin = Lower, ymax = Upper))
      
   } else if(color_option == "by trial"){
      if(all(color_set == "black and white")){
         color_set <- c("black", "white")
      }
      
      MyColors <- make_color_set(color_set = color_set, 
                                 num_colors = length(unique(PK_long$Trial)))
      MyFillColors <- MyColors
      
      if(length(unique(PK_long$Trial)) == 1){
         legend_position <- "none"
      }
      
      G <- ggplot(PK_long, aes(x = Trial, color = Trial, 
                               y = Center, ymin = Lower, ymax = Upper))
      
   } else if(color_option == "s or o"){
      
      if(color_set == "black and white"){
         MyColors <- c("black", "black")
      } else {
         MyColors <- makes_color_set(color_set = color_set, 
                                     num_colors = 2)
      }
      names(MyColors) <- c("simulated", "observed")
      
      MyFillColors <- c("white", "black")
      names(MyFillColors) <- c("simulated", "observed")
      
      G <- ggplot(PK_long, aes(x = Trial, fill = SorO, color = SorO, 
                               y = Center, ymin = Lower, ymax = Upper)) +
         labs(color = NULL, fill = NULL)
   }
   
   if(lines_for_population_stats != "none"){
      
      AggStats <- PKdata$aggregate[[1]]
      names(AggStats) <- renameStats(names(PKdata$aggregate[[1]]))
      
      # Simulator does not output arithmetic CIs, so need to add those here.
      CI90_arith <- confInt(PKdata$individual %>% 
                               filter(PKparameter == {{PKparameter}}) %>% 
                               pull(Value),
                            CI = 0.9, 
                            distribution_type = "t")
      
      AggStats <- c(AggStats, 
                    CI90_lowerer_arith = CI90_arith[[1]], 
                    CI90_upper_arith = CI90_arith[[2]])
      
      G <- G + 
         # Adding lines for population central stat
         geom_hline(yintercept = 
                       case_match(mean_type, 
                                  "arithmetic" ~ AggStats["Mean"], 
                                  "geometric" ~ AggStats["Geomean"], 
                                  "median" ~ AggStats["Median"]), 
                    color = LineAES[1], 
                    linetype = LineAES[2], 
                    linewidth = as.numeric(LineAES[4])) +
         # Adding lines for population variability stats. Lower line: 
         geom_hline(yintercept = 
                       case_when(
                          variability_type == "PERCENTILES" ~ AggStats["Per5"],
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "SD" ~ AggStats["Mean"] + AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "CV" ~ AggStats["Mean"] + AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_upper_arith"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "GCV" ~ AggStats["Geomean"] + 
                             AggStats["Geomean"] * AggStats["GCV"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_upper"], 
                          
                          variability_type == "RANGE" ~ AggStats["Maximum"]), 
                    
                    color = LineAES[1], 
                    linetype = LineAES[3], 
                    linewidth = as.numeric(LineAES[4])) +
         # Adding lines for population variability stats. Upper line: 
         geom_hline(yintercept = 
                       case_when(
                          variability_type == "PERCENTILES" ~ AggStats["Per95"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "SD" ~ AggStats["Mean"] - AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "CV" ~ AggStats["Mean"] - AggStats["SD"], 
                          
                          mean_type == "arithmetic" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_lowerer_arith"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "GCV" ~ AggStats["Geomean"] - 
                             AggStats["Geomean"] * AggStats["GCV"], 
                          
                          mean_type == "geometric" & 
                             variability_type ==  "90% CI" ~ AggStats["CI90_lowerer"], 
                          
                          variability_type == "RANGE" ~ AggStats["Minimum"]), 
                    
                    color = LineAES[1], 
                    linetype = LineAES[3], 
                    linewidth = as.numeric(LineAES[4]))
   }
   
   if(complete.cases(graph_title)){
      G <- G + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5, size = graph_title_size), 
               plot.title.position = "panel")
   }
   
   G <- G +
      geom_errorbar(width = bar_width) +
      geom_point(shape = 21, size = point_size) +
      scale_color_manual(values = MyColors) +
      scale_fill_manual(values = MyFillColors) +
      theme_consultancy() +
      ylab(PKexpressions[[sub("_last|_dose1", "", PKparameter)]]) + 
      theme(legend.position = legend_position)
   
   if(any(complete.cases(y_axis_limits_lin))){
      G <- G +
         scale_y_continuous(limits = y_axis_limits_lin)
   }
   
   Out <- list("graph" = G)
   
   if(return_caption){
      
      Legos <- make_text_legos(sim_data_file = basename(sim_data_file), 
                               existing_exp_details = PKdata[["ExpDetails"]], 
                               prettify_compound_names = prettify_compound_names)
      
      PKparameter_rmd <- case_match(sub("_dose1|_last", "", PKparameter), 
                                    "Cmax" ~ "C~max~", 
                                    "tmax" ~ "t~max~", 
                                    "AUCtau" ~ "AUC~tau~", 
                                    "AUCinf" ~ "AUC~inf~", 
                                    "AUCt" ~ "AUC~t~", 
                                    "CLt" ~ "CL/F", 
                                    "CLtau" ~ "CL/F", 
                                    "CLinf" ~ "CL/F")
      
      Caption <- 
         paste0("Figure shows ", 
                case_match(mean_type,
                           "arithmetic" ~ "arithmetic mean", 
                           "geometric" ~ "geometric mean", 
                           "median" ~ "median"), 
                " (point) and ", 
                case_match(variability_type, 
                           "PERCENTILES" ~ "5^th^ to 95^th^ percentiles", 
                           "SD" ~ "standard deviation", 
                           "CV" ~ "standard deviation", 
                           "GCV" ~ "standard deviation", 
                           "90% CI" ~ "90% confidence interval", 
                           "RANGE" ~ "range"), 
                " (error bars) for ", 
                PKparameter_rmd, 
                " for ",
                PKdata[["ExpDetails"]]$NumTrials, 
                " trials of ", 
                PKdata[["ExpDetails"]]$NumSubjTrial, 
                " subjects each (", 
                PKdata[["ExpDetails"]]$PercFemale * 100, "% female, ages ", 
                PKdata[["ExpDetails"]]$Age_min, " to ", 
                PKdata[["ExpDetails"]]$Age_max, 
                ") following ", 
                ifelse(
                   AllCompounds$DDIrole[AllCompounds$CompoundID == compoundToExtract] == "victim", 
                   Legos$DosingText_sub_lower, 
                   Legos$DosingText_inhib_lower), 
                ifelse(str_detect(PKparameter, "withInhib|ratio"), 
                       paste0(" following ", Legos$DosingText_inhib_lower), ""), 
                ifelse("logical" %in% class(observed_PK), 
                       "", 
                       paste0(". Observed data from clinical study ", 
                              ifelse(is.na(name_clinical_study), 
                                     "***XXX***", name_clinical_study))), 
                ". ", 
                ifelse(lines_for_population_stats == "none", 
                       "", 
                       paste0("Horizontal lines indicate the ", 
                              case_match(mean_type,
                                         "arithmetic" ~ "arithmetic mean", 
                                         "geometric" ~ "geometric mean", 
                                         "median" ~ "median"), 
                              " and ", 
                              case_match(variability_type, 
                                         "PERCENTILES" ~ "5^th^ to 95^th^ percentiles", 
                                         "SD" ~ "standard deviation", 
                                         "CV" ~ "standard deviation", 
                                         "GCV" ~ "standard deviation", 
                                         "90% CI" ~ "90% confidence interval", 
                                         "RANGE" ~ "range"), 
                              " for the simulated population. ")), 
                "Source simulated data: ", sim_data_file, ".")
      
      Heading <- paste0("Simulated ", 
                        ifelse("logical" %in% class(observed_PK), 
                               "", "and observed "), 
                        "values for ", PKparameter_rmd,
                        " for ", 
                        PKdata[["ExpDetails"]] %>% 
                           filter(File == sim_data_file) %>% 
                           select(AllCompounds$DetailNames[
                              AllCompounds$CompoundID == compoundToExtract]), 
                        " comparing variability across trials.")
      
      Out[["figure_heading"]] <- Heading
      Out[["figure_caption"]]  <-  Caption
   } 
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   # Saving -----------------------------------------------------------------
   
   if(complete.cases(save_graph)){
      
      # Checking for NA for fig_height and width
      if(is.na(fig_height)){
         fig_height <- 3
      }
      
      if(is.na(fig_width)){
         fig_width <- 6
      }
      
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("eps", "ps", "jpeg", "tiff",
                       "png", "bmp", "svg", "jpg", "docx") == FALSE){
            warning(wrapn(paste0("You have requested the graph's file extension be `", 
                                 Ext, "`, but we haven't set up that option. We'll save your graph as a `png` file instead.")),
                    call. = FALSE)
         }
         Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                  "png", "bmp", "svg", "jpg", "docx"), 
                       Ext, "png")
         FileName <- paste0(FileName, ".", Ext)
      } else {
         FileName <- paste0(FileName, ".png")
         Ext <- "png"
      }
      
      if(Ext == "docx"){
         
         # This is when they want a Word file as output
         OutPath <- dirname(FileName)
         if(OutPath == "."){
            OutPath <- getwd()
         }
         
         FileName <- basename(FileName)
         
         rmarkdown::render(system.file("rmarkdown/templates/trialmeansplot/skeleton/skeleton.Rmd",
                                       package="SimcypConsultancy"), 
                           output_dir = OutPath, 
                           output_file = FileName, 
                           quiet = TRUE)
         # Note: The "system.file" part of the call means "go to where the
         # package is installed, search for the file listed, and return its
         # full path.
         
      } else {
         # This is when they want any kind of graphical file format.
         ggsave(FileName, height = fig_height, width = fig_width, dpi = 600,
                plot = Out$graph)
      }
   }
   
   if(length(Out) == 1){
      Out <- Out[[1]]
   }
   
   return(Out)
   
}



