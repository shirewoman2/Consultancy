#' Make graphs of sensitivity analysis results
#'
#' @description \code{sensitivity_plot} will read a sensitivity-analysis Excel
#'   file and create a graph of the dependent variable of your choice as long as
#'   it was included in your results. If you ran a sensitivity analysis with
#'   more than 1 independent variable -- e.g., you wanted to see how adjusting
#'   both the kp scalar and the fa affected Cmax -- the best way we have come up
#'   with so far to show you how both of those variables affect your dependent
#'   variable is to break up the graphs into small multiples or facets (like on
#'   a gemstone). (Side note: We considered a 3D graph, but they're honestly
#'   hard to do well in a programmatic fashion like this.)
#'
#'   \strong{Notes:} \itemize{
#'
#'   \item{This function does not detect units, so please check
#'   your graph labels.}
#'
#'   \item{This function does not make 3D graphs
#'   and we have no plans to add option for 3D graphs because A) they're just
#'   not easy to interpret unless you can rotate them, and even then, that's a
#'   stretch in our humble opinion, and B) there aren't great R packages out there for 3D
#'   graphs.}
#'   }
#'
#' @param SA_file sensitivity analysis Excel file
#' @param dependent_variable dependent variable to plot. Options are: "plasma
#'   concentration" (just "plasma" will also work), "blood concentration" (just
#'   "blood" will also work), "AUC", "AUC over dose", "AUC over dose with
#'   interaction", "AUC ratio", "CL", "CLpo", "CLpo with interaction", "Cmax",
#'   "Cmax with interaction", "Cmax ratio", "dose over AUC", "dose over AUC with
#'   interaction", "fa", "Fg", "Fg with interaction", "Fh", "Fh with
#'   interaction", "Vss", "tmax", or "tmax with interaction". The dependent
#'   variable \emph{must} be one of the ones you requested when you ran the
#'   sensitivity analysis. Not case sensitive.
#' @param ind_var_label optionally specify what text use for labeling the
#'   independent variable (the x axis). If left as NA, R will find the value
#'   listed next to "Run Number" on the "ASA Summary" tab, which may be
#'   something not terribly pretty like "Fugut Value (Sub) (No Units)", and
#'   attempt to prettify it. If we haven't set things up to prettify that value,
#'   which is likely as we haven't used this function with very many
#'   sensitivity-analysis scenarios yet, the value will be unchanged. (If you
#'   have an independent variable you'd like to add to our list, please email
#'   Laura Shireman.)
#' @param ind_var_label2 If you have a 2nd independent variable, optionally
#'   specify what text use for labeling the 2nd one.
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{\item{"significant X" where "X" is a number
#'   (default: "significant 3")}{Output will be rounded to X significant figures.
#'   "signif X" also works fine.} \item{"none"}{No rounding will be performed.}
#'   \item{"round X" where "X" is a number}{Output will be
#'   rounded to X digits}}
#' @param target_DV optionally specify a target value for the dependent
#'   variable, which will add a horizontal red dotted line to the graph where
#'   the target lies.
#' @param color_by_which_indvar If you have more than one independent variable,
#'   you can specify which one you want to adjust the colors by. Options are
#'   "1", "1st", or "first" (all will work, default) to adjust the colors in the
#'   graph based on the first independent variable listed in the Excel SA file
#'   or "2", "2nd", or "second" to adjust colors based on the second. If you
#'   only have one independent variable, this will be ignored.
#' @param linear_or_log make the y or y axes linear or log. Options are:
#' \describe{\item{"linear" (default)}{make the x and y axes linear}
#'
#' \item{"log x"}{log10-transform the x axis but keep the y axis linear. This
#' does not apply to concentration-time profiles, which have time on the x axis.}
#'
#' \item{"log y"}{log10-transform the y axis but keep the x axis linear}
#'
#' \item{"both", "both vertical", or "both horizontal"}{include both a linear and a
#' semi-log (y axis is log transformed) plot in the results}
#'
#' \item{"both log"}{log10-transform both the x and y axes. This does not apply
#' to concentration-time profiles, which have time on the x axis.}
#'
#' }
#'
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected.
#' @param y_axis_limits_log optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to a round number. If left as the default NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#' @param y_axis_interval optionally set the y-axis major tick-mark interval.
#'   Acceptable input: any number or leave as NA to accept default values, which
#'   are generally reasonable guesses as to aesthetically pleasing intervals.
#'   This will be ignored for log-transformed y axes, where the intervals change
#'   and thus will be set automatically.
#' @param x_axis_limits_lin optionally set the X axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the X axis
#'   limits for the linear plot will be automatically selected. This does not
#'   apply when the dependent variable requested is plasma or blood
#'   concentrations.
#' @param x_axis_limits_log optionally set the X axis limits for the log plot,
#'   e.g., \code{c(10, 1000)}. Values will be rounded down and up, respectively,
#'   to a round number. If left as the default NA, the X axis limits for the
#'   semi-log plot will be automatically selected. This does not apply when the
#'   dependent variable requested is plasma or blood concentrations.
#' @param x_axis_interval optionally set the x-axis major tick-mark interval.
#'   Acceptable input: any number or leave as NA to accept default values, which
#'   are generally reasonable guesses as to aesthetically pleasing intervals.
#'   This will be ignored for log-transformed x axes, where the intervals change
#'   and thus will be set automatically.
#' @param time_range time range to show relative to the start of the simulation
#'   for any concentration-time plots. This does not apply when the plot is of
#'   some other dependent variable than plasma or blood.
#'   Options: \describe{
#'
#'   \item{NA}{(default) entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}
#'   }
#'
#' @param color_set the set of colors to use. Options: \describe{
#'
#'   \item{"blues" (default)}{a set of blues fading from sky to navy. Like
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
#'   \item{"blue-green"}{a set of blues fading into greens. This palette can be
#'   especially useful if you are comparing a systematic change in some
#'   continuous variable -- for example, increasing dose or predicting how a
#'   change in intrinsic solubility will affect concentration-time profiles --
#'   because the direction of the trend will be clear.}
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
#' @param legend_position specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all.
#' @param graph_title (optional) a title to include on your graph in quotes
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". If you leave off ".png" or ".docx" from the file name, it will
#'   be saved as a png file, but if you specify a different graphical file
#'   extension, it will be saved as that file format. Acceptable graphical file
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Do not include any slashes, dollar signs, or periods in the file name.
#'   Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 4
#' @param fig_width figure width in inches; default is 5
#' @param return_data TRUE or FALSE (default) on whether to return the data used
#'   to make the graph
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#'
#' sensitivity_plot(SA_file = "SA example.xlsx",
#'                  dependent_variable = "Cmax",
#'                  graph_title = "My pretty sensitivity-analysis graph that's not pink",
#'                  save_graph = "SA graph")
#' 

sensitivity_plot <- function(SA_file, 
                             dependent_variable, 
                             ind_var_label = NA,
                             ind_var_label2 = NA,
                             target_DV = NA,
                             color_by_which_indvar = "1st",
                             linear_or_log = "linear",
                             y_axis_limits_lin = NA,
                             y_axis_limits_log = NA,
                             y_axis_interval = NA, 
                             x_axis_limits_lin = NA,
                             x_axis_limits_log = NA,
                             x_axis_interval = NA,
                             time_range = NA,
                             legend_position = "right", 
                             rounding = "significant 3", 
                             color_set = "blues", 
                             graph_title = NA,
                             save_graph = NA,
                             fig_height = 4,
                             fig_width = 5, 
                             return_data = FALSE){
   
   # Error catching ------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
   
   if(missing(SA_file)){
      stop("You must provide a sensitivity-analysis Excel file for the argument 'SA_file'.", 
           call. = FALSE)
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   SA_file <- ifelse(str_detect(SA_file, "\\.xlsx$"), 
                     SA_file, paste0(SA_file, ".xlsx"))
   
   if(missing(dependent_variable)){
      stop(wrapn("You have not provided a dependent variable, so we don't know what to graph. Please check the help file for a list of acceptable values for the argument 'dependent_variable'."), 
           call. = FALSE)
   }
   
   color_by_which_indvar <- case_when(
      tolower(color_by_which_indvar) %in% c("1", "1st", "first") ~ "1st", 
      tolower(color_by_which_indvar) %in% c("2", "2nd", "second") ~ "2nd")
   
   if(is.na(color_by_which_indvar)){
      warning(wrapn("You have specified something for the argument `color_by_which_indvar` other than 1st or 2nd, which are the only possible options. We'll use the default value of 1st."), 
              call. = FALSE)
      color_by_which_indvar <- "1st"
   }
   
   if(str_detect(rounding, "signif|none|round") == FALSE){
      warning(wrapn("You have not entered one of the permissible options for rounding. The only options for rounding are `significant X` (default is to round by 3 sig figs), `round X`, or `none`, so we'll use the default rounding scheme."), 
              call. = FALSE)
      rounding <- "significant 3"
   }
   
   linear_or_log_orig <- tolower(linear_or_log)[1]
   linear_or_log <- case_match(linear_or_log_orig, 
                               "linear" ~ "linear x linear y", 
                               "lin" ~ "linear x linear y", 
                               "log x" ~ "log x linear y", 
                               "log y" ~ "linear x log y", 
                               "both" ~ "both vertical", 
                               "both vertical" ~ "both vertical", 
                               "both horizontal" ~ "both horizontal", 
                               "semi-log" ~ "linear x log y", 
                               "log" ~ "linear x log y", 
                               "both log" ~ "log x log y", 
                               "log x and log y" ~ "log x log y", 
                               "log x & log y" ~ "log x log y", 
                               "log x log y" ~ "log x log y")
   # # All options: 
   # LinLogOpts <- 
   #    c(
   #       # output is 1 graph 
   #       "linear x linear y", 
   #       "log x linear y", 
   #       "linear x log y", 
   #       "log x log y", 
   #       # output is 2 graphs
   #       "both vertical", # A) linear x linear y, B) linear x log y
   #       "both horizontal" # A) linear x linear y, B) linear x log y
   #    )
   
   if(str_detect(linear_or_log, "log x") &
      str_detect(dependent_variable, "plasma|blood")){
      warning(wrapn(paste0("You requested '", linear_or_log_orig, 
                           "' for the argument 'linear_or_log', but you also requested a plasma or blood concentration-time plot, and that's not an option there. We'll return both linear and semi-log concentration-time plots.")), 
              call. = FALSE)
      linear_or_log <- "both vertical"
   }
   
   if(is.na(linear_or_log)){
      warning(wrapn("You have specified something for the argument 'linear_or_log' that is not among the possible options. We'll make a linear graph."), 
              call. = FALSE)
      linear_or_log <- "linear"
   }
   
   if(str_detect(dependent_variable, "plasma|blood|conc")){
      if(any(complete.cases(time_range))){
         if("numeric" %in% class(time_range) == FALSE){
            warning(wrapn("You have specified something other than numeric data for the time range. For the sensitivity_plot function, you must specify two numbers for your time range or leave the agument time_range as NA. We'll use the full time range for now."), 
                    call. = FALSE)
            time_range <- NA
         } else if(length(time_range) != 2){
            warning(wrapn("You have specified something other than a start and a stop time for the x axis range. We'll use the full time range for now."), 
                    call. = FALSE)
            time_range <- NA
         }
      }
   }
   
   legend_position <- tolower(legend_position)[1]
   if(complete.cases(legend_position) && 
      legend_position %in% c("left", "right", "bottom", "top", "none") == FALSE){
      warning(wrapn("You have specified something for the legend position that is not among the possible options. We'll set it to 'right'."), 
              call. = FALSE)
      legend_position <- "right"
   }
   
   
   # Get data ------------------------------------------------------------
   AllSheets <- readxl::excel_sheets(path = SA_file)
   
   dependent_variable <- tolower(dependent_variable)
   
   DVsheets <- switch(
      dependent_variable, 
      
      "auc" = AllSheets[str_detect(AllSheets, "^AUC.*\\(") & 
                           !str_detect(AllSheets, "over|Ratio|with interaction")],
      "auc over dose" = AllSheets[str_detect(AllSheets, "AUC over Dose") & 
                                     !str_detect(AllSheets, "with int")],
      "auc over dose with interaction" = AllSheets[str_detect(AllSheets, "AUC over Dose \\(with")],
      "auc ratio" = AllSheets[str_detect(AllSheets, "AUC Ratio")], 
      "cl" = AllSheets[str_detect(AllSheets, "CL .L per h. .PKPD")],
      "clpo" = AllSheets[str_detect(AllSheets, "CLpo.*PKPD") & 
                            !str_detect(AllSheets, "with int")],
      "clpo with interaction" = AllSheets[str_detect(AllSheets, "CLpo \\(with interaction")], 
      "cmax" = AllSheets[str_detect(AllSheets, "^Cmax") & 
                            !str_detect(AllSheets, "over|Ratio|with interaction")],
      "cmax with interaction" = AllSheets[str_detect(AllSheets, "Cmax \\(with int")], 
      "cmax ratio" = AllSheets[str_detect(AllSheets, "Cmax Ratio")], 
      "dose over auc" = AllSheets[str_detect(AllSheets, "Dose over AUC") & 
                                     !str_detect(AllSheets, "with int")] ,
      "dose over auc with interaction" = AllSheets[str_detect(AllSheets, "Dose over AUC \\(with inter")],
      "fa" = AllSheets[str_detect(AllSheets, "fa .PKPD|fa..ADAM") & 
                          !str_detect(AllSheets, "with int")],
      "fg" = AllSheets[str_detect(AllSheets, "Fg .PKPD|Fg..ADAM") & 
                          !str_detect(AllSheets, "with int")],
      "fg with interaction" = AllSheets[str_detect(AllSheets, "Fg .PKPD.*with inter|Fg..ADAM.*with inter") & 
                                           !str_detect(AllSheets, "with int")],
      "fh" = AllSheets[str_detect(AllSheets, "Fh .PKPD") & 
                          !str_detect(AllSheets, "with int")],
      "fh with interaction" = AllSheets[str_detect(AllSheets, "Fh .PKPD.*with inter") & 
                                           !str_detect(AllSheets, "with int")],
      "vss" = AllSheets[str_detect(AllSheets, "Vss")],
      "tmax" = AllSheets[str_detect(AllSheets, "Tmax") & 
                            !str_detect(AllSheets, "with int")],
      "tmax with interaction" = AllSheets[str_detect(AllSheets, "Tmax.*with inter")],
      
      "plasma concentration" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
      "plasma" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
      "plasma conc" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
      "plasma concentrations" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
      
      "blood concentration" = AllSheets[str_detect(AllSheets, "Blood Concentration")], 
      "blood" = AllSheets[str_detect(AllSheets, "Blood Concentration")], 
      "blood conc" = AllSheets[str_detect(AllSheets, "Blood Concentration")], 
      "blood concentrations" = AllSheets[str_detect(AllSheets, "Blood Concentration")]
   )
   
   Summary <- openxlsx::read.xlsx(SA_file, sheet = "ASA Summary", 
                                  colNames = FALSE)
   
   # Getting the names of the independent variables
   SensParam <- Summary$X2[which(Summary$X1 == "Run Number")]
   SensParam2 <- Summary$X3[which(Summary$X1 == "Run Number")]
   
   if(is.na(SensParam2) & color_by_which_indvar != "1st"){
      warning("You requested that we color data by the 2nd independent variable, but you only have 1 independent variable present in your data, so we'll color the data by that one.\n", 
              call. = FALSE)
      color_by_which_indvar <- "1st"
   }
   
   RunInfo <- Summary[(which(Summary$X1 == "Run Number") + 1):nrow(Summary), 1:3]
   names(RunInfo) <- c("Run", "SensValue", "SensValue2")
   RunInfo <- RunInfo %>% mutate_all(.funs = as.numeric)
   
   # Reading data
   SAdata.xl <- openxlsx::read.xlsx(SA_file, sheet = DVsheets[1])
   
   # Stopping at 1st NA row in column 1 b/c sometimes people have added
   # additional data here, and it messes up the code below.
   Na1 <- which(is.na(as.character(SAdata.xl[, 1])))[1]
   if(complete.cases(Na1)){
      SAdata.xl <- SAdata.xl[1:(Na1-1), ]
      SAdata.xl <- SAdata.xl %>% purrr::discard(~all(is.na(.)))
   }
   
   ObsData <- list()
   
   if(str_detect(dependent_variable, "plasma|blood")){
      ObsRow <- which(SAdata.xl[, 1] == "Observed Data")
      SimT0Col <- which(names(SAdata.xl) == "RMSE") + 1
      SimT0Col <- ifelse(length(SimT0Col) == 0, 4, SimT0Col)
      
      if(length(ObsRow) > 0){
         
         for(i in seq(from = ObsRow + 1, to = nrow(SAdata.xl), by = 2)){
            ObsData[[i]] <- as.data.frame(t(SAdata.xl[i:(i + 1), 2:ncol(SAdata.xl)])) 
            names(ObsData[[i]]) <- c("Time", "Conc")
            ObsData[[i]] <- ObsData[[i]] %>% 
               filter(complete.cases(Time)) %>% 
               mutate(Subject = SAdata.xl[i+1, 1])
         }
         
         ObsData <- bind_rows(ObsData) %>% 
            mutate(Subject = sub(", DV 1", "", Subject))
         
         SAdata <- as.data.frame(t(SAdata.xl[1:(ObsRow-1), c(SimT0Col:ncol(SAdata.xl))]))
         
         
      } else {
         SAdata <- as.data.frame(t(SAdata.xl[, c(SimT0Col:ncol(SAdata.xl))]))
      }
      
      names(SAdata)[1] <- "Time"
      names(SAdata)[2:ncol(SAdata)] <- paste0("Run", RunInfo$Run)
      
      suppressMessages(
         SAdata <- SAdata %>% 
            pivot_longer(cols = matches("Run"), 
                         names_to = "Run", 
                         values_to = "Conc") %>% 
            mutate(Run = as.numeric(sub("Run", "", Run)),
                   Simulated = TRUE) %>% 
            left_join(RunInfo)
      )
      
      
   } else {
      
      SAdata <- SAdata.xl
      if(complete.cases(SensParam2)){
         names(SAdata)[1:3] <- c("SensValue", "SensValue2", "DV")
      } else {
         names(SAdata)[1:2] <- c("SensValue", "DV") 
      }
   }
   
   # Rounding for ease of reading legend. Could add an option for what rounding
   # to use here. Also making sure that values are numeric where appropriate.
   SAdata <- SAdata %>% 
      mutate(across(.cols = any_of(c("SensValue", "SensValue2")), 
                    .fns = function(x) round_opt(x, rounding)), 
             across(.cols = any_of(c("SensValue", "SensValue2", "DV", "SI", "EI")), 
                    .fns = as.numeric))
   
   # Setting up for graphing --------------------------------------------------
   
   ## labels -------------------
   PrettyDV <- list("auc" = expression(AUC~"(ng/mL.h)"), 
                    "auc over dose" = expression("AUC over Dose (h/L)"),
                    "auc over dose with interaction" = expression("AUC over Dose with Interaction (h/L)"),
                    "auc ratio" = PKexpressions[["AUC_ratio"]], 
                    "cl" = expression(CL~"L/h"), 
                    "clpo" = expression(CL[PO]~"(L/h)"), 
                    "clpo with interaction" = expression(CL[PO]~"with interaction (L/h)"), 
                    "cmax" = expression(C[max]~"(ng/mL)"), 
                    "cmax with interaction" = expression(C[max]~"with interaction (ng/mL)"), 
                    "cmax ratio" = PKexpressions[["Cmax_ratio"]], 
                    "dose over auc" = expression(Dose~over~AUC~"(L/h)"),
                    "dose over auc with interaction" = expression(Dose~over~AUC~"with interaction (L/h)"),
                    "fa" = expression(f[a]),
                    "fg" = expression(F[g]),
                    "fg with interaction" = expression(F[g]~with~interaction),
                    "fh" = expression(F[h]),
                    "fh with interaction" = expression(F[h]~with~interaction),
                    "tmax" = expression(t[max]~"(h)"),
                    "tmax" = expression(t[max]~"with interaction (h)"),
                    "vss" = expression(V[ss]~"(L/kg)"))
   
   # When the SA is with the Km, the names of that parameter are, e.g., "Kinetic
   # Routes Km 1 (Sub)(CYP 1)", but I think it will be hard to predict how to
   # label that. If the user has NOT specified labels to use, setting this to be
   # just "Km" in the data or, if they've used 2 ind vars and BOTH are Km
   # values, then leaving them as their original names.
   if(is.na(ind_var_label) & is.na(ind_var_label2) &
      any(c(str_detect(SensParam, "Km"),  
            str_detect(SensParam2, "Km")), na.rm = T) == TRUE &
      all(c(str_detect(SensParam, "Km"),
            str_detect(SensParam2, "Km")), na.rm = T) == FALSE){
      SensParam <- case_when(str_detect(SensParam, "Km") ~ "Km", 
                             .default = SensParam)
      SensParam2 <- case_when(str_detect(SensParam2, "Km") ~ "Km", 
                              .default = SensParam2)
   }
   
   PrettySensParam <- list("Dose .Sub" = "Dose (mg)",
                           "EC50" = expression(EC[50]~"("*mu*M*")"), 
                           "Emax" = expression(E[max]),
                           "fa" = expression(f[a]),
                           "Fugut" = expression(f[u[gut]]), 
                           "Ind Max" = expression(Ind[max]), 
                           "IndC50" = expression(IndC[50]~"("*mu*M*")"), 
                           "Ind Slope" = expression(induction~slope~"("*mu*M^-1*")"), 
                           "Intestinal ABCB1 .P-gp. CLint,T" = "Intestinal P-gp CLint,T (µL/min)",
                           "ka" = expression(k[a]), 
                           "Kinetic Routes.*CLint" = expression(CL[int]~"("*mu*"L/min/pmol)"), 
                           "Km" = expression(K[M]~"("*mu*"M)"), 
                           "^Kp Scalar" = expression(k[p]~scalar),
                           "Lag Time" = expression("lag time (h)"),
                           "LogP" = expression("logP"), 
                           "Peff" = expression(P[eff,human]), 
                           "slope" = expression(slope), 
                           "Tissue.plasma partition.*Additional Organ" = expression(k[p]~"scalar for additional organ"),
                           "Vss" = expression(V[ss]~"(L/kg)"))
   
   PrettySensParam_char <- list("Dose .Sub" = "Dose (mg)",
                                "EC50" = "EC50", 
                                "Emax" = "Emax", 
                                "fa" = "fa",
                                "Fugut" = "fu,gut", 
                                "Ind Max" = "Indmax",
                                "IndC50" = "IndC50", 
                                "Ind Slope" = "induction slope", 
                                "Intestinal ABCB1 .P-gp. CLint,T" = "Intestinal P-gp CLint,T (µL/min)",
                                "ka" = "ka",
                                "Kinetic Routes.*CLint" = "CLint", 
                                "Km" = "Km (µM)", 
                                "^Kp Scalar" = "kp scalar",
                                "Lag Time" = "lag time (h)",
                                "LogP" = "logP", 
                                "Peff" = "Peff",
                                "Tissue.plasma partition.*Additional Organ" = "kp scalar for additional organ",
                                "Vss" = "Vss (L/kg)")
   
   if(class(ind_var_label) == "logical" && is.na(ind_var_label)){
      # this is when they have not provided labels for ind var 1
      
      if(any(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))){
         # this is when their ind var 1 is one of the established pretty options
         
         ind_var_label <- PrettySensParam[
            which(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))][[1]]
         
         ind_var_label_char <- PrettySensParam_char[
            which(sapply(names(PrettySensParam_char), function(.) str_detect(SensParam, .)))][[1]]
         
      } else {
         # this is when their ind var 1 is NOT one of the established pretty options
         ind_var_label <- SensParam
         ind_var_label_char <- SensParam
      }
      
   } else {
      # this is when they HAVE provided labels for ind var 1
      ind_var_label_char <- ind_var_label
   }
   
   # Adding character labels for facets to match the labels
   SAdata <- SAdata %>% 
      mutate(SensValue_char = paste(ind_var_label_char, "=", SensValue), 
             SensValue_char = forcats::fct_reorder(.f = SensValue_char, .x = SensValue, .fun = min))
   
   
   if(complete.cases(SensParam2)){
      if(class(ind_var_label2) == "logical" && is.na(ind_var_label2)){
         # this is when they have not provided labels for ind var 2
         
         if(any(sapply(names(PrettySensParam), function(.) str_detect(SensParam2, .)))){
            # this is when their ind var 2 is one of the established pretty options
            
            ind_var_label2 <- PrettySensParam[
               which(sapply(names(PrettySensParam), function(.) str_detect(SensParam2, .)))][[1]]
            
            ind_var_label2_char <- PrettySensParam_char[
               which(sapply(names(PrettySensParam_char), function(.) str_detect(SensParam2, .)))][[1]]
            
         } else {
            # this is when their ind var 2 is NOT one of the established pretty options
            ind_var_label2 <- SensParam2
            ind_var_label2_char <- SensParam2
         }
         
      } else {
         # this is when they HAVE provided labels for ind var 2
         ind_var_label2_char <- ind_var_label2
      }
      
      # Adding character labels for facets to match the labels
      SAdata <- SAdata %>% 
         mutate(SensValue2_char = paste(ind_var_label2_char, "=", SensValue2), 
                SensValue2_char = forcats::fct_reorder(.f = SensValue2_char, .x = SensValue2, .fun = min))
      
   } 
   
   ## axis limits and breaks --------------------------------------------------
   
   # Setting axis limits 
   if(all(complete.cases(y_axis_limits_lin))){
      
      LinYMin <- sort(y_axis_limits_lin)[1]
      LinYMax <- sort(y_axis_limits_lin)[2]
      
   } else {
      
      if(str_detect(dependent_variable, "plasma|blood|conc")){
         
         LinYMin <- round_down(min(SAdata$Conc, na.rm = T))
         LinYMax <- round_up_nice(max(SAdata$Conc, na.rm = T))
         
      } else {
         
         LinYMin <- round_down(min(SAdata$DV, na.rm = T))
         LinYMax <- round_up_nice(max(SAdata$DV, na.rm = T))
         
      }
   }
   
   if(str_detect(dependent_variable, "plasma|blood|conc")){
      
      if(any(is.na(time_range))){
         
         XLim <- range(c(SAdata$Time, ObsData$Time), na.rm = T)
         
         LinXMin <- ifelse(is.na(time_range[1]), 
                           round_down(XLim[1]), time_range[1])
         LinXMax <- ifelse(is.na(time_range[2]), 
                           round_up_nice(XLim[2]), time_range[2])
         
      } else {
         
         XLim <- sort(time_range)
         LinXMin <- XLim[1]
         LinXMax <- XLim[2]
         
      }
      
   } else {
      if(all(complete.cases(x_axis_limits_lin))){
         
         LinXMin <- sort(x_axis_limits_lin)[1]
         LinXMax <- sort(x_axis_limits_lin)[2]
         
      } else {
         
         if(complete.cases(SensParam2)){
            
            LinXMin <- round_down(
               switch(color_by_which_indvar, 
                      "1st" = min(SAdata$SensValue2), 
                      "2nd" = min(SAdata$SensValue)))
            
            LinXMax <- round_up_nice(
               switch(color_by_which_indvar, 
                      "1st" = max(SAdata$SensValue2), 
                      "2nd" = max(SAdata$SensValue)))
            
         } else {
            
            LinXMin <- round_down(min(SAdata$SensValue, na.rm = T))
            LinXMax <- round_up_nice(max(SAdata$SensValue, na.rm = T))
            
         }
      }
   }
   
   if(all(complete.cases(y_axis_limits_log))){
      
      LogY <- make_log_breaks(
         data_range = sort(y_axis_limits_log), 
         axis_limits_log = sort(y_axis_limits_log))
      
   } else {
      
      if(str_detect(dependent_variable, "plasma|blood|conc")){
         
         LogY <- make_log_breaks(
            data_range = range(SAdata$Conc[SAdata$Conc > 0], na.rm = T), 
            axis_limits_log = y_axis_limits_log)
         
      } else {
         
         LogY <- make_log_breaks(
            data_range = range(SAdata$DV[SAdata$DV > 0], na.rm = T), 
            axis_limits_log = y_axis_limits_log)
      }
      
   }
   LogYMin <- LogY$axis_limits_log[1]
   LogYMax <- LogY$axis_limits_log[2]
   
   # LogXBreaks will only apply when they have requested log-transformed x
   # axis, which is not among the options when the DV is plasma or blood
   # concentrations. For that reason, it does not matter that the range does
   # not consider the range of time included. 
   if(all(complete.cases(x_axis_limits_log))){
      
      LogX <- make_log_breaks(
         data_range = sort(x_axis_limits_log), 
         axis_limits_log = sort(x_axis_limits_log))
      
   } else {
      
      if(complete.cases(SensParam2)){
         
         LogX <- make_log_breaks(
            data_range = switch(
               color_by_which_indvar, 
               "1st" = range(SAdata$SensValue2[SAdata$SensValue2 > 0]), 
               "2nd" = range(SAdata$SensValue[SAdata$SensValue > 0])), 
            axis_limits_log = x_axis_limits_log)
         
      } else {
         
         LogX <- make_log_breaks(
            data_range = range(SAdata$SensValue[SAdata$SensValue > 0]), 
            axis_limits_log = x_axis_limits_log)
         
      }
   }
   LogXMin <- LogX$axis_limits_log[1]
   LogXMax <- LogX$axis_limits_log[2]
   
   
   # Making graphs --------------------------------------------------------
   
   if(str_detect(dependent_variable, "plasma|blood|conc")){
      
      if(color_by_which_indvar == "1st"){
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue)))
         
         G <- ggplot(SAdata, 
                     aes(x = Time, y = Conc, color = as.factor(SensValue), 
                         group = SensValue)) +
            scale_color_manual(values = MyColors) +
            geom_line()
         
      } else {
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue2)))
         
         G <- ggplot(SAdata, 
                     aes(x = Time, y = Conc, 
                         color = as.factor(SensValue2), 
                         group = SensValue2)) +
            scale_color_manual(values = MyColors) +
            geom_line()
         
      }
      
      if(length(ObsRow) > 0){
         
         MyShapes <- rep(c(15:18, 0:14), 
                         length(unique(ObsData$Subject)))[
                            1:length(unique(ObsData$Subject))]
         
         G <- G +
            geom_point(data = ObsData, 
                       switch(as.character(length(unique(ObsData$Subject)) == 1), 
                              "TRUE" = aes(x = Time, y = Conc),
                              "FALSE" = aes(x = Time, y = Conc, 
                                            shape = Subject, group = Subject)), 
                       inherit.aes = FALSE) +
            scale_shape_manual(values = MyShapes)
      }
      
      if(complete.cases(SensParam2)){
         if(color_by_which_indvar == "1st"){
            G <- G + facet_wrap(~ SensValue2_char)
            
         } else {
            G <- G + facet_wrap(~ SensValue_char)
         }
      }
      
      G <- G +
         labs(color = switch(color_by_which_indvar, 
                             "1st" = ind_var_label, 
                             "2nd" = ind_var_label2)) +
         xlab("Time (h)") +
         ylab("Concentration (ng/mL)")
      
   } else {
      
      # This is when it's something other than plasma or blood profiles that
      # we're plotting
      
      if(color_by_which_indvar == "1st"){
         if(complete.cases(SensParam2)){
            
            # Setting up colors
            MyColors <- make_color_set(color_set = color_set, 
                                       num_colors = length(unique(SAdata$SensValue)))
            
            G <- ggplot(SAdata,
                        aes(x = SensValue2, y = DV, 
                            color = as.factor(SensValue), 
                            group = SensValue)) +
               scale_color_manual(values = MyColors) +
               geom_point() + geom_line() +
               xlab(ind_var_label2) 
            
         } else {
            
            # This will have only 1 color, but we're including color for
            # consistency w/other scenarios.
            MyColors <- make_color_set(color_set = "black", 
                                       num_colors = 1)
            
            G <- ggplot(SAdata, 
                        aes(x = SensValue, y = DV, color = "placeholder")) +
               geom_point() + geom_line() +
               scale_color_manual(values = MyColors) +
               guides(color = "none") +
               xlab(ind_var_label)
            
         }
         
      } else {
         # This is when they're coloring by the 2nd ind var, which only happens
         # when there are 2 of them.
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue2)))
         
         G <- ggplot(SAdata, 
                     aes(x = SensValue, y = DV, 
                         color = as.factor(SensValue2), 
                         group = SensValue2)) +
            scale_color_manual(values = MyColors) +
            geom_point() + geom_line() +
            xlab(ind_var_label)
      }
      
      G <- G +
         ylab(PrettyDV[[dependent_variable]])
      
      G <- G +
         labs(color = switch(color_by_which_indvar, 
                             "1st" = ind_var_label, 
                             "2nd" = ind_var_label2))
      
   }
   
   G <- G + 
      # Adding a border if there is a 2nd sensitivity parameter b/c that means
      # that the graphs will be faceted.
      theme_consultancy(border = complete.cases(SensParam2)) +
      theme(axis.title = element_text(color = "black", face = "plain")) # Note that this is NOT bold b/c can't make expressions bold and you could thus end up with 1 axis title bold and 1 regular.
   
   if(complete.cases(graph_title)){
      G <- G + ggtitle(graph_title) +
         theme(plot.title = element_text(hjust = 0.5))
   }
   
   # Adding legend as needed
   G <- G + theme(legend.position = legend_position)
   
   if(complete.cases(target_DV)){
      G <- G + 
         geom_hline(yintercept = target_DV, linetype = "dotted", 
                    color = "red") +
         annotate("text", x = -Inf, y = target_DV, 
                  vjust = -0.5, hjust = -0.5, color = "red", 
                  fontface = "italic",
                  label = paste0("target = ", prettyNum(target_DV, big.mark = ",")))
   }
   
   # Applying axis limits and breaks
   if(str_detect(dependent_variable, "plasma|blood|conc")){
      
      G <- G + 
         scale_x_time(time_range = c(LinXMin, LinXMax), 
                      x_axis_interval = x_axis_interval, 
                      impose_limits = F)
      
   } else {
      
      G <- G + 
         scale_x_continuous(
            limits = c(LinXMin, LinXMax), 
            breaks = switch(as.character(complete.cases(x_axis_interval)), 
                            "TRUE" = seq(LinXMin, LinXMax, by = x_axis_interval), 
                            "FALSE" = waiver())) 
      
   }
   
   # Need to make the log-transformed graph 1st and THEN apply the
   # scale_y_continuous, coord_cartesian functions.
   if(linear_or_log %in% c("linear x log y",
                           "log x log y",
                           "log x log y horizontal",
                           "log x log y vertical",
                           "both vertical",
                           "both horizontal")){
      
      suppressMessages(
         G_logY <- G +
            scale_y_log10(breaks = LogY$breaks, 
                          labels = LogY$labels, 
                          limits = LogY$axis_limits_log))
      
      if(linear_or_log %in% c("log x log y",
                              "log x log y horizontal",
                              "log x log y vertical")){
         
         G_logXlogY <- G_logY + 
            scale_x_log10(breaks = LogX$breaks, 
                          labels = LogX$labels, 
                          limits = LogX$axis_limits_log) + 
            coord_cartesian(xlim = c(LogXMin, LogXMax), 
                            ylim = c(LogYMin, LogYMax))
         
      } else {
         G_logY <- G_logY + 
            coord_cartesian(xlim = c(LinXMin, LinXMax), 
                            ylim = c(LogYMin, LogYMax))
      }
   } else if(linear_or_log %in% c("log x linear y")){
      G_logX <- G + 
         coord_cartesian(xlim = c(LogXMin, LogXMax), 
                         ylim = c(LinYMin, LinYMax))
   }
   
   G <- G + 
      scale_y_continuous(
         limits = c(LinYMin, LinYMax), 
         breaks = switch(as.character(complete.cases(y_axis_interval)), 
                         "TRUE" = seq(LinYMin, LinYMax, by = y_axis_interval), 
                         "FALSE" = waiver())) + 
      coord_cartesian(xlim = c(LinXMin, LinXMax), 
                      ylim = c(LinYMin, LinYMax))
   
   `&` <- patchwork:::`&.gg`
   
   # Situation where both or just x are log transformed is covered below. 
   suppressWarnings(
      Out_G <- switch(
         linear_or_log, 
         "linear x linear y" = G, 
         
         "log x linear y" = G_logX,
         
         "linear x log y" = G_logY,
         
         "log x log y" = G_logXlogY,
         
         "both vertical" = 
            patchwork::wrap_plots(G, G_logY, nrow = 2) +
            patchwork::plot_layout(guides = "collect") & 
            theme(legend.position = legend_position), 
         
         "both horizontal" = 
            patchwork::wrap_plots(G, G_logY, nrow = 1) +
            patchwork::plot_layout(guides = "collect") & 
            theme(legend.position = legend_position)
      ))
   
   
   # Saving -------------------------------------------------------------
   
   if(complete.cases(save_graph)){
      FileName <- save_graph
      if(str_detect(FileName, "\\.")){
         # Making sure they've got a good extension
         Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
         FileName <- sub(paste0(".", Ext), "", FileName)
         if(Ext %in% c("eps", "ps", "jpeg", "tiff",
                       "png", "bmp", "svg", "jpg", "docx") == FALSE){
            warning(paste0("You have requested the graph's file extension be `", 
                           Ext, "`, but we haven't set up that option. We'll save your graph as a `png` file instead.\n"),
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
         
         rmarkdown::render(system.file("rmarkdown/templates/sensitivity-analysis-plot/skeleton/skeleton.Rmd",
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
                plot = Out_G)
         
      }
   }
   
   if(return_data){
      
      if(length(ObsData) > 0){
         DF <- bind_rows(SAdata, ObsData)
      } else {
         DF <- SAdata
      }
      
      DF <- DF %>% 
         select(any_of(c("Run", SensParam, "DV", "SI", "EI",
                         "SensValue", "SensValue2", "Subject", 
                         "Simulated", "Time", "Conc")))
      
      if(SensParam %in% names(DF) == FALSE){
         names(DF)[names(DF) == "SensValue"] <- SensParam
      }
      
      if(complete.cases(SensParam2) &&
         SensParam2 %in% names(DF) == FALSE){
         names(DF)[names(DF) == "SensValue2"] <- SensParam2
      }
      
      return(list("graph" = Out_G, 
                  "data" = DF))
      
   } else {
      return(Out_G)
   }
   
}


