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
#'   \strong{Coding notes:} This function does not detect units, so please check
#'   your graph labels. We plan to add options for axis breaks as well as color
#'   schemes. We are in the process of adding more options for labeling the
#'   sensitivity parameter in the graphs. This funciton does not do 3D graphs
#'   and we have no plans to add option for 3D graphs because A) they're just
#'   not easy to interpret and B) there aren't great R packages out there for 3D
#'   graphs.
#'
#' @param SA_file sensitivity analysis Excel file
#' @param dependent_variable dependent variable to plot. Options are: "AUC",
#'   "AUC over dose", "AUC over dose with interaction", "AUC ratio", "CL",
#'   "CLpo", "CLpo with interaction", "Cmax", "Cmax with interaction", "Cmax
#'   ratio", "dose over AUC", "dose over AUC with interaction", "fa", "Fg", "Fg
#'   with interaction", "Fh", "Fh with interaction", "Vss", "tmax", "tmax with
#'   interaction", or "plasma concentration" (just "plasma" will also work).
#'   Other than "plasma concentration", which is always included in sensitivity
#'   analysis output, that parameter \emph{must} be one of the ones you
#'   requested when you ran the sensitivity analysis. Not case sensitive.
#' @param ind_var_label optionally specify what text use for labeling the
#'   independent variable (the x axis). If left as NA, R will find the value
#'   listed next to "Run Number" on the "ASA Summary" tab, which may be
#'   something not terribly pretty like "Fugut Value (Sub) (No Units)", and
#'   attempt to prettify it. If we haven't set things up to prettify that value,
#'   which is likely as we haven't used this function with very many
#'   sensitivity-analysis scenarios yet, the value will be unchanged. (If you
#'   have an independent variable you'd like to add to our list, please email
#'   Laura Shireman.)
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
#' @param linear_or_log make the y axis "linear" (default) or "log" for plasma
#'   concentration-time plots. If you're graphing some other dependent variable,
#'   you can also opt for "log x" to log-transform the x axis, "log y" to
#'   log-transform the y axis (same as a traditional semi-log plot), or "both
#'   log", which will log transform both the x and y axes.
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected.
#' @param y_axis_limits_log optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to a round number. If left as the default NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#' @param x_axis_limits_lin optionally set the X axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the X axis
#'   limits for the linear plot will be automatically selected. This does not
#'   apply when the dependent variable requested is plasma concentrations.
#' @param x_axis_limits_log optionally set the X axis limits for the log plot,
#'   e.g., \code{c(10, 1000)}. Values will be rounded down and up, respectively,
#'   to a round number. If left as the default NA, the X axis limits for the
#'   semi-log plot will be automatically selected. This does not apply when the
#'   dependent variable requested is plasma concentrations.
#' @param time_range time range to show relative to the start of the simulation
#'   for any concentration-time plots. This does not apply when the plot is of
#'   some other dependent variable than plasma.
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
                             target_DV = NA,
                             color_by_which_indvar = "1st",
                             linear_or_log = "linear",
                             y_axis_limits_lin = NA,
                             y_axis_limits_log = NA,
                             x_axis_limits_lin = NA,
                             x_axis_limits_log = NA,
                             time_range = NA,
                             rounding = "significant 3", 
                             color_set = "blues", 
                             graph_title = NA,
                             save_graph = NA,
                             fig_height = 4,
                             fig_width = 5, 
                             return_data = FALSE){
   
   # Error catching ------------------------------------------------------
   
   if(missing(SA_file)){
      stop("You must provide a sensitivity-analysis Excel file for the argument 'SA_file'.", 
           call. = FALSE)
   }
   
   # If they didn't include ".xlsx" at the end, add that.
   SA_file <- ifelse(str_detect(SA_file, "xlsx$"), 
                     SA_file, paste0(SA_file, ".xlsx"))
   
   if(missing(dependent_variable)){
      stop(wrapn("You have not provided a dependent variable, so we don't know what to graph. Please check the help file for a list of acceptable values for the argument 'dependent_variable'."), 
           call. = FALSE)
   }
   
   color_by_which_indvar <- case_when(
      tolower(color_by_which_indvar) %in% c("1", "1st", "first") ~ "1st", 
      tolower(color_by_which_indvar) %in% c("2", "2nd", "second") ~ "2nd")
   
   if(color_by_which_indvar %in% c("1st", "2nd") == FALSE){
      warning(wrapn("You have specified something for the argument `color_by_which_indvar` other than 1st or 2nd, which are the only possible options. We'll use the default value of 1st."), 
              call. = FALSE)
      color_by_which_indvar <- "1st"
   }
   
   if(str_detect(rounding, "signif|none|round") == FALSE){
      warning(wrapn("You have not entered one of the permissible options for rounding. The only options for rounding are `significant X` (default is to round by 3 sig figs), `round X`, or `none`, so we'll use the default rounding scheme."), 
              call. = FALSE)
      rounding <- "significant 3"
   }
   
   linear_or_log <- tolower(linear_or_log)
   
   if(linear_or_log %in% c("both log", "log x") &
      str_detect(dependent_variable, "plasma")){
      warning(wrapn(paste0("You requested '", linear_or_log, 
                           "' for the argument 'linear_or_log', but you also requested a plasma concentration-time plot, and that's not an option there. We'll return both linear and semi-log concentration-time plots.")), 
              call. = FALSE)
      linear_or_log <- "both vertical"
   }
   
   if(linear_or_log %in% c("both", "both vertical", "both horizontal", "linear", 
                           "semi-log", "log", "both log", "log x", 
                           "log y") == FALSE){
      warning(wrapn("You have specified something for the argument 'linear_or_log' that is not among the possible options. We'll make a linear graph."), 
              call. = FALSE)
      linear_or_log <- "linear"
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
      "fg" = AllSheets[str_detect(AllSheets, "Fg .PKPD") & 
                          !str_detect(AllSheets, "with int")],
      "fg with interaction" = AllSheets[str_detect(AllSheets, "Fg .PKPD.*with inter") & 
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
      "plasma concentrations" = AllSheets[str_detect(AllSheets, "Plasma Concentration")]
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
   
   if(str_detect(dependent_variable, "plasma")){
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
      
      # # Checking for a 2nd independent parameter
      # if(complete.cases(Summary$X3[which(Summary$X1 == "Run Number")])){
      #    
      #    # warning("It looks like this sensitivity analysis contains more than one independent variable. Unfortunately, this function has only been set up to graph a single independent variable unless you're graphing plasma, so only the first one will be graphed.\n",
      #    #         call. = FALSE)
      # }
      
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
   
   # Graph ----------------------------------------------------------------
   
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
                                "^Kp Scalar" = "kp scalar",
                                "Lag Time" = "lag time (h)",
                                "LogP" = "logP", 
                                "Peff" = "Peff",
                                "Tissue.plasma partition.*Additional Organ" = "kp scalar for additional organ",
                                "Vss" = "Vss (L/kg)")
   
   if(class(ind_var_label) == "logical" && is.na(ind_var_label)){
      if(any(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))){
         
         ind_var_label <- PrettySensParam[which(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))][[1]]
         ind_var_label_char <- PrettySensParam_char[which(sapply(names(PrettySensParam_char), function(.) str_detect(SensParam, .)))][[1]]
         
      } else {
         ind_var_label <- SensParam
         ind_var_label_char <- SensParam
      }
      
      # Adding character labels for facets if needed
      SAdata <- SAdata %>% 
         mutate(SensValue_char = paste(ind_var_label_char, "=", SensValue), 
                SensValue_char = forcats::fct_reorder(.f = SensValue_char, .x = SensValue, .fun = min))
      
      # Will need to expand this based on what output names are. Need to make sure
      # they match.
      
      if(complete.cases(SensParam2)){
         if(any(sapply(names(PrettySensParam), function(.) str_detect(SensParam2, .)))){
            
            ind_var_label2 <- PrettySensParam[which(sapply(names(PrettySensParam), function(.) str_detect(SensParam2, .)))][[1]]
            ind_var_label2_char <- PrettySensParam_char[which(sapply(names(PrettySensParam_char), function(.) str_detect(SensParam2, .)))][[1]]
            
         } else {
            ind_var_label2 <- SensParam2
            ind_var_label2_char <- SensParam2
         }
         
         SAdata <- SAdata %>% 
            mutate(SensValue2_char = paste(ind_var_label2_char, "=", SensValue2), 
                   SensValue2_char = forcats::fct_reorder(.f = SensValue2_char, .x = SensValue2, .fun = min))
         
      }
   }
   
   if(str_detect(dependent_variable, "plasma|conc")){
      
      if(color_by_which_indvar == "1st"){
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue)))
         
         G <- ggplot(SAdata, aes(x = Time, y = Conc, color = as.factor(SensValue), 
                                 group = SensValue)) +
            scale_color_manual(values = MyColors) +
            geom_line()
         
      } else {
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue2)))
         
         G <- ggplot(SAdata, aes(x = Time, y = Conc, 
                                 color = as.factor(SensValue2), 
                                 group = SensValue2)) +
            scale_color_manual(values = MyColors) +
            geom_line()
         
      }
      
      if(length(ObsRow) > 0){
         
         MyShapes <- rep(c(15:18, 0:14), length(unique(ObsData$Subject)))[1:length(unique(ObsData$Subject))]
         
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
      
      if(all(complete.cases(y_axis_limits_lin))){
         G <- G + scale_y_continuous(limits = y_axis_limits_lin)
      }
      
   } else {
      
      # This is when it's something other than plasma profiles that we're
      # plotting
      
      if(color_by_which_indvar == "1st"){
         if(complete.cases(SensParam2)){
            
            # Setting up colors
            MyColors <- make_color_set(color_set = color_set, 
                                       num_colors = length(unique(SAdata$SensValue)))
            
            G <- ggplot(SAdata, aes(x = SensValue2, y = DV, 
                                    color = as.factor(SensValue), 
                                    group = SensValue)) +
               scale_color_manual(values = MyColors) +
               geom_point() + geom_line() +
               xlab(ind_var_label2) 
         } else {
            
            G <- ggplot(SAdata, aes(x = SensValue, y = DV)) +
               geom_point() + geom_line() +
               xlab(ind_var_label)
         }
         
      } else {
         # This is when they're coloring by the 2nd ind var, which only happens
         # when there are 2 of them.
         
         # Setting up colors
         MyColors <- make_color_set(color_set = color_set, 
                                    num_colors = length(unique(SAdata$SensValue2)))
         
         G <- ggplot(SAdata, aes(x = SensValue, y = DV, 
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
      
      if(all(complete.cases(y_axis_limits_lin)) & 
         all(complete.cases(x_axis_limits_lin))){
         G <- G + coord_cartesian(ylim = y_axis_limits_lin, 
                                  xlim = x_axis_limits_lin)
      } else if(all(complete.cases(y_axis_limits_lin)) &
                all(is.na(x_axis_limits_lin))){
         G <- G + coord_cartesian(ylim = y_axis_limits_lin)
      } else if(all(complete.cases(x_axis_limits_lin)) &
                all(is.na(y_axis_limits_lin))){
         G <- G + coord_cartesian(xlim = x_axis_limits_lin)
      }
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
   
   if(complete.cases(target_DV)){
      G <- G + 
         geom_hline(yintercept = target_DV, linetype = "dotted", 
                    color = "red") +
         annotate("text", x = -Inf, y = target_DV, 
                  vjust = -0.5, hjust = -0.5, color = "red", 
                  fontface = "italic",
                  label = paste0("target = ", prettyNum(target_DV, big.mark = ",")))
   }
   
   # Setting axis limits as needed
   LinYRange <- switch(
      as.character(all(complete.cases(y_axis_limits_lin))), 
      "TRUE" = y_axis_limits_lin, 
      "FALSE" = switch(
         as.character(str_detect(dependent_variable, "plasma|conc")), 
         "TRUE" = range(SAdata$Conc, na.rm = T), 
         "FALSE" = range(SAdata$DV, na.rm = T)))
   
   LogYBreaks <- make_log_breaks(
      data_range = switch(
         as.character(all(complete.cases(y_axis_limits_log))), 
         "TRUE" = y_axis_limits_log, 
         "FALSE" = switch(
            as.character(str_detect(dependent_variable, "plasma|conc")), 
            "TRUE" = range(SAdata$Conc[SAdata$Conc > 0], na.rm = T), 
            "FALSE" = range(SAdata$DV[SAdata$DV > 0], na.rm = T))))
   
   LinXRange <- switch(
      as.character(all(complete.cases(x_axis_limits_lin))), 
      "TRUE" = x_axis_limits_lin, 
      "FALSE" = switch(
         as.character(str_detect(dependent_variable, "plasma|conc")), 
         "TRUE" = range(SAdata$Time, na.rm = T), 
         "FALSE" = switch(as.character(complete.cases(SensParam2)), 
                          "TRUE" = switch(color_by_which_indvar, 
                                          "1st" = range(SAdata$SensValue2), 
                                          "2nd" = range(SAdata$SensValue)), 
                          "FALSE" = range(SAdata$SensValue))))
   
   # LogXBreaks will only apply when they have requested log-transformed x axis,
   # which is not among the options when the DV is plasma concentrations. For
   # that reason, it does not matter that the range does not consider the range
   # of time included.
   LogXBreaks <- make_log_breaks(
      data_range = switch(
         as.character(all(complete.cases(x_axis_limits_log))), 
         "TRUE" = x_axis_limits_log, 
         "FALSE" = switch(as.character(complete.cases(SensParam2)), 
                          "TRUE" = switch(color_by_which_indvar, 
                                          "1st" = range(SAdata$SensValue2[SAdata$SensValue2 > 0]), 
                                          "2nd" = range(SAdata$SensValue[SAdata$SensValue > 0])), 
                          "FALSE" = range(SAdata$SensValue[SAdata$SensValue > 0]))))
   
   Glog <- G +
      scale_y_log10(breaks = LogYBreaks$breaks, 
                    labels = LogYBreaks$labels) 
   
   if(linear_or_log %in% c("both", "both vertical", "both horizontal",
                           "semi-log", "log", "log y")){
      
      if(str_detect(dependent_variable, "plasma|conc")){
         
         G <- G + 
            coord_cartesian(ylim = c(round_down(LinYRange[1]),
                                     round_up_nice(LinYRange[2]))) +
            scale_x_time(time_range = x_axis_limits_lin)
         
         Glog <- Glog +
            coord_cartesian(ylim = LogYBreaks$axis_limits_log) + 
            scale_x_time(time_range = x_axis_limits_lin)
         
      } else {
         
         G <- G + 
            coord_cartesian(ylim = c(round_down(LinYRange[1]),
                                     round_up_nice(LinYRange[2])), 
                            xlim = c(round_down(LinXRange[1]),
                                     round_up_nice(LinXRange[2])))
         
         Glog <- Glog +
            coord_cartesian(ylim = c(round_down(LinYRange[1]),
                                     round_up_nice(LinYRange[2])), 
                            xlim = c(round_down(LinXRange[1]),
                                     round_up_nice(LinXRange[2])))
         
      }
   } 
   # Situation where both or just x are log transformed is covered below. 
   
   if(linear_or_log %in% c("both", "both vertical")){
      G <- ggpubr::ggarrange(G, Glog, nrow = 2, align = "hv", common.legend = TRUE)
   } else if(linear_or_log %in% c("both horizontal")){
      G <- ggpubr::ggarrange(G, Glog, nrow = 1, align = "hv", common.legend = TRUE)
   } else if(linear_or_log %in% c("semi-log", "log", "log y")){
      G <- Glog
   } else if(linear_or_log %in% c("both log")){
      G <- G + 
         scale_x_log10(breaks = LogXBreaks$breaks, 
                       limits = LogXBreaks$axis_limits_log, 
                       labels = LogXBreaks$labels) +
         scale_y_log10(breaks = LogYBreaks$breaks, 
                       limits = LogYBreaks$axis_limits_log, 
                       labels = LogYBreaks$labels)
      
   } else if(linear_or_log %in% c("log x")){
      G <- G + scale_x_log10(breaks = LogXBreaks$breaks,
                             limits = LogXBreaks$axis_limits_log, 
                             labels = LogXBreaks$labels)
   }
   
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
                plot = G)
         
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
      
      return(list("graph" = G, 
                  "data" = DF))
      
   } else {
      return(G)
   }
   
}


