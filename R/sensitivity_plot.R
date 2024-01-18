#' Make graphs of sensitivity analysis results
#'
#' \code{sensitivity_plot} will read a sensitivity-analysis Excel file and
#' create a graph of the dependent variable of your choice as long as it was
#' included in your results. If you ran a sensitivity analysis with more than 1
#' independent variable -- e.g., you wanted to see how adjusting both the kp
#' scalar and the fa affected Cmax -- the best way we have come up with so far
#' to show you how both of those variables affect your dependent variable is to
#' break up the graphs into small multiples or facets (like on a gemstone).
#' (Side note: We considered a 3D graph, but they're honestly hard to do well in
#' a programmatic fashion like this.) This is set up for graphs of plasma
#' concentration-time profiles but not yet set up for other dependent variables.
#' UNDER CONSTRUCTION.
#'
#' \strong{Coding notes:} Doesn't detect units yet. Would need to check that
#' sheet names are consistent b/c I bet they're not. Would like to add options
#' for axis breaks and limits as well as color schemes. Will need to add more
#' options for labeling the sensitivity parameter in the graphs. Does not do 3D
#' graphs and I have no plans to add option for 3D graphs b/c A) they're just
#' not easy to interpret and B) there aren't great R packages out there for 3D
#' graphs.
#'
#' @param SA_file sensitivity analysis Excel file
#' @param dependent_variable dependent variable to plot. Options are: "CL",
#'   "Dose over AUC", "AUC over Dose", "Vss", "Fg", "Fh", "fa", "CLpo", "Cmax",
#'   "AUC", "tmax", or "plasma concentration" (just "plasma" will also work).
#'   Other than "plasma concentration", that parameter \emph{must} be one of the
#'   ones you requested when you ran the sensitivity analysis. Not case
#'   sensitive.
#' @param ind_var_label optionally specify what text use for labeling the
#'   independent variable (the x axis). If left as NA, R will find the value
#'   listed next to "Run Number" on the "ASA Summary" tab, which may be
#'   something not terribly pretty like "Fugut Value (Sub) (No Units)", and
#'   attempt to prettify it. If we haven't set things up to prettify that value,
#'   which is likely as we haven't used this function with very many
#'   sensitivity-analysis scenarios yet, the value will be unchanged. (If you
#'   have an independent variable you'd like to add to our list, email Laura
#'   Shireman!)
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
#'   concentration-time plots (this is ignored for all other plots)
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected.
#' @param y_axis_limits_log optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to a round number. If left as the default NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#' @param time_range time range to show relative to the start of the simulation.
#'   Options: \describe{
#'
#'   \item{NA}{(default) entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}
#'   }
#'
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
                             time_range = NA,
                             rounding = "significant 3", 
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
      stop("You have not provided a dependent variable, so we don't know what to graph. Please check the help file for a list of acceptable values for the argument 'dependent_variable'.", 
           call. = FALSE)
   }
   
   color_by_which_indvar <- case_when(
      tolower(color_by_which_indvar) %in% c("1", "1st", "first") ~ "1st", 
      tolower(color_by_which_indvar) %in% c("2", "2nd", "second") ~ "2nd")
   
   if(color_by_which_indvar %in% c("1st", "2nd") == FALSE){
      warning("You have specified something for the argument `color_by_which_indvar` other than 1st or 2nd, which are the only possible options. We'll use the default value of 1st.\n", 
              call. = FALSE)
      color_by_which_indvar <- "1st"
   }
   
   if(str_detect(rounding, "signif|none|round") == FALSE){
      warning("You have not entered one of the permissible options for rounding. The only options for rounding are `significant X` (default is to round by 3 sig figs), `round X`, or `none`, so we'll use the default rounding scheme.\n", 
              call. = FALSE)
      rounding <- "significant 3"
   }
   
   # Get data ------------------------------------------------------------
   AllSheets <- readxl::excel_sheets(path = SA_file)
   
   dependent_variable <- tolower(dependent_variable)
   
   DVsheets = c("cl" = AllSheets[str_detect(AllSheets, "CL .L per h. .PKPD")],
                "dose over auc" = AllSheets[str_detect(AllSheets, "Dose over AUC")],
                "auc over dose" = AllSheets[str_detect(AllSheets, "AUC over Dose")],
                "vss" = AllSheets[str_detect(AllSheets, "Vss")],
                "fg" = AllSheets[str_detect(AllSheets, "Fg .PKPD")],
                "fh" = AllSheets[str_detect(AllSheets, "Fh .PKPD")],
                "fa" = AllSheets[str_detect(AllSheets, "fa .PKPD|fa..ADAM")],
                "clpo" = AllSheets[str_detect(AllSheets, "CLpo.*PKPD")],
                "cmax" = AllSheets[str_detect(AllSheets, "^Cmax")],
                "auc" = AllSheets[str_detect(AllSheets, "^AUC.*\\(") & !str_detect(AllSheets, "over")],
                "tmax" = AllSheets[str_detect(AllSheets, "Tmax")],
                # "Multiple EI Plot",
                "plasma concentration" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
                "plasma" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
                "plasma conc" = AllSheets[str_detect(AllSheets, "Plasma Concentration")], 
                "plasma concentrations" = AllSheets[str_detect(AllSheets, "Plasma Concentration")])
   
   Summary <- xlsx::read.xlsx(SA_file, sheetName = "ASA Summary", 
                              header = FALSE)
   
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
   SAdata.xl <- xlsx::read.xlsx(SA_file, sheetName = DVsheets[dependent_variable])
   
   if(str_detect(dependent_variable, "plasma")){
      ObsRow <- which(SAdata.xl[, 1] == "Observed Data")
      SimT0Col <- which(names(SAdata.xl) == "RMSE") + 1
      SimT0Col <- ifelse(length(SimT0Col) == 0, 4, SimT0Col)
      
      if(length(ObsRow) > 0){
         ObsData <- list()
         
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
      
      # Checking for a 2nd independent parameter
      if(complete.cases(Summary$X3[which(Summary$X1 == "Run Number")])){
         warning("It looks like this sensitivity analysis contains more than one independent variable. Unfortunately, this function has only been set up to graph a single independent variable unless you're graphing plasma, so only the first one will be graphed.\n",
                 call. = FALSE)
      }
      
      SAdata <- SAdata.xl
      names(SAdata)[1:2] <- c("SensValue", "DV") 
      
   }
   
   # Rounding for ease of reading legend. Could add an option for what rounding
   # to use here.
   SAdata <- SAdata %>% 
      mutate(across(.cols = any_of(c("SensValue", "SensValue2")), 
                    .fns = function(x) round_opt(x, rounding)))
   
   # Graph ----------------------------------------------------------------
   
   PrettyDV <- list("auc" = expression(AUC~"(ng/mL.h)"), 
                    "auc over dose" = expression("AUC over Dose (h/L)"),
                    "cl" = expression(CL~"L/h"), 
                    "clpo" = expression(CL[PO]~"(L/h)"), 
                    "cmax" = expression(C[max]~"(ng/mL)"), 
                    "dose over auc" = expression(Dose~over~AUC~"(L/h)"),
                    "fa" = expression(f[a]),
                    "fg" = expression(F[g]),
                    "fh" = expression(F[h]),
                    "tmax" = expression(t[max]~"(h)"),
                    "vss" = expression(V[ss]~"(L/kg)"))
   
   PrettySensParam <- list("Dose .Sub" = "Dose (mg)",
                           "Intestinal ABCB1 .P-gp. CLint,T" = "Intestinal P-gp CLint,T (µL/min)",
                           "fa" = expression(f[a]),
                           "Fugut" = expression(f[u[gut]]), 
                           "ka" = expression(k[a]), 
                           "^Kp Scalar" = expression(k[p]~scalar),
                           "Lag Time" = expression("lag time (h)"),
                           "Tissue.plasma partition.*Additional Organ" = expression(k[p]~"scalar for additional organ"),
                           "Vss" = expression(V[ss]~"(L/kg)"), 
                           "Peff" = expression(P[eff,human]))
   
   PrettySensParam_char <- list("Dose .Sub" = "Dose (mg)",
                                "Intestinal ABCB1 .P-gp. CLint,T" = "Intestinal P-gp CLint,T (µL/min)",
                                "fa" = "fa",
                                "Fugut" = "fu,gut", 
                                "ka" = "ka", 
                                "^Kp Scalar" = "kp scalar",
                                "Lag Time" = "lag time (h)",
                                "Tissue.plasma partition.*Additional Organ" = "kp scalar for additional organ",
                                "Vss" = "Vss (L/kg)",
                                "Peff" = "Peff")
   
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
         G <- ggplot(SAdata, aes(x = Time, y = Conc, color = as.factor(SensValue), 
                                 group = SensValue)) +
            scale_color_manual(values = blues(ncolors = length(unique(SAdata$SensValue)))) +
            geom_line()
      } else {
         G <- ggplot(SAdata, aes(x = Time, y = Conc, color = as.factor(SensValue2), 
                                 group = SensValue2)) +
            scale_color_manual(values = blues(ncolors = length(unique(SAdata$SensValue2)))) +
            geom_line()
         
      }
      
      if(length(ObsRow) > 0){
         G <- G +
            geom_point(data = ObsData, 
                       switch(as.character(length(unique(ObsData$Subject)) == 1), 
                              "TRUE" = aes(x = Time, y = Conc),
                              "FALSE" = aes(x = Time, y = Conc, 
                                            shape = Subject, group = Subject)), 
                       inherit.aes = FALSE) 
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
         scale_x_time(pad_x_axis = FALSE, 
                      time_range = time_range) +
         xlab("Time (h)") +
         ylab("Concentration (ng/mL)")
      
      if(linear_or_log != "linear"){
         if(all(complete.cases(y_axis_limits_log))){
            G <- G + scale_y_log10(limits = y_axis_limits_log)
         } else {
            G <- G + scale_y_log10()
         }
      } else {
         if(all(complete.cases(y_axis_limits_lin))){
            G <- G + scale_y_continuous(limits = y_axis_limits_lin)
         }
      }
      
      
   } else {
      
      G <- ggplot(SAdata, aes(x = SensValue, y = DV)) +
         geom_point() + geom_line() + 
         ylab(PrettyDV[[dependent_variable]]) +
         xlab(ind_var_label)
      
      if(all(complete.cases(y_axis_limits_lin))){
         G <- G + scale_y_continuous(limits = y_axis_limits_lin)
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
      
      DF <- bind_rows(SAdata, ObsData) %>% 
         select(Run, SensValue, SensValue2, Subject, Simulated, Time, Conc)
      names(DF)[names(DF) == "SensValue"] <- SensParam
      names(DF)[names(DF) == "SensValue2"] <- SensParam2
      
      return(list("graph" = G, 
                  "data" = DF))
      
   } else {
      return(G)
   }
   
}


