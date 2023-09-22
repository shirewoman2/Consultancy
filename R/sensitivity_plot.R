#' Make graphs of sensitivity analysis results
#'
#' Doesn't detect units yet. Would need to check that sheet names are consistent
#' b/c I bet they're not. Would like to add options for axis breaks and limits
#' as well as color schemes. Will need to add more options for labeling the
#' sensitivity parameter in the graphs. Does not do 3D graphs and I have no
#' plans to add option for 3D graphs b/c A) they're just not easy to interpret
#' and B) there aren't great R packages out there for 3D graphs.
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
#' @param target_DV optionally specify a target value for the dependent
#'   variable, which will add a horizontal red dotted line to the graph where
#'   the target lies.
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
#' @param title (optional) a title to include on your graph in quotes
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
#'                  title = "My pretty sensitivity-analysis graph that's not pink",
#'                  save_graph = "SA graph")
#' 

sensitivity_plot <- function(SA_file, 
                             dependent_variable, 
                             ind_var_label = NA,
                             target_DV = NA,
                             linear_or_log = "linear",
                             y_axis_limits_lin = NA,
                             y_axis_limits_log = NA,
                             time_range = NA,
                             title = NA,
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
   
   # Checking for a 2nd independent parameter
   if(complete.cases(Summary$X3[which(Summary$X1 == "Run Number")])){
      warning("It looks like this sensitivity analysis contains more than one independent variable. Unfortunately, this function has only been set up to graph a single independent variable, so only the first one will be graphed.", 
              call. = FALSE)
   }
   
   # Getting the name of the independent variable
   SensParam <- Summary$X2[which(Summary$X1 == "Run Number")]
   # SensParam <- str_sub(SensParam, start = 1, 
   #                      end = str_locate(SensParam, " ")[1] - 1)
   
   RunInfo <- Summary[(which(Summary$X1 == "Run Number") + 1):nrow(Summary), 1:2]
   names(RunInfo) <- c("Run", "SensValue")
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
      SAdata <- SAdata.xl
      names(SAdata)[1:2] <- c("SensParameter", "DV")
   }
   
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
                           "Lag Time" = expression("lag time (h)"),
                           "Vss" = expression(V[ss]~"(L/kg)"), 
                           "Peff" = expression(P[eff,human]))
   
   if(class(ind_var_label) == "logical" && is.na(ind_var_label)){
      if(any(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))){
         
         ind_var_label <- PrettySensParam[which(sapply(names(PrettySensParam), function(.) str_detect(SensParam, .)))][[1]]
         
      } else {
         ind_var_label <- SensParam
      }
   }
   
   # Will need to expand this based on what output names are. Need to make sure
   # they match.
   
   if(str_detect(dependent_variable, "plasma|conc")){
      
      G <- ggplot(SAdata, aes(x = Time, y = Conc, color = SensValue, 
                              group = SensValue)) +
         geom_line()
      
      if(length(ObsRow) > 0){
         G <- G +
            geom_point(data = ObsData, 
                       switch(as.character(length(unique(ObsData$Subject)) == 1), 
                              "TRUE" = aes(x = Time, y = Conc),
                              "FALSE" = aes(x = Time, y = Conc, 
                                            shape = Subject, group = Subject)), 
                       inherit.aes = FALSE) 
      }
      
      G <- G +
         labs(color = ind_var_label) +
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
      
      G <- ggplot(SAdata, aes(x = SensParameter, y = DV)) +
         geom_point() + geom_line() + 
         ylab(PrettyDV[[dependent_variable]]) +
         xlab(ind_var_label)
      
      if(all(complete.cases(y_axis_limits_lin))){
         G <- G + scale_y_continuous(limits = y_axis_limits_lin)
      }
   }
   
   G <- G + 
      theme_consultancy() +
      theme(axis.title = element_text(color = "black", face = "plain")) # Note that this is NOT bold b/c can't make expressions bold and you could thus end up with 1 axis title bold and 1 regular.
   
   if(complete.cases(title)){
      G <- G + ggtitle(title)
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
      return(list("graph" = G, 
                  "data" = bind_rows(SAdata, ObsData)))
   } else {
      return(G)
   }
   
}


