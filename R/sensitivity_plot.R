#' Make graphs of sensitivity analysis results - UNDER CONSTRUCTION
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
#'   "AUC", "tmax", or "Plasma Concentration", and, other than "Plasma
#'   Concentration", that parameter \emph{must} be one of the ones you requested
#'   when you ran the sensitivity analysis. Currently case sensitive.
#' @param title (optional) a title to include on your graph in quotes
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png" or "My conc time
#'   graph.docx". If you leave off ".png" or ".docx" from the file name, it will
#'   be saved as a png file, but if you specify a different graphical file
#'   extension, it will be saved as that file format. Acceptable graphical file
#'   extensions are "eps", "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg".
#'   Leaving this as NA means the file will not be saved to disk.
#' @param fig_height figure height in inches; default is 4
#' @param fig_width figure width in inches; default is 5
#'
#' @return
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
                             title = NA,
                             save_graph = NA,
                             fig_height = 4,
                             fig_width = 5){
    
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
    
    DVsheets = c("CL" = AllSheets[str_detect(AllSheets, "CL .L per h. .PKPD")],
                 "Dose over AUC" = AllSheets[str_detect(AllSheets, "Dose over AUC")],
                 "AUC over Dose" = AllSheets[str_detect(AllSheets, "AUC over Dose")],
                 "Vss" = AllSheets[str_detect(AllSheets, "Vss")],
                 "Fg" = AllSheets[str_detect(AllSheets, "Fg .PKPD")],
                 "Fh" = AllSheets[str_detect(AllSheets, "Fh .PKPD")],
                 "fa" = AllSheets[str_detect(AllSheets, "fa .PKPD")],
                 "CLpo" = AllSheets[str_detect(AllSheets, "CLpo.*PKPD")],
                 "Cmax" = AllSheets[str_detect(AllSheets, "^Cmax")],
                 "AUC" = AllSheets[str_detect(AllSheets, "^AUC.*\\(")],
                 "tmax" = AllSheets[str_detect(AllSheets, "Tmax")],
                 # "Multiple EI Plot",
                 "Plasma Concentration" = AllSheets[str_detect(AllSheets, "Plasma Concentration")])
    
    Summary <- read.xlsx(SA_file, sheetName = "ASA Summary", 
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
    SAdata <- read.xlsx(SA_file, sheetName = DVsheets[dependent_variable])
    
    if(dependent_variable == "Plasma Concentration"){
        SAdata <- as.data.frame(t(SAdata[, c(4:ncol(SAdata))]))
        names(SAdata)[1] <- "Time"
        names(SAdata)[2:ncol(SAdata)] <- paste0("Run", RunInfo$Run)
        
        SAdata <- SAdata %>% 
            pivot_longer(cols = matches("Run"), 
                         names_to = "Run", 
                         values_to = "Conc") %>% 
            mutate(Run = as.numeric(sub("Run", "", Run))) %>% 
            left_join(RunInfo)
    } else {
        names(SAdata)[1:2] <- c("SensParameter", "DV")
    }
    
    # Graph ----------------------------------------------------------------
    
    PrettyDV <- list("CL" = expression(CL~"L/h"), 
                     "Dose over AUC" = expression(Dose~over~AUC~"(L/h)"),
                     "AUC over Dose" = expression("AUC over Dose (h/L)"),
                     "Vss" = expression(V[ss]~"(L/kg)"),
                     "Fg" = expression(F[g]),
                     "Fh" = expression(F[h]),
                     "fa" = expression(f[a]),
                     "CLpo" = expression(CL[PO]~"(L/h)"), 
                     "Cmax" = expression(C[max]~"(ng/mL)"), 
                     "AUC" = expression(AUC~"(ng/mL.h)"), 
                     "tmax" = expression(t[max]~"(h)"))
    
    PrettySensParam <- list("Fugut" = expression(f[u[gut]]), 
                            "fa" = expression(f[a]),
                            "ka" = expression(k[a]), 
                            "Vss" = expression(V[ss]~"(L/kg)"),
                            "Lag Time" = expression("lag time (h)"))
    
    if(SensParam %in% names(PrettySensParam) == FALSE){
        PrettySensParam <- list(SensParam)
        names(PrettySensParam) <- SensParam
    }
    # Will need to expand this based on what output names are. Need to make sure
    # they match.
    
    if(dependent_variable == "Plasma Concentration"){
        G <- ggplot(SAdata, aes(x = Time, y = Conc, color = SensValue, 
                                group = SensValue)) +
            geom_line() +
            labs(color = SensParam) +
            xlab("Time (h)") +
            ylab("Concentration (ng/mL)")
        
        
    } else {
        
        G <- ggplot(SAdata, aes(x = SensParameter, y = DV)) +
            geom_point() + geom_line() + 
            ylab(PrettyDV[[dependent_variable]]) +
            xlab(PrettySensParam[[SensParam]])
    }
    
    G <- G + 
        theme(panel.background = element_rect(fill="white", color=NA),
              legend.key = element_rect(fill = "white"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black", face = "bold"),
              axis.line.x.bottom = element_line(color = "black"),
              axis.line.y.left = element_line(color = "black"))
    
    if(complete.cases(title)){
        G <- G + ggtitle(title)
    }
    
    if(complete.cases(save_graph)){
        FileName <- save_graph
        if(str_detect(FileName, "\\.")){
            # Making sure they've got a good extension
            Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
            FileName <- sub(paste0(".", Ext), "", FileName)
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
    return(G)
    
}


