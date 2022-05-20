#' Make graphs of sensitivity analysis results - UNDER CONSTRUCTION
#'
#' Doesn't detect units yet. Would need to check that sheet names are
#' consistent. I bet they're not. Would like to add options for axis breaks and
#' limits as well as color schemes. Will need to add more options for labeling
#' the sensitivity parameter in the graphs. Does not do 3D graphs and I have no
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
#'
#' @return
#' @export
#'
#' @examples
#'
#' SA_file = "../SA example.xlsx"
#' dependent_variable = "CL"
#' 
sensitivity_plot <- function(SA_file, 
                             dependent_variable, 
                             title = NA){
    
    # If they didn't include ".xlsx" at the end, add that.
    SA_file <- ifelse(str_detect(SA_file, "xlsx$"), 
                      SA_file, paste0(SA_file, ".xlsx"))
    
    # Get data ------------------------------------------------------------
    AllSheets <- readxl::excel_sheets(path = SA_file)
    
    DVsheets = c("CL" = "CL (L per h) (PKPD Paramete...)",
                 "Dose over AUC" = "Dose over AUC (L per h) (Sub)",
                 "AUC over Dose" = "AUC over Dose (h per L) (Sub)",
                 "Vss" = "Vss(L per kg) (Sub)",
                 "Fg" = "Fg (PKPD Parameters) (Sub)",
                 "Fh" = "Fh (PKPD Parameters) (Sub)",
                 "fa" = "fa (PKPD Parameters) (Sub)",
                 "CLpo" = "CLpo (L per h) (PKPD Parame...)",
                 "Cmax" = "Cmax  (Sub)",
                 "AUC" = "AUC  (Sub)",
                 "tmax" = "Tmax (h) (Sub)",
                 # "Multiple EI Plot",
                 "Plasma Concentration" = "Plasma Concentration (Sub)")
    
    Summary <- read.xlsx(SA_file, sheetName = "ASA Summary", 
                         header = FALSE)
    SensParam <- Summary$X2[which(Summary$X1 == "Run Number")]
    SensParam <- str_sub(SensParam, start = 1, 
                         end = str_locate(SensParam, " ")[1] - 1)
    
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
    
    return(G)
    
}


