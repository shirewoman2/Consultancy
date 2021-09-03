#' extractPK
#'
#' @param sim_data_file name of the Excel file containing the simulator output
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file, all of which will be pulled from the "AUC" tab. Options are
#'   any combination of the following: \describe{ \item{"AUCtau_dose1"}{AUC from
#'   0 to tau for dose 1. This is the column "AUCt(0) (mg/L.h)".}
#'   \item{AUCtau_lastdose}{AUC from 0 to tau for the last dose in the
#'   simulation. This is the column "AUC (mg/L.h)" under the subheading "AUC
#'   integrated from X to Y".} \item{"AUCinf_dose1}{AUC from 0 to infinity for
#'   dose 1 (column: "AUC_INF (mg/L.h)")} \item{"HalfLife_dose1"}{half life,
#'   estimated for dose 1 (column: "Half-life (h)")} \item{"CL_dose1}{Clearance
#'   as calculated by dose / AUCinf for dose 1 (column: "CL (Dose/AUC_INF)
#'   (L/h)")} }
#'
#' @return
#' @export
#'
#' @examples
#'
#' sim_data_file <- "../Example simulator output.xlsx"
#' extractPK(sim_data_file)
#' extractPK(sim_data_file, PKparameters = "AUCinf_dose1")
#'
#'
extractPK <- function(sim_data_file,
                      PKparameters = c("AUCtau_dose1", "AUCtau_lastdose",
                                       "AUCinf_dose1", "HalfLife_dose1",
                                       "CL_dose1")){

      # Error catching
      if("AUC" %in% readxl::excel_sheets(sim_data_file) == FALSE){
            stop("The tab 'AUC' must be present in the Excel simulated data file to extract PK parameters.")
      }

      AUC_xl <- suppressMessages(
            readxl::read_excel(path = sim_data_file, sheet = "AUC",
                               col_names = FALSE))

      EndRow <- which(AUC_xl$...2 == "Statistics") - 2
      Out <- data.frame(Index = 1:(EndRow - 3))


      if("AUCtau_dose1" %in% PKparameters){
            ColNum <- which(str_detect(as.vector(t(AUC_xl[3, ])), "AUCt.0."))

            if(length(ColNum) == 0){
                  message("The column with information for AUCtau_dose1 cannot be found.")
                  rm(ColNum)
                  next
            } else {
                  Out$AUCtau_dose1 <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                        pull(Values) %>% as.numeric
                  rm(ColNum)
            }
      }

      if("AUCtau_lastdose" %in% PKparameters){
            ColNum <- which(str_detect(as.vector(t(AUC_xl[3, ])), "AUC \\("))[1]

            if(length(ColNum) == 0){
                  message("The column with information for AUCtau_lastdose cannot be found.")
                  rm(ColNum)
                  next
            } else {
                  Out$AUCtau_lastdose <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                        pull(Values) %>% as.numeric
                  rm(ColNum)
            }
      }

      if("AUCinf_dose1" %in% PKparameters){
            ColNum <- which(str_detect(as.vector(t(AUC_xl[3, ])), "AUC_INF"))[1]
            Out$AUCinf_dose1 <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                  pull(Values) %>% as.numeric
            rm(ColNum)
      }

      if("HalfLife_dose1" %in% PKparameters){
            ColNum <- which(str_detect(as.vector(t(AUC_xl[3, ])), "Half-life"))
            Out$HalfLife_dose1 <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                  pull(Values) %>% as.numeric
            rm(ColNum)
      }

      if("CL_dose1" %in% PKparameters){
            ColNum <- which(str_detect(as.vector(t(AUC_xl[3, ])), "CL .Dose/AUC_INF"))
            Out$CL_dose1 <- AUC_xl[4:EndRow, ColNum] %>% rename(Values = 1) %>%
                  pull(Values) %>% as.numeric
            rm(ColNum)
      }

      if(ncol(Out) == 2){
            Out <- Out %>% pull(2)
      }

      return(Out)

}

# extractPK(sim_data_file, "AUCtau_dose1")

