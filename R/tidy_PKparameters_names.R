#' INTERNAL USE. tidy the column names of PKparameters supplied to, e.g.,
#' pk_table.
#'
#' @param PKparameters a data.frame of PK parameters
#'
#' @return a data.frame of PK parameters with harmonized column names
#' 
#' 
tidy_PKparameters_names <- function(PKparameters){
   
   # PKparameters ------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "pkparameter"] <- "PKparameter"
   if("PKparameter" %in% names(PKparameters) == FALSE &
      any(c("pkparam", "param", "parameter", "pkparameters") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- which(tolower(names(PKparameters)) %in% 
                           c("pkparameter", "pkparameters", "pkparam", "param", 
                             "parameter"))[1]
      
      warning(paste0("We were looking for a column named `PKparameter` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for the PK parameter name.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "PKparameter"
      rm(ColToUse)
   } 
   
   
   # File -----------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "file"] <- "File"
   if(any(c("simulation", "workspace", "sim") %in% tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "simulation"), 
                    which(tolower(names(PKparameters)) == "workspace"), 
                    which(tolower(names(PKparameters)) == "sim"))[1]
      
      warning(paste0("We were looking for a column named `File` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data that column for the file names.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "File"
      rm(ColToUse)
   }
   
   
   # ObsValue ------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "obsvalue"] <- "ObsValue"
   if("ObsValue" %in% names(PKparameters) == FALSE &&
      any(c("geomean", "gm_mean", "gmean", "mean", "value") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "value"), 
                    which(tolower(names(PKparameters)) == "geomean"), 
                    which(tolower(names(PKparameters)) == "gm_mean"), 
                    which(tolower(names(PKparameters)) == "gmean"), 
                    which(tolower(names(PKparameters)) == "mean"))[1]
      
      warning(paste0("We were looking for a column named `ObsValue` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for any observed values.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "ObsValue"
      rm(ColToUse)
   } 
   
   
   # ObsVariability -------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "obsvariability"] <- "ObsVariability"
   if("ObsVariability" %in% names(PKparameters) == FALSE &&
      (any(c("variability", "cv", "var", "sd", "range", "min", "minimum") %in%
           tolower(names(PKparameters))) |
       any(str_detect(tolower(names(PKparameters)), "conf")))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "variability"), 
                    which(tolower(names(PKparameters)) == "var"), 
                    which(tolower(names(PKparameters)) == "cv"), 
                    which(tolower(names(PKparameters)) == "sd"), 
                    which(tolower(names(PKparameters)) == "range"), 
                    which(tolower(names(PKparameters)) == "minimum"), 
                    which(tolower(names(PKparameters)) == "min"), 
                    which(str_detect(tolower(names(PKparameters)), "conf")))[1]
      
      warning(paste0("We were looking for a column named `ObsVariability` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for any observed variability.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "ObsVariability"
      rm(ColToUse)
   } 
   
   
   # Sheet -------------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "sheet"] <- "Sheet"
   if(any(c("tab", "sheets", "sheet_pkparameter", "sheet_pkparameters") %in% 
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "tab"), 
                    which(tolower(names(PKparameters)) == "sheets"), 
                    which(tolower(names(PKparameters)) == "sheet_pkparameter"), 
                    which(tolower(names(PKparameters)) == "sheet_pkparameters"))[1]
      
      warning(paste0("We were looking for a column named `Sheet` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data that column for the sheet names for any user-defined intervals.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Sheet"
      rm(ColToUse)
   }
   
   
   # CompoundID --------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "compoundid"] <- "CompoundID"
   if("CompoundID" %in% names(PKparameters) == FALSE &&
      any(c("compound", "compounds", "compoundids", "cmpd", "drug") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "compoundids"), 
                    which(tolower(names(PKparameters)) == "compound"), 
                    which(tolower(names(PKparameters)) == "compounds"), 
                    which(tolower(names(PKparameters)) == "cmpd"), 
                    which(tolower(names(PKparameters)) == "drug"))[1]
      
      warning(paste0("We were looking for a column named `CompoundID` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for specifying which compound it is.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "CompoundID"
      rm(ColToUse)
   }
   
   
   # Tissue ------------------------------------------------------------------
   
   names(PKparameters)[tolower(names(PKparameters)) == "tissue"] <- "Tissue"
   if("Tissue" %in% names(PKparameters) == FALSE &&
      any(c("tissues", "matrix") %in%
          tolower(names(PKparameters)))){
      
      ColToUse <- c(which(tolower(names(PKparameters)) == "tissues"), 
                    which(tolower(names(PKparameters)) == "matrix"))[1]
      
      warning(paste0("We were looking for a column named `Tissue` in what you supplied for `PKparameters` and did not find it, but we *did* find a column called `", 
                     names(PKparameters)[ColToUse],
                     "`, which we think is what you might want. We'll use the data in that column for specifying which tissue it is.\n"), 
              call. = FALSE)
      
      names(PKparameters)[ColToUse] <- "Tissue"
      rm(ColToUse)
   }
   
   return(PKparameters)
   
}


