#' Convert concentration and time units as needed
#'
#' \code{convert_units} converts the units for a data.frame containing
#' concentration-time data as necessary to match some desired set of
#' concentration and time units.
#'
#' @param DF_to_convert the data.frame of concentration-time data with units
#'   that may need to be converted. If you're converting concentration units,
#'   this must include the columns "Conc" and "Conc_units", and, if you're
#'   converting time units, it must include the columns "Time", and
#'   "Time_units". (Outputs from \code{\link{extractConcTime}},
#'   \code{\link{extractConcTime_mult}}, and \code{\link{extractObsConcTime}}
#'   work here.) If you want to convert between mass per volume and molar
#'   concentrations and there are multiple compounds present, this must also
#'   include the column "CompoundID".
#' @param time_units time units to use, e.g., "hours". Options for time units
#'   are "hours", "days", and "minutes".
#' @param conc_units concentration units to use, e.g. "ng/mL". Options for
#'   concentration units are the same as the ones in the Excel form for PE data
#'   entry: "mg/L", "mg/mL", "µg/L" (or "ug/L"), "µg/mL" (or "ug/mL"), "ng/L",
#'   "ng/mL", "µM" (or "uM"), or "nM".
#' @param DF_with_good_units if you would like to just match another data.frame,
#'   supply here a data.frame that has the desired concentration and time units.
#'   Options for concentration units are the same as the ones in the Excel form
#'   for PE data entry, and options for time units are "hours", "days", and
#'   "minutes".
#' @param MW optionally supply a molecular weight for the compound of interest
#'   to enable conversions between mass per volume and molar concentrations. If
#'   you have more than one compound, list the compoundID and the MW as a named
#'   character vector, e.g., \code{MW = c("substrate" = 325.8, "inhibitor 1" =
#'   705.6)} for a DDI simulation of midazolam with itraconazole. If you want to
#'   make this type of conversion, the data.frame DF_to_convert must also
#'   include the column "CompoundID". Acceptable compoundIDs are "substrate",
#'   "primary metabolite 1", "primary metabolite 2", "secondary metabolite",
#'   "inhibitor 1", "inhibitor 2", or "inhibitor 1 metabolite".
#'
#' @return a data.frame with the converted units
#' @export
#'
#' @examples
#' DF_to_convert <- convert_units(DF_to_convert = SimulatedData,
#'                              DF_with_good_units = ObsData)
#' 
convert_units <- function(DF_to_convert, 
                          time_units = "hours", 
                          conc_units = "ng/mL", 
                          DF_with_good_units = NA, 
                          MW = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if("Conc_units" %in% names(DF_to_convert) == FALSE){
      stop("The function `convert_units` requires that DF_to_convert have a column titled `Conc_units`, and your data.frame does not. We cannot convert the concentration units here. Did you only want to convert the time units? If so, please use the function `convert_time_units`.", 
           call. = FALSE)
   }
   
   if("Time_units" %in% names(DF_to_convert) == FALSE){
      stop("The function `convert_units` requires that DF_to_convert have a column titled `Time_units`, and your data.frame does not. We cannot convert the time units here. Did you only want to convert the concentration units? If so, please use the function `convert_conc_units`.", 
           call. = FALSE)
   }
   
   
   # Main body of function -------------------------------------------------
   
   ## Converting concentration units --------------------------------------
   
   DF_to_convert <- convert_conc_units(DF_to_convert = DF_to_convert,
                                       conc_units = conc_units, 
                                       DF_with_good_units = DF_with_good_units, 
                                       MW = MW)
   
   ## Converting time units -------------------------------------------------
   
   DF_to_convert <- convert_time_units(DF_to_convert = DF_to_convert, 
                                       time_units = time_units)
   
   ## Output -----------------------------------------------------------
   
   return(DF_to_convert)
}


#' Convert the concentration units in a data.frame of concentration-time data
#'
#' @param DF_to_convert the data.frame of concentration-time data with units
#'   that may need to be converted. If you're converting concentration units,
#'   this must include the columns "Conc" and "Conc_units", and, if you're
#'   converting time units, it must include the columns "Time", and
#'   "Time_units". (Outputs from \code{\link{extractConcTime}},
#'   \code{\link{extractConcTime_mult}}, and \code{\link{extractObsConcTime}}
#'   work here.) If you want to convert between mass per volume and molar
#'   concentrations and there are multiple compounds present, this must also
#'   include the column "CompoundID".
#' @param conc_units concentration units to use, e.g. "ng/mL". Options for
#'   concentration units are the same as the ones in the Excel form for PE data
#'   entry: "mg/L", "mg/mL", "µg/L" (or "ug/L"), "µg/mL" (or "ug/mL"), "ng/L",
#'   "ng/mL", "µM" (or "uM"), or "nM".
#' @param DF_with_good_units if you would like to just match another data.frame,
#'   supply here a data.frame that has the desired concentration and time units.
#'   Options for concentration units are the same as the ones in the Excel form
#'   for PE data entry, and options for time units are "hours", "days", and
#'   "minutes".
#' @param MW optionally supply a molecular weight for the compound of interest
#'   to enable conversions between mass per volume and molar concentrations. If
#'   you have more than one compound, list the compoundID and the MW as a named
#'   character vector, e.g., \code{MW = c("substrate" = 325.8, "inhibitor 1" =
#'   705.6)} for a DDI simulation of midazolam with itraconazole. If you want to
#'   make this type of conversion, the data.frame DF_to_convert must also
#'   include the column "CompoundID". Acceptable compoundIDs are "substrate",
#'   "primary metabolite 1", "primary metabolite 2", "secondary metabolite",
#'   "inhibitor 1", "inhibitor 2", or "inhibitor 1 metabolite".
#'
#' @return a data.frame with converted concentration units
#' @export
#'
#' @examples
#' # none yet
#' 
convert_conc_units <- function(DF_to_convert, 
                               conc_units = "ng/mL", 
                               DF_with_good_units = NA, 
                               MW = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if("Conc_units" %in% names(DF_to_convert) == FALSE){
      stop("The function `convert_conc_units` requires that DF_to_convert have a column titled `Conc_units`, and your data.frame does not. We cannot convert the concentration units here.", 
           call. = FALSE)
   }
   
   if(complete.cases(MW[1]) && is.null(names(MW))){
      if(length(MW) > 1){
         stop("You have supplied more than one molecular weight but not specified which compound belongs to each. Please supply a named numeric vector that indicates which compound ID belongs to which weight. Please see the help file for an example.", 
              call. = FALSE)
      } 
      
      names(MW) <- unique(DF_to_convert$CompoundID)
      
      if(any(names(MW)) %in% 
         c("substrate", "inhibitor 1", "primary metabolite 1", 
           "primary metabolite 2", "inhibitor 2", "inhibitor 1 metabolite", 
           "secondary metabolite") == FALSE){
         stop("The names you have supplied for which molecular weight is which are not among the acceptable options for compound ID. Please see the help file for the acceptable names and an example.", 
              call. = FALSE)
      }
      
      if(all(names(MW) %in% unique(DF_to_convert$CompoundID)) == FALSE){
         stop("The names you have supplied for which molecular weight is which are not among the compound IDs present in your data. Please check the names and try again.", 
              call. = FALSE)
      }
   }
   
   # Having trouble sometimes w/encoding. Getting original value and setting as
   # needed and then re-setting to original.
   OrigEncoding <- Sys.getlocale(category = "LC_CTYPE")
   if(OrigEncoding %in% c("English_United States.utf8", "en_US.UTF-8") == FALSE){
      Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
   }
   
   # Converting concentration units --------------------------------------
   
   if("data.frame" %in% class(DF_with_good_units)){
      DF_with_good_units <- DF_with_good_units %>% 
         mutate(Conc_units = sub("ug", "µg", Conc_units), 
                Conc_units = sub("uM", "µM", Conc_units))
   } else if("list" %in% class(DF_with_good_units)){
      DF_with_good_units$Conc_units <- sub("ug", "µg", DF_with_good_units$Conc_units)
      DF_with_good_units$Conc_units <- sub("uM", "µM", DF_with_good_units$Conc_units)
   } else {
      DF_with_good_units <- list(Conc_units = conc_units)
   }
   
   # Allowing for "u" instead of "µ" as input
   if("Conc_units" %in% names(DF_to_convert)){
      DF_to_convert <- DF_to_convert %>% 
         mutate(Conc_units = sub("ug", "µg", Conc_units), 
                Conc_units = sub("uM", "µM", Conc_units))
   }
   
   DF_with_good_units$Conc_units <- sub("ug", "µg", DF_with_good_units$Conc_units)
   DF_with_good_units$Conc_units <- sub("uM", "µM", DF_with_good_units$Conc_units)
   
   MassUnits <- c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL")
   MolarUnits <- c("µM", "nM")
   
   if(unique(DF_with_good_units$Conc_units) %in% MassUnits &
      unique(DF_to_convert$Conc_units) %in% MolarUnits){
      
      suppressMessages(
         ConvTable_conc <- data.frame(
            
            OrigUnits = rep(c("µM", "nM"), 6), 
            
            RevUnits = rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL"), 
                           each = 2)) %>% 
            mutate(
               FactorNoMW = c(
                  # uM then nM
                  1/1000,    1/10^6,  # mg/L
                  1/10^6,    1/10^9,  # mg/mL
                  1,         1/1000,  # ug/L
                  1/1000,    1/10^6,  # ug/mL
                  1*1000,    1,  # ng/L
                  1,         1/1000  # ng/mL
               )) %>% 
            left_join(
               left_join(data.frame(MW = MW, 
                                    CompoundID = names(MW)),
                         expand_grid(CompoundID = names(MW), 
                                     OrigUnits = c("µM", "nM")))) %>% 
            mutate(Factor = MW * FactorNoMW)
      )
      
   } else if(unique(DF_with_good_units$Conc_units) %in% MolarUnits &
             unique(DF_to_convert$Conc_units) %in% MassUnits){
      
      suppressMessages(
         ConvTable_conc <- data.frame(
            OrigUnits = rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL"), 
                            each = 2),
            
            RevUnits = rep(c("µM", "nM"), 6)) %>% 
            mutate(
               FactorNoMW = c(
                  # uM then nM
                  1000,    10^6,  # mg/L
                  10^6,    10^9,  # mg/mL
                  1,       1000,  # ug/L
                  1000,    10^6,  # ug/mL
                  1000,    1,  # ng/L
                  1,       1000  # ng/mL
               )) %>% 
            left_join(
               left_join(data.frame(MW = MW, 
                                    CompoundID = names(MW)),
                         expand_grid(CompoundID = names(MW), 
                                     RevUnits = c("µM", "nM")))) %>% 
            mutate(Factor = FactorNoMW / MW)
      )
      
      
   } else {
      
      ConvTable_conc <- data.frame(
         OrigUnits = c(
            rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L",
                  "ng/mL"), 6),
            
            rep(c("µM", "nM"), 2),
            
            rep(c("mg", "µg", "ng"), 3),
            
            "mL", "PD response"),
         
         RevUnits = c(
            rep("mg/L", 6),
            rep("mg/mL", 6),
            rep("µg/L", 6),
            rep("µg/mL", 6),
            rep("ng/L", 6),
            rep("ng/mL", 6),
            
            rep("µM", 2),
            rep("nM", 2),
            
            rep(c("mg", "µg", "ng"), each = 3),
            
            "mL", "PD response"),
         
         Factor = c(1,    10^3, 10^-3, 1,     10^6,  10^-3, # mg/L
                    10^3, 1,    10^-6, 10^-3, 10^9,  10^-6, # mg/mL
                    10^3, 10^6, 1,     10^3,  10^-3, 1,    # ug/L
                    1,    10^3, 10^-3, 1,     10^-6, 10^-3,# ug/mL
                    10^6, 10^9, 10^3,  10^6,  1,     10^3, # ng/L
                    10^3, 10^6, 1,     10^3,  10^-3, 1,    # ng/mL
                    
                    1,    10^-3, # uM
                    10^3, 1,     # nM
                    
                    1,    10^-3, 10^-6, # mg
                    1^3,  1,     10^-3, # ug
                    10^6, 10^3,  1, # ng
                    
                    1, # mL
                    1 # PD response
         ) )
      
      
   }
   
   if(unique(DF_with_good_units$Conc_units) %in% ConvTable_conc$RevUnits == FALSE |
      unique(DF_to_convert$Conc_units) %in% ConvTable_conc$OrigUnits == FALSE){
      stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.",
           call. = FALSE)
   }
   
   ConvTable_conc <- ConvTable_conc %>% 
      filter(OrigUnits %in% unique(DF_to_convert$Conc_units) &
                RevUnits %in% unique(DF_with_good_units$Conc_units))
   
   # If there are multiple compound IDs in DF_to_convert, then there can be
   # multiple CompoundIDs in ConvTable_conc. For some applications, though, s/a
   # PK tables, there might not be any compuond IDs.
   
   if("CompoundID" %in% names(DF_to_convert) & 
      "CompoundID" %in% names(ConvTable_conc)){
      
      if(any(ConvTable_conc %>% filter(CompoundID %in% DF_to_convert$CompoundID) %>% 
             group_by(CompoundID) %>% summarize(N = n()) %>% 
             pull(N) < 1)){
         stop(paste0("You supplied concentration units of ",
                     str_comma(unique(DF_to_convert$Conc_units)), 
                     ", but we were not able to convert them to the desired units of ", 
                     unique(DF_with_good_units$Conc_units), 
                     ". No conversion of units was possible and thus no data can be returned here."),
              call. = FALSE)
      }
      
      DF_to_convert <- DF_to_convert %>% 
         left_join(ConvTable_conc, by = "CompoundID") %>% 
         mutate(Conc = Conc*Factor,
                Conc_units = RevUnits)
      
   } else {
      ConvFactor_conc <-
         ConvTable_conc$Factor[
            which(ConvTable_conc$OrigUnits == unique(DF_to_convert$Conc_units) &
                     ConvTable_conc$RevUnits == unique(DF_with_good_units$Conc_units))]
      
      if(length(ConvFactor_conc) < 1){
         stop(paste0("You supplied concentration units of ",
                     str_comma(unique(DF_to_convert$Conc_units)), 
                     ", but we were not able to convert them to the desired units of ", 
                     unique(DF_with_good_units$Conc_units), 
                     ". No conversion of units was possible and thus no data can be returned here."),
              call. = FALSE)
      }
      
      DF_to_convert <- DF_to_convert %>% mutate(Conc = Conc*ConvFactor_conc,
                                                Conc_units = unique(DF_with_good_units$Conc_units))
   }
   
   ## Output -----------------------------------------------------------
   
   if(OrigEncoding %in% c("English_United States.utf8", "en_US.UTF-8") == FALSE){
      Sys.setlocale("LC_CTYPE", OrigEncoding)
   }
   
   return(DF_to_convert)
   
}


#' Convert the time units in a data.frame of concentration-time data 
#'
#' @param DF_to_convert the data.frame of concentration-time data with units that
#'   may need to be converted. If you're converting concentration units, this must
#'   include the columns "Conc" and "Conc_units", and, if you're converting time
#'   units, it must include the columns "Time", and "Time_units". (Outputs from
#'   \code{\link{extractConcTime}}, \code{\link{extractConcTime_mult}}, and
#'   \code{\link{extractObsConcTime}} work here.) If you want to convert between
#'   mass per volume and molar concentrations and there are multiple compounds
#'   present, this must also include the column "CompoundID".
#' @param time_units time units to use, e.g., "hours". Options for time units
#'   are "hours", "days", and "minutes".
#' @param DF_with_good_units if you would like to just match another data.frame,
#'   supply here a data.frame that has the desired concentration and time units.
#'   Options for concentration units are the same as the ones in the Excel form
#'   for PE data entry, and options for time units are "hours", "days", and
#'   "minutes".
#'
#' @return a data.frame with converted time units
#' @export
#'
#' @examples
#' # none yet
#' 
#' 
convert_time_units <- function(DF_to_convert, 
                               time_units = "hours", 
                               DF_with_good_units = NA){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if("Time_units" %in% names(DF_to_convert) == FALSE){
      stop("The function `convert_time_units` requires that DF_to_convert have a column titled `Time_units`, and your data.frame does not. We cannot convert the time units here.", 
           call. = FALSE)
   }
   
   if("logical" %in% class(DF_with_good_units)){
      DF_with_good_units <- list(Time_units = time_units)
   }
   
   # Converting time units ----------------------------------------------------
   
   ConvTable_time <- data.frame(
      OrigUnits = rep(c("hours", "minutes", "days", "weeks"), 4),
      RevUnits = rep(c("hours", "minutes", "days", "weeks"), each = 4),
      # For determining factors, ask, "How many of the revised units are in the
      # original units?"
      Factor = c(1,     1/60,       24,     168, 
                 60,    1,          24*60,  168*60,  
                 1/24,  1/(24*60),  1,      7, 
                 1/168, 1/(168*60), 1/7,    1))
   
   if(unique(DF_with_good_units$Time_units) %in% ConvTable_time$OrigUnits == FALSE |
      unique(DF_to_convert$Time_units) %in% ConvTable_time$RevUnits == FALSE){
      stop("Our apologies, but we have not yet set up this function to deal with your time units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.",
           call. = FALSE)
   }
   
   ConvFactor_time <-
      ConvTable_time$Factor[
         which(ConvTable_time$OrigUnits == unique(DF_to_convert$Time_units) &
                  ConvTable_time$RevUnits == unique(DF_with_good_units$Time_units))]
   
   DF_to_convert <- DF_to_convert %>% mutate(Time = Time*ConvFactor_time,
                                             Time_units = unique(DF_with_good_units$Time_units))
   
   
   # Output -----------------------------------------------------------
   
   return(DF_to_convert)
   
}

