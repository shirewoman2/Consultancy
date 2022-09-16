#' Match concentration and time units as needed
#'
#' \code{match_units} adjusts the units for a data.frame containing
#' concentration-time data as necessary to match some desired set of
#' concentration and time units.
#'
#' @param DF_to_adjust the data.frame of concentration-time data with units that
#'   may need to be adjusted. This must include the columns "Conc",
#'   "Conc_units", "Time", and "Time_units". (Outputs from
#'   \code{\link{extractConcTime}}, \code{\link{extractConcTime_mult}}, and
#'   \code{\link{extractObsConcTime}} work here.) If you want to convert between
#'   mass per volume and molar concentrations and there are multiple compounds
#'   present, this must also include the column "CompoundID".
#' @param goodunits either a data.frame that has the desired concentration and
#'   time units OR a named list with the desired units. Example:
#'   \code{list("Conc_units" = "ng/mL", "Time_units" = "hours")}. Names of the
#'   list must be "Conc_units" and "Time_units", although you can provide only
#'   one or the other if you only want to convert one of them. Options for
#'   concentration units are the same as the ones in the Excel form for PE data
#'   entry, and options for time units are "hours" and "minutes".
#' @param MW optionally supply a molecular weight for the compound of interest
#'   to enable conversions between mass per volume and molar concentrations. If
#'   you have more than one compound, list the compoundID and the MW as a named
#'   character vector, e.g., \code{MW = c("substrate" = 325.8, "inhibitor 1" =
#'   705.6)} for a DDI simulation of midazolam with itraconazole. If you want to
#'   make this type of conversion, the data.frame DF_to_adjust must also include
#'   the column "CompoundID". Acceptable compoundIDs are "substrate", "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1", "inhibitor 2", or "inhibitor 1 metabolite".
#'
#' @return a data.frame with the corrected units
#' @export
#'
#' @examples
#' DF_to_adjust <- match_units(DF_to_adjust = SimulatedData,
#'                             goodunits = ObsData)
#' 
match_units <- function(DF_to_adjust, goodunits, MW = NA){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    if(complete.cases(MW[1]) && is.null(names(MW))){
        if(length(MW) > 1){
            stop("You have supplied more than one molecular weight but not specified which compound belongs to each. Please supply a named numeric vector that indicates which compound ID belongs to which weight. Please see the help file for an example.", 
                 call. = FALSE)
        } 
        
        names(MW) <- unique(DF_to_adjust$CompoundID)
        
        if(any(names(MW)) %in% 
           c("substrate", "inhibitor 1", "primary metabolite 1", 
             "primary metabolite 2", "inhibitor 2", "inhibitor 1 metabolite", 
             "secondary metabolite") == FALSE){
            stop("The names you have supplied for which molecular weight is which are not among the acceptable options for compound ID. Please see the help file for the acceptable names and an example.", 
                 call. = FALSE)
        }
        
        if(all(names(MW) %in% unique(DF_to_adjust$CompoundID)) == FALSE){
            stop("The names you have supplied for which molecular weight is which are not among the compound IDs present in your data. Please check the names and try again.", 
                 call. = FALSE)
        }
    }
    
    
    # Main body of function -------------------------------------------------
    # If user only wants to change conc units or only time units, make the other
    # option be whatever the DF_to_adjust already has.
    if("data.frame" %in% class(goodunits) == FALSE && 
       length(goodunits) == 1){
        
        if("Conc_units" %in% names(goodunits) == FALSE){
            goodunits$Conc_units <- unique(DF_to_adjust$Conc_units)
        }
        
        if("Time_units" %in% names(goodunits) == FALSE){
            goodunits$Time_units <- unique(DF_to_adjust$Time_units)
        }
    }
    
    
    # Allowing for "u" instead of "µ" as input
    DF_to_adjust <- DF_to_adjust %>% 
        mutate(Conc_units = sub("ug", "µg", Conc_units), 
               Conc_units = sub("uM", "µM", Conc_units))
    
    if("data.frame" %in% class(goodunits)){
        goodunits <- goodunits %>% 
            mutate(Conc_units = sub("ug", "µg", Conc_units), 
                   Conc_units = sub("uM", "µM", Conc_units))
    } else {
        goodunits$Conc_units <- sub("ug", "µg", goodunits$Conc_units)
        goodunits$Conc_units <- sub("uM", "µM", goodunits$Conc_units)
    }
    
    # Matching concentration units --------------------------------------
    
    MassUnits <- c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL")
    MolarUnits <- c("µM", "nM")
    
    if(unique(goodunits$Conc_units) %in% MassUnits &
       unique(DF_to_adjust$Conc_units) %in% MolarUnits){
        
        suppressMessages(
            ConvTable_conc <- data.frame(
                
                ToAdjustUnits = rep(c("µM", "nM"), 6), 
                
                GoodUnits = rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL"), 
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
                                          ToAdjustUnits = c("µM", "nM")))) %>% 
                mutate(Factor = MW * FactorNoMW)
        )
        
    } else if(unique(goodunits$Conc_units) %in% MolarUnits &
              unique(DF_to_adjust$Conc_units) %in% MassUnits){
        
        suppressMessages(
            ConvTable_conc <- data.frame(
                ToAdjustUnits = rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L", "ng/mL"), 
                                    each = 2),
                
                GoodUnits = rep(c("µM", "nM"), 6)) %>% 
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
                                          GoodUnits = c("µM", "nM")))) %>% 
                mutate(Factor = FactorNoMW / MW)
        )
        
    } else {
        
        ConvTable_conc <- data.frame(
            ToAdjustUnits = c(
                rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L",
                      "ng/mL"), 6),
                
                rep(c("µM", "nM"), 2),
                
                rep(c("mg", "µg"), 2),
                
                "mL", "PD response"),
            
            GoodUnits = c(
                rep("mg/L", 6),
                rep("mg/mL", 6),
                rep("µg/L", 6),
                rep("µg/mL", 6),
                rep("ng/L", 6),
                rep("ng/mL", 6),
                
                rep("µM", 2),
                rep("nM", 2),
                
                rep("mg", 2),
                rep("µg", 2),
                
                "mL", "PD response"),
            
            Factor = c(1,    10^3, 10^-3, 1,     10^6,  10^3, # mg/L
                       10^3, 1,    10^-6, 10^-3, 10^9,  10^6, # mg/mL
                       10^3, 10^6, 1,     10^3,  10^-3, 1,    # ug/L
                       1,    10^3, 10^-3, 1,     10^-6, 10^-3,# ug/mL
                       10^6, 10^9, 10^3,  10^6,  1,     10^3, # ng/L
                       10^3, 10^6, 1,     10^3,  10^-3, 1,    # ng/mL
                       
                       1,    10^-3, # uM
                       10^3, 1,     # nM
                       
                       1, 10^-3, # mg
                       1^3, 1,   # ug
                       
                       1, # mL
                       1 # PD response
            ) )
    }
    
    if(unique(goodunits$Conc_units) %in% ConvTable_conc$GoodUnits == FALSE |
       unique(DF_to_adjust$Conc_units) %in% ConvTable_conc$ToAdjustUnits == FALSE){
        stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.",
             call. = FALSE)
    }
    
    ConvFactor_conc <-
        ConvTable_conc$Factor[
            which(ConvTable_conc$ToAdjustUnits == unique(DF_to_adjust$Conc_units) &
                      ConvTable_conc$GoodUnits == unique(goodunits$Conc_units))]
    
    if(length(ConvFactor_conc) < 1){
        stop(paste0("You supplied concentration units of ",
                    str_comma(unique(DF_to_adjust$Conc_units)), 
                    ", but we were not able to convert them to the desired units of ", 
                    unique(goodunits$Conc_units), 
                    ". No adjustment of units was possible and thus no data can be returned here."),
             call. = FALSE)
    }
    
    DF_to_adjust <- DF_to_adjust %>% mutate(Conc = Conc*ConvFactor_conc,
                                            Conc_units = unique(goodunits$Conc_units))
    
    # Matching time units -------------------------------------------------
    
    ConvTable_time <- data.frame(
        GoodUnits = c("hours", "minutes"),
        ToAdjustUnits = rep(c("hours", "minutes"), each = 2),
        Factor = c(1, 60, 1/60, 1))
    
    if(unique(goodunits$Time_units) %in% ConvTable_time$ToAdjustUnits == FALSE |
       unique(DF_to_adjust$Time_units) %in% ConvTable_time$GoodUnits == FALSE){
        stop("Our apologies, but we have not yet set up this function to deal with your time units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.",
             call. = FALSE)
    }
    
    ConvFactor_time <-
        ConvTable_time$Factor[
            which(ConvTable_time$ToAdjustUnits == unique(DF_to_adjust$Time_units) &
                      ConvTable_time$GoodUnits == unique(goodunits$Time_units))]
    
    DF_to_adjust <- DF_to_adjust %>% mutate(Time = Time*ConvFactor_time,
                                            Time_units = unique(goodunits$Time_units))
    
    return(DF_to_adjust)
}
