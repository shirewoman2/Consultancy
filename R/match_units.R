#' Match concentration and time units as needed
#'
#' \code{match_units} adjusts the units for a data.frame containing
#' concentration-time data as necessary to match some desired set of
#' concentration and time units.
#'
#' @param DF the data.frame of concentration-time data with units that may need
#'   to be adjusted. This must include the columns "Conc", "Conc_units", "Time",
#'   and "Time_units".
#' @param goodunits either a data.frame that has the desired concentration and
#'   time units OR a named character vector with the desired units such as
#'   \code{c("Conc_units" = "ng/mL", "Time_units" = "hours")}
#'
#' @return a data.frame with the corrected units
#' @export
#'
#' @examples
#' DF_to_adjust <- match_units(DF_to_adjust = SimulatedData,
#'                             goodunits = ObsData)
#'
match_units <- function(DF_to_adjust, goodunits){

      # Matching concentration units --------------------------------------
      ConvTable_conc <- data.frame(
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

            ToAdjustUnits = c(
                  rep(c("mg/L", "mg/mL", "µg/L", "µg/mL", "ng/L",
                        "ng/mL"), 6),

                  rep(c("µM", "nM"), 2),

                  rep(c("mg", "µg"), 2),

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

      if(class(goodunits)[1] == "character"){
            if(unique(goodunits["Conc_units"]) %in% ConvTable_conc$ToAdjustUnits == FALSE |
               unique(DF_to_adjust$Conc_units) %in% ConvTable_conc$GoodUnits == FALSE){
                  stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
            }

            ConvFactor_conc <-
                  ConvTable_conc$Factor[
                        which(ConvTable_conc$ToAdjustUnits == unique(DF_to_adjust$Conc_units) &
                                    ConvTable_conc$GoodUnits == unique(goodunits["Conc_units"]))]

      } else {
            if(unique(goodunits$Conc_units) %in% ConvTable_conc$ToAdjustUnits == FALSE |
               unique(DF_to_adjust$Conc_units) %in% ConvTable_conc$GoodUnits == FALSE){
                  stop("Our apologies, but we have not yet set up this function to deal with your concentration units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
            }

            ConvFactor_conc <-
                  ConvTable_conc$Factor[
                        which(ConvTable_conc$ToAdjustUnits == unique(DF_to_adjust$Conc_units) &
                                    ConvTable_conc$GoodUnits == unique(goodunits$Conc_units))]
      }

      DF_to_adjust <- DF_to_adjust %>% mutate(Conc = Conc*ConvFactor_conc,
                                              Conc_units = unique(goodunits$Conc_units))

      # Matching time units -------------------------------------------------

      ConvTable_time <- data.frame(
            GoodUnits = c("hours", "minutes"),
            ToAdjustUnits = rep(c("hours", "minutes"), each = 2),
            Factor = c(1, 60, 1/60, 1))

      if(class(goodunits)[1] == "character"){
            if(goodunits["Time_units"] %in% ConvTable_time$ToAdjustUnits == FALSE |
               unique(DF_to_adjust$Time_units) %in% ConvTable_time$GoodUnits == FALSE){
                  stop("Our apologies, but we have not yet set up this function to deal with your time units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
            }

            ConvFactor_time <-
                  ConvTable_time$Factor[
                        which(ConvTable_time$ToAdjustUnits == unique(DF_to_adjust$Time_units) &
                                    ConvTable_time$GoodUnits == unique(goodunits["Time_units"]))]

      } else {
            if(unique(goodunits$Time_units) %in% ConvTable_time$ToAdjustUnits == FALSE |
               unique(DF_to_adjust$Time_units) %in% ConvTable_time$GoodUnits == FALSE){
                  stop("Our apologies, but we have not yet set up this function to deal with your time units. Please tell the Consultancy Team R working group what units you're working with and we can fix this.")
            }

            ConvFactor_time <-
                  ConvTable_time$Factor[
                        which(ConvTable_time$ToAdjustUnits == unique(DF_to_adjust$Time_units) &
                                    ConvTable_time$GoodUnits == unique(goodunits$Time_units))]
      }

      DF_to_adjust <- DF_to_adjust %>% mutate(Time = Time*ConvFactor_time,
                                              Time_units = unique(goodunits$Time_units))

      return(DF_to_adjust)
}
