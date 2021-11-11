#' Tidying Simcyp simulator population names for reports, etc.
#'
#' \code{tidyPop}
#'
#' @param input_pop input population to tidy up; this has been designed to work
#'   with \code{\link{extractExpDetails}} with the output item named "Pop"
#'
#' @return
#' @export
#'
#' @examples
#' # No examples yet
tidyPop <- function(input_pop){

      PopNiceNames <- c(
            "Healthy Volunteers" = "healthy volunteers",
            "Cancer" = "cancer patients",
            # not sure whether they're HVs since there's *also* an entry for "Chinese
            # Healthy Volunteers"
            "Chinese" = "Chinese subjects",
            "Chinese Healthy Volunteers" = "Chinese healthy volunteers",
            "CirrhosisCP - A" = "cirrhosis patients with CP score of A",
            "CirrhosisCP - B" = "cirrhosis patients with CP score of B",
            "CirrhosisCP - C" = "cirrhosis patients with CP score of C",
            "Geriatric NEC" = "normal elderly control subjects",
            "Japanese" = "Japanese healthy volunteers",
            "Japanese Paediatric" = "Japanese pediatric subjects",
            "Morbidly Obese" = "morbidly obese subjects",
            "NEurCaucasian" = "Northern European Caucasian subjects",
            "Obese" = "obese subjects",
            "Paediatric" = "pediatric subjects",
            "Pregnancy" = "pregnant subjects",
            "Preterm" = "preterm infants",
            "PsoriasisDermal" = "dermal psoriasis patients",
            "RenalGFR_30-60" = "patients with renal GFR of 30-60",
            "RenalGFR_less_30" = "patients with renal GFR less than 30",
            "Rheumatoid Arthritis" = "rheumatoid arthritis patients")


      Pop <- sub("Sim-", "", input_pop)

      Pop <- as.character(PopNiceNames[Pop])
      PopulationSimple <- ifelse(str_detect(tolower(Pop), "patients|subjects"),
                                 str_trim(
                                       str_extract(
                                             Pop,
                                             "patients|(morbidly obese|obese|pediatric|pregnant)? subjects")),
                                 "healthy volunteers")
      PopulationSimple <- ifelse(Pop == "preterm infants",
                                 "preterm infants", PopulationSimple)
      PopulationCap <- str_to_title(Pop)
      PopulationCap <- ifelse(str_detect(Pop, "CP"),
                              sub("Cp", "CP", PopulationCap), PopulationCap)
      PopulationCap <- ifelse(str_detect(Pop, "GFR"),
                              sub("Gfr", "GFR", PopulationCap), PopulationCap)
      PopulationCap <- gsub("Of", "of", PopulationCap)
      PopulationCap <- gsub("With", "with", PopulationCap)
      PopulationSimpleLower <- tolower(PopulationSimple)
      PopulationSimpleCap <- str_to_title(PopulationSimple)

      MyPops <- list(Population = Pop,
                     PopulationSimple = PopulationSimple,
                     PopulationCap = PopulationCap,
                     PopulationSimpleLower = PopulationSimpleLower,
                     PopulationSimpleCap = PopulationSimpleCap)
      return(MyPops)
}
