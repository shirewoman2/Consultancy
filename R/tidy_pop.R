#' Tidying Simcyp simulator population names for reports, etc.
#'
#' \code{tidyPop}
#'
#' @param input_pop input population to tidy up; this has been designed to work
#'   with \code{\link{extractExpDetails}} with the output item named
#'   "Population"
#' @param dialect Use "British" (default) or "American" spelling when there's a
#'   difference. Currently only affects "paediatric" vs. "pediatric"
#'   populations.
#'
#' @return a tidied set of names for a simulated population
#' @export
#'
#' @examples
#' # No examples yet
tidyPop <- function(input_pop, 
                    dialect = "British"){
    
    PopNiceNames <- c(
        "Healthy Volunteers" = "healthy volunteers",
        "Cancer" = "cancer patients",
        # not sure whether they're HVs since there's *also* an entry for "Chinese
        # Healthy Volunteers"
        "Chinese" = "Chinese subjects",
        "Chinese Healthy Volunteers" = "Chinese healthy volunteers",
        "Chinese Geriatric" = "Chinese geriatric healthy volunteers",
        "Chinese Paediatric" = ifelse(dialect == "British", 
                                      "Chinese paediatric healthy volunteers",
                                      "Chinese pediatric healthy volunteers"),
        "CirrhosisCP - A" = "cirrhosis patients with a Child-Pugh score of A",
        "CirrhosisCP - B" = "cirrhosis patients with a Child-Pugh score of B",
        "CirrhosisCP - C" = "cirrhosis patients with a Child-Pugh score of C",
        "Cirrhosis CP-A" = "cirrhosis patients with a Child-Pugh score of A",
        "Cirrhosis CP-B" = "cirrhosis patients with a Child-Pugh score of B",
        "Cirrhosis CP-C" = "cirrhosis patients with a Child-Pugh score of C",
        "Geriatric NEC" = "geriatric Northern European Caucasian healthy volunteers",
        "Japanese" = "Japanese healthy volunteers",
        "Japanese Paediatric" = "Japanese pediatric healthy volunteers",
        "Morbidly Obese" = "morbidly obese subjects",
        "NEurCaucasian" = "Northern European Caucasian healthy volunteers",
        "North American African American" = "North American African-American healthy volunteers",
        "North American Asian" = "North American Asian-American healthy volunteers",
        "North American Hispanic_Latino" = "North American Latino healthy volunteers",
        "North American White" = "North American Caucasian healthy volunteers",
        "Obese" = "obese subjects",
        "Paediatric" = ifelse(dialect == "British", 
                              "paediatric healthy volunteers",
                              "pediatric healthy volunteers"),
        "Paed-Cancer-Haem" = ifelse(dialect == "British", 
                              "paediatric blood cancer patients",
                              "pediatric blood cancer patients"),
        "Paed-Cancer-Solid" = ifelse(dialect == "British", 
                                    "paediatric cancer patients",
                                    "pediatric cancer patients"),
        "Pregnancy" = "pregnant healthy volunteers",
        "Preterm" = "preterm infants",
        "PsoriasisDermal" = "dermal psoriasis patients",
        "RenalGFR_30-60" = "patients with renal GFR of 30-60",
        "Renal Impaired_Mild" = "patients with mild renal impairment",
        "Renal Impaired_Moderate" = "patients with moderate renal impairment",
        "RenalGFR_less_30" = "patients with renal GFR less than 30",
        "Renal Impaired_Severe" = "patients with severe renal impairment",
        "Rheumatoid Arthritis" = "rheumatoid arthritis patients")
    
    
    Population <- sub("Sim-", "", input_pop)
    
    Population <- as.character(PopNiceNames[Population])
    PopulationSimple <- ifelse(str_detect(tolower(Population), "patients|subjects"),
                               str_trim(
                                   str_extract(
                                       Population,
                                       "patients|(morbidly obese|obese|p(a)?ediatric|pregnant)? subjects")),
                               "healthy volunteers")
    PopulationSimple <- ifelse(Population == "preterm infants",
                               "preterm infants", PopulationSimple)
    Population1stCap <- str_to_sentence(Population)
    Population1stCap <- ifelse(str_detect(Population1stCap, "score of [abc]"), 
                               sub("score of [abc]",
                                   switch(str_extract(Population1stCap, "score of [abc]"),
                                          "score of a" = "score of A", 
                                          "score of b" = "score of B",
                                          "score of c" = "score of C"),
                                   Population1stCap), Population1stCap)
    Population1stCap <- sub("child-pugh", "Child-Pugh", Population1stCap)
    PopulationCap <- str_to_title(Population)
    PopulationCap <- ifelse(str_detect(Population, "CP"),
                            sub("Cp", "CP", PopulationCap), PopulationCap)
    PopulationCap <- ifelse(str_detect(Population, "GFR"),
                            sub("Gfr", "GFR", PopulationCap), PopulationCap)
    PopulationCap <- gsub("Of", "of", PopulationCap)
    PopulationCap <- gsub("With", "with", PopulationCap)
    PopulationSimpleLower <- tolower(PopulationSimple)
    PopulationSimpleCap <- str_to_title(PopulationSimple)
    
    MyPops <- list(Population = Population,
                   PopulationSimple = PopulationSimple,
                   Population1stCap = Population1stCap,
                   PopulationCap = PopulationCap,
                   PopulationSimpleLower = PopulationSimpleLower,
                   PopulationSimpleCap = PopulationSimpleCap)
    return(MyPops)
}
