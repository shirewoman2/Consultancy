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
      "Healthy Volunteers" = "healthy subjects",
      "Cancer" = "cancer patients",
      "Chinese" = "Chinese subjects",
      # not sure whether they're HVs since there's *also* an entry for "Chinese Healthy Volunteers"
      "Chinese Healthy Volunteers" = "Chinese healthy subjects",
      "Chinese Geriatric" = "Chinese geriatric healthy subjects",
      "Chinese Paediatric" = ifelse(dialect == "British", 
                                    "Chinese paediatric healthy subjects",
                                    "Chinese pediatric healthy subjects"),
      "CirrhosisCP - A" = "cirrhosis patients with a Child-Pugh score of A",
      "CirrhosisCP - B" = "cirrhosis patients with a Child-Pugh score of B",
      "CirrhosisCP - C" = "cirrhosis patients with a Child-Pugh score of C",
      "Cirrhosis CP-A" = "cirrhosis patients with a Child-Pugh score of A",
      "Cirrhosis CP-B" = "cirrhosis patients with a Child-Pugh score of B",
      "Cirrhosis CP-C" = "cirrhosis patients with a Child-Pugh score of C",
      "Geriatric NEC" = "geriatric Northern European Caucasian healthy subjects",
      "Japanese" = "Japanese healthy subjects",
      "Japanese Paediatric" = "Japanese pediatric healthy subjects",
      "Morbidly Obese" = "morbidly obese subjects",
      "NEurCaucasian" = "Northern European Caucasian healthy subjects",
      "North American African American" = "North American African-American healthy subjects",
      "North American Asian" = "North American Asian-American healthy subjects",
      "North American Hispanic_Latino" = "North American Latino healthy subjects",
      "North American White" = "North American Caucasian healthy subjects",
      "Obese" = "obese subjects",
      "Paediatric" = ifelse(dialect == "British", 
                            "paediatric healthy subjects",
                            "pediatric healthy subjects"),
      "Paed-Cancer-Haem" = ifelse(dialect == "British", 
                                  "paediatric blood cancer patients",
                                  "pediatric blood cancer patients"),
      "Paed-Cancer-Solid" = ifelse(dialect == "British", 
                                   "paediatric cancer patients",
                                   "pediatric cancer patients"),
      "Pregnancy" = "pregnant healthy subjects",
      "Preterm" = "preterm infants",
      "PsoriasisDermal" = "dermal psoriasis patients",
      "RenalGFR_30-60" = "patients with renal GFR of 30-60",
      "Renal Impaired_Mild" = "patients with mild renal impairment",
      "Renal Impaired_Moderate" = "patients with moderate renal impairment",
      "RenalGFR_less_30" = "patients with renal GFR less than 30",
      "Renal Impaired_Severe" = "patients with severe renal impairment",
      "Rheumatoid Arthritis" = "rheumatoid arthritis patients", 
      # Discovery populations (really, species)
      "Beagle" = "beagles", 
      "HealthyVolunteer" = "healthy subjects",
      "Monkey" = "monkeys", 
      "Mouse" = "mice", 
      "Rat" = "rats")
   
   TidySteps <- data.frame(Step1 = sub("Sim-", "", input_pop))
   TidySteps$Step2 <- sapply(TidySteps$Step1, function(x) which(str_detect(x, names(PopNiceNames)))[1])
   TidySteps$SimulatorName <- names(PopNiceNames)[TidySteps$Step2]
   
   TidySteps <- TidySteps %>% 
      cbind(str_locate(TidySteps$Step1, TidySteps$SimulatorName)) %>% 
      mutate(FrontExtras = str_sub(Step1, 1, start-1), 
             BackExtras = str_sub(Step1, end+1, nchar(Step1)), 
             Population = as.character(PopNiceNames[SimulatorName]), 
             Population = ifelse(is.na(Population), Step1, Population),
             Population = paste0(ifelse(is.na(FrontExtras), 
                                        "", FrontExtras), 
                                 Population, 
                                 ifelse(is.na(BackExtras), "", BackExtras)),
             PopulationSimple = ifelse(str_detect(tolower(Population), "patients|subjects"),
                                       str_trim(
                                          str_extract(
                                             Population,
                                             "patients|(morbidly obese|obese|p(a)?ediatric|pregnant)? subjects")),
                                       "healthy subjects"), 
             PopulationSimple = ifelse(Population == "preterm infants",
                                       "preterm infants", PopulationSimple), 
             Population1stCap = str_to_sentence(Population),
             Population1stCap =
                case_when(str_extract(Population1stCap, "score of [abc]") == "score of a" ~
                             sub("score of a", "score of A", Population1stCap),
                          str_extract(Population1stCap, "score of [abc]") == "score of b" ~ 
                             sub("score of b", "score of B", Population1stCap),
                          str_extract(Population1stCap, "score of [abc]") == "score of c" ~
                             sub("score of c", "score of C", Population1stCap),
                          TRUE ~ Population1stCap), 
             Population1stCap = sub("child-pugh", "Child-Pugh", Population1stCap),
             PopulationCap = str_to_title(Population),
             PopulationCap = ifelse(str_detect(Population, "CP"),
                                    sub("Cp", "CP", PopulationCap), PopulationCap),
             PopulationCap = ifelse(str_detect(Population, "GFR"),
                                    sub("Gfr", "GFR", PopulationCap), PopulationCap),
             PopulationCap = gsub("Of", "of", PopulationCap),
             PopulationCap = gsub("With", "with", PopulationCap),
             PopulationSimpleLower = tolower(PopulationSimple),
             PopulationSimpleCap = str_to_title(PopulationSimple))
   
   MyPops <- list(Population = TidySteps$Population,
                  PopulationSimple = TidySteps$PopulationSimple,
                  Population1stCap = TidySteps$Population1stCap,
                  PopulationCap = TidySteps$PopulationCap,
                  PopulationSimpleLower = TidySteps$PopulationSimpleLower,
                  PopulationSimpleCap = TidySteps$PopulationSimpleCap)
   return(MyPops)
}
