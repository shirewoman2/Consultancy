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
#' @param output_type Which outputs do you want? Options:
#' \describe{\item{"Population"}{the population, prettified}
#'
#' \item{"PopulationSimple"}{the prettified and simplified version of the
#' population. For example, "Healthy Volunteers" would become "healthy subjects"
#' and "Cirrhosis CP-A" would become "patients".}
#'
#' \item{"Population1stCap"}{the prettified population with the
#'   first letter capitalized, e.g., for use at the beginning of a sentence}
#'
#'   \item{"PopulationCap"}{the population with title-case capitalization}
#'
#'   \item{"PopulationSimpleLower"}{the lower-case version of the simplified
#'   population, e.g., "subjects" or "patients"}
#'
#'   \item{"PopulationSimpleCap"}{the simplified population but capitalized}
#'   
#'   \item{"all" (default)}{a list of all of the above}}
#'
#' @return a tidied set of names for a simulated population
#' @export
#'
#' @examples
#' # No examples yet
tidyPop <- function(input_pop, 
                    dialect = "British", 
                    output_type = "all"){
   
   # Error catching ----------------------------------------------------------
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop(paste0(wrapn("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run"), 
                  "\nlibrary(tidyverse)\n\n    ...and then try again.\n"), 
           call. = FALSE)
   }
      
   output_type <- tolower(output_type)
   if(all(output_type == "all")){
      output_type <- c("population", "populationsimple", 
                       "population1stcap", "populationcap", 
                       "populationsimplelower", "populationsimplecap")
   }
   BadOutputType <- setdiff(output_type, 
                            c("population", "populationsimple", 
                              "population1stcap", "populationcap", 
                              "populationsimplelower", "populationsimplecap"))
   
   if(length(BadOutputType) > 0){
      output_type <- intersect(output_type, 
                               c("population", "populationsimple", 
                                 "population1stcap", "populationcap", 
                                 "populationsimplelower", "populationsimplecap"))
      if(length(output_type) == 0){
         warning(wrapn("None of the output types you requested are among the available options. We'll give you the output_type of 'Population'."), 
                 call. = FALSE)
         output_type <- "population"
      } else {
         warning(wrapn(paste0("You requested some output types that are not among the available options. Specifically, these options are not available: ", 
                              str_comma(paste0("'", BadOutputType, "'")), ". We will ignore these.")), 
                 call. = FALSE)
      }
   }
   
   # Main body of function -------------------------------------------------
   
   PopNiceNames <- c(
      "Healthy Volunteers" = "healthy subjects",
      "Cancer" = "patients with cancer",
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
                                  "paediatric patients with blood cancer",
                                  "pediatric patients with blood cancer"),
      "Paed-Cancer-Solid" = ifelse(dialect == "British", 
                                   "paediatric patients with cancer",
                                   "pediatric patients with cancer"),
      "Pregnancy" = "pregnant healthy subjects",
      "Preterm" = "preterm infants",
      "PsoriasisDermal" = "patients with dermal psoriasis",
      "RenalGFR_30-60" = "patients with renal GFR of 30-60",
      "Renal Impaired_Mild" = "patients with mild renal impairment",
      "Renal Impaired_Moderate" = "patients with moderate renal impairment",
      "RenalGFR_less_30" = "patients with renal GFR less than 30",
      "Renal Impaired_Severe" = "patients with severe renal impairment",
      "Rheumatoid Arthritis" = "patients with rheumatoid arthritis", 
      # Discovery populations (really, species)
      "Beagle" = "beagles", 
      "HealthyVolunteer" = "healthy subjects",
      "Monkey" = "monkeys", 
      "Mouse" = "mice", 
      "Rat" = "rats")
   
   TidySteps <- data.frame(Step1 = sub("Sim-", "", input_pop))
   
   TidySteps$Step2 <-
      sapply(TidySteps$Step1, 
             function(x) which(str_detect(x, names(PopNiceNames)))[1])
   TidySteps$SimulatorName <- names(PopNiceNames)[TidySteps$Step2]
   
   TidySteps <- TidySteps %>% 
      mutate(SimulatorName = case_when(is.na(SimulatorName) ~ "NOT FOUND", 
                                       .default = SimulatorName), 
             Step2 = case_when(is.na(Step2) ~ 0, 
                               .default = Step2))
   
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
   
   # Need to change the case of output_type to match the names I want for the
   # output.
   output_type_goodcase <- c("population" = "Population",
                             "populationsimple" = "PopulationSimple", 
                             "population1stcap" = "Population1stCap",
                             "populationcap" = "PopulationCap", 
                             "populationsimplelower" = "PopulationSimpleLower",
                             "populationsimplecap" = "PopulationSimpleCap")
   
   output_type <- as.character(output_type_goodcase[output_type])
   
   MyPops <- MyPops[output_type]
   
   if(length(MyPops) == 1){
      MyPops <- MyPops[[1]]
   }
   
   return(MyPops)
}

