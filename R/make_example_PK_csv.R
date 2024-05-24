#' Create an example csv file for supplying PK parameters to the functions
#' \code{\link{pksummary_table}} or \code{\link{pksummary_mult}}
#'
#' @param study_type For what kind of study do you want to specify PK
#'   parameters? Options are: \describe{
#'
#'   \item{"DDI"}{DDI studies}
#'
#'   \item{"single dose"}{studies with a single dose or when you only want the
#'   1st dose PK}
#'
#'   \item{"multiple dose"}{studies with multiple doses}
#'
#'   \item{"specific sheets"}{when you've got a user-defined AUC interval
#'   and want to pull data only from there}
#'
#'   \item{"specific tissues"}{when you want PK for a specific tissue or
#'   tissues}
#'
#'   \item{"specific compounds"}{when you want PK for a specific compound or
#'   compounds}
#'
#'   \item{"ADAM"}{when you want ADAM-related parameters}
#'
#'   \item{"I want it all"}{This actually saves a csv file of all the possible PK 
#'   parameters you can extract automatically. It doesn't actually give you 
#'   examples of each of them but, instead, is meant to give you an overview of 
#'   what's possible.}}
#'
#' @return saves a csv file of example PK parameters
#' @export
#'
#' @examples
#' make_example_PK_csv("specific sheets")
#' 
#' 
make_example_PK_csv <- function(study_type = "DDI"){
   
   # NOTE: I'm setting up the template example csv so that, when you filter for
   # the items you want, it will only show a few of them rather than all
   # possible combinations. That's why you might see that some last-dose PK
   # don't show up when the user specifically wants to see examples for multiple
   # dosing or why there are some replicate files with different T/F values for
   # different options. I'm trying to curate a bit to not overwhelm people. -
   # LSh
   
   # Error catching
   study_type <- tolower(str_trim(study_type))
   if(study_type %in% c("ddi", "single dose", "multiple dose", 
                        "specific sheets", "specific compounds", "adam", 
                        "specific tissues", "i want it all") == FALSE){
      warning("You specified a study_type that is not among the possible options. We'll give you an example for a DDI study for now.\n", 
              call. = FALSE)
      study_type <- "ddi"
   }
   
   OutFile <- paste0(
      "Examples for specifying PK values - ",
      ifelse(study_type == "I want it all", 
             "all", study_type), 
      ".csv")
   
   if(file.exists(OutFile)){
      stop(paste0("The example file we're trying to make for you, `", 
                  OutFile, 
                  "`, already exists, and we don't want to overwrite it in case you've been using that as a template for the PK data you want. Please change the existing file name to something else and then try re-running this."), 
           call. = FALSE)
   }
   
   if(study_type == "i want it all"){
      message("We'll save a csv file of all the possible PK parameters you can extract automatically. To use specific ones in the functions pksummary_table or pksummary_mult, please try specifying the study_type to something specific.\n")
      
      MyPKexamples <- PKParameterDefinitions
      
   } else {
      
      MyPKexamples <- PKexamples %>% 
         rename(MyCol = switch(study_type, 
                               "ddi" = "DDI", 
                               "single dose" = "SingleDose", 
                               "multiple dose" = "MultipleDose",
                               "specific sheets" = "Sheets", 
                               "specific tissues" = "Tissues", 
                               "specific compounds" = "Compounds", 
                               "adam" = "ADAM")) %>% 
         filter(MyCol == TRUE) %>% 
         arrange(SortOrder) %>% 
         select(File, CompoundID, Tissue, Sheet, PKparameter, Value, Variability, 
                Notes)
   }
   
   write.csv(MyPKexamples, OutFile, row.names = F)
   
}



