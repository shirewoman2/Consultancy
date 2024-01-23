#' Convert the column names in a table of PK parameters into more human-readable
#' names
#'
#' \code{prettify_column_names} converts columns in a table of PK values from
#' names such as "AUCinf_dose1" or "Cmax_last" to prettier names such as "Dose 1
#' AUCinf (ng/mL.h)" or "Last dose Cmax (ng/mL)"
#'
#' @param PKtable a table of PK data with column names such as "AUCinf_dose1" or
#'   "Cmax_last" or a vector of the names of a PK table
#' @param prettify_compound_names Do you want to prettify your compound
#'   names? Options: \describe{
#'
#'   \item{TRUE (default)}{Make the compound names pretty in column titles and
#'   in any Word output files. This was designed for simulations where the
#'   substrate and any metabolites, perpetrators, or perpetrator metabolites
#'   are among the standard options for the simulator, and leaving
#'   \code{prettify_compound_names = TRUE} will make the name of those compounds
#'   something more human readable. For example, "SV-Rifampicin-MD" will become
#'   "rifampicin", and "Sim-Midazolam" will become "midazolam".}
#'
#'   \item{FALSE}{Don't change any compound names.}
#'
#'   \item{a named character vector of compound IDs and what each should be
#'   prettified to}{For example, \code{prettify_compound_names = c("perpetrator" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "perpetrator"
#'   includes \emph{any} of inhibitor 1, inhibitor 2, and inhibitor 1 metabolite,
#'   so, if you're setting the perpetrator name, you really should use something
#'   like this if you're including perpetrator metabolites:
#'   \code{prettify_compound_names = c("perpetrator" = "teeswiftavir and 1-OH-teeswiftavir",
#'   "substrate" = "superstatin")}.}
#'   }
#' @param pretty_or_ugly_cols Do you want "pretty" column names such as "Dose 1
#'   AUCinf (ng/mL.h)" or do you want "ugly" but R friendly column names such as
#'   "AUCinf_dose1"? Options are "pretty" (default) or "ugly".
#'
#' @return a PK table with prettier column names
#' @export
#'
#' @examples
#' # None yet
#' 
prettify_column_names <- function(PKtable, 
                                  prettify_compound_names = TRUE, 
                                  pretty_or_ugly_cols = "pretty"){
   
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Check for appropriate input for arguments
   if("data.frame" %in% class(PKtable)){
      PKtable_class <- "data.frame"
   } else if("character" %in% class(PKtable)){
      PKtable_class <- "character"
   } else {
      stop("PKtable must be a data.frame for this to work, and it doesn't appear to be.", 
           call. = FALSE)
   }
   
   # Main body of function ---------------------------------------------------
   
   # calc_PK_ratios columns will include "DenominatorSim" and "NumeratorSim".
   # Noting that and saving original column names.
   if(PKtable_class == "data.frame"){
      OrigColNames <- names(PKtable)
   } else {
      OrigColNames <- PKtable
   }
   if(any(str_detect(OrigColNames, "NumeratorSim|DenominatorSim"))){
      names(PKtable) <- sub(" NumeratorSim| DenominatorSim", "", names(PKtable))
      names(PKtable) <- sub("_dose1 Ratio", "_ratio_dose1", names(PKtable))
      names(PKtable) <- sub("_last Ratio", "_ratio_last", names(PKtable))
   }
   
   AllPKParameters_mod <- 
      AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
      mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
             PrettifiedNames = str_trim(sub("Last dose|Dose 1| for dose 1", "", 
                                            PrettifiedNames))) %>% 
      unique()
   
   # Adding some prettified names that are not present in AllPKParameters
   # only b/c they're not included in Simulator output. Sometimes, though, as
   # when using calc_PK_ratios, you *can* get these parameters. 
   ExtraPKParam <- data.frame(PKparameter = c("tmax_ratio_dose1",
                                              "tmax_ratio_last", 
                                              "tmax_ratio"), 
                              PrettifiedNames = c("Dose 1 tmax ratio", 
                                                  "Last dose tmax ratio", 
                                                  "tmax ratio"))
   
   AllPKParameters_mod <- bind_rows(AllPKParameters_mod, 
                                    ExtraPKParam)
   
   # 1st step: This will leave some values as NA for the column PrettifiedNames.
   TableNames <-
      data.frame(OrigColNames = OrigColNames, 
                 PKparameter_orig = switch(PKtable_class, 
                                           "data.frame" = names(PKtable), 
                                           "character" = PKtable), 
                 OrigOrder = switch(PKtable_class, 
                                    "data.frame" = 1:ncol(PKtable), 
                                    "character" = 1:length(PKtable))) %>% 
      mutate(IsPretty = PKparameter_orig %in% c(AllPKParameters$PrettifiedNames, 
                                                AllPKParameters_mod$PrettifiedNames), 
             IsNotPretty = PKparameter_orig %in% c(AllPKParameters_mod$PKparameter, 
                                                   AllPKParameters$PKparameter),
             IsPKParam = IsPretty | IsNotPretty, 
             NeedsPrettifying = IsPKParam & IsNotPretty, 
             PKparameter = case_when(NeedsPrettifying == TRUE ~ PKparameter_orig, 
                                     NeedsPrettifying == FALSE & IsPKParam == FALSE ~ PKparameter_orig, 
                                     NeedsPrettifying == FALSE & IsPKParam ~ NA), 
             PrettifiedNames = case_when(NeedsPrettifying == FALSE ~ PKparameter_orig, 
                                         NeedsPrettifying == FALSE & IsPKParam == FALSE ~ PKparameter_orig, 
                                         TRUE ~ NA))
   # Some columns may need prettifying and others may need uglifying. Need to figure
   # out what values to fill in for any NA values in either PrettifiedNames or
   # in PKparameter, so splitting table here.
   
   TableNamesToPrettify <- TableNames %>% filter(is.na(PrettifiedNames)) %>% 
      select(-PrettifiedNames) %>% 
      left_join(bind_rows(AllPKParameters, AllPKParameters_mod) %>% 
                   select(PKparameter, PrettifiedNames) %>% 
                   bind_rows(ExtraPKParam), 
                by = "PKparameter") %>% unique()
   
   TableNamesToUglify <- TableNames %>% filter(is.na(PKparameter)) %>% 
      select(-PKparameter) %>% 
      left_join(bind_rows(AllPKParameters, AllPKParameters_mod) %>% 
                   select(PKparameter, PrettifiedNames) %>% 
                   bind_rows(ExtraPKParam), 
                by = "PrettifiedNames") %>% unique()
   
   TableNames <- TableNames %>% filter(IsPKParam == FALSE) %>% 
      bind_rows(TableNamesToPrettify, TableNamesToUglify) %>% 
      arrange(OrigOrder)
   
   if(any(str_detect(OrigColNames, "NumeratorSim|DenominatorSim"))){
      Suffix <- str_extract(OrigColNames, "NumeratorSim|DenominatorSim")
      Suffix[is.na(Suffix)] <- ""
      
      TableNames$PrettifiedNames <- str_trim(paste(TableNames$PrettifiedNames, Suffix))
   }
   
   # Setting semi-finalized column names. 
   TableNames <- TableNames %>% 
      mutate(FinalNames = case_when({pretty_or_ugly_cols} == "pretty" ~ PrettifiedNames, 
                                    {pretty_or_ugly_cols} == "ugly" ~ PKparameter)) %>% 
      select(OrigColNames, OrigOrder, FinalNames) %>% unique()
   
   # Making sure that we don't have duplicates for OrigOrder b/c that would mean
   # that final column names might be offset. An example of when this can
   # happen: When you're trying to uglify CL/F b/c could be CLinf or CLt or
   # CLtau. If there are duplicates here, then it's best to just use original
   # column names to be safe.
   if(any(duplicated(TableNames$OrigOrder))){
      warning(paste0("There's something ambiguous about the input table column names that means that we don't know what value(s) to use for the final column names, so we cannot ", 
                     switch(pretty_or_ugly_cols, 
                            "pretty" = "prettify", 
                            "ugly" = "uglify"), 
                     " your columns. Please check your input and tell a member of the R Working Group if you're uncertain what the problem might be.\n"), 
              call. = FALSE)
      TableNames <- TableNames %>% 
         mutate(FinalNames = OrigColNames) %>% unique()
   }
   
   # Check that all column names would be unique.
   if(any(duplicated(TableNames$FinalNames))){
      warning("Some table names would be duplicated after prettification. Please check the output. For now, we're adding a number to the end of each replicate column name.\n", 
              call. = FALSE)
      TableNames$FinalNames[duplicated(TableNames$FinalNames)] <- 
         paste(TableNames$FinalNames[duplicated(TableNames$FinalNames)], 
               1:length(TableNames$FinalNames[duplicated(TableNames$FinalNames)]))
   }
   
   # if(complete.cases(adjust_conc_units)){
   #    PrettyCol <- gsub(Deets$Units_Cmax,  adjust_conc_units, PrettyCol)
   # } # <--- Will need to add this back into pksummary_table if we replace column prettification there w/this function. 
   
   # # Adjusting units as needed.
   # PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
   # PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
   # PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
   # PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
   # PrettyCol <- gsub("ug/mL", "µg/mL", PrettyCol)
   
   # MyPerpetrator <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
   #                 Deets$Inhibitor2)
   # 
   # if(any(complete.cases(MyPerpetrator))){
   #    MyPerpetrator <- str_comma(MyPerpetrator[complete.cases(MyPerpetrator)])
   #    
   #    if(class(prettify_compound_names) == "logical" &&
   #       prettify_compound_names){
   #       MyPerpetrator <- prettify_compound_name(MyPerpetrator)
   #    }
   #    
   #    if(class(prettify_compound_names) == "character" &
   #       "perpetrator" %in% names(prettify_compound_names)){
   #       names(prettify_compound_names)[
   #          str_detect(tolower(names(prettify_compound_names)), 
   #                     "perpetrator")][1] <- "perpetrator"
   #       MyPerpetrator <- prettify_compound_names["perpetrator"]
   #    }
   #    
   #    PrettyCol <- sub("perpetrator", MyPerpetrator, PrettyCol)
   # }
   # 
   
   # MyPerpetrator <- determine_myperpetrator(existing_exp_details, prettify_compound_names)
   # <-- Also will need to add this back in to pksummary_table.
   
   # if(any(complete.cases(MyPerpetrator))){
   #    PrettyCol <- sub("perpetrator", MyPerpetrator, PrettyCol)
   # }
   
   if(PKtable_class == "data.frame"){
      names(PKtable) <- TableNames$FinalNames
   } else {
      PKtable <- TableNames$FinalNames
   }
   
   # } else if(complete.cases(sheet_PKparameters) & 
   #           any(str_detect(names(PKtable_all[[1]]), "_dose1|_last")) == FALSE){
   #    # This is when it's a user-defined sheet but we're not prettifying column
   #    # names. We don't know whether an AUC was actually AUCtau, so make it
   #    # AUCt.
   #    PKparameters <- sub("AUCtau", "AUCt", PKparameters)
   
   return(PKtable)
   
}
