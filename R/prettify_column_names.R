#' Convert the column names in a table of PK parameters into more human-readable
#' names
#'
#' \code{prettify_column_names} converts columns in a table of PK values from
#' names such as "AUCinf_dose1" or "Cmax_last" to prettier names such as "Dose 1
#' AUCinf (ng/mL.h)" or "Last dose Cmax (ng/mL)". Alternatively, it can also be
#' used to convert pretty column names into R-friendly coded names, e.g., "Dose
#' 1 AUCinf (ng/mL.h)" becomes "AUCinf_dose1".
#'
#' @param PKtable a table of PK data with column names such as "AUCinf_dose1" or
#'   "Cmax_last" or a character vector of the names of a PK table
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
#' @param return_which_are_PK TRUE or FALSE (default) for whether to, instead of
#'   prettifying, return a data.frame saying which column names are PK
#'   parameters and which are not. This will also include some other info s/a
#'   the PK parameter coded name.
#'
#' @return a PK table with prettier column names
#' @export
#'
#' @examples
#' # None yet
#' 
prettify_column_names <- function(PKtable, 
                                  prettify_compound_names = TRUE, 
                                  pretty_or_ugly_cols = "pretty", 
                                  return_which_are_PK = FALSE){
   
   
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
      ColNamesNoDecorations <- names(PKtable)
   } else {
      ColNamesNoDecorations <- PKtable
   }
   
   # Noting original names
   TableNames <-
      data.frame(OrigColNames = ColNamesNoDecorations, 
                 OrigOrder = switch(PKtable_class, 
                                    "data.frame" = 1:ncol(PKtable), 
                                    "character" = 1:length(PKtable))) %>% 
      mutate(
         # Adjusting for output that includes NumeratorSim and DenominatorSim,
         # ratios, or user-defined intervals
         ColNamesNoDecorations = str_replace(OrigColNames, " NumeratorSim| DenominatorSim", ""), 
         ColNamesNoDecorations = str_replace(ColNamesNoDecorations, "_dose1 Ratio|_dose1_withInhib Ratio", "_ratio_dose1"), 
         ColNamesNoDecorations = str_replace(ColNamesNoDecorations, "_last Ratio|_last_withInhib Ratio", "_ratio_last"), 
         ColNamesNoDecorations = str_replace(ColNamesNoDecorations, "_withInhib Ratio", "_ratio"), 
         ColNamesNoDecorations = str_replace(ColNamesNoDecorations, "for interval from.*to [0-9]{1,} h ", ""), 
         
         # Also changing any instances of "steady-state" that the user may have
         # added to "last dose" just for the purposes of determining which are
         # PK parameters
         ColNamesNoDecorations = str_replace(ColNamesNoDecorations, "[Ss]teady[- ]state", "Last dose"), 
         
         # Dealing with unit differences b/c could have units in table other
         # than the most common ng/mL and h.
         Conc = str_extract(ColNamesNoDecorations, 
                            "mg/L|mg/mL|µg/L|ug/L|µg/mL|ug/mL|ng/L|ng/mL|µM|nM"), 
         # haven't yet seen any time units other than h but leaving this open
         # to minutes or days just in case
         Time = str_extract(ColNamesNoDecorations, 
                            "\\(L/(h|day|min)|\\(h|day|min\\)|\\.h\\)"), 
         Time = gsub("\\(L?|\\)|/|\\.", "", Time), 
         # placeholder for if we need to adjust column names to deal
         # w/nonstandard units
         ColNamesStdUnits = ColNamesNoDecorations)
   
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
   
   # Dealing w/multiple conc or time units
   ConcUnits <- sort(unique(TableNames$Conc))
   if(length(ConcUnits) > 1){
      message(paste0(str_wrap(paste0(
         "You have more than one concentration unit in your PK table; specifically, you have units of ", 
         str_comma(paste0("`", ConcUnits, "`")), 
         ". Just fyi, if you're running a function for making a PK table, and you see this message, we can adjust concentration units for you if you request that with the argument convert_conc_units.")), 
         "\n"))
   }   
   
   OtherConcUnits <- setdiff(ConcUnits, "ng/mL")
   if(length(ConcUnits) > 0 & length(OtherConcUnits) > 0){
      TableNames$ColNamesStdUnits <- 
         sub(str_c(OtherConcUnits, collapse = "|"), 
             "ng/mL", TableNames$ColNamesStdUnits)
   }
   
   # I haven't encountered a table with more than 1 time unit, so this has not
   # been well tested.
   TimeUnits <- sort(unique(TableNames$Time)) 
   if(length(TimeUnits) > 1){
      TableNames$ColNamesStdUnits <- 
         sub(paste0("\\(", setdiff(TimeUnits, "h"), "\\)"),
             "(h)", TableNames$ColNamesStdUnits)
      
      TableNames$ColNamesStdUnits <- 
         sub(paste0("\\(L/", setdiff(TimeUnits, "h"), "\\)"),
             "(L/h)", TableNames$ColNamesStdUnits)
   }
   
   # 1st step: This will leave some values as NA for the column PrettifiedNames.
   TableNames <- TableNames %>% 
      mutate(IsPretty = ColNamesStdUnits %in% c(AllPKParameters$PrettifiedNames, 
                                                AllPKParameters_mod$PrettifiedNames), 
             IsNotPretty = ColNamesStdUnits %in% c(AllPKParameters_mod$PKparameter, 
                                                   AllPKParameters$PKparameter),
             IsPKParam = IsPretty | IsNotPretty, 
             NeedsPrettifying = IsPKParam & IsNotPretty, 
             PKparameter = case_when(NeedsPrettifying == TRUE ~ ColNamesStdUnits, 
                                     NeedsPrettifying == FALSE & IsPKParam == FALSE ~ ColNamesStdUnits, 
                                     NeedsPrettifying == FALSE & IsPKParam ~ NA), 
             PrettifiedNames = case_when(IsPretty == TRUE ~ ColNamesStdUnits, 
                                         IsPretty == FALSE & NeedsPrettifying == FALSE ~ ColNamesStdUnits, 
                                         .default = NA),
             # Dealing with any differences in units
             PrettifiedNames = ifelse(complete.cases(Conc), 
                                      str_replace(PrettifiedNames, "ng/mL", Conc), 
                                      PrettifiedNames), 
             PrettifiedNames = ifelse(complete.cases(Time), 
                                      str_replace(PrettifiedNames, "h", Time),
                                      PrettifiedNames))
   
   # Some columns may need prettifying and others may need uglifying. Need to
   # figure out what values to fill in for any NA values in either
   # PrettifiedNames or in PKparameter, so splitting table here.
   
   TableNamesToPrettify <- TableNames %>% filter(is.na(PrettifiedNames)) %>% 
      select(-PrettifiedNames) %>% 
      left_join(bind_rows(AllPKParameters, AllPKParameters_mod) %>% 
                   select(PKparameter, PrettifiedNames) %>% 
                   bind_rows(ExtraPKParam), 
                by = "PKparameter") %>% unique()
   
   TableNamesToUglify <- TableNames %>% filter(is.na(PKparameter)) %>% 
      select(-PKparameter, -PrettifiedNames) %>% 
      rename(PrettifiedNames = ColNamesStdUnits) %>% 
      left_join(bind_rows(AllPKParameters, AllPKParameters_mod) %>% 
                   select(PKparameter, PrettifiedNames) %>% 
                   bind_rows(ExtraPKParam), 
                by = "PrettifiedNames") %>% unique()
   
   TableNames <- TableNames %>% filter(IsPKParam == FALSE) %>% 
      bind_rows(TableNamesToPrettify, TableNamesToUglify) %>% 
      arrange(OrigOrder)
   
   # Putting numerator and denominator back into col names
   if(any(str_detect(TableNames$OrigColNames, "NumeratorSim|DenominatorSim"))){
      TableNames <- TableNames %>% 
         mutate(Suffix = str_extract(OrigColNames, "NumeratorSim|DenominatorSim"), 
                Suffix = ifelse(is.na(Suffix), "", Suffix), 
                PrettifiedNames = str_trim(paste(PrettifiedNames, Suffix))) %>% 
         select(-Suffix)
   }
   
   # Putting "steady state" back into col names
   if(any(str_detect(TableNames$OrigColNames, "[Ss]teady[- ]state"))){
      TableNames <- TableNames %>% 
         mutate(PrettifiedNames = case_when(
         str_detect(OrigColNames, "[Ss]teady[- ]state") ~
            str_replace(PrettifiedNames, "Last dose", 
                        str_extract(OrigColNames, "[Ss]teady[- ]state")), 
         .default = PrettifiedNames))
   }
   
   # Putting time interval back into col names
   if(any(str_detect(TableNames$OrigColNames, "for interval from"))){
      TableNames <- TableNames %>% 
         mutate(Suffix = str_extract(OrigColNames, "for interval from.*to [0-9]{1,} h "), 
                PrettifiedNames = ifelse(complete.cases(Suffix), 
                                         str_replace(PrettifiedNames, 
                                                     " \\(", 
                                                     paste0(" ", Suffix, "(")), 
                                         PrettifiedNames)) %>% 
         select(-Suffix)
   }
   
   # Setting semi-finalized column names. 
   TableNames <- TableNames %>% 
      mutate(FinalNames = case_when({pretty_or_ugly_cols} == "pretty" ~ PrettifiedNames, 
                                    {pretty_or_ugly_cols} == "ugly" ~ PKparameter), 
             FinalNames = ifelse(is.na(FinalNames), OrigColNames, FinalNames), 
             # Adding what the interval was
             Interval = str_extract(PrettifiedNames, "Dose 1|Last dose|for interval from.*to [0-9]{1,} h"), 
             Interval = tolower(Interval), 
             Interval = sub("for interval ", "", Interval)) %>% 
      unique()
   
   # Making sure that we don't have duplicates for OrigOrder b/c that would mean
   # that final column names might be offset. An example of when this can
   # happen: When you're trying to uglify CL/F b/c could be CLinf or CLt or
   # CLtau. If there are duplicates here BUT there were NO DUPLICATES in the
   # original data.frame (e.g., user supplied prettified data.frame with a
   # column titled "CL/F (L/h)", which could be CLinf_dose1, CLtau_last, or
   # CLt_dose1), then use CLinf_dose1 and MAKE A WARNING. For other duplicates,
   # it's best to just use original column names to be safe.
   if(any(duplicated(TableNames$OrigOrder))){
      if(any(duplicated(ColNamesNoDecorations)) |
         any(str_detect(TableNames$FinalNames[duplicated(TableNames$OrigOrder)], 
                        "CL")) == FALSE){
         warning(paste0("There's something ambiguous about the input table column names that means that we don't know what value(s) to use for the final column names, so we cannot ", 
                        switch(pretty_or_ugly_cols, 
                               "pretty" = "prettify", 
                               "ugly" = "uglify"), 
                        " your columns. Please check your input and tell a member of the R Working Group if you're uncertain what the problem might be.\n"), 
                 call. = FALSE)
         TableNames <- TableNames %>% 
            mutate(FinalNames = ColNamesNoDecorations) %>% unique()
      } else {
         # This is when there's a duplicate b/c of CLinf vs. CLt vs. CLtau. Just
         # remove the duplicate CL names.
         warning("At least one column was ambiguous and could have been CLinf_dose1, CLtau_last, or CLt_dose1, but we're not sure which. We are calling it CLinf_dose1 as a placeholder.\n", 
                 call. = FALSE)
         
         TableNames$Duplicated <- FALSE
         TableNames$Duplicated[duplicated(TableNames$OrigOrder)] <- TRUE
         
         TableNames <- TableNames %>% 
            filter(Duplicated == FALSE) %>% select(-Duplicated)
      }
   }
   
   # Check that all column names would be unique.
   if(any(duplicated(TableNames$FinalNames))){
      warning(paste0(str_wrap(paste0("Some table names would be duplicated after ",
                                     ifelse(pretty_or_ugly_cols == "pretty", 
                                            "prettification", "uglification"), 
                                     ". Please check the output. For now, we're adding a number to the end of each replicate column name.")), 
                     "\n"), 
              call. = FALSE)
      TableNames$FinalNames[duplicated(TableNames$FinalNames)] <- 
         paste(TableNames$FinalNames[duplicated(TableNames$FinalNames)], 
               1:length(TableNames$FinalNames[duplicated(TableNames$FinalNames)]))
   }
   
   # Returning to original conc and time units as necessary
   if(any(TableNames$Conc != "ng/mL", na.rm = T)){
      TableNames <- TableNames %>% 
         mutate(FinalNames = case_when(
            complete.cases(Conc) &
               Conc != "ng/mL" ~ str_replace(FinalNames, "ng/mL", Conc), 
            TRUE ~ FinalNames))
   }
   
   if(any(TableNames$Time != "h", na.rm = T)){
      TableNames <- TableNames %>% 
         mutate(FinalNames = case_when(
            complete.cases(Time) &
               Time != "h" & 
               str_detect(ColNamesStdUnits, "\\(h\\)") ~ 
               str_replace(FinalNames, "\\(h\\)", paste0("(", Time, ")")), 
            
            complete.cases(Time) & 
               Time != "h" & 
               str_detect(ColNamesStdUnits, "\\(L/h\\)") ~ 
               str_replace(FinalNames, "\\(L/h\\)", paste0("(L/", Time, ")")),
            
            TRUE ~ FinalNames))
   }
   
   TableNames <- TableNames %>% arrange(OrigOrder)
   
   if(PKtable_class == "data.frame"){
      PKtable <- PKtable[, TableNames$OrigColNames]
      names(PKtable) <- TableNames$FinalNames
   } else {
      # This is when it was a character vector rather than a data.frame. 
      PKtable <- TableNames$FinalNames
   }
   
   # Returning which are PK if that's all user wanted
   if(return_which_are_PK){
      return(TableNames %>% 
                select(OrigColNames, IsPKParam, FinalNames, PKparameter, 
                       Interval) %>%
                rename(ColName = OrigColNames, 
                       PrettifiedNames = FinalNames) %>% unique())
   } else {
      return(PKtable)
   }
   
}

