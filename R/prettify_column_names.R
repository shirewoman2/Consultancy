#' Convert the column names in a table of PK parameters into more human-readable
#' names
#'
#' \code{prettify_column_names} converts columns in a table of PK values from
#' names such as "AUCinf_dose1" or "Cmax_last" to prettier names such as "Dose 1
#' AUCinf (ng/mL.h)" or "Last dose Cmax (ng/mL)"
#'
#' @param PKtable a table of PK data with column names such as "AUCinf_dose1" or
#'   "Cmax_last"
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, effectors, or effector metabolites are among the standard
#'   options for the simulator, and leaving \code{prettify_compound_names =
#'   TRUE} will make the name of those compounds something more human readable.
#'   For example, "SV-Rifampicin-MD" will become "rifampicin", and
#'   "Sim-Midazolam" will become "midazolam". Set each compound to the name
#'   you'd prefer to see in your column titles if you would like something
#'   different. For example, \code{prettify_compound_names = c("effector" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "effector"
#'   includes \emph{all} the effectors and effector metabolites present, so, if
#'   you're setting the effector name, you really should use something like this
#'   if you're including effector metabolites: \code{prettify_compound_names =
#'   c("effector" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
#'   "superstatin")}.
#'
#' @return a PK table with prettier column names
#' @export
#'
#' @examples
#' # None yet
#' 
prettify_column_names <- function(PKtable, 
                                  prettify_compound_names = TRUE){
   
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Check for appropriate input for arguments
   if("data.frame" %in% class(PKtable) == FALSE){
      stop("PKtable must be a data.frame for this to work, and it doesn't appear to be.", 
           call. = FALSE)
   }
   
   # Main body of function ---------------------------------------------------
   
   # calc_PK_ratios columns will include "DenominatorSim" and "NumeratorSim".
   # Noting that and saving original column names.
   OrigColNames <- names(PKtable)
   if(any(str_detect(OrigColNames, "NumeratorSim|DenominatorSim"))){
      names(PKtable) <- sub(" NumeratorSim| DenominatorSim", "", names(PKtable))
      names(PKtable) <- sub("_dose1 Ratio", "_ratio_dose1", names(PKtable))
      names(PKtable) <- sub("_last Ratio", "_ratio_last", names(PKtable))
   }
   
   AllPKParameters_mod <- 
      AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
      mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
             PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
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
   
   suppressMessages(
      TableNames <-
         data.frame(OrigColNames = OrigColNames, 
                    PKparameter = names(PKtable)) %>% 
         mutate(IsPretty = PKparameter %in% c(AllPKParameters$PrettifiedNames, 
                                              AllPKParameters_mod$PrettifiedNames), 
                IsNotPretty = PKparameter %in% c(AllPKParameters_mod$PKparameter, 
                                                 AllPKParameters$PKparameter),
                IsPKParam = IsPretty | IsNotPretty, 
                NeedsPrettifying = IsPKParam & IsNotPretty) %>% 
         left_join(AllPKParameters %>% 
                      select(PKparameter, PrettifiedNames) %>% 
                      bind_rows(ExtraPKParam)) %>% 
         unique() %>% 
         mutate(PrettifiedNames = ifelse(is.na(PrettifiedNames), 
                                         PKparameter, PrettifiedNames))
   )
   
   if(any(str_detect(OrigColNames, "NumeratorSim|DenominatorSim"))){
      Suffix <- str_extract(OrigColNames, "NumeratorSim|DenominatorSim")
      Suffix[is.na(Suffix)] <- ""
      
      TableNames$PrettifiedNames <- str_trim(paste(TableNames$PrettifiedNames, Suffix))
   }
   
   # Check that all column names would be unique.
   if(any(duplicated(TableNames$PrettifiedNames))){
      warning("Some table names would be duplicated after prettification. Please check the output. For now, we're adding a number to the end of each replicate column name.\n", 
              call. = FALSE)
      TableNames$PrettifiedNames[duplicated(TableNames$PrettifiedNames)] <- 
         paste(TableNames$PrettifiedNames[duplicated(TableNames$PrettifiedNames)], 
               1:length(TableNames$PrettifiedNames[duplicated(TableNames$PrettifiedNames)]))
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
   
   # MyEffector <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
   #                 Deets$Inhibitor2)
   # 
   # if(any(complete.cases(MyEffector))){
   #    MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
   #    
   #    if(class(prettify_compound_names) == "logical" &&
   #       prettify_compound_names){
   #       MyEffector <- prettify_compound_name(MyEffector)
   #    }
   #    
   #    if(class(prettify_compound_names) == "character" &
   #       "effector" %in% names(prettify_compound_names)){
   #       names(prettify_compound_names)[
   #          str_detect(tolower(names(prettify_compound_names)), 
   #                     "effector")][1] <- "effector"
   #       MyEffector <- prettify_compound_names["effector"]
   #    }
   #    
   #    PrettyCol <- sub("effector", MyEffector, PrettyCol)
   # }
   # 
   
   # MyEffector <- determine_myeffector(existing_exp_details, prettify_compound_names)
   # <-- Also will need to add this back in to pksummary_table.
   
   # if(any(complete.cases(MyEffector))){
   #    PrettyCol <- sub("effector", MyEffector, PrettyCol)
   # }
   
   names(PKtable) <- TableNames$PrettifiedNames
   
   # } else if(complete.cases(sheet_PKparameters) & 
   #           any(str_detect(names(PKtable_all[[1]]), "_dose1|_last")) == FALSE){
   #    # This is when it's a user-defined sheet but we're not prettifying column
   #    # names. We don't know whether an AUC was actually AUCtau, so make it
   #    # AUCt.
   #    PKparameters <- sub("AUCtau", "AUCt", PKparameters)
   
   return(PKtable)
   
}
