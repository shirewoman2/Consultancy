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
   
   
   AllPKParameters_mod <- 
      AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
      mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
             PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
                                            PrettifiedNames))) %>% 
      unique()
   
   suppressMessages(
      TableNames <-
         data.frame(PKparameter = names(PKtable)) %>% 
         mutate(IsPretty = PKparameter %in% c(AllPKParameters$PrettifiedNames, 
                                              AllPKParameters_mod$PrettifiedNames), 
                IsNotPretty = PKparameter %in% c(AllPKParameters_mod$PKparameter, 
                                                 AllPKParameters$PKparameter),
                IsPKParam = IsPretty | IsNotPretty, 
                NeedsPrettifying = IsPKParam & IsNotPretty) %>% 
         left_join(AllPKParameters %>% 
                      select(PKparameter, PrettifiedNames)) %>% 
         unique() %>% 
         mutate(PrettifiedNames = ifelse(is.na(PrettifiedNames), 
                                         PKparameter, PrettifiedNames))
   )
   
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
   
   return(PKtable)
   
}
