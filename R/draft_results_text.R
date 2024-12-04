#' Create some draft text to insert into the results section of a report
#' describing the trial design of a simulation
#'
#' @param sim_data_file name of the Excel file containing the simulator output,
#'   in quotes
#' @param existing_exp_details the output from running either
#'   \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} --
#'   either is fine as long as it contains details for \code{sim_data_file}.
#' @param prettify_compound_names TRUE (default) or FALSE for whether to make
#'   compound names prettier in legend entries and in any Word output files.
#'   This was designed for simulations where the substrate and any metabolites,
#'   perpetrators, or perpetrator metabolites are among the standard options for
#'   the simulator, and leaving \code{prettify_compound_names = TRUE} will make
#'   the name of those compounds something more human readable. For example,
#'   "SV-Rifampicin-MD" will become "rifampicin", and "Sim-Midazolam" will
#'   become "midazolam". 
#' @param mean_type "arithmetic" or "geometric" (default) means in PK tables
#'
#' @return list of generic results text
#' @export
#'
#' @examples
#' # none yet
#' 
draft_results_text <- function(sim_data_file, 
                               existing_exp_details, 
                               prettify_compound_names = TRUE, 
                               mean_type = "geometric"){
   
   # Text to match from report template:
   
   # Dev or ver with obs data
   
   # The simulated single and multiple dose data as illustrated in Figure 11 for
   # Day 1 and in Figure 12 for Day ** are comparable to the clinical data. In
   # addition, the simulated geometric/arithmetic mean AUC0-x, AUCtau, Cmax and
   # median tmax values for [drug] on Day 1 and on Day ** were within **-fold of
   # the observed values (Table 8).
   
   
   # Application DDI
   
   # Mean simulated plasma concentrations following multiple oral doses of
   # [drug] in the absence of [perpetrator] and on the **th day of ** days of
   # administration with [perpetrator] (** mg QD/BID/TID) to healthy subjects /
   # patients are illustrated in Figure 22. Simulated plasma concentrations of
   # [perpetrator] are depicted in Figure 23. Simulated hepatic [CYP**] levels
   # in the absence and during administration of [perpetrator] are indicated in
   # Figure 24. The simulated geometric mean AUCtau and Cmax values and
   # corresponding GMRs for [drug] in the presence and absence of [perpetrator]
   # are listed in Table X.
   
   
   TextPieces <- make_text_legos(sim_data_file = sim_data_file, 
                                 existing_exp_details = existing_exp_details, 
                                 prettify_compound_names = prettify_compound_names)
   
   Deets <- TextPieces$Deets
   MySubstrate <- TextPieces$MySubstrate
   MyPerpetrator <- TextPieces$MyPerpetrator
   SingMult_sub <- TextPieces$SingMult_sub
   Pop <- TextPieces$Pop
   Heading <- TextPieces$Heading
   DosingText_sub_lower <- TextPieces$DosingText_sub_lower
   LastDoseDay_sub <- TextPieces$LastDoseDay_sub
   DoseDay_ordinal <- TextPieces$DoseDay_ordinal
   NumDaysInhib <- TextPieces$NumDaysInhib
   DoseFreq_inhib <- TextPieces$DoseFreq_inhib
   
   # Development or verification ----------------------------------------------
   
   Body_dev_ver_noDDI <- paste0(
      "The simulated ", 
      switch(SingMult_sub, 
             "single" = "single-", 
             "multiple" = "single- and multiple-"), 
      "dose data as illustrated in **Figure XXX** ", 
      switch(SingMult_sub, 
             "single" = "", 
             "multiple" = paste0("for Day 1 and in **Figure XXX** for Day ", 
                                 LastDoseDay_sub, " ")), 
      "were comparable to the clinical data. In addition, the simulated ", 
      mean_type, " mean ", 
      switch(SingMult_sub, 
             "single" = "AUC~inf~ and C~max~ ", 
             "multiple" = "AUC~inf~, AUC~tau~, and C~max~ "), 
      "and median t~max~ values for ", 
      MySubstrate, 
      switch(SingMult_sub, 
             "single" = " ", 
             "multiple" = paste0(" on Day 1 and on Day ", 
                                 LastDoseDay_sub, " ")), 
      "were within **XXX** fold of the observed values (**Table XXX**)."
   )
   
   
   Body_DDI1 <-
      ifelse(MyPerpetrator == "none",
             "",
             paste0("in the absence of ",
                    MyPerpetrator, " and ",
                    ifelse(SingMult_sub == "multiple", "with",
                           paste("on the", DoseDay_ordinal, "day of")),
                    " ", NumDaysInhib, " days of dosing of ",
                    MyPerpetrator, " (", Deets$Dose_inhib, " ",
                    Deets$Units_dose_inhib, " ",
                    DoseFreq_inhib, ")"))
   
   
   Body_dev_ver_DDI <- paste0(
      "The simulated ", 
      case_match(Deets$DoseRoute_sub, 
                 "Oral" ~ "oral", 
                 "IV" ~ "IV", 
                 .default = Deets$DoseRoute_sub), 
      switch(SingMult_sub, 
             "single" = "single- ", 
             "multiple" = "single- and multiple-"), 
      "dose data ", Body_DDI1, " to ", Pop, 
      " are illustrated in **Figure XXX**. Simulated ", 
      MyPerpetrator, " plasma concentations are depicted in **Figure XXX**. ", 
      "Simulated **hepatic  and gut CYPXXX** levels in the absence and during administration of ", 
      MyPerpetrator, " are provided in **Figure XXX**. Simulated and observed ", 
      mean_type, " mean ",
      switch(SingMult_sub, 
             "single" = "AUC~inf~ and C~max~ ", 
             "multiple" = "AUC~tau~ and C~max~ "), 
      "values and corresponding geometric mean ratios (GMR) for ", 
      MySubstrate, " in the absence or presence of ", 
      MyPerpetrator, " are listed in **Table XXX**. The simulated profiles of ", 
      MySubstrate, " were comparable to the clinical data. In addition, the simulated ", 
      mean_type, " mean ",
      switch(SingMult_sub, 
             "single" = "AUC~inf~ and C~max~ ", 
             "multiple" = "AUC~tau~ and C~max~ "), 
      "ratios for ", MySubstrate, " in the presence of ", 
      MyPerpetrator, " were within **XXX** fold of the observed values. Thus, a ", 
      MySubstrate, " **fm~CYPXXX~** of **XXX** was assumed in all further simulations."
   )
   
   
   
   Body_dev_ver <- ifelse(MyPerpetrator == "none",
                          Body_dev_ver_noDDI, 
                          Body_dev_ver_DDI)
   
   
   #    MySubstrate, " as illustrated in **Figure XXX** ", 
   #    switch(SDorMD, 
   #           "single dose" = "was ", 
   #           "multiple dose" = paste0("for Day 1 and **Figure XXX** for Day ", 
   #                                    Deets$SimDuration %/% 24, 
   #                                    " were "),
   #           "custom dosing" = paste0("for Day 1 and **Figure XXX** for Day ", 
   #                                    Deets$SimDuration %/% 24, 
   #                                    " were ")), 
   #    "comparable to the clinical data (**Figure XXX**). In addition, the simulated ", 
   #    mean_type, switch(SDorMD, 
   #                      "single dose" = " AUC~inf~, ", 
   #                      "multiple dose" = " AUC~inf~, AUC~tau~, ", 
   #                      "custom dosing" = " AUC~inf~, AUC~tau~, "),
   #    "CL/F, C~max~, and half life values and the median t~max~ value for ", 
   #    MySubstrate, 
   #    " administered to ", 
   #    Pop, 
   #    " were within **XXX fold** of the observed values (**Table XXX**)", 
   #    switch(SDorMD, 
   #           "single dose" = ".", 
   #           "multiple dose" = paste0(" on Day 1 and Day ", 
   #                                    Deets$SimDuration %/% 24, "."),
   #           "custom dosing" = paste0(" on Day 1 and Day ", 
   #                                    Deets$SimDuration %/% 24, ".")) 
   # )
   
   
   
   # Body_DDI2 <- ifelse(MyPerpetrator == "none", 
   #                     "", 
   #                     paste0("Simulated plasma concentrations of ", 
   #                            MyPerpetrator, 
   #                            " are depicted in **Figure XXX**. Simulated hepatic **CYPXXX** levels in the absence and during administration of ", 
   #                            MyPerpetrator, " are indicated in **Figure XXX**. "))
   
   # Body_app <- 
   #    paste0("Mean simulated plasma concentrations following ", 
   #           ifelse(SDorMD == "single dose", 
   #                  SDMD_sub_txt, 
   #                  paste0("a single and ", SDMD_sub_txt)), 
   #           " of ", 
   #           MySubstrate, " ", 
   #           Body_DDI1, "to ", Pop, 
   #           " are illustrated in **Figure XXX**", 
   #           ifelse(SDorMD == "single dose", 
   #                  ". ", 
   #                  " and **Figure XXX**, respectively. "),
   #           Body_DDI2, 
   #           "The simulated ",
   #           mean_type, switch(SDorMD, 
   #                             "single dose" = " AUC~inf~ ", 
   #                             "multiple dose" = " AUC~inf~, AUC~tau~, ", 
   #                             "custom dosing" = " AUC~inf~, AUC~tau~, "),
   #           "and C~max~ values ", 
   #           ifelse(MyPerpetrator == "none", 
   #                  "", 
   #                  paste0("and corresponding GMRs for ", 
   #                         MySubstrate, 
   #                         " in the presence and absence of ", 
   #                         MyPerpetrator, " ")),
   #           "are listed in **Table XXX**."
   #    )
   
   
   # Application --------------------------------------------------------------
   
   Body_app_noDDI <- 
      paste0("Mean simulated plasma ", 
             MySubstrate, 
             " concentrations following ", 
             DosingText_sub_lower, 
             " to ", Pop, " are illustrated in **Figure XXX**. The simulated ", 
             mean_type, " mean ", 
             switch(SingMult_sub, 
                    "single" = "AUC~inf~ and C~max~ ", 
                    "multiple" = "AUC~tau~ and C~max~ "), 
             "values are listed in **Table XXX**.")
   
   Body_app_DDI <- 
      paste0("Mean simulated plasma ", 
             MySubstrate, 
             " concentrations following ", 
             DosingText_sub_lower, " ",
             Body_DDI1, 
             " to ", Pop, 
             " are illustrated in **Figure XXX**. Simulated ", 
             MyPerpetrator, " plasma concentations are depicted in **Figure XXX**. ", 
             "Simulated **hepatic  and gut CYPXXX** levels in the absence and during administration of ", 
             MyPerpetrator, " are provided in **Figure XXX**. Simulated and observed ", 
             mean_type, " mean ",
             switch(SingMult_sub, 
                    "single" = "AUC~inf~ and C~max~ ", 
                    "multiple" = "AUC~tau~ and C~max~ "), 
             "values and corresponding geometric mean ratios (GMR) for ", 
             MySubstrate, " in the absence or presence of ", 
             MyPerpetrator, " are listed in **Table XXX**.")
   
   Body_app <- ifelse(MyPerpetrator == "none", 
                      Body_app_noDDI, 
                      Body_app_DDI)
   
   return(list(Heading = Heading, 
               Body_dev_ver = Body_dev_ver, 
               Body_app = Body_app))
   
}


