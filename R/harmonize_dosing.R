#' INTERNAL - Harmonize the dosing information for an object with details about
#' simulations. Use information from MainDetails and CustomDosing to add an item
#' to the list called "Dosing" that will include ALL dosing info.
#'
#' @param existing_exp_details output from running extractExpDetails_mult
#'
#' @return harmonized existing_exp_details
#' 
harmonize_dosing <- function(existing_exp_details){
   
   if("MainDetails" %in% names(existing_exp_details) == FALSE){
      existing_exp_details <- harmonize_details(existing_exp_details)
   }
   
   Main <- existing_exp_details$MainDetails
   
   # Sometimes File might not be present, e.g., when hacking calc_dosenumber for
   # use w/obs data. In those cases, it should be ok to set File to "all" as
   # long as there's only 1 row.
   if("File" %in% names(Main) == FALSE & nrow(Main) == 1){Main$File <- "all"}
   
   Main <- split(Main, f = Main$File)
   Dosing <- list()
   
   for(ff in names(Main)){
      
      # Skipping this when it's from extractExpDetails_XML or _DB for now. 
      if(str_detect(ff, "\\.wksz$|\\.db$") | 
         ("Treatment" %in% names(Main[[ff]]) && 
          any(complete.cases(Main[[ff]]$Treatment)))){
         next
      }
      
      Dosing[[ff]] <- list()
      
      for(cmpd in unique(AllCompounds$DosedCompoundID)){
         
         MyCompoundName <- Main[[ff]][[AllCompounds$DetailNames[
            AllCompounds$CompoundID == cmpd]]]
         
         if(is.null(MyCompoundName) || is.na(MyCompoundName)){next}
         
         # If it was custom dosing, then we won't be able to get any info from
         # dosing interval, etc., so skipping those. 
         if(Main[[ff]][switch(cmpd, 
                              "substrate" = "DoseRoute_sub", 
                              "inhibitor 1" = "DoseRoute_inhib", 
                              "inhibitor 2" = "DoseRoute_inhib2")] == "custom dosing"){next}
         
         if(is.na(switch(cmpd, 
                         "substrate" = 
                         suppressWarnings(as.numeric(Main[[ff]]$DoseInt_sub)), 
                         
                         "inhibitor 1" = 
                         suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib)), 
                         
                         "inhibitor 2" = 
                         suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib2))))){
            
            Dosing[[ff]][[cmpd]] <- data.frame(
               Time = switch(cmpd, 
                             "substrate" = as.numeric(Main[[ff]]$StartHr_sub), 
                             "inhibitor 1" = as.numeric(Main[[ff]]$StartHr_inhib), 
                             "inhibitor 2" = as.numeric(Main[[ff]]$StartHr_inhib2)))
            
         } else {
            
            Dosing[[ff]][[cmpd]] <- data.frame(
               Time = seq(from = 
                             switch(cmpd, 
                                    "substrate" = as.numeric(Main[[ff]]$StartHr_sub), 
                                    "inhibitor 1" = as.numeric(Main[[ff]]$StartHr_inhib), 
                                    "inhibitor 2" = as.numeric(Main[[ff]]$StartHr_inhib2)), 
                          
                          to = ifelse(is.na(switch(cmpd, 
                                                   "substrate" = Main[[ff]]$NumDoses_sub, 
                                                   "inhibitor 1" = Main[[ff]]$NumDoses_inhib, 
                                                   "inhibitor 2" = Main[[ff]]$NumDoses_inhib)), 
                                      
                                      as.numeric(Main[[ff]]$SimDuration), 
                                      
                                      (switch(cmpd, 
                                              "substrate" = as.numeric(Main[[ff]]$NumDoses_sub), 
                                              "inhibitor 1" = as.numeric(Main[[ff]]$NumDoses_inhib), 
                                              "inhibitor 2" = as.numeric(Main[[ff]]$NumDoses_inhib)) - 1) * 
                                         switch(cmpd, 
                                                "substrate" = 
                                                   suppressWarnings(as.numeric(Main[[ff]]$DoseInt_sub)), 
                                                
                                                "inhibitor 1" = 
                                                   suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib)), 
                                                
                                                "inhibitor 2" = 
                                                   suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib2))) + 
                                         switch(cmpd, 
                                                "substrate" = as.numeric(Main[[ff]]$StartHr_sub), 
                                                "inhibitor 1" = as.numeric(Main[[ff]]$StartHr_inhib), 
                                                "inhibitor 2" = as.numeric(Main[[ff]]$StartHr_inhib2))), 
                          
                          by = 
                             switch(cmpd, 
                                    "substrate" = 
                                       suppressWarnings(as.numeric(Main[[ff]]$DoseInt_sub)), 
                                    
                                    "inhibitor 1" = 
                                       suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib)), 
                                    
                                    "inhibitor 2" = 
                                       suppressWarnings(as.numeric(Main[[ff]]$DoseInt_inhib2))))) 
            
         }
         
         # Setting things up for calculating clock time
         SimStartTime_clock <- sub("Day 1, ", "", Main[[ff]]$SimStartDayTime) %>% 
            str_split_1(pattern = ":")
         
         suppressWarnings(
            Dosing[[ff]][[cmpd]] <- Dosing[[ff]][[cmpd]] %>% 
               mutate(File = ff, 
                      CompoundID = cmpd, 
                      Compound = Main[[ff]][[AllCompounds$DetailNames[
                         AllCompounds$CompoundID == cmpd]]], 
                      Time_units = Main[[ff]]$Units_tmax, 
                      Dose = switch(cmpd, 
                                    "substrate" = as.numeric(Main[[ff]]$Dose_sub), 
                                    "inhibitor 1" = as.numeric(Main[[ff]]$Dose_inhib), 
                                    "inhibitor 2" = as.numeric(Main[[ff]]$Dose_inhib2)), 
                      Dose_units = switch(cmpd, 
                                          "substrate" = Main[[ff]]$Units_dose_sub, 
                                          "inhibitor 1" = Main[[ff]]$Units_dose_inhib, 
                                          "inhibitor 2" = Main[[ff]]$Units_dose_inhib2), 
                      DoseRoute = switch(cmpd, 
                                         "substrate" = Main[[ff]]$DoseRoute_sub, 
                                         "inhibitor 1" = Main[[ff]]$DoseRoute_inhib, 
                                         "inhibitor 2" = Main[[ff]]$DoseRoute_inhib2), 
                      Day = Time %/% 24 + 1, 
                      # clock time 
                      Time_clock_h1 = Time %% 24) %>% 
               separate_wider_delim(cols = Time_clock_h1, 
                                    names = c("Time_clock_h", "Time_clock_m"), 
                                    delim = "\\.", 
                                    too_few = "align_start") %>% 
               mutate(Time_clock_h = as.numeric(Time_clock_h) + as.numeric(SimStartTime_clock[1]), 
                      Time_clock_m = case_when(is.na(Time_clock_m) ~ 0, 
                                               .default = as.numeric(Time_clock_m)), 
                      Time_clock_m = 60 * Time_clock_m + as.numeric(SimStartTime_clock[2]), 
                      Time_clock_h = case_when(Time_clock_m > 1 ~ Time_clock_h + Time_clock_m %/% 1, 
                                               .default = Time_clock_h), 
                      Time_clock_m = case_when(Time_clock_m > 1 ~ Time_clock_m - 1, 
                                               .default = Time_clock_m), 
                      TimeOfDay = paste0(formatC(Time_clock_h, width = 2, format = "d", flag = "0"), 
                                         ":", 
                                         formatC(Time_clock_m, width = 2, format = "d", flag = "0"))) %>% 
               mutate(DoseNum = 1:nrow(.)) %>% 
               select(File, CompoundID, Compound, Time, Time_units, Day, TimeOfDay, DoseNum,
                      Dose, Dose_units, DoseRoute)
         )
         
         rm(SimStartTime_clock)
      }
      
      Dosing[[ff]] <- bind_rows(Dosing[[ff]])
      
   }
   
   existing_exp_details$Dosing <- bind_rows(Dosing)
   
   if("DoseRoute" %in% names(existing_exp_details$Dosing)){
      existing_exp_details$Dosing <- existing_exp_details$Dosing %>% 
         filter(!DoseRoute == "custom dosing")
   }
   
   if("Day" %in% names(existing_exp_details$CustomDosing)){
      suppressWarnings(
         existing_exp_details$CustomDosing <- existing_exp_details$CustomDosing %>% 
            mutate(Day = as.numeric(Day))
      )
   }
   
   existing_exp_details$Dosing <- existing_exp_details$Dosing %>% 
      bind_rows(existing_exp_details$CustomDosing)
   
   if("TimeOfDay" %in% names(existing_exp_details$Dosing)){
      existing_exp_details$Dosing <- existing_exp_details$Dosing %>% 
         # Not including seconds here. Making, e.g., 09:00:00 be 09:00. 
         mutate(TimeOfDay = str_extract(TimeOfDay, "[0-9]{2}:[0-9]{2}")) 
   } else {
      existing_exp_details$Dosing <- existing_exp_details$Dosing %>% 
         mutate(TimeOfDay = NA)
   }
   
   existing_exp_details$Dosing <- existing_exp_details$Dosing %>% unique()
   
   return(existing_exp_details)
   
}



