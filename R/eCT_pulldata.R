#' Pull the appropriate data from the harmonized Excel tab
#'
#' INTERNAL USE ONLY.
#'
#' @param cmpd cmpd
#' @param AllPerpsPresent AllPerpsPresent
#' @param pull_interaction_data T or F
#' @param fromMultFunction T or F
#' @param Deets 1 row
#' @param ADAM T or F
#' @param ss tissue subtype
#' @param AdvBrainModel T or F
#' @param tissue tissue
#' @param SimConcUnits SimConcUnits
#' @param returnAggregateOrIndiv returnAggregateOrIndiv
#' @param sim_data_xl sim_data_xl
#' @param ADC T or F
#' @param SimTimeUnits time units
#'
#' @return data.frame of conc time data
#'
#' @examples
#' # none
#' 
eCT_pulldata <- function(sim_data_xl, 
                         cmpd, 
                         AllPerpsPresent, 
                         pull_interaction_data, 
                         fromMultFunction, 
                         Deets,
                         ADAM, 
                         ADC = FALSE, 
                         ss, 
                         AdvBrainModel, 
                         tissue, 
                         SimTimeUnits,
                         SimConcUnits, 
                         returnAggregateOrIndiv){
   
   NamesToCheck <- sim_data_xl$...1
   InteractionIndices <- which(str_detect(NamesToCheck, "WITHINTERACTION"))
   TimeRow <- which(str_detect(sim_data_xl$...1, "^Time "))[1]
   
   if(ADAM & cmpd != "substrate"){
      if(tissue %in% c("faeces", "gut tissue")){
         if(cmpd != "inhibitor 1"){
            warning(paste0(str_to_title(cmpd),
                           " concentrations are not available for ",
                           tissue, " and thus will not be extracted.\n"),
                    call. = FALSE)
            
            return()
         }
      } else {
         warning(paste0(str_to_title(cmpd),
                        " concentrations are not available for ",
                        tissue, " and thus will not be extracted.\n"),
                 call. = FALSE)
         
         return()
         
      }
   }
   
   if(ADC & !cmpd %in% c("primary metabolite 1")){
      
      MyCompound <- cmpd
      
      if(length(AllPerpsPresent) == 0){
         
         CompoundIndices <- which(
            str_detect(sim_data_xl$...1,
                       switch(cmpd,
                              "total antibody" = "PROTEINTOTAL",
                              # "intact adc" = "conjugated protein .dar1", # ??
                              "total antibody" = "cantibody total", # ??
                              "conjugated payload" = "PROTEINCONJDRUG" # CHECK THIS ONE with an example; just guessing for now
                       )))
         CompoundIndices <- CompoundIndices[
            which(CompoundIndices > which(str_detect(sim_data_xl$...1, "Population Statistics")))]
         
      }
   } else {
      
      MyCompound <-
         switch(cmpd,
                "substrate" = Deets$Substrate,
                "inhibitor 1" = Deets$Inhibitor1,
                "inhibitor 2" = Deets$Inhibitor2,
                "inhibitor 1 metabolite" = Deets$Inhibitor1Metabolite,
                "primary metabolite 1" = Deets$PrimaryMetabolite1,
                "primary metabolite 2" = Deets$PrimaryMetabolite2,
                "secondary metabolite" = Deets$SecondaryMetabolite) %>%
         as.character()
      
      MainCompoundIDs <- c("substrate", "primary metabolite 1", "primary metabolite 2",
                           "secondary metabolite",
                           "inhibitor 1", "inhibitor 2", "inhibitor 1 metabolite",
                           "inhibitor 2 metabolite")
      
      
      # Determining rows to use ----------------------------------------------
      
      # Figuring out which rows contain which data
      FirstBlank <- intersect(which(is.na(sim_data_xl$...1)),
                              which(1:nrow(sim_data_xl) > TimeRow))[1]
      FirstBlank <- ifelse(is.na(FirstBlank), nrow(sim_data_xl) + 1, FirstBlank)
      # NamesToCheck <- sim_data_xl$...1[TimeRow:(FirstBlank-1)]
      
      if(cmpd %in% MainCompoundIDs & ADAM == FALSE & AdvBrainModel == FALSE){
         # cmpd is one of main compounds is not ADAM or AdvBrainModel 
         
         # released payload looks like primary metabolite 1, so extracting
         # those data here rather than with the rest of the ADC data
         
         CompoundIndices <- which(str_detect(NamesToCheck,
                                             switch(cmpd, 
                                                    "substrate" = "SUBSTRATE",
                                                    "primary metabolite 1" = "PRIMET1", 
                                                    "primary metabolite 2" = "PRIMET2", 
                                                    "secondary metabolite" = "SECMET", 
                                                    "inhibitor 1" = "PERPETRATOR1INHIB", 
                                                    "inhibitor 2" = "PERPETRATOR2", 
                                                    "inhibitor 1 metabolite" = "PERPETRATOR1MET")))
         
      } else if(ADAM|AdvBrainModel){
         # cmpd is ADAM model compound
         
         # Step 1: Find all rows w/ADAM model concentrations
         CompoundIndices <- which(str_detect(NamesToCheck, 
                                             "^Ms|^Dissolution Rate Solid State|^C Lumen Free|^C Lumen Total|^Heff|^Absorption Rate|^Mur|^Md|^Inh Md|^Luminal CLint|CTissue|ITissue|dissolved|absorbed|^C Enterocyte|Release fraction|CIntracranial|CBrainI[CS]F|CCSF(Spinal|Cranial)|Kpuu_I[CS]F|Kpuu I[CS]F|Kpuu_BrainMass|Kpuu brain mass|CTotal( )?Brain"))
         # IMPORTANT: If you change the above regex b/c you find some new weird way
         # that the Simulator output refers to things, ALSO CHANGE IT IN
         # extractConcTime.
         
         # Step 2: Find only those rows with this particular tissue subtype
         temp_regex <- paste0(ifelse(
            str_detect(SimConcUnits$TypeCode[SimConcUnits$Type == ss],
                       "dissolved|absorbed"),
            "", "^"),
            SimConcUnits$TypeCode[SimConcUnits$Type == ss])
         
         # gut tissue tab is set up slightly differently, so need to
         # adjust what regex to use.
         if(tissue == "gut tissue"){ # FIXME - CHECK THIS
            temp_regex <- ifelse(cmpd == "inhibitor 1",
                                 "^ITissue", "^CTissue")
         }
         
         CompoundIndices <- intersect(
            which(str_detect(NamesToCheck, temp_regex)), 
            CompoundIndices)
         
      } 
   } 
   
   if(pull_interaction_data){
      Include <- intersect(CompoundIndices, 
                           InteractionIndices)
   } else {
      Include <- setdiff(CompoundIndices, 
                         InteractionIndices)
   }
   
   # Need to remove trial means from here if they're included. 
   Include <- setdiff(Include, 
                      which(str_detect(tolower(sim_data_xl$...1), "trial")))
   
   # FIXME - Return to this and add trial means here. 
   IncludeTrialMeans <- intersect(Include, 
                                  which(str_detect(tolower(sim_data_xl$...1), "trial")))
   
   # If Include has length 0, then this particular set of data is not available,
   # e.g., metabolite for a solid tissue or interaction data for Heff data.
   # Return empty data.
   if(length(Include) == 0){
      return()
   }
   
   # NB: Making NamesToCheck lower case now that we've checked for the
   # all-caps compound IDs.
   NamesToCheck <- tolower(NamesToCheck)
   
   Out <- list()
   
   if("aggregate" %in% returnAggregateOrIndiv){
      
      RowsToUse <- c(
         "mean"    = intersect(
            which(str_detect(NamesToCheck, "mean") &
                     !str_detect(NamesToCheck, "geome(t)?ric")), # There's a spelling error in some simulator output, and geometric is listed as "geomeric".
            Include),
         "per5"    = intersect(
            which(str_detect(NamesToCheck," 5(th)? percentile|5th ptile|^5th percentile$") &
                     !str_detect(NamesToCheck, "95")),
            Include),
         "per95"   = intersect(
            which(str_detect(NamesToCheck, " 95(th)? percentile|95th ptile|^95th percentile$")),
            Include),
         "per10"   = intersect(
            which(str_detect(NamesToCheck," 10(th)? percentile|10th ptile")),
            Include),
         "per90"   = intersect(
            which(str_detect(NamesToCheck, " 90(th)? percentile|90th ptile")),
            Include),
         "geomean" = intersect(
            which(str_detect(NamesToCheck, "geome(t)?ric mean")),
            Include),
         "median"  = intersect(
            which(str_detect(NamesToCheck, "median")),
            Include))
      
      # dealing with instances where there are no aggregate data s/a animal sims
      if(length(RowsToUse) == 0){
         Out[["agg"]] <- list()
      } else {
         suppressWarnings(
            Out[["agg"]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
               t() %>%
               as.data.frame() %>%
               slice(-(1:3)) %>%
               mutate_all(as.numeric)
         )
         
         names(Out[["agg"]]) <- c("Time", names(RowsToUse))
         Out[["agg"]] <- Out[["agg"]] %>%
            pivot_longer(names_to = "Trial", values_to = "Conc",
                         cols = -c(Time)) %>%
            mutate(Compound = MyCompound,
                   CompoundID = cmpd,
                   Inhibitor = ifelse(pull_interaction_data |
                                         cmpd %in% c("inhibitor 1", 
                                                     "inhibitor 2", 
                                                     "inhibitor 1 metabolite"), 
                                      str_comma(AllPerpsPresent), 
                                      "none"),
                   Time_units = SimTimeUnits,
                   Conc_units = ifelse(ADAM|AdvBrainModel,
                                       SimConcUnits$ConcUnit[
                                          SimConcUnits$Type == ss],
                                       SimConcUnits),
                   Tissue = tissue,
                   Tissue_subtype = ifelse(ADAM|AdvBrainModel, ss, NA), 
                   IndivOrAgg = "aggregate")
      }
   } 
   
   if("individual" %in% returnAggregateOrIndiv){
      
      # Certain animal sims will only have "Statistics" above to the single row
      # of conc time data since they only have 1 individual.
      RowsToUse <- Include[which(Include > which(str_detect(
         sim_data_xl$...1, "^(Individual )?Statistics")))]
      
      if(Deets$Species != "human" & length(RowsToUse) == 0){
         # Sometimes, animal sims will list "Population Statistics" even though
         # there's only 1 individual.
         RowsToUse <- Include[which(Include > which(str_detect(
            sim_data_xl$...1, "Population Statistics")))]
         
      }
      
      suppressWarnings(
         Out[["indiv"]] <- sim_data_xl[c(TimeRow, RowsToUse), ] %>%
            t() %>%
            as.data.frame() %>% slice(-(1:3)) %>%
            mutate_all(as.numeric) %>%
            rename(Time = "V1")
      )
      
      if(length(RowsToUse) > 1){
         SubjTrial <- sim_data_xl[RowsToUse, 2:3] %>%
            rename(Individual = ...2, Trial = ...3) %>%
            mutate(SubjTrial = paste0("ID", Individual, "_", Trial))
         
         names(Out[["indiv"]])[2:ncol(Out[["indiv"]])] <- SubjTrial$SubjTrial
      }
      
      suppressWarnings(
         Out[["indiv"]] <- Out[["indiv"]] %>%
            pivot_longer(names_to = "SubjTrial", values_to = "Conc",
                         cols = -Time) %>%
            mutate(Compound = MyCompound,
                   CompoundID = cmpd,
                   Inhibitor = ifelse(pull_interaction_data |
                                         cmpd %in% c("inhibitor 1", 
                                                     "inhibitor 2", 
                                                     "inhibitor 1 metabolite"), 
                                      str_comma(AllPerpsPresent), 
                                      "none"), 
                   SubjTrial = sub("ID", "", SubjTrial),
                   Time_units = SimTimeUnits,
                   Conc_units = ifelse(ADAM|AdvBrainModel,
                                       SimConcUnits$ConcUnit[
                                          SimConcUnits$Type == ss],
                                       SimConcUnits),
                   Tissue = tissue,
                   Tissue_subtype = ifelse(ADAM|AdvBrainModel, ss, NA)) %>%
            separate(SubjTrial, into = c("Individual", "Trial"),
                     sep = "_") %>% 
            mutate(Individual = as.character(Individual),
                   Trial = as.character(Trial), 
                   IndivOrAgg = "individual")
      )
      
      # Adjusting for when there was only 1 individual
      if(length(RowsToUse) == 1){
         Out[["indiv"]]$Individual <- "1"
         Out[["indiv"]]$Trial <- "mean"
      }
      
      rm(RowsToUse)
      
   }
   
   Out <- bind_rows(Out)
   
   return(Out)
   
}

