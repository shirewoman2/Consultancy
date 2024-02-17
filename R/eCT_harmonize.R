#' Harmonize the compound names on sim_data_xl to made finding the correct rows
#' easier later.
#'
#' INTERNAL USE ONLY.
#'
#' @param sim_data_xl sim_data_xl
#' @param compoundToExtract compoundToExtract
#' @param PerpPresent T or F
#' @param ADAM T or F
#' @param AdvBrainModel T or F
#'
#' @return a data.frame of sim_data_xl with compound names and compound codes
#'   switched to, e.g., SUBSTRATE
#'
#' @examples
#' # none
eCT_harmonize <- function(sim_data_xl, 
                          compoundToExtract, 
                          AllCompoundsPresent,
                          tissue, 
                          PerpPresent, 
                          Deets, 
                          ADAM,
                          AdvBrainModel){
   
   # Renaming compounds -------------------------------------------------------
   
   if(any(ADAM, AdvBrainModel) == FALSE){
      
      # If "interaction" or "Csys" or other similar strings are part of the name
      # of any of the compounds, that messes up the regex. Plus, the Simulator
      # has a variety of inconsistent ways that it names compounds that are hard
      # to figure out sometimes. Substituting to standardize the compound names.
      # Also need to consider the possibility that user may have had to hack
      # things and may have the same compound in multiple positions.
      
      # Scenario 1: Systemic tissue, no perpetrator
      # CSys will be listed for all concs. Each compound ID will be on its own tab.
      
      # Scenario 2: Systemic tissue, + perpetrator
      # Substrate: CSys for substrate alone, CSys + interaction for substrate + perp,
      # Each metabolite will be on its own tab. All concs labeled as CSys.
      # Perpetrators: ISys for inhibitor 1 concs, ISys 2 for inhibitor 2 concs, ISys and some other number for inhibitor metabolite concs. Not clear how they pick the number for the metabolite concs.
      
      # REMINDER TO SELF: As far as I can tell, only substrate and inhibitor concentrations are available for solid tissues. 
      
      # Scenario 3: Solid tissue except sometimes liver, no perpetrator
      # Many concs possible. 
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Lung, Muscle, Skin, Spleen, Pancreas: CTissue
      
      # Scenario 4: Solid tissue except sometimes liver, + perpetrator
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Lung, Muscle, Skin, Spleen, Pancreas: CTissue or CTissue + Interaction for substrate, ITissue(Inh X) for inhibitor. This will be 1 for inhibitor 1 but need to check on other perp compound IDs. 
      
      # Scenario 5: Liver sometimes
      # Can have sub, PM1, PM2, maybe sec met?, inhib1, inhib2, maybe inhib1 met?. Example: "mdz-met1-met2-inhib1-inhib2-md-alltissues-v22.xlsx"
      
      NApos <- which(is.na(sim_data_xl$...1))
      
      # Looking for all possible compounds. If there is an inhibitor, this will
      # include substrate alone as well as substrate + interaction.
      CmpdMatches1 <- sim_data_xl$...1[(NApos[1] + 1):(NApos[2]-1)] 
      CmpdMatches1 <- CmpdMatches1[!str_detect(CmpdMatches1, "Trial")]
      
      # For some pediatric simulations, subjects are binned by age, and there
      # will be at least 1 extra row for the age bin. This should not be
      # included for CmpdMatches1, I'm pretty sure. I have only encountered one
      # example of this so far. If there is a "bin", then any row that has
      # "mean" and also "bin" should have the "mean" deleted or we end up with
      # two "mean" rows and that messes everything up.
      CmpdMatches1 <- CmpdMatches1[!str_detect(CmpdMatches1, " \\(Bin [0-9]")]
      BinRows <- which(str_detect(sim_data_xl$...1, " Bin [0-9]| \\(Bin [0-9]"))
      sim_data_xl$...1[BinRows] <- sub("[Mm]ean", "", sim_data_xl$...1[BinRows])
      
      # # If the compound is not on the same tab as the substrate, then removing
      # # all the "Trial" rows removes all the rows with the compound name.
      # # Adjusting for that. Also adjusting for the weird situation with some
      # # liver concentrations where the metabolite concentrations are on the same
      # # tab as the substrate and inhibitors.
      # if(any(compoundToExtract %in% c("primary metabolite 1", "primary metabolite 2", 
      #                                 "secondary metabolite")) &
      #    !(tissue == "liver" & 
      #    as.numeric(str_extract(Deets$SimulatorVersion, "[0-9]{2}")) >= 22)){
      #    CmpdMatches1 <- rep(AllCompoundsPresent[[compoundToExtract]], length(CmpdMatches1))
      # }
      
      # Next, need to figure out which combination of CSys and ISys 1 or ISys 3
      # or whatever number belongs to which actual compound. Looking for what
      # compounds were listed under "Population Statistics" b/c that's where
      # they use that kind of coding.
      StartRow     <- which(str_detect(sim_data_xl$...1, "Population Statistics"))[1]
      EndRow       <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))[1]-1
      EndRow       <- max(which(complete.cases(sim_data_xl$...1[1:EndRow])))
      CmpdMatches2 <- sim_data_xl$...1[StartRow:EndRow]
      CmpdMatches2 <- CmpdMatches2[which(str_detect(CmpdMatches2, "^(P)?[CM](2)?(II)?(Sys|liver|pv|Tissue| lumen free)(.*[iI]nteraction)?|I(Sys|liver|pv| lumen free) [1-9]?|ITissue.Inh|InhM"))]
      CmpdMatches2 <- str_trim(str_extract(CmpdMatches2, "^(P)?[CM](2)?(II)?(Sys|liver|pv|Tissue| lumen free)(.*[iI]nteraction)?|I(Sys|liver|pv| lumen free) [1-9]?|ITissue.Inh|InhM"))
      CmpdMatches2 <- CmpdMatches2[complete.cases(CmpdMatches2)]
      CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?[iI]nteraction")] <- 
         paste(str_extract(CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?[iI]nteraction")], "(P)?[CIM](2)?(II)?(Sys|liver|pv|Tissue| lumen free)"), 
               "interaction")
      
      # For some tissues, regex above will result in inhibitor 1 being labeled
      # as "ITissue(Inh", and that's not optimal for subsequent steps. Remove
      # the "(Inh" bit.
      CmpdMatches2 <- sub("\\(Inh", "", CmpdMatches2)
      
      # Last step: Find the unique versions of the coding.
      CmpdMatches2 <- unique(CmpdMatches2)
      
      if(PerpPresent == FALSE & str_detect(tissue, "plasma|blood") == FALSE){
         CmpdMatches2 <- CmpdMatches1
      }
      
      if(length(CmpdMatches1) != length(CmpdMatches2)){
         warning("PLEASE TELL LAURA SHIREMAN YOU SAW AN ERROR CALLED `COMPOUNDCODE` WHEN TRYING TO EXTRACT CONCENTRATION TIME DATA")
      }
      
      AllCompoundsInv <- names(AllCompoundsPresent)
      names(AllCompoundsInv) <- AllCompoundsPresent
      # This works fine as long as there are no duplicate compounds, e.g., Drug
      # X is both the primary metabolite 1 AND inhibitor 1, which CAN happen
      # when we need to hack things in the Simulator. Need to filter to retain
      # ONLY compounds in compoundsToExtract or the function glitches farther
      # down.
      AllCompoundsInv <- AllCompoundsInv[AllCompoundsInv %in% compoundToExtract]
      
      # Admittedly, this step here where we say that CmpdMatches1, which is the
      # actual compound names, is going to be in the same order as CmpdMatches2,
      # which lists the coded versions of the compounds, makes me nervous just
      # b/c it's coding by index and the two items aren't perfectly matched --
      # we had to remove a bunch of excess junk in between them. So far, though,
      # I haven't found an example of this failing. The order that compounds are
      # listed -- whether by their actual names or by their coded names -- seems
      # to be the same always.
      
      CmpdMatches <- data.frame(NamesInExcel = CmpdMatches1, 
                                CompoundCode = CmpdMatches2) %>% 
         mutate(CompoundName = sub("( )?\\+( )?[Ii]nteraction", "", NamesInExcel), 
                CompoundID = AllCompoundsInv[CompoundName], 
                CompoundID = ifelse(str_detect(CompoundCode, "I(Sys|liver|pv|Tissue)") & 
                                       CompoundID %in% c("substrate", 
                                                         "primary metabolite 1", 
                                                         "primary metabolite 2", 
                                                         "secondary metabolite"), 
                                    AllCompoundsInv[str_detect(AllCompoundsInv, "inhibitor")][CompoundName], 
                                    CompoundID), 
                CompoundNameForRegex = sub("\\+|\\(|\\)|\\%|\\%", ".", CompoundName), 
                Interaction = str_detect(CompoundCode, "interaction"))
      
      # Output sometimes dosen't list the compound name. Not sure what the
      # pattern is for when this happens. They only list, e.g., "CPlasma" or
      # "ITissue".
      
      CompoundThatShouldBePresent <- compoundToExtract
      ShouldBeButNotInhib <- CompoundThatShouldBePresent[!str_detect(CompoundThatShouldBePresent, "inhibitor")]
      if(length(ShouldBeButNotInhib) == 0){
         ShouldBeButNotInhib <- NA
      }
      
      if(any(is.na(CmpdMatches$CompoundID))){
         CmpdMatches <- CmpdMatches %>% 
            mutate(CompoundID = case_when(
               is.na(CompoundID) & CompoundCode == "CPlasma" ~ ShouldBeButNotInhib, 
               
               is.na(CompoundID) & CompoundCode == "CPlasma interaction" ~ ShouldBeButNotInhib, 
               
               is.na(CompoundID) & CompoundCode == "CSys" ~ ShouldBeButNotInhib,
               
               is.na(CompoundID) & CompoundCode == "CSys interaction" ~ ShouldBeButNotInhib,
               
               is.na(CompoundID) & CompoundCode == "CTissue" ~ ShouldBeButNotInhib,
               
               is.na(CompoundID) & CompoundCode == "CTissue interaction" ~ ShouldBeButNotInhib,
               
               is.na(CompoundID) & CompoundCode == "ITissue" ~ "inhibitor 1", 
               is.na(CompoundID) & CompoundCode == "IPlasma" ~ "inhibitor 1", 
               TRUE ~ CompoundID))
      }
      
      rm(CmpdMatches1, CmpdMatches2, NApos, StartRow, EndRow, AllCompoundsInv)
      
      for(cmpd in compoundToExtract){
         
         if(complete.cases(Deets[AllCompounds$DetailNames[AllCompounds$CompoundID == cmpd]]) &
            cmpd %in% CmpdMatches$CompoundID){
            
            # NB: I made inhibitor 1 be "PERPETRATOR1INHIB" rather than just
            # "PERPETRATOR1" for ease of regex. If it's just "PERPETRATOR1"
            # then, later, the regex will match both "PERPETRATOR1" and
            # "PERPETRATOR1METABOLITE".
            
            # Challenging scenario: Sometimes people have to hack the simulator
            # and have the same compound in multiple positions in the Simulator,
            # making it extremely challenging to make sure to match the right
            # compound IDs with the right compounds b/c one compound will have
            # multiple matches. For that reason, only doing regex ONE COMPOUNDID
            # AT A TIME and only for the subset of rows that apply to that
            # compound ID.
            
            CmpdRows <- which(str_detect(sim_data_xl$...1, 
                                         CmpdMatches %>% 
                                            filter(CompoundID == cmpd & 
                                                      Interaction == FALSE) %>% 
                                            pull(CompoundCode)))
            
            sim_data_xl$...1[CmpdRows] <- 
               sub(pattern = 
                      str_c(c(
                         CmpdMatches$CompoundCode[
                            which(CmpdMatches$CompoundID == cmpd)], 
                         CmpdMatches$CompoundNameForRegex[
                            which(CmpdMatches$CompoundID == cmpd)]),
                         collapse = "|"), 
                   replacement = 
                      switch(cmpd, 
                             "substrate" = "SUBSTRATE",
                             "primary metabolite 1" = "PRIMET1", 
                             "primary metabolite 2" = "PRIMET2", 
                             "secondary metabolite" = "SECMET", 
                             "inhibitor 1" = "PERPETRATOR1INHIB", 
                             "inhibitor 2" = "PERPETRATOR2", 
                             "inhibitor 1 metabolite" = "PERPETRATOR1MET"), 
                   x = 
                      sim_data_xl$...1[CmpdRows])
            
         } else if(cmpd %in% compoundToExtract){
            compoundToExtract <- setdiff(compoundToExtract, cmpd)
         }
      }
   } else if(ADAM){
      # ADAM and AdvBrainModel data have different requirements and generally
      # only have a more limited set of compounds available. 
      
      # Release fraction doesn't include that verbiage in the aggregated data
      # section. Fixing that. Cumulative release has its own tab, but I'm
      # checking that so that we don't inadvertently reassign some aggregated
      # data to release faction incorrectly.
      if(any(str_detect(sim_data_xl$...1, "Release Fraction"), na.rm = T) &
         any(str_detect(sim_data_xl$...1, "^Ms|^Dissolution Rate Solid State|^C Lumen Free|^C Lumen Total|^Heff|^Absorption Rate|^Mur|^Md|^Inh Md|^Luminal CLint|CTissue|dissolved|absorbed|^C Enterocyte|CIntracranial|CBrainI[CS]F|CCSF(Spinal|Cranial)|Kpuu_I[CS]F|Kpuu_BrainMass|CTotalBrain"), 
             na.rm = T) == FALSE){
         sim_data_xl$...1[tolower(sim_data_xl$...1) %in% c("mean", 
                                                           "95th percentile", 
                                                           "5th percentile", 
                                                           "95 percentile", 
                                                           "95th ptile")] <- 
            paste("Release Fraction", 
                  sim_data_xl$...1[tolower(sim_data_xl$...1) %in% c("mean", 
                                                                    "95th percentile", 
                                                                    "5th percentile", 
                                                                    "95 percentile", 
                                                                    "95th ptile")])
      }
      
   }
   
   
   
   # Renaming interaction text -------------------------------------------------
   
   # Multiple ways to specify that the data are the substrate or metabolite data
   # in the present of the perpetrator. Harmonizing those, too.
   sim_data_xl$...1 <- sub("After Inh|[iI]nteraction", "WITHINTERACTION", 
                           sim_data_xl$...1)
   
   # When it's lumen free, for some reason, it's labeled differently. Dealing
   # with that.
   sim_data_xl$...1 <- sub("Inh C Lumen Free", "C Lumen Free WITHINTERACTION", 
                           sim_data_xl$...1)
   
   return(sim_data_xl)
   
}