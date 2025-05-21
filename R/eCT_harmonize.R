#' Harmonize the compound names on sim_data_xl to made finding the correct rows
#' easier later.
#'
#' INTERNAL USE ONLY.
#'
#' @param sim_data_xl sim_data_xl
#' @param compoundToExtract compoundToExtract
#' @param PerpPresent T or F
#' @param AllRegCompoundsPresent all compounds present
#' @param tissue tissue
#' @param Deets 1 row for existing_exp_details$MainDetails
#' @param ADAM T or F
#' @param AdvBrainModel T or F
#' @param ADC T or F
#'
#' @return a data.frame of sim_data_xl with compound names and compound codes
#'   switched to, e.g., SUBSTRATE
#'
#' @examples
#' # none
eCT_harmonize <- function(sim_data_xl, 
                          compoundToExtract, 
                          AllRegCompoundsPresent,
                          tissue, 
                          PerpPresent, 
                          Deets, 
                          ADAM,
                          AdvBrainModel, 
                          ADC = FALSE){
   
   # Renaming compounds -------------------------------------------------------
   
   # AdvBrainModel and PD sheets do not need to be harmonized and will pass
   # through.
   
   PD <- tissue == "pd"
   
   if(any(ADAM, PD, ADC) == FALSE |
      (ADC == TRUE & any(compoundToExtract %in% c("primary metabolite 1")))){
      # Here's what we're trying to overcome with this function: If
      # "interaction" or "Csys" or other similar strings are part of the name of
      # any of the compounds, that messes up the regex, so we need to remove
      # those. Second, the Simulator has a variety of inconsistent ways that it
      # names compounds that are hard to figure out sometimes. Substituting here
      # to standardize the compound names. Also need to consider the possibility
      # that user may have had to hack things and may have the same compound in
      # multiple positions, so this will also account for that.
      
      # Scenario 1: Systemic tissue, no perpetrator
      # "CSys" will be listed for all concs. Each compound ID will be on its own tab.
      
      # Scenario 2: Systemic tissue, + perpetrator
      # Substrate: "CSys" for substrate alone, "CSys + interaction" for substrate + perp,
      # Each metabolite will be on its own tab. All concs labeled as "CSys."
      # Perpetrators: "ISys" for inhibitor 1 concs, "ISys" 2 for inhibitor 2 concs, ISys and some other number for inhibitor metabolite concs. Not clear how they pick the number for the metabolite concs.
      
      # CODING NOTE: As far as I can tell, only substrate and inhibitor
      # concentrations are available for solid tissues.
      
      # Scenario 3: Solid tissue except sometimes liver, no perpetrator
      # Many concs possible. 
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Lung, Muscle, Skin, Spleen, Pancreas: Labeled as "CTissue"
      
      # Scenario 4: Solid tissue except sometimes liver, + perpetrator
      # Adipose, Bone, Brain (except when AdvBrainModel), Gut, Heart, Kidney, Lung, Muscle, Skin, Spleen, Pancreas: Labeled as "CTissue" or "CTissue + Interaction" for substrate, "ITissue(Inh X)" for inhibitor. This will be 1 for inhibitor 1 but need to check on other perp compound IDs. 
      
      # Scenario 5: Liver sometimes -- NOT WELL TESTED
      # Can have sub, PM1, PM2, maybe sec met?, inhib1, inhib2, maybe inhib1 met?. Example: "mdz-met1-met2-inhib1-inhib2-md-alltissues-v22.xlsx"
      
      # Scenario 6: Biologics
      # This is NOT WELL TESTED. 
      # Possible names: "Therapeutic protein CSys"
      
      # Noting NA positions b/c that will help figure out which rows to extract
      NApos <- which(is.na(sim_data_xl$...1))
      
      # Here is the strategy: The upper part of a concentration-time profile tab
      # in the Excel results often lists the compound name and not necessarily
      # the compound ID, e.g., "midazolam" or "Itraconazole_Fed Capsule", but
      # then lower on the tab it will list only "CSys" or "ISys1". It seems to
      # do that when the compound is substrate or any of the perpetrator
      # compounds and the tissue is a systemic one. For other compounds or other
      # tissues, the top only lists, e.g., "CSys" or "CTissue", and then it
      # lists the same thing lower down. What we're doing with this function is
      # replacing ALL the instances where there's a reference to a specific
      # compound with a standardized, all caps name for that compound. This will
      # make it easier and more accurate when we pull specific data and assign
      # them to specific compounds later. CmpdMatches1 is for figuring out what
      # was listed at the top of the tab and CmpdMatches2 is for figuring out
      # what was listed at the bottom of the tab and matching things up. 
      
      # Looking for all possible compounds. If there is an inhibitor, this will
      # include substrate alone as well as substrate + interaction.
      CmpdMatches1_rows <- c(NApos[1] + 1, NApos[2]-1)
      if(CmpdMatches1_rows[1] < CmpdMatches1_rows[2]){
         CmpdMatches1 <- sim_data_xl$...1[(NApos[1] + 1):(NApos[2]-1)] 
         CmpdMatches1 <- CmpdMatches1[!str_detect(CmpdMatches1, "Trial")]
      } else if(length(unique(CmpdMatches1_rows)) == 1){
         CmpdMatches1 <- sim_data_xl$...1[(NApos[1] + 1)] 
      } else {
         CmpdMatches1 <- "UNCONVENTIONAL COMPOUND"
      }
      
      # For some pediatric simulations, subjects are binned by age, and there
      # will be at least 1 extra row for the age bin. This should not be
      # included for CmpdMatches1, I'm pretty sure. I have only encountered one
      # example of this so far. If there is a "bin", then any row that has
      # "mean" and also "bin" should have the "mean" deleted or we end up with
      # two "mean" rows and that messes everything up.
      CmpdMatches1 <- CmpdMatches1[!str_detect(CmpdMatches1, " \\(Bin [0-9]")]
      BinRows <- which(str_detect(sim_data_xl$...1, " Bin [0-9]| \\(Bin [0-9]"))
      sim_data_xl$...1[BinRows] <- sub("[Mm]ean", "", sim_data_xl$...1[BinRows])
      
      # Next, need to figure out which combination of CSys and ISys 1 or ISys 3
      # or whatever number belongs to which actual compound. Looking for what
      # compounds were listed under "Population Statistics" b/c that's where
      # they use that kind of coding.
      StartRow     <- which(str_detect(sim_data_xl$...1, "Population Statistics"))[1]
      StartRow <- ifelse(is.na(StartRow), 
                         # Need to deal w/animal sim data, which will not have
                         # any aggregate data. Instead, there will be a row
                         # titled "Statistics" for plasma data. It's different
                         # for brain tissue, fyi.
                         which(str_detect(sim_data_xl$...1, "^(Individual )?Statistics"))[1], 
                         StartRow)
      EndRow       <- which(str_detect(sim_data_xl$...1, "Individual Statistics"))[1]-1
      EndRow       <- ifelse(is.na(EndRow),
                             nrow(sim_data_xl), 
                             max(which(complete.cases(sim_data_xl$...1[1:EndRow]))))
      CmpdMatches2 <- sim_data_xl$...1[StartRow:EndRow]
      CmpdMatches2 <- CmpdMatches2[which(str_detect(CmpdMatches2, "^(P)?[CM](2)?(II)?(Sys|liver|pv|Tissue| lumen free|Peripheral)(.*[iI]nteraction)?|I(Sys|liver|pv| lumen free|Peripheral) [1-9]?|ITissue.Inh|InhM"))]
      CmpdMatches2 <- str_trim(str_extract(CmpdMatches2, "^(P)?[CM](2)?(II)?(Sys|liver|pv|Tissue| lumen free|Peripheral)(.*[iI]nteraction)?|I(Sys|liver|pv| lumen free|Peripheral) [1-9]?|ITissue.Inh|InhM"))
      CmpdMatches2 <- CmpdMatches2[complete.cases(CmpdMatches2)]
      CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?[iI]nteraction")] <- 
         paste(str_extract(CmpdMatches2[str_detect(CmpdMatches2, "\\+( )?[iI]nteraction")], "(P)?[CIM](2)?(II)?(Sys|liver|pv|Tissue| lumen free|Peripheral)"), 
               "interaction")
      
      # For some tissues, regex above will result in inhibitor 1 being labeled
      # as "ITissue(Inh", and that's not optimal for subsequent steps. Remove
      # the "(Inh" bit.
      CmpdMatches2 <- sub("\\(Inh", "", CmpdMatches2)
      
      # Last step: Find the unique versions of the coding.
      CmpdMatches2 <- unique(CmpdMatches2)
      
      if(CmpdMatches1[1] == "UNCONVENTIONAL COMPOUND"){
         CmpdMatches1 <- CmpdMatches2
      }
      
      if(length(CmpdMatches1) != length(CmpdMatches2)){
         warning("PLEASE TELL LAURA SHIREMAN YOU SAW AN ERROR CALLED `COMPOUNDCODE` WHEN TRYING TO EXTRACT CONCENTRATION TIME DATA")
      }
      
      AllRegCompoundsInv <- names(AllRegCompoundsPresent)
      names(AllRegCompoundsInv) <- AllRegCompoundsPresent
      # This works fine as long as there are no duplicate compounds, e.g., Drug
      # X is both the primary metabolite 1 AND inhibitor 1, which CAN happen
      # when we need to hack things in the Simulator. Need to filter to retain
      # ONLY compounds in compoundsToExtract or the function glitches farther
      # down.
      AllRegCompoundsInv <- AllRegCompoundsInv[AllRegCompoundsInv %in% compoundToExtract]
      
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
                CompoundID = AllRegCompoundsInv[CompoundName], 
                CompoundID = case_when(
                   str_detect(CompoundCode, "I(Sys|liver|pv|Tissue)") & 
                      CompoundID %in% c("substrate", 
                                        "primary metabolite 1", 
                                        "primary metabolite 2", 
                                        "secondary metabolite") ~ 
                      AllRegCompoundsInv[str_detect(AllRegCompoundsInv, "inhibitor")][CompoundName], 
                   
                   is.na(CompoundID) & str_detect(NamesInExcel, "Tissue.*Sub") ~ 
                      # This is when it's a solid tissue. Only options are
                      # substrate and inhibitor 1, so if "Sub" is there, it must
                      # be substrate. Covering inhibitor 1 in next line. 
                      "substrate", 
                   
                   is.na(CompoundID) & str_detect(NamesInExcel, "Tissue.*Inh 1") ~ 
                      "inhibitor 1", 
                   
                   .default = CompoundID), 
                CompoundNameForRegex = sub("\\+|\\(|\\)|\\%|\\%", ".", CompoundName), 
                Interaction = str_detect(CompoundCode, "interaction"))
      
      # Output sometimes doesn't list the compound name, I think only when it's
      # not substrate or any inhibitor compounds or it's not a systemic tissue.
      # Somtimes the ouput will only list, e.g., "CPlasma" or "ITissue". Dealing
      # with this.
      CompoundThatShouldBePresent <- compoundToExtract
      ShouldBeButNotInhib <- CompoundThatShouldBePresent[
         !str_detect(CompoundThatShouldBePresent, "inhibitor")]
      if(length(ShouldBeButNotInhib) == 0){
         ShouldBeButNotInhib <- NA
      }
      
      if(any(is.na(CmpdMatches$CompoundID))){
         CmpdMatches <- CmpdMatches %>% 
            mutate(CompoundID = case_when(
               is.na(CompoundID) & 
                  CompoundCode %in% c("CPlasma", 
                                      "CPlasma interaction", 
                                      "CSys", 
                                      "CSys interaction", 
                                      "CTissue", 
                                      "CTissue Sub", 
                                      "CTissue interaction") ~ ShouldBeButNotInhib,
               
               is.na(CompoundID) & CompoundCode == "ITissue" ~ "inhibitor 1", 
               is.na(CompoundID) & CompoundCode == "IPlasma" ~ "inhibitor 1", 
               TRUE ~ CompoundID))
      }
      
      rm(CmpdMatches1, CmpdMatches2, NApos, StartRow, EndRow, AllRegCompoundsInv)
      
      for(cmpd in compoundToExtract){
         
         if(complete.cases(Deets[AllRegCompounds$DetailNames[
            AllRegCompounds$CompoundID == cmpd]]) &
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
      
   } else if(ADC){
      
      # PopStatRow <- which(str_detect(sim_data_xl$...1, "Population Statistics"))
      # NArows <- which(is.na(sim_data_xl$...1))
      # CmpdRows <- sim_data_xl$...1[(PopStatRow + 2):
      #                                 (NArows[which(NArows > PopStatRow)][1] - 1)]
      
      # FIXME: I previously found an instance where "Protein Total" at the top
      # of the Excel sheet was listed as "Therapeutic Protein" lower down. I
      # originally made them all be "PROTEINTOTAL", but that means that I can't
      # discriminate between therapeutic protein concentrations and the protein
      # of an ADC. Returning to making ONLY "protein total" be "PROTEINTOTAL" so
      # that I get ONLY that and not any therapeutic protein concentrations. Not
      # sure how to get around that if I encounter that scenario again, though.
      # I just don't know enough about when it will occur.
      sim_data_xl$...1 <- sub("^Protein [Tt]otal",
                              "PROTEINTOTAL", 
                              sim_data_xl$...1)
      
      # Protein Conjugated Drug seems to consistent throughout but making it
      # consistent to be sure. Same with therapeutic protein but also need to
      # discriminate between therapeutic protein and TMDD complex.
      sim_data_xl$...1 <- sub("^Protein [Cc]onjugated [Dd]rug",
                              "PROTEINCONJDRUG", 
                              sim_data_xl$...1)
      
      sim_data_xl$...1 <- sub("conjugated protein .dar1", # FIXME: Check this. 
                              "INTACTADC", 
                              sim_data_xl$...1)
      
      sim_data_xl$...1 <- sub("cantibody total", # FIXME: Check this. 
                              "TOTALAB", 
                              sim_data_xl$...1)
      
      sim_data_xl$...1 <- sub("^Therapeutic [Pp]rotein.*TMDD complex",
                              "TMDDCOMPLEX", 
                              sim_data_xl$...1)
      
      sim_data_xl$...1 <- sub("^Therapeutic [Pp]rotein",
                              "THERPROTEIN", 
                              sim_data_xl$...1)
      
      # There are some instances where the therapeutic protein is listed as
      # "Therapeutic protein CSys" and a few where it's only "Csys". This is
      # likely the case for the other large molecules as well, but I'll need to
      # check. Dealing with that.
      if(any(str_detect(sim_data_xl$...1, "THERPROTEIN"))){
         sim_data_xl$...1 <- sub("CSys",
                                 "THERPROTEIN", 
                                 sim_data_xl$...1)
      }
      
   } 
   
   
   # Renaming interaction text -------------------------------------------------
   
   # Multiple ways to specify that the data are the substrate or metabolite data
   # in the present of the perpetrator. Harmonizing those, too.
   sim_data_xl$...1 <- sub("After Inh|[iI]nteraction", "WITHINTERACTION", 
                           sim_data_xl$...1)
   
   # When it's lumen free or feces sometimes, for some reason, it's labeled
   # differently. Dealing with that.
   sim_data_xl$...1 <- sub("Inh C Lumen Free", "C Lumen Free WITHINTERACTION", 
                           sim_data_xl$...1)
   sim_data_xl$...1 <- sub("Inh Mur", "Mur WITHINTERACTION", 
                           sim_data_xl$...1)
   sim_data_xl$...1 <- sub("Inh Md", "Md WITHINTERACTION", 
                           sim_data_xl$...1)
   
   return(sim_data_xl)
   
}

