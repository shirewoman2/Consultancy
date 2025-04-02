#' Read into memory an Excel sheet for extracting conc-time data.
#'
#' INTERNAL USE ONLY. Determines correct sheet to read based on tissue and
#' compound ID requested.
#'
#' @param compoundToExtract compoundID
#' @param tissue tissue
#' @param Deets 1 row data.frame of MainDetails for that sim_data_file 
#' @param TissueType systemic or tissue
#' @param SheetNames all sheets in sim_data_file
#' @param sim_data_file sim_data_file
#'
#' @return sim_data_xl - the entire, unedited contents of the sheet. 
#'
#' @examples
#' # none 
#' 
eCT_readxl <- function(sim_data_file, 
                       Deets, 
                       compoundToExtract, 
                       tissue, 
                       TissueType,
                       SheetNames){
   
   CompoundToFind <- case_when(
      TissueType == "systemic" & 
         compoundToExtract %in% c("substrate",
                                  "inhibitor 1", 
                                  "inhibitor 1 metabolite", 
                                  "inhibitor 2") ~ "substrate", 
      
      TissueType == "systemic" & 
         compoundToExtract %in% c("substrate",
                                  "inhibitor 1", 
                                  "inhibitor 1 metabolite", 
                                  "inhibitor 2") == FALSE ~ compoundToExtract, 
      
      TissueType %in% c("liver", "faeces", "tissue") ~ "substrate") %>% 
      unique()
   
   if("SimulatorUsed" %in% names(Deets) && Deets$SimulatorUsed == "Simcyp Discovery"){
      # Simcyp Discovery data extraction
      
      # Already took care of this situation elsewhere, so we can probably delete this.
      if(all(compoundToExtract %in% c("substrate", "primary metabolite 1")) == FALSE){
         warning(paste0("This seems to be a Simcyp Discovery simulation, and the only compunds you can extract from that are `substrate` or `primary metabolite 1`, and you requested `", 
                        compoundToExtract, "`. We'll return substrate concentrations instead.\n"), 
                 call. = FALSE)
         compoundToExtract <- "substrate"
      }
      
      Sheet <- switch(CompoundToFind, 
                      "substrate" = 
                         switch(tissue, 
                                "plasma" = "Conc Profiles", 
                                "liver" = "Liver Conc Profiles", 
                                "portal vein" = "PV Conc Profiles"), 
                      "primary metabolite 1" =
                         switch(tissue, 
                                "plasma" = "Sub Pri Metab Conc Profiles"))
      
      if(is.null(Sheet)){
         warning("The combination of compound ID and tissue you requested is not availble for Simcyp Discovery files. Please contact the R Working Group if you think it should be.\n", 
                 call. = FALSE)
         
         return(data.frame())
      }
      
   } else {
      # Simcyp Simulator output 
      
      # Only looking for only sheets with conc-time data and not AUC, etc.
      PossSheets <- SheetNames[
         !str_detect(tolower(SheetNames), "auc|absorption|summary|ode state|demographic|fm and fe|input|physiology|cl profiles|^cl |^clint|clearance|clinical|cmax|cyp|ugt|population|pk( )?pd parameters|tmax|vss")
      ]
      
      if(TissueType == "systemic"){
         
         if(all(CompoundToFind %in% c(# "intact ADC", 
                                      "total antibody", 
                                      "conjugated payload"))){ 
            
            PossSheets <- PossSheets[
               str_detect(PossSheets, 
                          case_when(
                             CompoundToFind[1] %in% c("total antibody", 
                                                      # "intact ADC", 
                                                      "conjugated payload") ~
                                case_match(tissue, 
                                           "plasma" ~ "Conc Profiles C[Ss]ys|Protein Conc Trials", 
                                           "lymph" ~ "Lymph Conc Profiles")))]
            
         } else {
            
            # Searching for correct tissue
            PossSheets <- PossSheets[
               str_detect(tolower(PossSheets), 
                          switch(tissue, 
                                 "plasma" = "cplasma",
                                 "unbound plasma" = "cuplasma",
                                 "peripheral plasma" = "cplasma",
                                 "peripheral unbound plasma" = "cuplasma",
                                 "portal vein plasma" = "cplasma",
                                 "portal vein unbound plasma" = "cuplasma",
                                 "portal vein blood" = "cblood",
                                 "portal vein unbound blood" = "cublood",
                                 "blood" = "cblood",
                                 "unbound blood" = "cublood",
                                 "peripheral blood" = "cblood",
                                 "peripheral unbound blood" = "cublood"
                          ))]
            
            # Animal sims aren't labeled the same way. 
            if(length(PossSheets) == 0 & Deets$Species != "human"){
               PossSheets <- "Conc Profiles"
            }
            
            # add criteria for peripheral when needed
            if(str_detect(tissue, "peripheral")){
               Cond1 <- str_extract(tissue, "peripheral")
               PossSheets <- PossSheets[
                  str_detect(tolower(PossSheets), "periph")]
            }
            
            # Making sure to get correct compound. 
            if(any(CompoundToFind %in%  c("primary metabolite 1",
                                          "primary metabolite 2",
                                          "secondary metabolite"))){
               PossSheets <- PossSheets[
                  switch(CompoundToFind, 
                         "primary metabolite 1" = 
                            str_detect(tolower(PossSheets), "sub met|sub pri met1") & 
                            !str_detect(tolower(PossSheets), "sub met2"),
                         "primary metabolite 2" = 
                            str_detect(tolower(PossSheets), "sub met2|sub pri met2"), 
                         "secondary metabolite" = 
                            str_detect(tolower(PossSheets), "sub sm|sub sec met")
                  )]
            } else {
               PossSheets <- PossSheets[!str_detect(tolower(PossSheets), 
                                                    "sub met|sub pri met|sub sm|sub sec met")]
            }
         }
         
      } else if(TissueType == "liver"){
         # Note that this includes portal vein concs b/c those tabs are set up
         # the same as liver: they have all possible compounds on a single
         # sheet.
         
         PossSheets <- PossSheets[
            str_detect(tolower(PossSheets), 
                       switch(tissue, 
                              "liver" = "liver", 
                              "portal vein plasma" = "^pv.*cplasma", 
                              "portal vein blood" = "^pv.*cblood", 
                              "portal vein unbound plasma" = "^pv.*cuplasma", 
                              "portal vein unbound blood" = "^pv.*cublood"))]
         
      } else if(TissueType == "faeces"){
         
         PossSheets <- PossSheets[
            str_detect(tolower(PossSheets), 
                       switch(compoundToExtract, 
                              "inhibitor 1" = "faeces prof. .inh 1", 
                              # inhibitor 2 does not appear to be available
                              "substrate" = "faeces prof. .sub"))
         ]
         
      } else if(TissueType == "tissue"){
         
         PossSheets <-
            switch(tissue,
                   "gi tissue" = "Gut Tissue Conc",
                   "gut tissue" = "Gut Tissue Conc", 
                   "git" = "Gut Tissue Conc",
                   "lung" = "Lung Conc",
                   "additional organ" = "Additional Organ Conc",
                   "adipose" = "Adipose Conc",
                   "heart" = "Heart Conc",
                   "muscle" = "Muscle Conc",
                   "bone" = "Bone Conc",
                   "kidney" = "Kidney Conc",
                   "skin" = "Skin Conc",
                   "pancreas" = "Pancreas Conc",
                   "brain" = "Brain Conc",
                   "spleen" = "Spleen Conc",
                   "feto-placenta" = "Feto-Placenta", # Need to check this one. I don't have an example output file for this yet!
                   "stomach" = "Stomach Prof",
                   "duodenum" = "Duodenum Prof",
                   "jejunum i" = "Jejunum I Prof",
                   "jejunum ii" = "Jejunum II Prof",
                   "jejunum iii" = "Jejunum III Prof", 
                   "jejunum iv" = "Jejunum IV Prof", 
                   "ileum i" = "Ileum I Prof",
                   "ileum ii" = "Ileum II Prof",
                   "ileum iii" = "Ileum III Prof",
                   "ileum iv" = "Ileum IV Prof",
                   "colon" = "Colon Prof",
                   "faeces" = "Faeces Prof",
                   "feces" = "Faeces Prof",
                   "cumulative absorption" = "Cumulative Abs",
                   "cumulative fraction released" = "CR Profile",
                   "cumulative dissolution" = paste0("Cum.*Dissolution.*",
                                                     switch(CompoundToFind, # FIXME - I don't know where this info is or what sheet names to expect.
                                                            "substrate" = "Sub",
                                                            "inhibitor 1" = "Inhib")) # Need to check this for inhibitor 1 ADAM model data. This is just my guess as to what the sheet name will be!
            )
         
         PossSheets <- SheetNames[str_detect(SheetNames, PossSheets)]
         
      }
      
      Sheet <- PossSheets[1]
      
   }
   
   if(length(Sheet) == 0 | is.na(Sheet) | Sheet %in% SheetNames == FALSE){
      warning(wrapn(paste0("You requested data for ", str_comma(compoundToExtract),
                           " in ", tissue,
                           " from the file `",
                           sim_data_file, "``, but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped.")),
              call. = FALSE)
      return(data.frame())
   }
   
   # Reading in simulated concentration-time profile data
   sim_data_xl <- suppressMessages(
      readxl::read_excel(path = sim_data_file,
                         sheet = Sheet,
                         col_names = FALSE))
   
   return(sim_data_xl)
   
}

