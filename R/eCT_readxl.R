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
                       CompoundType,
                       tissue, 
                       TissueType,
                       SheetNames){
   
   if("SimulatorUsed" %in% names(Deets) && Deets$SimulatorUsed == "Simcyp Discovery"){
      if(compoundToExtract %in% c("substrate", 
                                  "primary metabolite 1") == FALSE){
         warning(paste0("This seems to be a Simcyp Discovery simulation, and the only compunds you can extract from that are `substrate` or `primary metabolite 1`, and you requested `", 
                        compoundToExtract, "`. We'll return substrate concentrations instead.\n"), 
                 call. = FALSE)
         compoundToExtract <- "substrate"
      }
      
      Sheet <- switch(compoundToExtract, 
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
      
   } else if(TissueType == "systemic"){
      
      # Only looking for only sheets with conc-time data and not AUC, etc.
      PossSheets <- SheetNames[
         !str_detect(tolower(SheetNames), "auc|absorption|summary|ode state|demographic|fm and fe|input|physiology|cl profiles|^cl |^clint|clearance|clinical|cmax|cyp|ugt|population|pk( )?pd parameters|tmax|vss")
      ]
      
      if(all(CompoundType == "ADC")){
         PossSheets <- PossSheets[
            str_detect(PossSheets, 
                       switch(compoundToExtract, 
                              "conjugated protein" = "Conc Profiles C[Ss]ys|Protein Conc Trials", 
                              "total protein" = "Conc Profiles C[Ss]ys|Protein Conc Trials", 
                              "released payload" = paste0("Sub Pri Met1.*",
                                                          str_to_title(tissue))
                       ))]
         
         Sheet <- PossSheets[1]
         
      } else {
         
         # Searching for correct tissue
         PossSheets <- PossSheets[
            str_detect(tolower(PossSheets), 
                       switch(tissue, 
                              "plasma" = "cplasma",
                              "unbound plasma" = "cuplasma",
                              "peripheral plasma" = "cuplasma",
                              "peripheral unbound plasma" = "cuplasma",
                              "portal vein plasma" = "cplasma",
                              "portal vein unbound plasma" = "cuplasma",
                              
                              "blood" = "cblood",
                              "unbound blood" = "cublood",
                              "peripheral blood" = "cblood",
                              "peripheral unbound blood" = "cublood",
                              "portal vein blood" = "cblood",
                              "portal vein unbound blood" = "cublood", 
                              
                              "liver" = "liver" # At least in 1 instance I've found in V22, liver tissue has same format as systemic.
                       ))]
         
         # add criteria for peripheral, pv when needed
         if(str_detect(tissue, "peripheral|portal vein")){
            Cond1 <- str_extract(tissue, "peripheral|portal vein")
            PossSheets <- PossSheets[
               str_detect(tolower(PossSheets), 
                          switch(Cond1,
                                 "peripheral" = "periph", 
                                 "portal vein" = "^pv"))]
         }
         
         # Searching for correct compound. Substrate, inhibitor 1, inhibitor 1
         # metabolite, and inhibitor 2 concentrations will all be on the main
         # concentration-time data tab, but other compounds will be on separate
         # tabs. 
         if(any(compoundToExtract %in%  c("primary metabolite 1",
                                          "primary metabolite 2",
                                          "secondary metabolite")) &
            tissue != "liver"){
            PossSheets <- PossSheets[
               switch(compoundToExtract[1], 
                      "primary metabolite 1" = 
                      str_detect(tolower(PossSheets), "sub met|sub pri met1") & 
                      !str_detect(tolower(PossSheets), "sub met2"),
                      "primary metabolite 2" = 
                      str_detect(tolower(PossSheets), "sub met2|sub pri met2"), 
                      "secondary metabolite" = 
                      str_detect(tolower(PossSheets), "sub sm|sub sec met")
               )]
         }
         
         Sheet <- PossSheets[1]
      }
      
      } else {
         
         # when tissue is not systemic: 
         
         SheetToDetect <-
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
                   "liver" = "Liver Conc",
                   "spleen" = "Spleen Conc",
                   "feto-placenta" = "Feto-Placenta", # Need to check this one. I don't have an example output file for this yet!
                   "stomach" = "Stomach Prof",
                   "duodenum" = "Duodenum Prof",
                   "jejunum i" = "Jejunum I Prof",
                   "jejunum ii" = "Jejunum II Prof",
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
                                                     switch(CompoundType, # FIXME - I don't know where this info is or what sheet names to expect.
                                                            "substrate" = "Sub",
                                                            "inhibitor 1" = "Inhib")) # Need to check this for inhibitor 1 ADAM model data. This is just my guess as to what the sheet name will be!
            )
         
         if(tissue == "faeces"){ 
            SheetToDetect <- switch(compoundToExtract, 
                                    "inhibitor 1" = "Faeces Prof. .Inh 1", 
                                    # inhibitor 2 does not appear to be available
                                    "substrate" = "Faeces Prof. .Sub")
         }
         
         Sheet <- SheetNames[str_detect(SheetNames, SheetToDetect)][1]
         
      }
   
   if(length(Sheet) == 0 | is.na(Sheet) | Sheet %in% SheetNames == FALSE){
      warning(paste0("You requested data for ", str_comma(compoundToExtract),
                     " in ", tissue,
                     " from the file `",
                     sim_data_file, "``, but that compound and/or tissue or that combination of compound and tissue is not available in that file and will be skipped.\n"),
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