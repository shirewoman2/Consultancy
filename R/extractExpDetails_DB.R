#' Extract simulation experimental details from a database file
#'
#' @description \code{extractExpDetails_DB} reads a Simcyp Simulator database
#'   file for information about how the simulation was set up. This uses the
#'   Simcyp package under the hood.
#'
#' @param sim_data_file the database file to get information from. To get
#'   \emph{all} possible information, please be sure that a matching workspace
#'   MUST also be present, as in, the exact same file name but ending in ".wksz"
#'   instead of ".db". This is because some information is not present in the
#'   database file but \emph{is} in the workspace.
#'
#' @return a list of information about how your simulations were set up, and
#'   this list will take the same format as the lists you would receive from
#'   other extractExpDetails_x functions.
#' @export
#'
#' @examples
#' # none yet
#' 
extractExpDetails_DB <- function(sim_data_file){
   
   # Error catching ----------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   # Check whether Simulator has been initialized. 
   SimInit <- check_simulator_initialized()
   if(SimInit == FALSE){
      warning(paste0(str_wrap(paste0(
         "The Simcyp Simulator has not been initialized, which must happen to pull data from the database file '", 
         sim_data_file, 
         "'. To initialize it, please run something like")), 
         
         "\n", 
         
         "   Simcyp::Initialise(species = Simcyp::SpeciesID$Human, requestedVersion = 24) \n"), 
         call. = FALSE)
      
      return(list())
   }
   
   if(length(sim_data_file) > 1){
      warning(wrapn("The extractExpdetails_DB function is for getting information from one database simlation file at a time, and you have provided more than that. Please check your input and try again."), 
              call. = FALSE)
      return(list())
   }
   
   # Getting the basic file name w/out extension
   sim_basename <- sub("\\.db|\\.wksz|\\.xlsx", "", sim_data_file)
   
   # If they didn't include ".db" at the end, add that.
   sim_data_file <- paste0(sim_basename, ".db")
   
   # Main body of function ---------------------------------------------------
   
   conn <- RSQLite::dbConnect(RSQLite::SQLite(), sim_data_file) 
   on.exit(expr = RSQLite::dbDisconnect(conn))
   
   Workspace <- paste0(GetParameter(
      Tag = "WorkspaceName", Category = 9, SubCategory = NA), ".wksz")
   
   suppressMessages(
      Simcyp::SetWorkspace(Workspace, verbose = FALSE))
   
   # Figure out which compound positions were active. 
   ActiveCompounds <- 
      sapply(paste0("idInhEnabled", AllRegCompounds$CompoundID_num_Simcyp[
         complete.cases(AllRegCompounds$CompoundID_num_Simcyp)]), 
         \(x) Simcyp::GetParameter(Tag = x, 
                                   Category = Simcyp::CategoryID$SimulationData, 
                                   SubCategory = 0))
   names(ActiveCompounds) <- AllRegCompounds$DetailNames[
      complete.cases(AllRegCompounds$CompoundID_num_Simcyp)]
   ActiveCompounds <- names(ActiveCompounds)[ActiveCompounds]
   
   # # Comment this out for actual function, but here is how to see all possible
   # parameters for compound, population, and simulation data. You can get
   # subcategories by the following: 1) find the tag for the parameter you want
   # in the Simulator by right clicking on it, 2) click on the tag, 3) paste
   # into R. This will give you the path for that parameter within the XML file.
   
   # AllSimulationParameters <- 
   #    data.frame(Name = names(Simcyp::SimulationParameterID), 
   #               Tag = as.character(unlist(Simcyp::SimulationParameterID)))
   # 
   # AllCompoundParameters <- 
   #    data.frame(Name = names(Simcyp::CompoundParameterID), 
   #               Tag = as.character(unlist(Simcyp::CompoundParameterID)))
   #
   # AllPopulationIDs <- 
   #    data.frame(Name = names(Simcyp::PopulationID), 
   #               Tag = as.character(unlist(Simcyp::PopulationID)))
   
   ParameterConversion <- AllExpDetails %>% 
      filter(DataSource == "workspace or database" &
                complete.cases(SimcypParameterType)) %>% 
      select(Detail, CompoundID, matches("Level"), XMLswitch,
             SimcypParameterType, SimcypTag, SimcypSubcategory, 
             ColsChangeWithCmpd) %>% 
      mutate(Detail_nosuffix = case_when(
         ColsChangeWithCmpd == TRUE ~ sub("_sub|_inhib2|_inhib$|_met1|_met2|_secmet|_inhib1met|_endog", 
                                          "", Detail), 
         TRUE ~ Detail)) %>% 
      left_join(AllRegCompounds %>% select(CompoundID, Suffix, CompoundID_Simcyp), 
                by = "CompoundID") %>% 
      mutate(CompoundID_Simcyp = factor(CompoundID_Simcyp, 
                                        levels = c("Substrate", "SubPriMet1", "SubPriMet2", 
                                                   "SubSecMet", "Inhibitor1", "Inhibitor1Met", 
                                                   "Inhibitor2", "Endogenous"))) %>% 
      arrange(CompoundID_Simcyp)
   
   Details <- list()
   
   # Compound parameters --------------------------------------------------------
   
   CmpdParam <- unique(
      ParameterConversion$Detail_nosuffix[
         ParameterConversion$SimcypParameterType == "compound"])
   CmpdParam <- setdiff(CmpdParam, AllRegCompounds$DetailNames)
   
   for(k in CmpdParam){
      
      ParamConv_subset <- ParameterConversion %>% 
         filter(Detail_nosuffix == k)
      
      suppressMessages(Details_toadd <- 
                          map(.x = Simcyp::CompoundID[ParamConv_subset$CompoundID_Simcyp], 
                              .f = function(x){Simcyp::GetCompoundParameter(
                                 Tag = unique(ParamConv_subset$SimcypTag), 
                                 Compound = x)}))
      
      Details_toadd <- Details_toadd[sapply(Details_toadd, length) > 0]
      
      if(length(Details_toadd) == 0){next}
      
      # Decoding as necessary. Add to the options for k as needed.
      Details_toadd <- lapply(Details_toadd, as.character)
      Details_toadd <- lapply(Details_toadd, 
                              function(DeetValue) case_when(
                                 
                                 str_detect(k, "Abs_model") ~  
                                    case_match(DeetValue, 
                                               "0" ~ "1st order", 
                                               "2" ~ "ADAM"), 
                                 
                                 str_detect(k, "DistributionModel") ~ 
                                    case_match(DeetValue, 
                                               "1" ~ "Full PBPK Model", 
                                               "0" ~ "Minimal PBPK Model"), 
                                 
                                 str_detect(k, "DoseRoute") ~ 
                                    case_match(DeetValue, 
                                               "1" ~ "Oral", 
                                               "2" ~ "IV"), 
                                 
                                 str_detect(k, "Peff_MechPeff_totalvsfree") ~
                                    case_match(DeetValue, 
                                               "0" ~ "total", 
                                               "1" ~ "free"), 
                                 
                                 str_detect(k, "Peff_pred_or_user") ~
                                    case_match(DeetValue, 
                                               "0" ~ "user", 
                                               "1" ~ "predicted"), 
                                 
                                 str_detect(k, "Peff_prediction_method") ~
                                    case_match(DeetValue, 
                                               "3" ~ "Papp using PSA",
                                               "5" ~ "Papp", # need to add info here 
                                               "6" ~ "MechPeff"), 
                                 
                                 str_detect(k, "Permeability_reference_lock") ~
                                    case_match(DeetValue, 
                                               "true" ~ "unlocked", 
                                               "false" ~ "locked"), 
                                 
                                 TRUE ~ DeetValue))
      
      # FIXME - Need to deal with switches still. 
      
      names(Details_toadd) <- ParamConv_subset$Detail
      Details <- c(Details, Details_toadd)
      rm(Details_toadd, ParamConv_subset)
      
   }
   
   # Removing info for compounds that are not active
   Details <- Details[
      which(str_detect(names(Details), 
                       str_c(paste0(AllRegCompounds$Suffix[AllRegCompounds$DetailNames %in% ActiveCompounds], 
                                    "$"), 
                             collapse = "|")))]
   
   
   # General parameters --------------------------------------------------------
   
   # Not sure why, but NumTimeSamples is not working, probably b/c I haven't set
   # up the subcategory correctly.
   GenParam <- ParameterConversion %>% 
      filter(SimcypParameterType == "general" & 
                Detail != "NumTimeSamples") %>% 
      pull(Detail)
   
   for(k in GenParam){
      
      ParamConv_subset <- ParameterConversion %>% 
         filter(Detail_nosuffix == k)
      
      Details_toadd <- Simcyp::GetParameter(
         Tag = ParamConv_subset$SimcypTag, 
         Category = Simcyp::CategoryID$SimulationData, 
         SubCategory = ifelse(is.na(ParamConv_subset$SimcypTag),
                              0, ParamConv_subset$SimcypSubcategory))
      
      if(is.null(Details_toadd)){
         next
      }
      
      names(Details_toadd) <- k
      Details <- c(Details, Details_toadd)
      
      if(str_detect(k, "NumDoses")){
         MoreDetails_toadd <- 
            ifelse(Details_toadd > 1, "Multiple Dose", "Single Dose")
         names(MoreDetails_toadd) <- 
            sub("NumDoses", "Regimen", k)
         Details <- c(Details, MoreDetails_toadd)
         rm(MoreDetails_toadd)
      }
      
      rm(Details_toadd, ParamConv_subset)
      
   }
   
   # Setting units here b/c they're always the same for database files; at
   # least, they always have been. This should allow for some flexibility later
   # if that changes.
   Details[["Units_AUC"]] <- "mg/L.h"
   Details[["Units_CL"]] <- "L/h"
   Details[["Units_Cmax"]] <- "mg/L"
   Details[["Units_tmax"]] <- "h"
   
   
   # Compound names --------------------------------------------------------
   
   # Getting compound names separately.
   CmpdNames <- 
      map(.x = AllRegCompounds$CompoundID_num_Simcyp[
         complete.cases(AllRegCompounds$CompoundID_num_Simcyp)], 
         .f = function(x){Simcyp::GetCompoundParameter(
            Tag = "idName", 
            Compound = x)})
   names(CmpdNames) <- AllRegCompounds$DetailNames[
      complete.cases(AllRegCompounds$CompoundID_num_Simcyp)]
   
   Details <- c(Details, 
                CmpdNames[intersect(AllRegCompounds$DetailNames, ActiveCompounds)])
   
   # t(as.data.frame(Details))
   
   # Other functions call on "Inhibitor1", etc., so we need those objects to
   # exist, even if they were not used in this simulation. Setting them to NA if
   # they don't exist.
   MissingCmpd <- setdiff(AllRegCompounds$DetailNames, 
                          names(Details))
   MissingCmpd_list <- as.list(rep(NA, length(MissingCmpd)))
   names(MissingCmpd_list) <- MissingCmpd
   
   Details <- c(Details, MissingCmpd_list)
   
   
   # Dosing ------------------------------------------------------------------
   
   Dosing.db <- DBI::dbGetQuery(conn, "SELECT * FROM Doselist")
   
   Dosing <- Dosing.db %>%
      rename(DoseNum = DoseIndex, 
             Dose = Site1Dose, 
             Dose_units = Site1Units, 
             DoseRoute = DosingRouteDescription, 
             InfusionDuration = Duration) %>% 
      mutate(File = sim_data_file, 
             PrandialSt = case_match(DosingFedState,
                                     3 ~ "Fasted"),
             CompoundID_num_Simcyp = CompoundIndex + 1) %>% 
      left_join(AllRegCompounds %>% select(CompoundID_num_Simcyp, CompoundID), 
                by = "CompoundID_num_Simcyp") %>% 
      filter(CompoundID %in% unique(AllRegCompounds$DosedCompoundID)) %>% 
      select(File, CompoundID, Time, DoseNum, PrandialSt, InfusionDuration, 
             Dose, DoseRoute, Dose_units) %>% unique()
   
   # Adding prandial state info. NB: This assumes that the prandial state was
   # the SAME for all doses and compounds. You *can* change that with a
   # custom-dosing regimen, but I haven't seen that ever. Not setting that up
   # here for now.
   PrandialState <- Dosing %>% 
      select(CompoundID, PrandialSt) %>% unique() %>% 
      left_join(AllRegCompounds %>% select(CompoundID, Suffix), 
                by = "CompoundID") %>% 
      mutate(Parameter = paste0("PrandialSt", Suffix))
   
   PrandialState_list <- setNames(as.list(PrandialState$PrandialSt), 
                                  nm = PrandialState$Parameter)
   
   Details <- c(Details, 
                PrandialState_list)
   
   # Finishing adding dosing info
   Dosing <- Dosing %>% 
      mutate(Compound = case_match(CompoundID, 
                                   "substrate" ~ Details$Substrate, 
                                   "inhibitor 1" ~ Details$Inhibitor1, 
                                   "inhibitor 2" ~ Details$Inhibitor2), 
             # Time units are always hours for db
             Time_units = "h") %>% 
      select(File, CompoundID, Compound, #Day, TimeOfDay, 
             Time, Time_units, DoseNum, 
             Dose, Dose_units, DoseRoute, InfusionDuration)
   
   
   
   # Pulling from workspace file -------------------------------------------
   
   # Checking that the workspace file is available. This will ignore the
   # date/time stamp on the Excel results if it's still there. Not sure if this
   # is necessary b/c I think you have to have the workspace exist when we
   # called SetWorkspace.
   WkspFile <- Workspace[which(file.exists(Workspace))]
   
   if(length(WkspFile) > 0){
      
      TEMP <- extractExpDetails_XML(
         sim_workspace_files = WkspFile, 
         compoundsToExtract = "all",
         exp_details = "all")
      
      TEMP$MainDetails <- TEMP$MainDetails %>% 
         # This currently removes anything that we already have from the
         # Excel or database file. May change that later to verify that Excel
         # or db and workspace match.
         select(!any_of(c("Substrate", "Inhibitor1", "Inhibitor2", 
                          "PrimaryMetabolite1", "PrimaryMetabolite2", 
                          "SecondaryMetabolite", "Inhibitor1Metabolite", 
                          paste0("DistributionModel",
                                 c("inhib1met", 
                                   "_met1", "_met2", "_secmet")))))
      
      # Note: Currently, we are not extracting anything from the workspace
      # that would be its own separate list. When we DO do that, we'll need
      # to adjust this code to bind the MainDetails and whatever that list
      # is.
      Details <- c(Details,
                   TEMP$MainDetails[
                      setdiff(names(TEMP$MainDetails)[
                         names(TEMP$MainDetails) != "Workspace"], 
                         names(Details))])
      
      rm(TEMP)
      
   }
   
   # Finishing up --------------------------------------------------------
   
   # RSQLite::dbDisconnect(conn)
   
   Details <- as.data.frame(Details) %>% 
      mutate(File = sim_data_file)
   
   Details <- harmonize_details(list(MainDetails = Details))
   Details[["Dosing"]] <- Dosing
   
   return(Details)
   
}

