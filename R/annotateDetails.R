#' Annotate Simcyp Simulator experimental details and format into a long
#' data.frame instead of a wide one
#'
#' \code{annotateDetails} converts output from either
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} into
#' a long data.frame and adds to that data.frame columns for
#' \enumerate{\item{which compound the information pertains to (substrate,
#' inhibitor, etc.),} \item{which section of the Simcyp Simulator this detail is
#' found in (physchem, absorption, distribution, etc.),} \item{notes describing
#' what the detail is, and} \item{which sheet in the Excel file the information
#' was pulled from.}} It will also optionally filter the data to return only
#' specifically requested information.
#'
#' \emph{Nota bene:} When you initially extracted your simulation experimental
#' details, if you set \code{annotate_output = TRUE} and \code{show_compound_col
#' = "concatenate"}, please understand that you cannot go backwards here an
#' un-concatenate the compound names. You'll still get concatenated compound
#' names in the output.
#'
#' @param Deets output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}
#' @param show_compound_col TRUE (default), FALSE, or "concatenate" for whether
#'   to include in the results the column "Compound", which is the compound's
#'   specific name in each simulation. Why would you ever omit this? If you have
#'   a compound with a slightly different name across multiple simulations,
#'   e.g., "DrugX" and "Drug X", and "Drug X - reduced Ki", you'll get a new row
#'   for every possible combination of "Compound" and "Detail", which might not
#'   make for easy comparisons. For example, a Ki value for "DrugX" will be in
#'   one row and the same Ki value for "Drug X" will be on a separate row. Try
#'   setting this to TRUE when you have similarly named compounds that really
#'   should be compared and see how that compares to the default. If you set
#'   this to "concatenate", you'll get all the possible compound names together;
#'   for example, you might see "DrugX, Drug X, or Drug X - reduced Ki" listed
#'   as the compound.
#' @param omit_all_missing TRUE or FALSE (default) for whether to omit a detail
#'   if the values are NA for all files
#' @param compoundID optionally supply one or more of "substrate", "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1", "inhibitor 2", or "inhibitor 1 metabolite" to return information
#'   \emph{only} on that/those compound(s). Remember to contain more than one
#'   compound ID with \code{c(...)}.
#' @param compound optionally supply a specific compound name or part of a
#'   specific compound name to get all possible compounds that match that and
#'   \emph{only} compounds that match that. Regular expressions are acceptable
#'   here, e.g., \code{compound = "midaz|keto"} to find any compound with either
#'   "midaz" or "keto" in the name. Not case sensitive.
#' @param detail_set optionally supply a set of details to return \emph{only}
#'   those details. Options are the same as the ones listed as possibilities for
#'   \code{exp_details} for \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}: \describe{
#'
#'   \item{"Summary tab"}{details available from the "Summary tab" (default)}
#'
#'   \item{"Input Sheet"}{details available from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{details about the population used (data come from
#'   the tab with the same name as the population simulated)}
#'
#'   \item{"Simcyp inputs"}{the details that you normally fill out on the
#'   "Simcyp inputs (and QC)" tab of a compound data sheet}
#'
#'   \item{"all"}{all possible details}
#'
#'   \item{a string of the specific details you want, each in quotes and
#'   encapsulated with \code{c(...)},}{For a complete list of possibilities,
#'   type \code{data(ExpDetailDefinitions); view(ExpDetailDefinitions)} into the
#'   console. Parameters are reported with a suffix depending on which compound
#'   they pertain to: "_sub" for the substrate, "_met1" for the primary
#'   metabolite, "_met2" for the second primary metabolite, "_secmet" for the
#'   secondary metabolite, "_inhib" for the 1st inhibitor or inducer listed,
#'   "_inhib2" for the 2nd inhibitor or inducer listed, or "_inh1met" for the
#'   inhibitor 1 metabolite. An example of acceptable input: \code{c("pKa1_sub",
#'   "fa_inhib2", "Regimen_sub")}}  Not case sensitive.}
#' @param simulator_section optionally supply a specific simulator section or
#'   sections from which to find simulation experimental details and then return
#'   \emph{only} those details. Options are "Absorption", "Distribution",
#'   "Elimination", "Interaction", "Phys Chem and Blood Binding", "Population",
#'   "Transport", or "Trial Design". Not case sensitive. If you want more than
#'   one, enclose them with \code{c(...)}.
#'
#' @param save_output optionally save the output by supplying a file name in
#'   quotes here, e.g., "My experimental details.csv". If you leave off ".csv",
#'   it will still be saved as a csv file.
#'
#' @return
#' @export
#'
#' @examples
#'
#' annotateDetails(Deets)
#' 
#' # Get annotated details regarding absorption.
#' annotateDetails(Deets = MyDetails, simulator_section = "absorption")
#'
#' # Get annotated details on whatever was used as inhibitor 1
#' annotateDetails(Deets = MyDetails, compoundID = "inhibitor 1")
#'
#' # Get annotated details for any compounds with "midaz" or "keto" in the name.
#' annotateDetails(Deets = MyDetails, compound = "midaz|keto")
#'
#' # Get (most of) the details to put into a table of simulation inputs:
#' annotateDetails(Deets = MyDetails, detail_set = "Simcyp inputs")
#'
#' # Combine multiple options to get just a few specific details:
#' annotateDetails(Deets = MyDetails, 
#'             simulator_section = "absorption",
#'             compound = "midaz|keto", 
#'             compoundID = "substrate")
#'
#' 
annotateDetails <- function(Deets,
                            compoundID = NA, 
                            compound = NA,
                            detail_set = NA, 
                            simulator_section = NA, 
                            show_compound_col = TRUE,
                            omit_all_missing = FALSE, 
                            save_output = NA){
    
    # Error catching --------------------------------------------------------
    compoundID <- tolower(compoundID)
    
    if(all(complete.cases(compoundID)) &&
       any(compoundID %in% c("substrate", "primary metabolite 1",
                             "primary metabolite 2", "secondary metabolite",
                             "inhibitor 1", "inhibitor 2", 
                             "inhibitor 1 metabolite") == FALSE)){
        
        warning(paste0("You requested the following compoundIDs that are not among the permissible options: ",
                       str_comma(setdiff(compoundID, c("substrate", 
                                                       "primary metabolite 1",
                                                       "primary metabolite 2",
                                                       "secondary metabolite",
                                                       "inhibitor 1", "inhibitor 2", 
                                                       "inhibitor 1 metabolite"))),
                       ". These will not be included in the output. Please check the help file for acceptable options for compoundID."),
                call. = FALSE)
        
        compoundID <- intersect(c("substrate", "primary metabolite 1",
                                  "primary metabolite 2", "secondary metabolite",
                                  "inhibitor 1", "inhibitor 2", 
                                  "inhibitor 1 metabolite"), compoundID)
    }
    
    
    if(length(compound) > 1){
        compound <- str_c(compound, collapse = "|")
    }
    
    simulator_section_orig <- simulator_section
    simulator_section <- str_c(unique(tolower(simulator_section)), collapse = " ")
    
    if("Substrate" %in% names(Deets) == FALSE & 
       (show_compound_col == TRUE | show_compound_col == "concatenate") & 
       "Compound" %in% names(Deets) == FALSE){
        warning(paste0("You set show_compound_col to ", show_compound_col,
                       ", but you appear to have already run annotateDetails on these data with show_compound_col = FALSE. This column no longer exists in your data, so we can't show it."), 
                call. = FALSE)
        show_compound_col <- FALSE
        compound <- NA
    }
    
    # Formatting as needed ---------------------------------------------------
    if(class(Deets)[1] == "list"){
        # This is when the output is the default list from extractExpDetails
        Deets <- as.data.frame(Deets)
    }
    
    PrevAnnotated <- all(c("SimulatorSection", "Sheet") %in% names(Deets))
    
    if(PrevAnnotated){
        
        CompoundNames <- Deets %>% 
            select(Compound, CompoundID, matches("xlsx$")) %>% 
            pivot_longer(cols = -c(Compound, CompoundID),
                         names_to = "File", values_to = "Value") %>%
            filter(complete.cases(Value) & complete.cases(Compound)) %>% 
            select(File, Compound, CompoundID) %>% unique()
        
        # This is when Deets has been annotated.
        # Ironically, need to de-annotate here to make this work well
        # with the rest of the function.
        Deets <- Deets %>% 
            select(-any_of(c("SimulatorSection", "Sheet", "Notes",
                             "CompoundID", "Compound"))) %>% 
            pivot_longer(cols = -Detail, 
                         names_to = "File", values_to = "Value") %>% 
            # Need to remove NA values here b/c they can otherwise lead to
            # multiple values for a given detail when one value is for the
            # correct compound and the other is for compounds that are present
            # in other files but DO have that particular compound ID. For
            # example, metabolite 1 might be OH-MDZ for some files and
            # OH-bupropion for others, and that will have multiple rows. Removed
            # NA values should mostly come out in the wash, I think, but there
            # is a risk that we'll lose some NA values that should be included.
            # I think that's an acceptable risk. - LSh
            filter(complete.cases(Value)) %>% 
            pivot_wider(names_from = Detail, values_from = Value)
        
        
        
    } else if("File" %in% names(Deets) == FALSE){
        Deets$File <- paste("unknown file", 1:nrow(Deets))
    }
    
    Out <- Deets %>% 
        mutate(across(.cols = everything(), .fns = as.character)) %>% 
        pivot_longer(cols = -File,
                     names_to = "Detail", 
                     values_to = "Value") %>% 
        mutate(CompoundID = str_extract(Detail, "_sub$|_inhib$|_met1$|_met2$|_secmet$|_inhib2$|_inhib1met$"),
               CompoundID = case_when(CompoundID == "_sub" | Detail == "Substrate" ~ "substrate",
                                      CompoundID == "_inhib" | Detail == "Inhibitor1" ~ "inhibitor 1",
                                      CompoundID == "_inhib2" | Detail == "Inhibitor2" ~ "inhibitor 2", 
                                      CompoundID == "_met1" | Detail == "PrimaryMetabolite1" ~ "primary metabolite 1",
                                      CompoundID == "_met2" | Detail == "PrimaryMetabolite2" ~ "primary metabolite 2", 
                                      CompoundID == "_secmet" | Detail == "SecondaryMetabolite" ~ "secondary metabolite",
                                      CompoundID == "_inhib1met" | Detail == "Inhibitor1Metabolite" ~ "inhibitor 1 metabolite"))
    
    if(PrevAnnotated == FALSE){
        CompoundNames <- Out %>%
            filter(Detail %in% c("Substrate", "PrimaryMetabolite1", 
                                 "PrimaryMetabolite2", "SecondaryMetabolite", 
                                 "Inhibitor1", "Inhibitor2", 
                                 "Inhibitor1Metabolite")) %>% 
            mutate(CompoundID = case_when(Detail == "Substrate" ~ "substrate",
                                          Detail == "PrimaryMetabolite1" ~ "primary metabolite 1",
                                          Detail == "PrimaryMetabolite2" ~ "primary metabolite 2", 
                                          Detail == "SecondaryMetabolite" ~ "secondary metabolite",
                                          Detail == "Inhibitor1" ~ "inhibitor 1", 
                                          Detail == "Inhibitor2" ~ "inhibitor 2", 
                                          Detail == "Inhibitor1Metabolite" ~ "inhibitor 1 metabolite")) %>% 
            rename("Compound" = Value) %>% 
            filter(complete.cases(Compound) & complete.cases(CompoundID)) %>%
            select(-Detail)
    }
    
    suppressMessages(
        Out <- Out %>% 
            left_join(ExpDetailDefinitions, by = c("Detail", "CompoundID")) %>% 
            # Finding some artifacts from row binding output from both of
            # extractExpDetails and extractExpDetails_mult. I think this should
            # fix the issue.
            filter(!Detail %in% c("Detail", "Value")) %>% 
            group_by(across(any_of(c("File", "Detail", "CompoundID", "SimulatorSection", 
                                     "Notes", "Value")))) %>% 
            summarize(Sheet = str_comma(Sheet, conjunction = "or")) %>% 
            mutate(Sheet = ifelse(str_detect(Sheet, "calculated or Summary|Summary or calculated") &
                                      Detail == "SimDuration", 
                                  "Summary", Sheet)) %>% 
            ungroup() %>% 
            left_join(CompoundNames))
    
    # Metabolism and interaction parameters won't match input details, so
    # adding which sheet they came from and what simulator section they
    # were.
    Out <- Out %>% 
        mutate(Sheet = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint|^CLadd|^CLbiliary|^CLiv|^CLrenal"), 
                              "Input Sheet", Sheet), 
               SimulatorSection = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint|^CLadd|^CLbiliary|^CLiv|^CLrenal"), 
                                         "Elimination", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                                         "Interaction", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Transport"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Transport"), 
                                         "Transporters", SimulatorSection)) %>% 
        select(SimulatorSection, Sheet, Notes, CompoundID, Compound, Detail, everything()) %>% 
        arrange(SimulatorSection, Detail)
    
    if(class(show_compound_col) == "logical"){
        if(show_compound_col == FALSE){
            Out <- Out %>% select(-Compound)
        }
    } else if(show_compound_col == "concatenate"){
        AllCompounds <- Out %>% select(Compound, CompoundID) %>% 
            filter(complete.cases(CompoundID)) %>% 
            group_by(CompoundID) %>% 
            summarize(Compound = str_comma(sort(unique(Compound)), conjunction = "or"))
        
        Out <- Out %>% select(-Compound) %>% left_join(AllCompounds) %>% 
            select(SimulatorSection, Sheet, Notes, CompoundID, Compound, Detail,
                   File, Value)
    }
    
    
    # Filtering as requested -----------------------------------------
    
    # Need everything to be in long format and then will pivot back to wide at
    # end. Otherwise, too difficult to filter b/c don't know in advance the names
    # of the files.
    
    ## compoundID ---------------------------------------------------------------
    if(any(complete.cases(compoundID))){
        
        Out <- Out %>% filter(CompoundID %in% sort(unique(compoundID)))
    }
    
    ## compound -------------------------------------------------------------
    if(complete.cases(compound)){
        Out <- Out %>% filter(str_detect(tolower(Compound), compound))
    }
    
    ## simulator_section --------------------------------------------------
    
    if(complete.cases(simulator_section)){
        
        MySections <- c()
        
        if(str_detect(simulator_section, "summary")){
            MySections <- c(MySections, "Summary")
            
        }
        
        if(str_detect(simulator_section, "input")){
            MySections <- c(MySections, "Input Sheet")
            
        }
        
        if(str_detect(simulator_section, "absorption")){
            MySections <- c(MySections, "Absorption")
            
        }
        
        if(str_detect(simulator_section, "distrib")){
            MySections <- c(MySections, "Distribution")
            
        }
        
        if(str_detect(simulator_section, "elim|metab")){
            MySections <- c(MySections, "Elimination")
            
        }
        
        if(str_detect(simulator_section, "phys chem|physchem|binding")){
            MySections <- c(MySections, "Phys Chem and Blood Binding")
            
        }
        
        if(str_detect(simulator_section, "population")){
            MySections <- c(MySections, "Population")
            
        } 
        
        if(str_detect(simulator_section, "interaction")){
            MySections <- c(MySections, "Interaction")
            
        } 
        
        if(str_detect(simulator_section, "transport")){
            MySections <- c(MySections, "Transporters")
            
        }
        
        if(str_detect(simulator_section, "trial design")){
            MySections <- c(MySections, "Trial Design")
        }
        
        MySections <- sort(unique(MySections))
        
        if(length(MySections) == 0){
            warning(paste0("You entered ", simulator_section_orig), 
                    " for the argument `simulator_section`, but that is not among the acceptable options. Please check the help file. We will not filter your results based on simulator section.", 
                    call. = FALSE)
        }
        
        Out <- Out %>% filter(SimulatorSection %in% MySections)
        
    }
    
    ## detail_set -------------------------------------------------------------
    
    if(complete.cases(detail_set)){
        
        if(tolower(detail_set) == "simcyp inputs"){
            
            Out <- Out %>%
                filter(Detail %in%
                           c("Substrate",
                             paste0(c("MW", "logP", "CompoundType", "pKa1", "pKa2",
                                      "BPratio", "fu", "Abs_model",
                                      "Papp_Caco", "Papp_MDCK", "Papp_calibrator",
                                      "Qgut", "fu_gut", "ka", "fa", "tlag",
                                      "ModelType", "VssPredMeth", "Vss_input",
                                      "kp_scalar", "kin_sac", "kout_sac",
                                      "Vsac", "CLint", "CLrenal"),
                                    "_sub")) |
                           SimulatorSection %in% c("Elimination", "Interaction", "Transporters")) %>%
                mutate(Detail = factor(Detail,
                                       levels = unique(c("Substrate",
                                                         paste0(c("MW", "logP", "CompoundType", "pKa1", "pKa2",
                                                                  "BPratio", "fu", "Abs_model",
                                                                  "Papp_Caco", "Papp_MDCK", "Papp_calibrator",
                                                                  "Qgut", "fu_gut", "ka", "fa", "tlag",
                                                                  "ModelType", "VssPredMeth", "Vss_input",
                                                                  "kp_scalar", "kin_sac", "kout_sac",
                                                                  "Vsac", "CLint", "CLrenal"),
                                                                "_sub"),
                                                         unique(Out$Detail))))) %>%
                arrange(Detail) %>% 
                filter(complete.cases(Value))
            
            # Removing unnecessary compounds.
            if(all(is.na(Out %>% filter(str_detect(Detail, "_inhib2")) %>%
                         pull(Value)))){
                Out <- Out %>% filter(!str_detect(Detail, "_inhib2|Inhibitor2"))
            }
            
            if(all(is.na(Out %>% filter(str_detect(Detail, "_inhib1")) %>%
                         pull(Value)))){
                Out <- Out %>% filter(!str_detect(Detail, "_inhib1|Inhibitor1"))
            }
        } else if(str_detect(tolower(detail_set), "summary")){
            Out <- Out %>% filter(Sheet == "Summary")
        } else if(str_detect(tolower(detail_set), "input")){
            Out <- Out %>% filter(Sheet == "Input Sheet")
        } else if(str_detect(tolower(detail_set), "population")){
            Out <- Out %>% filter(Sheet == "population")
        }
    }
    
    
    # Pivoting wider again ------------------------------------------------
    
    Out <- Out %>% 
        pivot_wider(names_from = File, 
                    values_from = Value)
    
    if("Compound" %in% names(Out)){
        Out <- Out %>% 
            mutate(ToOmit = complete.cases(CompoundID) & 
                       is.na(Compound)) %>% 
            filter(ToOmit == FALSE) %>% select(-ToOmit)
    }
    
    # Removing anything that was all NA's if that's what user requested
    if(omit_all_missing){
        Out$AllNA <- apply(Out[, names(Out)[str_detect(names(Out), "xlsx$")]], 
                           MARGIN = 1, FUN = function(.) all(is.na(.)))    
        
        Out <- Out %>% filter(AllNA == FALSE) %>% select(-AllNA)
    }
    
    
    # Saving ---------------------------------------------------------------
    if(complete.cases(save_output)){
        
        if(str_detect(save_output, "\\.")){
            # If they specified a file extension, replace whatever they supplied
            # with csv b/c that's the only option for file format here.
            FileName <- sub("\\..*", ".csv", save_output)
        } else {
            # If they didn't specify file extension, make it csv.
            FileName <- paste0(save_output, ".csv")
        }
        
        write.csv(Out, FileName, row.names = F)
    }
    
    return(Out)
    
}


