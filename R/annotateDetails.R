#' Annotate Simcyp Simulator experimental details
#'
#' \code{annotateDetails} converts output from either
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} into
#' a long data.frame and adds to that data.frame columns for
#' \enumerate{\item{which compound the information pertains to (substrate,
#' inhibitor, etc.),} \item{which section of the Simcyp Simulator this detail is
#' found in (physchem, absorption, distribution, etc.),} \item{notes describing
#' what the detail is, and} \item{which sheet in the Excel file the information
#' was pulled from.}} It will also optionally filter the data to return only
#' specifically requested information. \strong{A recent change as of Jan. 2023:}
#' There are now \emph{so many} possible details that it can be overwhelming to
#' get all of them! For that reason, we changed the default setting for
#' \code{omit_all_missing} to be TRUE so that, if there are only NA values for
#' all the files you're annotating, that detail will be dropped. If you still
#' get a \emph{ton} of information all at once, we recommend looking at just one
#' compound at a time. You can set the \code{compoundID} argument to just the
#' compound ID you want -- "substrate", "inhibitor 1", etc. -- or set the
#' \code{compound} argument to just the compound you want -- "midazolam" or
#' "Client Drug X" -- and that will also help make things less overwhelming and
#' easier to find.
#'
#' \emph{Nota bene:} When you initially extracted your simulation experimental
#' details, if you set \code{annotate_output = TRUE} and \code{show_compound_col
#' = "concatenate"}, please understand that you cannot go backwards here and
#' un-concatenate the compound names because, once we combine everything, we
#' don't know which piece of data belonged to which compound. You'll still get
#' concatenated compound names in the output.
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
#' @param omit_all_missing TRUE (default) or FALSE for whether to omit a detail
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
#' @param find_matching_details optionally supply a string of text to search for
#'   in the column "Detail". Regular expressions are supported here, so use,
#'   e.g., \itemize{
#'
#'   \item{"|" to mean "or",}
#'
#'   \item{".*" to mean "match any character any number of times",}
#'
#'   \item{"^" to mean "starts with...", and}
#'
#'   \item{"$" to mean "ends with...".}} An example: \code{find_matching_details
#'   = "^CLint.*CYP3A.*sub$"} to find all details that start with "CLint", then,
#'   somewhere in the name, include "CYP3A", and then end with "sub" (for
#'   "substrate"). Case sensitive.
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
#'   type \code{view(ExpDetailDefinitions)} into the console. Parameters are
#'   reported with a suffix depending on which compound they pertain to: "_sub"
#'   for the substrate, "_met1" for the primary metabolite, "_met2" for the
#'   second primary metabolite, "_secmet" for the secondary metabolite, "_inhib"
#'   for the 1st inhibitor or inducer listed, "_inhib2" for the 2nd inhibitor or
#'   inducer listed, or "_inh1met" for the inhibitor 1 metabolite. An example of
#'   acceptable input: \code{c("pKa1_sub", "fa_inhib2", "Regimen_sub")}}  Not
#'   case sensitive.}
#' @param simulator_section optionally supply a specific simulator section or
#'   sections from which to find simulation experimental details and then return
#'   \emph{only} those details. Options are "Absorption", "Distribution",
#'   "Elimination", "Interaction", "Phys Chem and Blood Binding", "Population",
#'   "Transport", or "Trial Design". Not case sensitive. If you want more than
#'   one, enclose them with \code{c(...)}.
#'
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx". If you leave off the file extension, it will be saved as a
#'   csv file.
#'
#' @return Returns a data.frame of simulation experimental details including the
#'   following columns: \describe{
#'
#'   \item{SimulatorSection}{the simulator section this detail is from, e.g.,
#'   "absorption" or "elimination"}
#'
#'   \item{Sheet}{the sheet in the Excel output where the data were found}
#'
#'   \item{Notes}{an explanation of what this detail is}
#'
#'   \item{CompoundID}{the simulator compound ID that this information pertains
#'   to, e.g., "substrate" or "inhibitor 1"}
#'
#'   \item{Compound}{the name of the compound in the simulator, e.g.,
#'   "Sim-Midazolam"}
#'
#'   \item{Detail}{the specific experimental detail}
#'
#'   \item{All files have this value for this compound and compound ID}{If all
#'   of the files have the \emph{exact same value} for this particular
#'   combination of detail, compound, and compound ID, that value will show up
#'   in this column. The idea is that this makes it easy to check that
#'   everything that \emph{should} be the same in a plethora of simulations
#'   actually \emph{is} the same.}
#'
#'   \item{one column for every file included}{the extracted simulator
#'   experimental details for each file will show up in their own columns}}
#' @export
#'
#' @examples
#'
#' annotateDetails(Deets = MDZdetails)
#'
#' # Get annotated details regarding absorption.
#' annotateDetails(Deets = MDZdetails, simulator_section = "absorption")
#'
#' # Get annotated details on whatever was used as inhibitor 1
#' annotateDetails(Deets = MDZdetails, compoundID = "inhibitor 1")
#'
#' # Get annotated details for any compounds with "midaz" or "keto" in the name.
#' annotateDetails(Deets = MDZdetails, compound = "midaz|keto")
#'
#' # Get (most of) the details to put into a table of simulation inputs:
#' annotateDetails(Deets = MDZdetails, detail_set = "Simcyp inputs")
#'
#' # Combine multiple options to get just a few specific details:
#' annotateDetails(Deets = MDZdetails,
#'             simulator_section = "absorption",
#'             compound = "midaz|keto",
#'             compoundID = "substrate")
#'
#' 
annotateDetails <- function(Deets,
                            compoundID = NA, 
                            compound = NA,
                            detail_set = NA, 
                            find_matching_details = NA,
                            simulator_section = NA, 
                            show_compound_col = TRUE,
                            omit_all_missing = TRUE, 
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
        
        Deets <- deannotateDetails(Deets, apply_class = FALSE)
        
    } else if("File" %in% names(Deets) == FALSE){
        Deets$File <- paste("unknown file", 1:nrow(Deets))
        FileOrder <- Deets$File
        Deets <- Deets %>% 
            mutate(File = factor(File, levels = FileOrder))
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
    
    # If the model was not ADAM, Deets will include a bunch of irrelevant
    # details. Removing those if none of the models for that compound ID were
    # ADAM. NOTE TO SELF: There must be a better way to do this that doesn't
    # require so much copying and pasting!
    if("Abs_model_sub" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "substrate" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_inhib" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "inhibitor 1" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_inhib2" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "inhibitor 2" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_met1" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "primary metabolite 1" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_met2" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "primary metabolite 2" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_secmet" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "secondary metabolite" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }

    if("Abs_model_inhib1met" %in% names(Deets) &&
       any(Deets$Abs_model_sub == "ADAM", na.rm = T) == FALSE){
        Out <- Out %>%
            filter(Detail %in% (AllExpDetails %>%
                                    filter(CompoundID == "inhibitor 1 metabolite" &
                                               ADAMParameter == TRUE) %>%
                                    pull(Detail)) == FALSE)
    }
    
    
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
    
    suppressMessages(
        Out <- Out %>% 
            arrange(File) %>% 
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
            ungroup())
    
    suppressMessages(
        Out <- Out %>% 
            left_join(CompoundNames))
    
    # Metabolism and interaction parameters won't match input details, so
    # adding which sheet they came from and what simulator section they
    # were.
    Out <- Out %>% unique() %>% 
        mutate(Sheet = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint|^CLadd|^CLbiliary|^CLiv|^CLrenal|^CLpo"), 
                              "Input Sheet", Sheet), 
               SimulatorSection = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint|^CLadd|^CLbiliary|^CLiv|^CLrenal|^CLpo"), 
                                         "Elimination", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                                         "Interaction", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Transport"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Transport"), 
                                         "Transporters", SimulatorSection)) %>% 
        select(any_of(c("SimulatorSection", "Sheet", "Notes", "CompoundID",
                        "Compound", "Detail")), everything()) %>% 
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
        
        suppressMessages(
            Out <- Out %>% select(-Compound) %>% left_join(AllCompounds) %>% 
                select(SimulatorSection, Sheet, Notes, CompoundID, Compound, Detail,
                       File, Value))
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
                    " for the argument `simulator_section`, but that is not among the acceptable options, which are listed in the help file. We will not filter your results based on simulator section.", 
                    call. = FALSE)
        }
        
        Out <- Out %>% filter(SimulatorSection %in% MySections)
        
    }
    
    ## detail_set -------------------------------------------------------------
    
    if(all(complete.cases(detail_set))){
        
        if(length(detail_set) == 1){
            if(tolower(detail_set) == "simcyp inputs"){
                
                CDSdetails <- c("MW", "logP", "CompoundType", "pKa1", "pKa2",
                                "BPratio", "fu", "BindingProtein", "Abs_model",
                                "Papp_Caco", "Papp_MDCK", "Papp_calibrator",
                                "Qgut", "fu_gut", "ka", "fa", "tlag",
                                "ModelType", "VssPredMeth", "Vss_input",
                                "kp_scalar", "kin_sac", "kout_sac",
                                "Vsac", "CLint", "CLrenal")
                
                Out <- Out %>%
                    filter(Detail %in%
                               c("Substrate", "PrimaryMetabolite1", 
                                 "PrimaryMetabolite2", "SecondaryMetabolite", 
                                 AllExpDetails %>% filter(complete.cases(CDSInputMatch)) %>%
                                     pull(Detail)) | SimulatorSection %in%
                               c("Elimination", "Interaction", "Transporters")) %>%
                    mutate(CompoundID = factor(CompoundID, 
                                               levels = c("Substrate", "PrimaryMetabolite1", 
                                                          "PrimaryMetabolite2", "SecondaryMetabolite", 
                                                          "Inhibitor1", "Inhibitor2", 
                                                          "Inhibitor1Metabolite"))) %>% 
                    arrange(CompoundID) %>% 
                    mutate(Detail = factor(
                        Detail,
                        levels = unique(c("Substrate", "PrimaryMetabolite1", 
                                          "PrimaryMetabolite2", "SecondaryMetabolite", 
                                          "Inhibitor1", "Inhibitor2", 
                                          "Inhibitor1Metabolite", 
                                          paste0(rep(CDSdetails, 7), 
                                                 rep(c("_sub", "_met1", "_met2", 
                                                       "_secmet", "_inhib", 
                                                       "_inhib2", "_inhib1met"), 
                                                     each = length(CDSdetails))),
                                          unique(Out$Detail))))) %>%
                    # Yes, this is taking multiple steps to get the factors in the
                    # correct order; I'm not sure of a more concise way to do this
                    # that will still accomplish what I want. -LSh
                    arrange(CompoundID, Detail) %>% 
                    mutate(Detail = factor(Detail, levels = unique(Detail))) %>% 
                    filter(complete.cases(Value))
                
            } else if(str_detect(tolower(detail_set), "summary")){
                Out <- Out %>% filter(Sheet == "Summary")
                
            } else if(str_detect(tolower(detail_set), "input")){
                Out <- Out %>% filter(Sheet == "Input Sheet")
                
            } else if(str_detect(tolower(detail_set), "population")){
                Out <- Out %>% filter(Sheet == "population")
            }
        } else {
            # This is when they have requested individual details.
            Out <- filter(Detail %in% detail_set)
        }
        
        # Removing unnecessary compounds.
        if(all(is.na(Out %>% filter(str_detect(Detail, "_inhib$")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_inhib$|Inhibitor1$"))
        }
        
        if(all(is.na(Out %>% filter(str_detect(Detail, "_inhib2")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_inhib2|Inhibitor2"))
        }
        
        if(all(is.na(Out %>% filter(str_detect(Detail, "_inhib1met")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_inhib1met|Inhibitor1Metabolite"))
        }
        
        if(all(is.na(Out %>% filter(str_detect(Detail, "_met1")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_met1|PrimaryMetabolite1"))
        }
        
        if(all(is.na(Out %>% filter(str_detect(Detail, "_met2")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_met2|PrimaryMetabolite2"))
        }
        
        if(all(is.na(Out %>% filter(str_detect(Detail, "_secmet")) %>%
                     pull(Value)))){
            Out <- Out %>% filter(!str_detect(Detail, "_secmet|SecondaryMetabolite"))
        }
    }
    
    
    # find_matching_details --------------------------------------------------------
    
    if(complete.cases(find_matching_details)){
        Out <- Out %>% 
            filter(str_detect(Detail, find_matching_details))
    }
    
    
    # Pivoting wider again ------------------------------------------------
    
    AllFiles <- unique(Out$File)
    
    Out <- Out %>% 
        pivot_wider(names_from = File, 
                    values_from = Value)
    
    if(length(AllFiles) > 1){
        # Checking for details that are the SAME across all files
        AllSame <- Out %>% 
            select(any_of(c("CompoundID", "Compound", "Detail")),
                   matches("xlsx$")) %>% 
            pivot_longer(cols = matches("xlsx$"), 
                         names_to = "File", values_to = "Value") %>% 
            group_by(across(.cols = any_of(c("Detail", "CompoundID", "Compound")))) %>% 
            summarize(Length = length(unique(Value)), 
                      UniqueVal = unique(Value)[1]) %>% 
            filter(Length == 1) %>% 
            select(-Length)
        
        suppressMessages(
            Out <- Out %>%
                left_join(AllSame)
        )
    }
    
    Out <- Out %>% 
        select(any_of(c("SimulatorSection", "Sheet", "Notes",
                        "CompoundID", "Compound", "Detail", 
                        "UniqueVal")), 
               everything())
    
    if("UniqueVal" %in% names(Out)){
        if("Compound" %in% names(Out)){
            Out <- Out %>% 
                mutate(ToOmit = complete.cases(CompoundID) & 
                           is.na(Compound)) %>% 
                filter(ToOmit == FALSE) %>% select(-ToOmit) %>% 
                rename("All files have this value for this compound ID and compound" = UniqueVal)
        } else {
            Out <- Out %>% 
                rename("All files have this value for this compound ID" = UniqueVal)
        }
    }
    
    # Removing anything that was all NA's if that's what user requested
    if(omit_all_missing){
        Out$AllNA <- apply(Out[, names(Out)[str_detect(names(Out), "xlsx$")]], 
                           MARGIN = 1, FUN = function(.) all(is.na(.)))    
        
        Out <- Out %>% filter(AllNA == FALSE) %>% select(-AllNA)
    }
    
    
    # Saving ---------------------------------------------------------------
    if(complete.cases(save_output)){
        FileName <- save_output
        if(str_detect(FileName, "\\.")){
            # Making sure they've got a good extension
            Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
            FileName <- sub(paste0(".", Ext), "", FileName)
            Ext <- ifelse(Ext %in% c("csv", "xlsx"), 
                          Ext, "csv")
            FileName <- paste0(FileName, ".", Ext)
        } else {
            FileName <- paste0(FileName, ".csv")
            Ext <- "csv"
        }
        
        switch(Ext, 
               "csv" = write.csv(Out, FileName, row.names = F), 
               "xlsx" = formatXL(
                   Out, FileName, sheet = "Simulation experimental details",
                   styles = list(
                       list(columns = which(names(Out) == "Notes"), 
                            textposition = list(wrapping = TRUE)),
                       list(rows = 0, font = list(bold = TRUE),
                            textposition = list(alignment = "middle",
                                                wrapping = TRUE)), 
                       list(columns = which(str_detect(names(Out), "All files have this value")),
                            fill = "#E7F3FF"), 
                       list(rows = 0, columns = which(str_detect(names(Out), "All files have this value")), 
                            font = list(bold = TRUE), 
                            textposition = list(alignment = "middle",
                                                wrapping = TRUE), 
                            fill = "#E7F3FF"))))
    }
    
    return(Out)
    
}


