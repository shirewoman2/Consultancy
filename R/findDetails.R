#' Find specific details or sets of details from the output from
#' extractExpDetails
#'
#' Nota bene: If you ran \code{\link{annotateDetails}} on your data first and,
#' when you ran that, you asked to concatenate the values in the compound
#' column, please understand that you cannot go backwards here an un-concatenate
#' the compound names. You'll still get concatenated compound names in the
#' output.
#'
#' \code{findDetails} takes output from \code{\link{extractExpDetails}} or
#' \code{\link{extractExpDetails_mult}}, annotates it, and finds specific
#' details or sets of details from that output. For example, it can find all the
#' details on anything that was used as "inhibitor 1" or all the details for a
#' compound with "midazo" in the name. You can also search by Simulator section
#' to get everything on absorption, for example, or everything on interactions.
#'
#' @param Deets output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}
#' @param compoundID optionally supply one or more of "substrate", "primary
#'   metabolite 1", "primary metabolite 2", "secondary metabolite", "inhibitor
#'   1", "inhibitor 2", or "inhibitor 1 metabolite" to get information on
#'   that/those compound(s). Remember to contain more than one value with
#'   \code{c(...)}!
#' @param compound optionally supply a specific compound name or part of a
#'   specific compound name to get all possible compounds that match that.
#'   Regular expressions are acceptable here, e.g., \code{compound =
#'   "midaz|keto"} to find any compound with either "midaz" or "keto" in the
#'   name. Not case sensitive. 
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
#' @param detail_set optionally supply a set of details to extract where options
#'   are the same as the ones listed as possiblilities for \code{exp_details}
#'   for \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}: \describe{
#'
#'   \item{"Summary tab"}{Extract details available from the "Summary tab"
#'   (default)}
#'
#'   \item{"Input Sheet"}{Extract details available from the "Input Sheet" tab}
#'
#'   \item{"population tab"}{Extract details about the population used (data
#'   come from the tab with the same name as the population simulated)}
#'
#'   \item{"Simcyp inputs"}{Extract all the details that you normally fill out
#'   on the "Simcyp inputs (and QC)" tab of a compound data sheet}
#'
#'   \item{"all"}{Extract all possible parameters}
#'
#'   \item{a string of the specific parameters you want, each in quotes and
#'   encapsulated with \code{c(...)},}{For a complete list, type
#'   \code{data(ExpDetailDefinitions); view(ExpDetailDefinitions)} into the
#'   console. Parameters are reported with a suffix depending on which compound
#'   they pertain to: "_sub" for the substrate, "_met1" for the primary
#'   metabolite, "_met2" for the second primary metabolite, "_secmet" for the
#'   secondary metabolite, "_inhib" for the 1st inhibitor or inducer listed,
#'   "_inhib2" for the 2nd inhibitor or inducer listed, or "_inh1met" for the
#'   inhibitor 1 metabolite. An example of acceptable input: \code{c("pKa1_sub",
#'   "fa_inhib2", "Regimen_sub")}}  Not case sensitive.}#'
#' @param simulator_section optionally supply a specific simulator section from
#'   which to find simulation setup details. Options are "Absorption",
#'   "Distribution", "Elimination", "Interaction", "Phys Chem and Blood
#'   Binding", "Population", "Transport", or "Trial Design". Not case sensitive.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Find details regarding absorption.
#' findDetails(Deets = MyDetails, simulator_section = "absorption")
#'
#' # Find details on whatever was used as inhibitor 1
#' findDetails(Deets = MyDetails, compoundID = "inhibitor 1")
#'
#' # Find details for any compounds with "midaz" or "keto" in the name.
#' findDetails(Deets = MyDetails, compound = "midaz|keto")
#'
#' # Find (most of) the details to put into a table of simulation inputs:
#' findDetails(Deets = MyDetails, detail_set = "Simcyp inputs")
#'
#' # Combine multiple options to get just a few specific details:
#' findDetails(Deets = MyDetails, simulator_section = "absorption",
#'             compound = "midaz|keto", compoundID = "substrate")
#' 
findDetails <- function(Deets, 
                        compoundID = NA, 
                        compound = NA,
                        show_compound_col = TRUE,
                        detail_set = NA, 
                        simulator_section = NA){
    
    # Error catching --------------------------------------------------------
    compoundID <- tolower(compoundID)
    
    if(all(complete.cases(compoundID)) &&
       any(compoundID) %in% c("substrate", "primary metabolite 1",
                              "primary metabolite 2", "secondary metabolite",
                              "inhibitor 1", "inhibitor 2", 
                              "inhibitor 1 metabolite") == FALSE){
        
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
    
    if("Substrate" %in% names(Deets) == FALSE & # this happens when the user has NOT run annotateDetails on Deets yet
        (show_compound_col == TRUE | show_compound_col == "concatenate") & 
       "Compound" %in% names(Deets) == FALSE){
        warning(paste0("You set show_compound_col to ", show_compound_col,
                      ", but you appear to have already run annotateDetails on these data with show_compound_col = FALSE. This column no longer exists in your data, so we can't show it."), 
                call. = FALSE)
        show_compound_col <- FALSE
        compound <- NA
    }
    
    # Main body of function ---------------------------------------------------
    ## Getting set up -------------------------------------------------------
    Out <- Deets
    
    if(class(Out)[1] == "list"){
        # This is when the output is the default list from extractExpDetails
        suppressMessages(Out <- as.data.frame(Out))
    }
    
    if("SimulatorSection" %in% names(Out) == FALSE){
        Out <- annotateDetails(Out, 
                               show_compound_col = TRUE) 
        # We will concatenate or remove columns as desired LATER, after we've
        # done any filtering requested.
    }
    
    # Need everything to be in long format and then will pivot back to wide at
    # end. Otherwise, too difficult to filter b/c don't know in advance the names
    # of the files.
    Out <- Out %>% 
        pivot_longer(cols = -any_of(c("SimulatorSection", "Sheet", "Notes", 
                                      "CompoundID", "Compound", "Detail")), 
                     names_to = "File", values_to = "Value")
    
    
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
        
        if(str_detect(tolower(simulator_section), "summary")){
            Out <- Out %>% filter(Sheet == "Summary")
            
        } else if(str_detect(tolower(simulator_section), "input")){
            Out <- Out %>% filter(Sheet == "Input Sheet")
            
        } else if(str_detect(tolower(simulator_section), "absorption")){
            Out <- Out %>% filter(SimulatorSection == "Absorption")
            
        } else if(str_detect(tolower(simulator_section), "distrib")){
            Out <- Out %>% filter(SimulatorSection == "Distribution")
            
        } else if(str_detect(tolower(simulator_section), "elim")){
            Out <- Out %>% filter(SimulatorSection == "Elimination")
            
        } else if(str_detect(tolower(simulator_section), "phys chem|physchem|binding")){
            Out <- Out %>% filter(SimulatorSection == "Phys Chem and Blood Binding")
            
        } else if(str_detect(tolower(simulator_section), "population")){
            Out <- Out %>% filter(SimulatorSection == "Population")
            
        } else if(str_detect(tolower(simulator_section), "interaction")){
            Out <- Out %>% filter(SimulatorSection == "Interaction")
            
        } else if(str_detect(tolower(simulator_section), "transport")){
            Out <- Out %>% filter(SimulatorSection == "Transporters")
            
        } else if(str_detect(tolower(simulator_section), "trial design")){
            Out <- Out %>% filter(SimulatorSection == "Trial Design")
        }
        
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
    
    
    # Return -----------------------------------------------------------------
    
    # Dealing with compound column as user requested. 
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
    
    # Converting back to wide
    Out <- Out %>% pivot_wider(names_from = File, values_from = Value)
    
    return(Out)
    
}


