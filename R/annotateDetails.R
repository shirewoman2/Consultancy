#' Annotate Simcyp Simulator experimental details and format into a long
#' data.frame instead of a wide one
#'
#' \code{annotateDetails} converts output from either
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}} into
#' a long data.frame and adds to that data.frame columns for a) which compound
#' the information pertains to (substrate, inhibitor, etc.), b) which section of
#' the Simcyp Simulator this detail is found in (physchem, absorption,
#' distribution, etc.), c) notes describing what the detail is, and d) which
#' sheet in the Excel file the information was pulled from.
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
#'
#' @return
#' @export
#'
#' @examples
#'
#' annotateDetails(Deets)
#' 
annotateDetails <- function(Deets,
                            show_compound_col = TRUE){
    
    if(class(Deets)[1] == "list"){
        # This is when the output is the default list from extractExpDetails
        Deets <- as.data.frame(Deets)
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
    
    Out <- Out %>% 
        pivot_wider(names_from = File, 
                    values_from = Value)
    
    
    return(Out)
    
}


