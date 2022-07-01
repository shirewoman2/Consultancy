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
#'
#' @return
#' @export
#'
#' @examples
#'
#' annotateDetails(Deets)
#' 
annotateDetails <- function(Deets){
    
    if(class(Deets)[1] == "list"){
        Deets <- as.data.frame(t(as.data.frame(Deets))) %>% 
            mutate(Detail = row.names(.)) %>% 
            rename(Value = V1) %>% 
            select(Detail, Value)
    }
    
    if("File" %in% names(Deets)){
        # This is when Deets came from extractExpDetails_mult; this step isn't
        # necessary when Deets came from extractExpDetails.
        OutDF <- Deets %>% 
            mutate(across(.cols = everything(), .fns = as.character)) %>% 
            pivot_longer(cols = -File,
                         names_to = "Detail", 
                         values_to = "Value")
    } else {
        OutDF <- Deets
    }
    
    suppressMessages(
        OutDF <- OutDF %>% 
            left_join(ExpDetailDefinitions, by = "Detail") %>% 
            # Finding some artifacts from row binding output from both of
            # extractExpDetails and extractExpDetails_mult. I think this should
            # fix the issue.
            filter(!Detail %in% c("Detail", "Value")) %>% 
            group_by(across(any_of(c("File", "Detail", "CompoundID", "SimulatorSection", 
                                     "Notes", "Value")))) %>% 
            summarize(Sheet = str_comma(Sheet, conjunction = "or")) %>% 
            mutate(Sheet = ifelse(str_detect(Sheet, "calculated or Summary|Summary or calculated") &
                                      Detail == "SimDuration", 
                                  "Summary", Sheet))
    )
    
    if("File" %in% names(OutDF)){
        OutDF <- OutDF %>% 
            pivot_wider(names_from = File, 
                        values_from = Value)
    }
    
    # Metabolism and interaction parameters won't match input details, so
    # adding which sheet they came from and what simulator section they
    # were.
    OutDF <- OutDF %>% 
        mutate(Sheet = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint"), 
                              "Input Sheet", Sheet), 
               SimulatorSection = ifelse(str_detect(Detail, "^fu_mic|^fu_inc|^Km_|^Vmax|^CLint"), 
                                         "Elimination", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                                         "Interaction", SimulatorSection), 
               Sheet = ifelse(str_detect(Detail, "^Transport"), 
                              "Input Sheet", Sheet),
               SimulatorSection = ifelse(str_detect(Detail, "^Transport"), 
                                         "Transporters", SimulatorSection)) %>% 
        select(SimulatorSection, Sheet, Notes, CompoundID, Detail, everything()) %>% 
        arrange(SimulatorSection, Detail)
    
}


