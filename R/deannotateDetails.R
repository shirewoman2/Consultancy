#' De-annotate a data.frame of experimental details that was previously
#' annotated
#'
#' \code{deannotateDetails} takes a data.frame that contains annotated
#' experimental details and converts it from having columns for each file and
#' rows for each experimental detail to having rows for each file and columns
#' for each detail.
#'
#' @param Deets the annotated output from \code{extractExpDetails} or
#'   \code{extractExpDetails_mult} where the argument "annotate_details" was set
#'   to TRUE or the output from \code{annotateDetails}
#' @param apply_class TRUE (default) or FALSE for whether to set the class of
#'   each column to numeric or character data based on the data classes listed
#'   in \code{AllExpDetails}. If set to FALSE, all data classes will be
#'   character.
#'
#' @return
#' @export
#'
#' @examples
#'
#' Annotated <- annotateDetails(MDZdetails)
#' Deannotated <- deannotateDetails(Annotated)
#' 

deannotateDetails <- function(Deets, 
                              apply_class = TRUE){
    
    FileOrder <- names(Deets)[str_detect(names(Deets), "xlsx")]
    
    CompoundNames <- Deets %>% 
        select(Compound, CompoundID, matches("xlsx$")) %>% 
        pivot_longer(cols = -c(Compound, CompoundID),
                     names_to = "File", values_to = "Value") %>%
        filter(complete.cases(Value) & complete.cases(Compound)) %>% 
        select(File, Compound, CompoundID) %>% unique() %>% 
        mutate(File = factor(File, levels = FileOrder))
    
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
        pivot_wider(names_from = Detail, values_from = Value) %>% 
        mutate(File = factor(File, levels = FileOrder))
    
    if(apply_class){
        suppressWarnings(suppressMessages(
            Deets <- Deets %>% 
                mutate(across(.cols = any_of(AllExpDetails %>% 
                                                 filter(Class == "numeric") %>% pull(Detail)), 
                              .fns = as.numeric), 
                       across(.cols = matches("CLint|CLadd|CLbiliary|CLrenal|CLiv|^fu|Km|^Vmax|Jmax|RAFREF|HalfLife|^Ind|^MBI"),
                              .fns = as.numeric))
        ))
    }
    
    Deets <- Deets %>% mutate(File = factor(File, levels = FileOrder)) %>% 
        arrange(File) %>% 
        mutate(File = as.character(File))
    
    return(Deets)
    
}

