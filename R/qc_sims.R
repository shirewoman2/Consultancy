#' Make an Excel file for QCing simulations
#'
#' \code{qc_sims} will create an Excel file specifically designed for general
#' QCing
#'
#' @param existing_exp_details output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}
#' @param template_sim optionally include a specific file name -- it must be one
#'   of the files included in the object you supply for
#'   \code{existing_exp_details} -- that can be used as a template simulation to
#'   compare all the other simulations to. Any details in any simulations that
#'   do NOT match the template simulation details will be highlighted in red if
#'   you save the output to an Excel file using \code{save_output}.
#'   \strong{NOTE:} If you use a template simulation, we \emph{strongly
#'   recommend} you also set \code{compoundID} to be only the compound ID you
#'   want, e.g., \code{compoundID = "substrate"} and/or set the compound name to
#'   be only the compound you want, e.g., \code{compound = "midaz"} (this will
#'   match any compound names that include the letters "midaz" and is NOT case
#'   sensitive), and set \code{show_compound_col = FALSE}. This will result in a
#'   much clearer Excel file.
#' @param compound optionally supply a specific compound name or part of a
#'   specific compound name to get all possible compounds that match that and
#'   \emph{only} compounds that match that. Regular expressions are acceptable
#'   here, e.g., \code{compound = "midaz|keto"} to find any compound with either
#'   "midaz" or "keto" in the name. Not case sensitive.
#' @param save_output optionally save the output by supplying an Excel file name
#'   in quotes here, e.g., "Simulation details.xlsx". If you leave off the file
#'   extension, it will be saved as an xlsx file. This will save the output with
#'   one tab per compound ID (one for substrate, one for inhibitor 1, etc.)
#'
#' @return Returns a list of data.frames, broken up by the compound ID, and
#'   saves each data.frame in a formatted Excel file with one tab per compound
#'   ID (one for substrate, one for inhibitor 1, etc.)
#' @export
#'
#' @examples
#' # No examples yet.
#' 
qc_sims <- function(existing_exp_details,
                    template_sim = NA,
                    compound = NA,
                    save_output = NA){
    
    # Error catching --------------------------------------------------------
    
    # Most error catching takes place within the annotateDetails function.
    
    
    # Main body of function ---------------------------------------------------
    
    if(class(existing_exp_details)[1] == "list"){
        # This is when the output is the default list from extractExpDetails
        existing_exp_details <- as.data.frame(existing_exp_details)
    }
    
    PrevAnnotated <- all(c("SimulatorSection", "Sheet") %in% names(existing_exp_details))
    
    if(PrevAnnotated){
        
        existing_exp_details <- deannotateDetails(existing_exp_details, apply_class = FALSE)
        
    } else if("File" %in% names(existing_exp_details) == FALSE){
        existing_exp_details$File <- paste("unknown file", 1:nrow(existing_exp_details))
        FileOrder <- existing_exp_details$File
        existing_exp_details <- existing_exp_details %>% 
            mutate(File = factor(File, levels = FileOrder))
    }
    
    AllFiles <- unique(existing_exp_details$File)
    TSim <- paste("TEMPLATE SIMULATION -", template_sim)
    
    AllCompoundIDs <- existing_exp_details %>% 
        select(any_of(c("Substrate", "PrimaryMetabolite1", "PrimaryMetabolite2", 
                        "SecondaryMetabolite", "Inhibitor1", "Inhibitor2", 
                        "Inhibitor1Metabolite"))) %>% 
        select_if(function(x) any(complete.cases(x)))
    GoodCompoundIDs <- c("Substrate" = "substrate", 
                         "PrimaryMetabolite1" = "primary metabolite 1",
                         "PrimaryMetabolite2" = "primary metabolite 2", 
                         "SecondaryMetabolite" = "secondary metabolite",
                         "Inhibitor1" = "inhibitor 1",
                         "Inhibitor2" = "inhibitor 2", 
                         "Inhibitor1Metabolite" = "inhibitor 1 metabolite")
    AllCompoundIDs <- GoodCompoundIDs[names(AllCompoundIDs)]
    
    Out <- list()
    
    for(i in AllCompoundIDs){
        
        suppressWarnings(
            Out[[i]] <- annotateDetails(existing_exp_details = existing_exp_details,
                                        template_sim = template_sim,
                                        compoundID = i, 
                                        compound = compound,
                                        detail_set = "Simcyp inputs", 
                                        show_compound_col = "concatenate",
                                        omit_all_missing = TRUE, 
                                        save_output = NA) %>% 
                select(Detail, matches("All files|xlsx"))
        )
    }
    
    # Saving ---------------------------------------------------------------
    
    # NOTE TO CODER: This is IDENTICAL to the "Saving" section in
    # "annotateDetails" EXCEPT that we're appending tabs and naming them
    # according to compoundID. Some arguments do not apply here, but, for
    # simplicity in coding, they'll just get ignored here. (At some point, we
    # could make this an internal, stand-alone function that each calls on. That
    # would be a better practice but I don't have time at the moment.) These are
    # the only things that differ:
    
    # NUMBER 1
    # for(j in names(Out)){  <-- This is now a loop. Note that it's indexed with
    # j and not i.
    
    
    # NUBMER 2
    # if(is.na(template_sim)){
    
    # becomes 
    # if(any(str_detect(names(Out[[j]]), "TEMPLATE")) == FALSE){ 
    
    # because there could be a template simulation generally but there might be
    # a specific compound ID that was not present in that template sim
    
    
    # NUBMER 3
    # formatXL(
    #     Out, FileName, sheet = "Simulation experimental details",
    
    # becomes
    
    # formatXL(
    #     Out, FileName, sheet = j,
    
    # NUMBER 4 All the places that originally were just "Out" now are "Out[[j]]"
    # because of the loop.
    
    
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
        
        if(Ext == "csv"){
            write.csv(Out, FileName, row.names = F)
        } else if(Ext == "xlsx"){
            for(j in names(Out)){
                
                if(any(str_detect(names(Out[[j]]), "^TEMPLATE")) == FALSE){
                    # This is when there is no template simulation, but we are
                    # including a column noting when a given value was the same for
                    # all simulations.
                    
                    formatXL(
                        Out[[j]], FileName, sheet = j,
                        styles = list(
                            list(columns = which(names(Out[[j]]) == "Notes"), 
                                 textposition = list(wrapping = TRUE)),
                            list(rows = 0, font = list(bold = TRUE),
                                 textposition = list(alignment = "middle",
                                                     wrapping = TRUE)), 
                            list(columns = which(str_detect(names(Out[[j]]), "All files have this value")),
                                 fill = "#E7F3FF"), 
                            list(rows = 0, columns = which(str_detect(names(Out[[j]]), "All files have this value")), 
                                 font = list(bold = TRUE), 
                                 textposition = list(alignment = "middle",
                                                     wrapping = TRUE), 
                                 fill = "#E7F3FF")))
                } else {
                    # This is when there IS a template simulation. Formatting to
                    # highlight in red all the places where things differ.
                    
                    # Checking whether things match
                    Diffs <- list()
                    MyStyles <- list()
                    NontempFiles <- setdiff(AllFiles, template_sim)
                    
                    for(i in 1:length(NontempFiles)){
                        Diffs[[i]] <- 
                            list(columns = which(names(Out[[j]]) == NontempFiles[i]),
                                 rows = which(Out[[j]][ , NontempFiles[i]] != Out[[j]][, TSim] |
                                                  (complete.cases(Out[[j]][ , NontempFiles[i]]) &
                                                       is.na(Out[[j]][, TSim])) |
                                                  (is.na(Out[[j]][ , NontempFiles[i]]) & 
                                                       complete.cases(Out[[j]][, TSim]))), 
                                 fill = "#FFC7CE", 
                                 font = list(color = "#9B030C"))
                    }
                    
                    MyStyles[[1]] <- 
                        # wrapping text in the notes column since it's sometimes long
                        list(columns = which(names(Out[[j]]) == "Notes"), 
                             textposition = list(wrapping = TRUE))
                    
                    MyStyles[[2]] <- 
                        # making header row bold and centered
                        list(rows = 0, font = list(bold = TRUE),
                             textposition = list(alignment = "middle",
                                                 wrapping = TRUE))
                    
                    # making the template sim column blue
                    MyStyles[[3]] <- 
                        list(columns = which(str_detect(names(Out[[j]]), template_sim)),
                             fill = "#E7F3FF")
                    
                    MyStyles[[4]] <- 
                        list(columns = which(str_detect(names(Out[[j]]), template_sim)),
                             rows = 0, font = list(bold = TRUE), 
                             textposition = list(alignment = "middle",
                                                 wrapping = TRUE), 
                             fill = "#E7F3FF")
                    
                    MyStyles <- append(MyStyles, Diffs)
                    
                    formatXL(
                        Out[[j]], FileName, sheet = j,
                        styles = MyStyles)
                    
                }
            }
        }
    }
    
    return(Out)
    
}



