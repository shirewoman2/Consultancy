#' Make an Excel file for QCing simulations
#'
#' \code{qc_sims} will create an Excel file specifically designed for general
#' QCing. It is, by design, more succinct than \code{\link{annotateDetails}} and
#' will only return information that would be included in the "Simcyp Inputs"
#' table.
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
#' @param save_output optionally save the output by supplying an Excel file name
#'   in quotes here, e.g., "Simulation details.xlsx".  Do not include any slashes, dollar signs, or periods in the file name. If you leave off the file
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
                    compoundID = NA,
                    compound = NA,
                    save_output = NA){
   
   # Error catching --------------------------------------------------------
   
   # Most error catching takes place within the annotateDetails function.
   
   
   # Main body of function ---------------------------------------------------
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   AllFiles <- unique(existing_exp_details$MainDetails$File)
   TSim <- paste("TEMPLATE SIMULATION -", template_sim)
   
   AllCompoundIDs <- existing_exp_details$MainDetails %>% 
      select(any_of(c("Substrate", "PrimaryMetabolite1", "PrimaryMetabolite2", 
                      "SecondaryMetabolite", "Inhibitor1", "Inhibitor2", 
                      "Inhibitor1Metabolite"))) %>% 
      select(where(function(x) any(complete.cases(x))))
   GoodCompoundIDs <- c("Substrate" = "substrate", 
                        "PrimaryMetabolite1" = "primary metabolite 1",
                        "PrimaryMetabolite2" = "primary metabolite 2", 
                        "SecondaryMetabolite" = "secondary metabolite",
                        "Inhibitor1" = "inhibitor 1",
                        "Inhibitor2" = "inhibitor 2", 
                        "Inhibitor1Metabolite" = "inhibitor 1 metabolite")
   AllCompoundIDs <- GoodCompoundIDs[names(AllCompoundIDs)]
   if(any(complete.cases(compoundID))){
      AllCompoundIDs <- intersect(AllCompoundIDs, compoundID)
   }
   
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
                                     save_output = NA)
      )
      Out[[i]] <- Out[[i]] %>% select(Detail, matches("All files|xlsx"))
   }
   
   # Changing the order to be just alphabetical by detail name since that's all
   # we have in the output and not the simulator section, etc.
   for(i in names(Out)){
      Out[[i]] <- Out[[i]] %>% 
         mutate(Detail = fct_relevel(as.factor(Detail), sort))
      
      MyDeets <- levels(Out[[i]]$Detail)
      Out[[i]] <- Out[[i]] %>% 
         mutate(Detail = factor(Detail, 
                                levels = union(c("SimulatorVersion",
                                                 "Substrate", 
                                                 "PrimaryMetabolite1",
                                                 "PrimaryMetabolite2",
                                                 "SecondaryMetabolite", 
                                                 "Inhibitor1",
                                                 "Inhibitor2",
                                                 "Inhibitor1Metabolite"), 
                                               MyDeets))) %>% 
         arrange(Detail)
      
      rm(MyDeets)
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
   # sheet = output_tab_name
   
   # becomes
   
   # sheet = j
   
   
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
         
         # Using openxlsx to format things. NB: Versions <= 2.12.17 used the
         # xlsx package to save, but openxlsx allows you to insert graphs and
         # also seems to have a much more intuitive interface for saving
         # formatting.
         WB <- openxlsx::createWorkbook()
         
         BlueColumn <- openxlsx::createStyle(wrapText = TRUE, 
                                             fgFill = "#E7F3FF")
         
         BlueColumnHeader <- openxlsx::createStyle(textDecoration = "bold",
                                                   wrapText = TRUE, 
                                                   halign = "center", 
                                                   valign = "center", 
                                                   fgFill = "#E7F3FF")
         
         HeaderStyle <- openxlsx::createStyle(textDecoration = "bold",
                                              wrapText = TRUE, 
                                              halign = "center", 
                                              valign = "center")
         
         ProbCells <- openxlsx::createStyle(wrapText = TRUE, 
                                            fgFill = "#FFC7CE", 
                                            fontColour = "#9B030C")
         
         for(j in names(Out)){
            
            openxlsx::addWorksheet(wb = WB, 
                                   sheetName = j)
            
            openxlsx::writeData(wb = WB, 
                                sheet = j,
                                x = as.data.frame(Out[[j]]), 
                                headerStyle = HeaderStyle)
            
            openxlsx::setColWidths(wb = WB, 
                                   sheet = j, 
                                   cols = 1:ncol(Out[[j]]), 
                                   widths = c(30, rep(15, ncol(Out[[j]]) - 1)))
            
            # Freezing view 
            UnfrozCol1 <- which(
               str_detect(names(Out[[j]]), "TEMPLATE|^All files")) + 1
            UnfrozCol1 <- UnfrozCol1[1]
            UnfrozCol1 <- ifelse(is.na(UnfrozCol1), 2, UnfrozCol1)
            
            openxlsx::freezePane(wb = WB,
                                 sheet = j,
                                 firstActiveRow = 2,
                                 firstActiveCol =  UnfrozCol1)
            
            
            if(any(str_detect(names(Out[[j]]), "^TEMPLATE")) == FALSE |
               length(AllFiles) == 1){
               # This is when there is no template simulation, but we are
               # including a column noting when a given value was the same for
               # all simulations.
               
               openxlsx::addStyle(wb = WB, 
                                  sheet = j, 
                                  style = BlueColumn, 
                                  rows = 2:(nrow(Out[[j]]) + 1), 
                                  cols = which(str_detect(names(Out[[j]]),
                                                          "All files have this")))
               
               openxlsx::addStyle(wb = WB, 
                                  sheet = j, 
                                  style = BlueColumnHeader, 
                                  rows = 1, 
                                  cols = which(str_detect(names(Out[[j]]),
                                                          "All files have this")))
               
            } else {
               # This is when there IS a template simulation. Formatting to
               # highlight in red all the places where things differ.
               
               # making the template sim column blue
               openxlsx::addStyle(wb = WB, 
                                  sheet = j, 
                                  style = BlueColumn, 
                                  rows = 2:(nrow(Out[[j]]) + 1), 
                                  cols = which(str_detect(names(Out[[j]]),
                                                          template_sim)))
               
               openxlsx::addStyle(wb = WB, 
                                  sheet = j, 
                                  style = BlueColumnHeader, 
                                  rows = 1, 
                                  cols = which(str_detect(names(Out[[j]]),
                                                          template_sim)))
               
               # Checking whether things match
               Diffs <- list()
               MyStyles <- list()
               NontempFiles <- setdiff(AllFiles, template_sim)
               
               for(i in 1:length(NontempFiles)){
                  Diffs[[i]] <- 
                     list(columns = which(names(Out[[j]]) == NontempFiles[i]),
                          rows = which(
                             Out[[j]][ , NontempFiles[i]] != Out[[j]][, TSim] |
                                (complete.cases(Out[[j]][ , NontempFiles[i]]) & is.na(Out[[j]][, TSim])) |
                                (is.na(Out[[j]][ , NontempFiles[i]]) & complete.cases(Out[[j]][, TSim]))))
                  
                  if(length(Diffs[[i]]$rows) > 0){
                     openxlsx::addStyle(wb = WB, 
                                        sheet = j, 
                                        style = ProbCells, 
                                        rows = Diffs[[i]]$rows + 1, 
                                        cols = Diffs[[i]]$columns)
                  }
               }
            }
         }
         
         openxlsx::saveWorkbook(wb = WB, 
                                file = FileName, 
                                overwrite = TRUE)
         
      }
   }
   
   return(Out)
   
}



