#' Annotate Simcyp Simulator experimental details
#'
#' \code{annotateDetails} uses output from either
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}},
#' formats it into a long data.frame, adds columns for \enumerate{\item{which
#' compound the information pertains to (substrate, inhibitor, etc.),}
#' \item{which section of the Simcyp Simulator this detail is found in
#' (physchem, absorption, distribution, etc.),} \item{notes describing what the
#' detail is, and} \item{which sheet in the Excel file the information was
#' pulled from.}} It will also optionally filter the data to return only
#' specifically requested information. If you find yourself overwhelmed at the
#' amount of information, we recommend looking at just one compound at a time.
#' You can set the \code{compoundID} argument to just the compound ID you want
#' -- "substrate", "inhibitor 1", etc. -- or set the \code{compound} argument to
#' just the compound you want -- "midazolam" or "Client Drug X" -- and that will
#' also help make things less overwhelming and easier to find. For detailed
#' instructions and examples, please see the SharePoint file "Simcyp PBPKConsult
#' R Files - Simcyp PBPKConsult R Files/SimcypConsultancy function examples and
#' instructions/Checking simulation experimental
#' details/Checking-simulation-experimental-details.docx". (Sorry, we are unable
#' to include a link to it here.)
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
#' @param show_only_diff_from_template TRUE or FALSE (default) to show only the
#'   details that differ from the template simulation, which reduces the number
#'   of rows in your output and can make it easier to find what has changed
#'   between simulations
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
#'   "midaz" or "keto" in the name. Not case sensitive. If you request only
#'   information on a specific compound, we will assume what what you care about
#'   is the set of parameters used for that compound and not whether that
#'   compound was the substrate or the inhibitor. This will change the output
#'   somewhat because we won't include the column "CompoundID" and we will
#'   concatenate all the possible compound names in the column "Compound".
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
#'   \item{"Simcyp inputs"}{all the details that you normally fill out on the
#'   "Simcyp inputs (and QC)" tab of a compound data sheet plus trial design
#'   information}
#'
#'   \item{"methods" or "trial design"}{all the details that show up in the
#'   methods section of a report: number of trials, number of individuals
#'   simulated, population, percent female, age range, dose amount and regimen,
#'    etc.}
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
#'   "Elimination", "Transport", "Interaction", "Phys Chem and Blood Binding",
#'   "Population", or "Trial Design". Not case sensitive. If you want more than
#'   one, enclose them with \code{c(...)}
#' @param file_order optionally specify the order in which files should be shown
#'   in the annotated output, e.g., \code{file_order = c("file A.xlsx", "file
#'   B.xlsx", "file C.xlsx")}
#' @param return_list TRUE or FALSE (default) for whether to return the entire
#'   list of information from \code{existing_exp_details}. Before running this
#'   function, \code{existing_exp_details} was a list. Probably, though, all you
#'   want \emph{out} of this function most of the time is a single data.frame
#'   with the main information annotated. If you want more than that, set this
#'   to TRUE.
#' @param save_output optionally save the output by supplying a csv or Excel
#'   file name in quotes here, e.g., "Simulation details.csv" or "Simulation
#'   details.xlsx".  Do not include any slashes, dollar signs, or periods in the
#'   file name. If you leave off the file extension, it will be saved as a csv
#'   file.
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
#' annotateDetails(existing_exp_details = MDZdetails)
#'
#' # Get annotated details regarding absorption.
#' annotateDetails(existing_exp_details = MDZdetails, simulator_section = "absorption")
#'
#' # Get annotated details on whatever was used as inhibitor 1
#' annotateDetails(existing_exp_details = MDZdetails, compoundID = "inhibitor 1")
#'
#' # Get annotated details for any compounds with "midaz" or "keto" in the name.
#' annotateDetails(existing_exp_details = MDZdetails, compound = "midaz|keto")
#'
#' # Get (most of) the details to put into a table of simulation inputs:
#' annotateDetails(existing_exp_details = MDZdetails, detail_set = "Simcyp inputs")
#'
#' # Combine multiple options to get just a few specific details:
#' annotateDetails(existing_exp_details = MDZdetails,
#'             simulator_section = "absorption",
#'             compound = "midaz|keto",
#'             compoundID = "substrate")
#'
#' 
annotateDetails <- function(existing_exp_details,
                            compoundID = NA, 
                            compound = NA,
                            template_sim = NA,
                            show_only_diff_from_template = FALSE,
                            simulator_section = NA, 
                            detail_set = NA, 
                            find_matching_details = NA,
                            show_compound_col = TRUE,
                            omit_all_missing = TRUE, 
                            file_order = NA,
                            return_list = FALSE,
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
   
   if(any(compound %in% c("substrate", 
                          "primary metabolite 1",
                          "primary metabolite 2",
                          "secondary metabolite",
                          "inhibitor 1", "inhibitor 2", 
                          "inhibitor 1 metabolite"))){
      warning(paste0("You requested `", compound, 
                     "` for the compound, but we think you actually want that for the argument `compoundID`, so we're going to set `compound = NA` and compoundID = `",
                     compound, "`. Please see the help file for the distinction between the arguments `compound` (uses actual name of the compound) and `compoundID` (uses position in the simulation, e.g., `substrate`)."), 
              call. = FALSE)
      compoundID <- compound
      compound <- NA
   }
   
   if(length(compound) > 1){
      compound <- str_c(compound, collapse = "|")
   }
   
   simulator_section_orig <- simulator_section
   simulator_section <- str_c(unique(tolower(simulator_section)), collapse = " ")
   
   if("Substrate" %in% names(existing_exp_details$MainDetails) == FALSE & 
      (show_compound_col == TRUE | show_compound_col == "concatenate") & 
      "Compound" %in% names(existing_exp_details$MainDetails) == FALSE){
      warning(paste0("You set show_compound_col to ", show_compound_col,
                     ", but you appear to have already run annotateDetails on these data with show_compound_col = FALSE. This column no longer exists in your data, so we can't show it."), 
              call. = FALSE)
      show_compound_col <- FALSE
      compound <- NA
   }
   
   # Checking input for template_sim 
   if(length(template_sim) > 1){
      template_sim <- template_sim[complete.cases(template_sim)]
      warning("You can only enter one value for `template_sim` and you've entered more. We'll only use the first one as a template simulation.", 
              call. = FALSE)
   }
   
   if(complete.cases(template_sim) && str_detect(template_sim, "xlsx") == FALSE){
      template_sim <- paste0(template_sim, ".xlsx")
   }
   
   if(show_only_diff_from_template & is.na(template_sim)){
      warning("You requested that we only show you differences from the template simulation, but you haven't specified which file to use as the template. We don't know what details to show you, so we'll set `show_only_diff_from_template` to be FALSE.", 
              call. = FALSE)
      show_only_diff_from_template <- FALSE
   }
   
   # We'll check whether that file was included once we've re-formatted existing_exp_details
   
   
   # Getting things set up ---------------------------------------------------
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if("File" %in% names(existing_exp_details$MainDetails) == FALSE){
      existing_exp_details$MainDetails$File <-
         paste("unknown file", 1:nrow(existing_exp_details$MainDetails))
   }
   
   if(is.na(file_order)){
      FileOrder <- existing_exp_details$MainDetails$File
   } else {
      FileOrder <- unique(c(file_order, existing_exp_details$MainDetails$File))
   }
   
   if(complete.cases(template_sim) && 
      template_sim %in% existing_exp_details$MainDetails$File == FALSE){
      warning(paste0("You requested a template_sim of `", 
                     template_sim, 
                     "`, but that is not one of the files included in `existing_exp_details`. We won't be able to compare parameters to a template simulation in the output."), 
              call. = FALSE)
      template_sim <- NA
   }
   
   Main <- existing_exp_details$MainDetails %>% 
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
   
   # Checking whether template sim was included in File
   if(complete.cases(template_sim) &&
      template_sim %in% existing_exp_details$File == FALSE){
      warning(paste0("Your template simulation file, `", 
                     template_sim, 
                     "`, was not included in the object you supplied for `existing_exp_details`. We thus don't have a good template simulation to compare other files to, so we'll have to ignore your input for `template_sim`."), 
              call. = FALSE)
      template_sim <- NA
   }
   
   # If the model was *not* ADAM, existing_exp_details will include a bunch of
   # irrelevant details. Removing those if none of the models for that compound
   # ID were ADAM.
   ADAMcheck <- data.frame(CompoundID = AllCompounds$CompoundID, 
                           Detail = paste0("Abs_model", AllCompounds$Suffix)) %>% 
      filter(Detail %in% names(existing_exp_details$MainDetails))
   
   if(nrow(ADAMcheck) > 0){
      ADAMcheck <- ADAMcheck %>% 
         mutate(ADAM = lapply(existing_exp_details$MainDetails[ADAMcheck$Detail], 
                              FUN = function(x) any(x == "ADAM", na.rm = T)))
      
      for(i in ADAMcheck$Detail[ADAMcheck$ADAM == FALSE]){
         
         Main <- Main %>% 
            filter(Detail %in% 
                      (AllExpDetails %>%
                          filter(CompoundID ==  ADAMcheck$CompoundID[
                             ADAMcheck$Detail == i] &
                                ADAMParameter == TRUE) %>%
                          pull(Detail)) == FALSE)   
      }
   }
   
   CompoundNames <- Main %>%
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
      select(-Detail) %>% 
      mutate(CompoundNameID = paste(File, CompoundID))
   
   suppressMessages(
      Main <- Main %>% 
         arrange(File) %>% 
         mutate(CompoundNameID = paste(File, CompoundID), 
                Detail_base = sub("_sub|_inhib1met|_inhib2|_inhib|_secmet|_met1|_met2", 
                                  "", Detail)) %>% 
         filter(CompoundNameID %in% CompoundNames$CompoundNameID | is.na(CompoundID)) %>% 
         left_join(bind_rows(ExpDetailDefinitions, 
                             ExpDetailDefinitions_Discovery) %>% 
                      rename(Detail_base = Detail),
                   by = c("Detail_base"), 
                   relationship = "many-to-many") %>% 
         # Finding some artifacts from row binding output from both of
         # extractExpDetails and extractExpDetails_mult. I think this should
         # fix the issue.
         filter(!Detail %in% c("Detail", "Value")) %>% 
         group_by(across(any_of(c("File", "Detail", "CompoundID", "SimulatorSection", 
                                  "Notes", "Value")))) %>% 
         summarize(Sheet = str_comma(sort(Sheet), conjunction = "or")) %>% 
         mutate(Sheet = ifelse(str_detect(Sheet, "calculated or Summary|Summary or calculated") &
                                  Detail == "SimDuration", 
                               "Summary", Sheet),
                SimulatorSection = case_when(Sheet == "population" ~ "Population", 
                                             TRUE ~ SimulatorSection)) %>% 
         ungroup())
   
   suppressMessages(
      Main <- Main %>% 
         left_join(CompoundNames) %>% 
         select(-CompoundNameID)
   )
   
   # Metabolism and interaction parameters won't match input details, so
   # adding which sheet they came from and what simulator section they
   # were. Also adding some notes explaining what detail is.
   Main <- Main %>% unique() %>% 
      mutate(
         # Elimination details
         Sheet = ifelse(str_detect(Detail, "^fu_mic|^Transporter|^fu_inc|^Km_|^Vmax|^CL(int|add|biliary|iv|renal|po|pd)|^CL_"), 
                        "Input Sheet", Sheet), 
         SimulatorSection = ifelse(str_detect(Detail, "^fu_mic|Transporter|^fu_inc|^Km_|^Vmax|^CL(int|add|biliary|iv|renal|po|pd)|^CL_"), 
                                   "Elimination", SimulatorSection), 
         
         # Interaction details
         Sheet = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                        "Input Sheet", Sheet),
         SimulatorSection = ifelse(str_detect(Detail, "^Ki_|^kinact|^Kapp|^MBI|^Ind"), 
                                   "Interaction", SimulatorSection), 
         
         # Transport details   
         Sheet = ifelse(str_detect(Detail, "^Transport"), 
                        "Input Sheet", Sheet),
         SimulatorSection = ifelse(str_detect(Detail, "^Transport"), 
                                   "Transport", SimulatorSection), 
         
         # Adding some notes
         Notes = case_when(
            str_detect(Detail, "CLadditional_InVivo") ~ "Additional in vivo clearance",
            str_detect(Detail, "CLbiliary_InVivoCL") ~ "Additional in vivo biliary clearance",
            str_detect(Detail, "CLint_AddHLM") ~ "Addtional HLM CLint",
            str_detect(Detail, "CLint_biliary") ~ "Additional biliary CLint",
            
            str_detect(Detail, "CLint_CYP|CLint_UGT|CLint_ENZ.USER[1-9]|CLint_Intestine|CLint_Liver") ~ 
               paste(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|Intestine|Liver"),
                     "CLint"),
            
            str_detect(Detail, "CLiv_InVivo") ~ "In vivo CLiv",
            str_detect(Detail, "CLpo_InVivo") ~ "In vivo CLpo",
            
            str_detect(Detail, "CL_(Liver|Intestine).*UseMetabolite") ~
               paste0(str_extract(Detail, "Liver|Intestine"), ": Use metabolite?"),
            
            str_detect(Detail, "CL_(Liver|Intestine)_Type") ~
               paste(str_extract(Detail, "Liver|Intestine"), "clearance type"),
            
            str_detect(Detail, "CL_(Liver|Intestine).*MetabPerc") ~ 
               paste(str_extract(Detail, "Liver|Intestine"), "metabolite percentage"),
            
            str_detect(Detail, "CL_(Liver|Intestine).*(Scrapings|Elution)Correction") ~ 
               paste(str_extract(Detail, "Liver|Intestine"), 
                     tolower(str_extract(Detail, "Scrapings|Elution")),
                     "correction factor"),
            
            str_detect(Detail, "CL_(Liver|Intestine)_UseSaturableKinetics") ~ 
               paste0(str_extract(Detail, "Liver|Intestine"), ": Use saturable kinetics?"),
            
            str_detect(Detail, "CL_(Liver|Intestine)_fuinc") ~ 
               paste(str_extract(Detail, "Liver|Intestine"), "fu,inc"),
            
            str_detect(Detail, "CL_PercentAvailReabsorption") ~ 
               "Percent available for reabsorption",
            
            str_detect(Detail, "fu_mic") ~ 
               paste("fu,mic for", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|Intestine|Liver")),
            
            str_detect(Detail, "Km_") ~ 
               paste(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|Intestine|Liver"), "Km"), 
            
            str_detect(Detail, "Vmax_") ~ 
               paste(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|Intestine|Liver"), "Vmax"), 
            
            str_detect(Detail, "Ki_") ~ 
               paste(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}"), 
                     "competitive inhibition constant"), 
            
            str_detect(Detail, "MBI_fu_mic") ~ 
               paste("fu,mic for MBI of", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}")), 
            
            str_detect(Detail, "MBI_Kapp") ~ 
               paste("Kapp for MBI of", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}")), 
            
            str_detect(Detail, "MBI_kinact") ~ 
               paste("kinact for MBI of", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}")), 
            
            TRUE ~ Notes),
         
         # Setting factors for sorting
         SimulatorSection = factor(SimulatorSection, 
                                   levels = c("SimulatorVersion",
                                              "Phys Chem and Blood Binding", 
                                              "Absorption",
                                              "Distribution",
                                              "Elimination",
                                              "Transport",
                                              "Interaction",
                                              "Trial Design", 
                                              "Population")),
         CompoundID = factor(CompoundID, 
                             levels = c("substrate", 
                                        "primary metabolite 1", 
                                        "primary metabolite 2",
                                        "secondary metabolite", 
                                        "inhibitor 1", 
                                        "inhibitor 2", 
                                        "inhibitor 1 metabolite"))) %>% 
      select(any_of(c("SimulatorSection", "Sheet", "Notes", "CompoundID",
                      "Compound", "Detail")), everything()) %>% 
      arrange(SimulatorSection, Detail)
   
   # Filtering as requested -----------------------------------------
   
   # Need everything to be in long format and then will pivot back to wide at
   # end. Otherwise, too difficult to filter b/c don't know in advance the names
   # of the files.
   
   ## compoundID ---------------------------------------------------------------
   if(any(complete.cases(compoundID))){
      
      Main <- Main %>% filter(CompoundID %in% sort(unique(compoundID)) |
                                 is.na(CompoundID))
   }
   
   ## compound -------------------------------------------------------------
   if(complete.cases(compound)){
      
      if(any(str_detect(tolower(Main$Compound), tolower(compound))) == FALSE){
         warning(paste0("None of the simulations had information on the compound you requested. You requested a compound called `", 
                        compound, 
                        "``, but the compounds present in these simulations are: ", 
                        str_comma(sort(unique(Main$Compound))), ". All the information specific to those compounds will be omitted from your output."), 
                 call. = FALSE)
      }
      
      Main <- Main %>% filter(str_detect(tolower(Compound), tolower({{compound}})) |
                                 is.na(Compound)) %>% 
         mutate(Notes = str_trim(gsub("(for|of) (the )?(substrate|primary metabolite 1|primary metabolite 2|secondary metabolite|inhibitor 1|inhibitor 2|inhibitor 1 metabolite)",
                                      "", Notes)), 
                Detail = sub("_sub$|_inhib$|_met1$|_met2$|_secmet$|_inhib2$|_inhib1met$", 
                             "", Detail))
      show_compound_col <- "concatenate"
   }
   
   ## simulator_section --------------------------------------------------
   
   if(complete.cases(simulator_section)){
      
      MySections <- c()
      
      if(str_detect(simulator_section, "summary")){
         MySections <- c(MySections, "Summary", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "input")){
         MySections <- c(MySections, "Input Sheet", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "absorption")){
         MySections <- c(MySections, "Absorption", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "distrib")){
         MySections <- c(MySections, "Distribution", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "elim|metab")){
         MySections <- c(MySections, "Elimination", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "phys chem|physchem|binding")){
         MySections <- c(MySections, "Phys Chem and Blood Binding", "SimulatorVersion")
         
      }
      
      if(str_detect(simulator_section, "population")){
         MySections <- c(MySections, "Population", "SimulatorVersion")
      } 
      
      if(str_detect(simulator_section, "interaction")){
         MySections <- c(MySections, "Interaction", "SimulatorVersion")
      } 
      
      if(str_detect(simulator_section, "transport")){
         MySections <- c(MySections, "Transport", "SimulatorVersion")
      }
      
      if(str_detect(simulator_section, "trial design")){
         MySections <- c(MySections, "Trial Design", "SimulatorVersion")
      }
      
      MySections <- sort(unique(MySections))
      
      if(length(MySections) == 0){
         warning(paste0("You entered ", simulator_section_orig), 
                 " for the argument `simulator_section`, but that is not among the acceptable options, which are listed in the help file. We will not filter your results based on simulator section.", 
                 call. = FALSE)
      }
      
      Main <- Main %>% filter(SimulatorSection %in% MySections)
      
   }
   
   ## detail_set -------------------------------------------------------------
   
   if(any(complete.cases(detail_set))){
      
      DetailSet <- c()
      
      if("simcyp inputs" %in% tolower(detail_set)){
         
         DetailSet <- c(
            DetailSet, (AllExpDetails %>%
                           filter(complete.cases(CDSInputMatch)) %>%
                           pull(Detail)), 
            Main %>% 
               filter(str_detect(Detail, 
                                 "^fu_mic|^Transporter|^fu_inc|^Km_|^Vmax|^CL(int|add|biliary|iv|renal|po|pd)|^Ki_|^kinact|^Kapp|^MBI|^Ind")) %>% 
               pull(Detail),
            "SimulatorVersion", "Substrate", 
            "PrimaryMetabolite1", "PrimaryMetabolite2",
            "SecondaryMetabolite", 
            "Inhibitor1", "Inhibitor2", "Inhibitor1Metabolite")
      }
      
      if(any(str_detect(tolower(detail_set), "methods|trial design"))){
         DetailSet <- unique(c(DetailSet,  "Substrate", 
                               "PrimaryMetabolite1", "PrimaryMetabolite2",
                               "SecondaryMetabolite", 
                               "Inhibitor1", "Inhibitor2", "Inhibitor1Metabolite",
                               "Dose_sub", "Dose_inhib", "Regimen_sub", "Regimen_inhib",
                               "FixedTrialDesign", "FixedTrialDesignFile", 
                               "SimulatorVersion", 
                               "NumTrials", "NumSubjTrial", "PercFemale", 
                               "Age_min", "Age_max", "NumDoses", "Regimen", 
                               "Inhibitor1", "Inhibitor2", "SimDuration", 
                               "StartDayTime", 
                               paste0(rep(c("StartDayTime_", "StartHr_"), 
                                          each = 7),
                                      c("sub", "inhib", "inhib2", "met1", 
                                        "met2", "secmet", "inhib1met"))))
      }
      
      if(any(str_detect(tolower(detail_set), "summary"))){
         DetailSet <- c(DetailSet, 
                        Main %>% filter(Sheet %in% c("Input Sheet", 
                                                     "Input Sheet or Summary", 
                                                     "Input Sheet, Summary, or workspace XML file",
                                                     "calculated, Summary, or workspace XML file")) %>% pull(Detail))
      }
      
      if(any(str_detect(tolower(detail_set), "input sheet"))){
         DetailSet <- c(DetailSet, 
                        Main %>% filter(Sheet %in% c("Input Sheet", 
                                                     "Input Sheet or Summary", 
                                                     "Input Sheet, Summary, or workspace XML file")) %>% pull(Detail))
      }
      
      if(any(str_detect(tolower(detail_set), "population"))){
         DetailSet <- c(DetailSet, 
                        Main %>% filter(Sheet == "population") %>% pull(Detail))
      }
      
      # This is when they have requested individual details.
      DetailSet <- unique(c(DetailSet, setdiff(detail_set, DetailSet)))
      
      # Replacing any "_X" details with the compound suffixes
      Xdetails <- DetailSet[str_detect(DetailSet, "_X")]
      Xdetails <- unlist(lapply(c("_sub", "_inhib", "_inhib2", "_inhib1met", 
                                  "_met1", "_met2", "_secmet"),
                                FUN = function(x) sub("_X", x, Xdetails)))
      DetailSet <- c(DetailSet, Xdetails)
      
      Main <- Main %>% filter(Detail %in% 
                                 switch(as.character(complete.cases(compound)),
                                        "TRUE" = sub("_sub|_inhib|_inhib2|_met1|_met2|_secmet|_inhib1met", 
                                                     "", DetailSet),
                                        "FALSE" = DetailSet))
   }
   
   
   # find_matching_details --------------------------------------------------------
   
   if(complete.cases(find_matching_details)){
      Main <- Main %>% 
         filter(str_detect(Detail, find_matching_details))
   }
   
   
   # Cleaning up and removing unnecessary info ----------------------------
   # Removing unnecessary compounds.
   
   CmpdCheck <- Main %>%
      filter(complete.cases(CompoundID)) %>% 
      group_by(CompoundID, Detail) %>% 
      summarize(Keep = any(complete.cases(Value))) %>% 
      pull(CompoundID) %>% unique() %>% as.character()
   
   Main <- Main %>% 
      filter(is.na(CompoundID) | CompoundID %in% {{CmpdCheck}})
   
   # Removing compound column if they don't want it
   if(class(show_compound_col) == "logical"){
      if(show_compound_col == FALSE){
         Main <- Main %>% select(-Compound)
      }
   } else if(show_compound_col == "concatenate"){
      if(complete.cases(compound)){
         
         # Checking whether user has asked to concatenate compounds that occupy
         # multiple positions in the simulator for the same simulation, e.g.,
         # asked to concatenate info on the substrate and the inhibitor 1 b/c
         # that would result in multiple values in the same cell. 
         
         # NB: This SHOULD be existing_exp_details here and NOT main. Need it to
         # be in the original, wide format.
         CmpdCheck <- existing_exp_details$MainDetails %>% 
            select(File, any_of(c("Substrate", 
                                  "PrimaryMetabolite1", 
                                  "PrimaryMetabolite2",
                                  "SecondaryMetabolite", 
                                  "Inhibitor1", 
                                  "Inhibitor2", 
                                  "Inhibitor1Metabolite"))) %>% 
            mutate(across(.cols = -File, .fns = function(x){str_detect(tolower(x), compound)})) %>% 
            pivot_longer(cols = -File, names_to = "CompoundID", 
                         values_to = "Detected") %>% 
            filter(complete.cases(Detected)) %>% 
            group_by(File) %>% 
            summarize(N = length(which(Detected == TRUE)))
         
         if(any(CmpdCheck$N > 1)){
            
            warning(paste0("You have asked to concatenate the compound column and also requested all details that match `", 
                           compound, 
                           "`. The problem, though, is that you have requested information for compounds that occupy more than one position in the Simulator, e.g., one is the substrate and one is inhibitor 1 in the same simulation, which means that there would be more than one value for a given detail. This wouldn't be workable in the results, so we cannot concatenate the compound column in this situation."), 
                    call. = FALSE)
         } else {
            
            AllCompounds <- str_comma(sort(unique(Main$Compound)), conjunction = "or")
            Main <- Main %>% mutate(Compound = ifelse(complete.cases(Compound), 
                                                      AllCompounds, NA)) %>% 
               select(SimulatorSection, Sheet, Notes, Compound, Detail,
                      File, Value)
         }
      } else {
         AllCompounds <- Main %>% select(Compound, CompoundID) %>% 
            filter(complete.cases(CompoundID)) %>% 
            group_by(CompoundID) %>% 
            summarize(Compound = str_comma(sort(unique(Compound)), conjunction = "or"))
         
         suppressMessages(
            Main <- Main %>% select(-Compound) %>% left_join(AllCompounds) %>% 
               select(SimulatorSection, Sheet, Notes, CompoundID, Compound, Detail,
                      File, Value))
      }
   }
   
   
   # Pivoting wider again ------------------------------------------------
   
   AllFiles <- unique(Main$File)
   
   Main <- Main %>% 
      pivot_wider(names_from = File, 
                  values_from = Value)
   
   # Need to check again whether template_sim is included b/c it might not be
   # any more if the user has filtered results for a specific compound ID that
   # doesn't exist in template_sim.
   if(complete.cases(template_sim) & template_sim %in% names(Main) == FALSE){
      warning(paste0("Your template simulation file, `", 
                     template_sim, 
                     "`, was originally included in the object you supplied for `existing_exp_details`, but that particular simulation didn't have any of the combination of details or compound IDs or compounds that you requested we filter the results by. We thus don't have a good template simulation to compare other files to, so we'll have to ignore your input for `template_sim`."), 
              call. = FALSE)
      
      template_sim <- NA
   }
   
   if(length(AllFiles) > 1){
      # Checking for details that are the SAME across all files
      AllSame <- Main %>% 
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
         Main <- Main %>%
            left_join(AllSame)
      )
   }
   
   if(complete.cases(template_sim)){
      Main <- Main %>% 
         select(-UniqueVal) %>% 
         select(any_of(c("SimulatorSection", "Sheet", "Notes",
                         "CompoundID", "Compound", "Detail")), 
                template_sim,
                everything())
      
      TSim <- paste("TEMPLATE SIMULATION -", template_sim)
      names(Main)[names(Main) == template_sim] <- TSim
      
   } else if("UniqueVal" %in% names(Main)){
      Main <- Main %>% 
         select(any_of(c("SimulatorSection", "Sheet", "Notes",
                         "CompoundID", "Compound", "Detail", 
                         "UniqueVal")), 
                everything())
      
      if("Compound" %in% names(Main)){
         if(complete.cases(compound)){
            Main <- Main %>% 
               rename("All files have this value for this compound" = UniqueVal)
         } else {
            Main <- Main %>% 
               mutate(ToOmit = complete.cases(CompoundID) & 
                         is.na(Compound)) %>% 
               filter(ToOmit == FALSE) %>% select(-ToOmit) %>% 
               rename("All files have this value for this compound ID and compound" = UniqueVal)
         }
      } else {
         Main <- Main %>% 
            rename("All files have this value for this compound ID" = UniqueVal)
      }
   }  
   
   # Removing anything that was all NA's if that's what user requested
   if(omit_all_missing){
      Main$AllNA <- apply(Main[, names(Main)[str_detect(names(Main), "xlsx$")]], 
                          MARGIN = 1, FUN = function(.) all(is.na(.)))    
      
      Main <- Main %>% filter(AllNA == FALSE) %>% select(-AllNA)
   }
   
   # Sorting to help organize output
   Main <- Main %>%
      mutate(BaseDetail = sub("_sub|_inhib|_inhib2|_met1|_met2|_secmet|_inhib1met", 
                              "", Detail), 
             EnzymeForSorting = str_extract(Detail, "(CYP|UGT)[1-9][A-Z][1-9]{1,2}")) %>% 
      left_join(AllExpDetails %>% 
                   mutate(BaseDetail = sub("_sub|_inhib|_inhib2|_met1|_met2|_secmet|_inhib1met", 
                                           "", Detail)) %>% 
                   select(BaseDetail, SortOrder) %>% unique(), 
                by = "BaseDetail") %>% 
      arrange(across(any_of(c("SimulatorSection", "SortOrder", "EnzymeForSorting",
                              "Detail",  "CompoundID", "Compound")))) %>% 
      select(-BaseDetail, -SortOrder, -EnzymeForSorting) %>% 
      unique()
   
   # Checking for differences from template sim
   if(complete.cases(template_sim)){
      Diffs <- list()
      MyStyles <- list()
      NontempFiles <- setdiff(AllFiles, template_sim)
      
      for(i in 1:length(NontempFiles)){
         Diffs[[i]] <- 
            list(columns = which(names(Main) == NontempFiles[i]),
                 rows = which(Main[ , NontempFiles[i]] != Main[, TSim] |
                                 (complete.cases(Main[ , NontempFiles[i]]) &
                                     is.na(Main[, TSim])) |
                                 (is.na(Main[ , NontempFiles[i]]) & 
                                     complete.cases(Main[, TSim]))), 
                 fill = "#FFC7CE", 
                 font = list(color = "#9B030C"))
      }
   }
   
   # Only showing differences from template sim if that's what user requested
   if(show_only_diff_from_template){
      RowsWithDiffs <- lapply(Diffs, FUN = function(x) x[["rows"]])
      RowsWithDiffs <- sort(unique(unlist(RowsWithDiffs)))
      Main <- Main[RowsWithDiffs, ]
   }
   
   # Check for when there isn't any information beyond what the simulator
   # version is, which can happen if the user extracts only some information and
   # then wants information that would not have been extracted. For example,
   # extractExpDetails pulls only the "Summary" tab by default, so it won't have
   # a LOT of useful information.
   if(nrow(Main) == 1){
      warning("There is only 1 row in your output. When you ran `extractExpDetails` or `extractExpDetails_mult`, did you request all the information you wanted? For example, if you only requested information from the `Summary` tab, that won't include any elimination information.", 
              call. = FALSE)
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
      
      if(Ext == "csv"){
         write.csv(Main, FileName, row.names = F)
      } else if(Ext == "xlsx"){
         if(is.na(template_sim)){
            # This is when there is no template simulation, but we are
            # including a column noting when a given value was the same for
            # all simulations.
            formatXL(
               Main, FileName, sheet = "Simulation experimental details",
               styles = list(
                  list(columns = which(names(Main) == "Notes"), 
                       textposition = list(wrapping = TRUE)),
                  list(rows = 0, font = list(bold = TRUE),
                       textposition = list(alignment = "middle",
                                           wrapping = TRUE)), 
                  list(columns = which(str_detect(names(Main), "All files have this value")),
                       fill = "#E7F3FF"), 
                  list(rows = 0, columns = which(str_detect(names(Main), "All files have this value")), 
                       font = list(bold = TRUE), 
                       textposition = list(alignment = "middle",
                                           wrapping = TRUE), 
                       fill = "#E7F3FF")))
            
         } else {
            # This is when there IS a template simulation. Formatting to
            # highlight in red all the places where things differ.
            
            MyStyles[[1]] <- 
               # wrapping text in the notes column since it's sometimes long
               list(columns = which(names(Main) == "Notes"), 
                    textposition = list(wrapping = TRUE))
            
            MyStyles[[2]] <- 
               # making header row bold and centered
               list(rows = 0, font = list(bold = TRUE),
                    textposition = list(alignment = "middle",
                                        wrapping = TRUE))
            
            # making the template sim column blue
            MyStyles[[3]] <- 
               list(columns = which(str_detect(names(Main), template_sim)),
                    fill = "#E7F3FF")
            
            MyStyles[[4]] <- 
               list(columns = which(str_detect(names(Main), template_sim)),
                    rows = 0, font = list(bold = TRUE), 
                    textposition = list(alignment = "middle",
                                        wrapping = TRUE), 
                    fill = "#E7F3FF")
            
            MyStyles <- append(MyStyles, Diffs)
            
            formatXL(
               Main, FileName, sheet = "Simulation experimental details",
               styles = MyStyles)
            
         }
         
         # Checking for other things we should save
         if(any(complete.cases(detail_set)) == FALSE |
            (any(complete.cases(detail_set)) & detail_set == "all")){
            ToWrite <- names(existing_exp_details)[
               sapply(existing_exp_details, is.null) == FALSE]
            ToWrite <- names(existing_exp_details[ToWrite])[
               sapply(existing_exp_details[ToWrite], nrow) > 0]
            ToWrite <- setdiff(ToWrite, "MainDetails")
            
            for(i in ToWrite){
               formatXL_head(DF = existing_exp_details[[i]], 
                             file = FileName, 
                             sheet = i)
            }
         }
      }
   }
   
   if(return_list){
      Out <- existing_exp_details
      Out$MainDetails <- Main
   } else {
      Out <- Main
   }
   
   return(Out)
   
}


