#' Annotate Simcyp Simulator experimental details
#'
#' \code{annotateDetails} uses output from either
#' \code{\link{extractExpDetails}} or \code{\link{extractExpDetails_mult}},
#' formats it, adds columns for \enumerate{\item{which
#' compound the information pertains to (substrate, inhibitor, etc.),}
#' \item{which section of the Simcyp Simulator this detail is found in
#' (physchem, absorption, distribution, etc.),} \item{notes describing what the
#' detail is, and} \item{which sheet in the Simulator output Excel file or in
#' the Simulator workspace the information was
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
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input:
#'
#'   \describe{\item{NA (default)}{get all the simulations included in
#'   \code{existing_exp_details}}
#'
#'   \item{a character vector of the file names you want}{The items in the character
#'   vector must \emph{exactly} match file names in the column "File" of the
#'   "MainDetails" item in \code{existing_exp_details}, including the ".xlsx" or ".db"
#'   file extension}
#'
#'   \item{a regular expression}{This will include in the output only files
#'   that match the regular expression. This must have length = 1, and it IS
#'   case sensitive. For example, say you only want to look at development or
#'   verification simulations and you included "dev" or "ver" in those file
#'   names, respectively. Here is how you could specify that (the vertical pipe |
#'   means "or" for regular expressions): \code{sim_to_include = "dev|ver"}}}
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
#'   in the example we just gave, you would see "DrugX, Drug X, or Drug X -
#'   reduced Ki" listed as the compound.
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
#'   concatenate all the possible compound names in the column "Compound". Try
#'   this out and you'll see what we mean.
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
#' @param detail_set optionally supply a set of details and this will return
#'   \emph{only} those details. Options: \describe{
#'
#'   \item{"all"}{all possible details (default)}
#'
#'   \item{"Summary tab"}{details available from the "Summary tab"}
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
#'  \item{"lactation"}{details from the "Input Sheet" tab that pertain to
#'  lactation such as the milk-to-plasma ratio and whether a breast
#'  perfusion-limited model was used.}
#'
#'  \item{"CustomDosing" ("custom dosing" is also ok)}{custom-dosing
#'  information. This will be on a tab named "CustomDosing" unless you specify
#'  a tab name you want.}
#'
#'  \item{"DissolutionProfiles" ("dissolution profiles" is also ok)}{drug-dissolution
#'  profiles. This will be on a tab named "DissolutionProfiles" unless you specify
#'  a tab name you want.}
#'
#'  \item{"ReleaseProfiles" ("release profiles" is also ok)}{drug-release
#'  profiles This will be on a tab named "ReleaseProfiles" unless you specify
#'  a tab name you want.}
#'
#'  \item{"ConcDependent_fup"}{concentration-dependent fu,p. This will be on a
#'  tab named "ConcDependent_fup" unless you specify a tab name you want.}
#'
#'  \item{"ConcDependent_BP"}{concentration-dependent B/P. This will be on a
#'  tab named "ConcDependent_BP" unless you specify a tab name you want.}
#'
#'  \item{"pH_dependent_solubility"}{pH-dependent drug solubility. This will be on a
#'  tab named "pH_dependent_solubility" unless you specify a tab name you want.}
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
#'
#' @param simulator_section optionally supply a specific simulator section or
#'   sections from which to find simulation experimental details and then return
#'   \emph{only} those details. Options are "Absorption", "Distribution",
#'   "Elimination", "Transport", "Interaction", "Phys Chem and Blood Binding",
#'   "Population", or "Trial Design". Not case sensitive. If you want more than
#'   one, enclose them with \code{c(...)}
#' @param filename_text SOON TO BE DEPRECATED optionally specify a string of
#'   text to use for looking at only a subset of files.
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
#' @param output_tab_name the tab name to use when saving the output as an Excel
#'   file. If you specify a tab that does not already exist in the Excel file
#'   you specified with \code{save_output}, this will add a new tab and not
#'   overwrite the existing ones. This means that you can, for example, run
#'   annotateDetails one time where you look at the detail set "Simcyp inputs"
#'   and save that tab eponymously and then you can run annotateDetails again,
#'   this time using the detail set "Trial design" and naming this second tab
#'   accordingly, and the result will be a single Excel file named according to
#'   what you specified with \code{save_output} with one tab for Simcyp inputs
#'   and one tab for the trial design parameters.
#'
#' @return Returns a data.frame of simulation experimental details including the
#'   following columns: \describe{
#'
#'   \item{SimulatorSection}{the simulator section this detail is from, e.g.,
#'   "absorption" or "elimination"}
#'
#'   \item{DataSource}{the source of the data; this will be a sheet name in an
#'   Excel output file or a workspace or database file}
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
                            sims_to_include = NA, 
                            show_only_diff_from_template = FALSE,
                            simulator_section = NA, 
                            detail_set = "all", 
                            find_matching_details = NA,
                            show_compound_col = TRUE,
                            omit_all_missing = TRUE, 
                            file_order = NA,
                            return_list = FALSE,
                            save_output = NA, 
                            output_tab_name = "Simulation experimental details", 
                            filename_text = NA){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   compoundID <- tolower(compoundID)
   
   if(all(complete.cases(compoundID)) &&
      any(compoundID %in% c("substrate", "primary metabolite 1",
                            "primary metabolite 2", "secondary metabolite",
                            "inhibitor 1", "inhibitor 2", 
                            "inhibitor 1 metabolite") == FALSE)){
      
      warning(wrapn(paste0("You requested the following compoundIDs that are not among the permissible options: ",
                           str_comma(setdiff(compoundID, c("substrate", 
                                                           "primary metabolite 1",
                                                           "primary metabolite 2",
                                                           "secondary metabolite",
                                                           "inhibitor 1", "inhibitor 2", 
                                                           "inhibitor 1 metabolite"))),
                           ". These will not be included in the output. Please check the help file for acceptable options for compoundID.")),
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
      warning(wrapn(paste0("You requested `", compound, 
                           "` for the compound, but we think you actually want that for the argument `compoundID`, so we're going to set `compound = NA` and compoundID = `",
                           compound, "`. Please see the help file for the distinction between the arguments `compound` (uses actual name of the compound) and `compoundID` (uses position in the simulation, e.g., `substrate`).")), 
              call. = FALSE)
      compoundID <- compound
      compound <- NA
   }
   
   if(length(compound) > 1){
      compound <- str_c(compound, collapse = "|")
   }
   
   simulator_section_orig <- simulator_section
   simulator_section <- str_c(unique(tolower(simulator_section)), collapse = " ")
   
   # Need to harmonize input to check some of these other bits
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   # Had previously used filename_text to specify which sims to include with
   # regex and then additionally had the option of sims_to_include as a
   # character vector, but just using sims_to_include going forward. I think
   # that should be simpler for the user.
   if(any(complete.cases(filename_text))){
      if(any(complete.cases(sims_to_include))){
         warning(wrapn("You have specified values for both `filename_text`, an argument we plan to deprecate, and `sims_to_include`. We'll only look at the information in `sims_to_include` to determine which simulations you want."), 
                 call. = FALSE)
         filename_text <- NA
      } else {
         warning(wrapn("You have specified which simulations to include with the argument `filename_text`, which we plan to deprecate in the near future and replace with the (hopefully clearer and more versatile) argument `sims_to_include`. For now, we will assign your input for `filename_text` to the argument `sims_to_include`."), 
                 call. = FALSE)
         sims_to_include <- filename_text
      }
   }
   
   # If user has supplied regex, that should have length 1. If they supplied a
   # character vector of files, that should probably have length > 1. Even if
   # they only supplied a single file name here, it should still work to use
   # regex instead of a perfect match.
   if(any(complete.cases(sims_to_include)) && 
      length(sims_to_include) == 1){
      sims_to_include <- existing_exp_details$MainDetails$File[
         str_detect(existing_exp_details$MainDetails$File, 
                    sims_to_include)]
      # At this point, sims_to_include should be a character vector of file
      # names.
   }
   
   # Keeping only the requested sims for sims_to_include
   if(any(complete.cases(sims_to_include))){
      existing_exp_details <- filter_sims(which_object = existing_exp_details, 
                                          which_sims = sims_to_include,
                                          include_or_omit = "include")
   }
   
   # Checking for file name issues
   CheckFileNames <- check_file_name(existing_exp_details$MainDetails$File)
   BadFileNames <- CheckFileNames[!CheckFileNames == "File name meets naming standards."]
   if(length(BadFileNames)> 0){
      BadFileNames <- paste0(names(BadFileNames), ": ", BadFileNames)
      warning(paste0("The following file names do not meet file-naming standards for the Simcyp Consultancy Team:\n", 
                     str_c(paste0("     ", BadFileNames), collapse = "\n"), "\n"),
              call. = FALSE)
   }
   
   if("Substrate" %in% names(existing_exp_details$MainDetails) == FALSE & 
      (show_compound_col == TRUE | show_compound_col == "concatenate") & 
      "Compound" %in% names(existing_exp_details$MainDetails) == FALSE){
      warning(wrapn(paste0("You set show_compound_col to ", show_compound_col,
                           ", but you appear to have already run annotateDetails on these data with show_compound_col = FALSE. This column no longer exists in your data, so we can't show it.")), 
              call. = FALSE)
      show_compound_col <- FALSE
      compound <- NA
   }
   
   # Checking input for template_sim 
   if(length(template_sim) > 1){
      template_sim <- template_sim[complete.cases(template_sim)]
      warning(wrapn("You can only enter one value for `template_sim` and you've entered more than that. We'll only use the first one as a template simulation."), 
              call. = FALSE)
   }
   
   if(complete.cases(template_sim) && str_detect(template_sim, "xlsx") == FALSE){
      template_sim <- paste0(template_sim, ".xlsx")
   }
   
   if(show_only_diff_from_template & is.na(template_sim)){
      warning(wrapn("You requested that we only show you differences from the template simulation, but you haven't specified which file to use as the template. We don't know what details to show you, so we'll set `show_only_diff_from_template` to be FALSE."), 
              call. = FALSE)
      show_only_diff_from_template <- FALSE
   }
   
   
   # Getting things set up ---------------------------------------------------
   
   if("File" %in% names(existing_exp_details$MainDetails) == FALSE){
      existing_exp_details$MainDetails$File <-
         paste("unknown file", 1:nrow(existing_exp_details$MainDetails))
   }
   
   if(any(is.na(file_order))){
      FileOrder <- existing_exp_details$MainDetails$File
   } else {
      FileOrder <- unique(c(intersect(file_order,
                                      existing_exp_details$MainDetails$File), 
                            existing_exp_details$MainDetails$File))
   }
   
   if(complete.cases(template_sim) && 
      template_sim %in% existing_exp_details$MainDetails$File == FALSE){
      warning(wrapn(paste0("You requested a template_sim of `", 
                           template_sim, 
                           "`, but that is not one of the files included in `existing_exp_details`. We won't be able to compare parameters to a template simulation in the output.")), 
              call. = FALSE)
      template_sim <- NA
   }
   
   # Setting up ways to detect elimination and interaction parameters since we
   # won't know what they're called in advance.
   EliminationRegex <- "^fu_mic|Transporter|^fu_inc|^Km_|^Vmax|^CL(int|add|biliary|iv|renal|po|pd)|^CL_|^HalfLife"
   InteractionRegex <- "^Ki_|^kinact|^Kapp|^MBI|^Ind"
   
   MainDetails <- existing_exp_details$MainDetails %>% 
      mutate(across(.cols = everything(), .fns = as.character)) 
   
   if("Treatment" %in% names(MainDetails) && 
      any(complete.cases(MainDetails$Treatment))){
      # For VBE sims, adding Treatment to File so that pivoting will work right
      # since there will be multiples of every detail in a single simulation
      # based on what treatment it was
      MainDetails <- MainDetails %>% 
         # NB: Code lower down requires order to be File and then Treatment.
         # Plus, I think that looks nicer in the output; it's clearer to group
         # by File and then Treatment, so I think you naturally expect that to
         # be the order.
         mutate(File = paste(File, Treatment)) %>% 
         select(-Treatment)
      
      # Need to revise FileOrder now that we've changed the names 
      if(any(is.na(file_order))){
         FileOrder <- paste(existing_exp_details$MainDetails$File, 
                            existing_exp_details$MainDetails$Treatment) %>% 
            unique()
      } else {
         FileOrder <- existing_exp_details$MainDetails %>% 
            mutate(File = factor(
               File, 
               levels = unique(c(intersect(file_order,
                                           existing_exp_details$MainDetails$File), 
                                 existing_exp_details$MainDetails$File)))) %>% 
            arrange(File) %>% 
            mutate(File = paste(File, Treatment)) %>% 
            pull(File)
      }
      
      # also need to change File in existing_exp_details$MainDetails and in
      # template_sim to note the change so that downstream code will work
      existing_exp_details$MainDetails <- existing_exp_details$MainDetails %>% 
         mutate(FileAlone = File, 
                File = paste(File, Treatment))
      
      template_sim <- case_when(
         is.na(template_sim) ~ as.character(NA), 
         
         complete.cases(template_sim) ~ 
            intersect(unique(existing_exp_details$MainDetails$File),
                      paste(template_sim, "Treatment 1"))[1]
      )
      
      # Noting that there were VBE sims present
      VBEsims <- TRUE
   } else {
      VBEsims <- FALSE
   }
   
   MainDetails <- MainDetails %>% 
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
   
   # If the model was *not* ADAM, existing_exp_details will include a bunch of
   # irrelevant details. Removing those if none of the models for that compound
   # ID were ADAM.
   ADAMcheck <- data.frame(CompoundID = AllRegCompounds$CompoundID, 
                           Detail = paste0("Abs_model", AllRegCompounds$Suffix)) %>% 
      filter(Detail %in% names(existing_exp_details$MainDetails))
   
   if(nrow(ADAMcheck) > 0){
      ADAMcheck <- ADAMcheck %>% 
         mutate(ADAM = lapply(existing_exp_details$MainDetails[ADAMcheck$Detail], 
                              FUN = function(x) any(x == "ADAM", na.rm = T)))
      
      for(i in ADAMcheck$Detail[ADAMcheck$ADAM == FALSE]){
         
         MainDetails <- MainDetails %>% 
            filter(Detail %in% 
                      (AllExpDetails %>%
                          filter(CompoundID ==  ADAMcheck$CompoundID[
                             ADAMcheck$Detail == i] &
                                ADAMParameter == TRUE) %>%
                          pull(Detail)) == FALSE)   
      }
   }
   
   # Figuring out what compound names are for each file. Will use the column
   # CompoundNameID to match things up later inside annotate_subfun.
   CompoundNames <- MainDetails %>%
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
      MainDetails <- MainDetails %>% 
         arrange(File) %>% 
         mutate(CompoundNameID = paste(File, CompoundID), 
                Detail_base = sub("_sub|_inhib1met|_inhib2|_inhib|_secmet|_met1|_met2", 
                                  "", Detail)) %>% 
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
         summarize(DataSource = str_comma(sort(unique(DataSource)), conjunction = "or")) %>% 
         mutate(DataSource = ifelse(str_detect(DataSource, "calculated or Summary|Summary or calculated") &
                                       Detail == "SimDuration", 
                                    "Summary", DataSource),
                SimulatorSection = case_when(DataSource == "population" ~ "Population", 
                                             TRUE ~ SimulatorSection)) %>% 
         ungroup())
   
   suppressMessages(
      MainDetails <- MainDetails %>% 
         left_join(CompoundNames)
   )
   
   # Metabolism and interaction parameters won't match input details, so
   # adding which sheet they came from and what simulator section they
   # were. Also adding some notes explaining what detail is.
   MainDetails <- MainDetails %>% unique() %>% 
      mutate(
         # Elimination details
         DataSource = ifelse(str_detect(Detail, EliminationRegex), 
                             "Input Sheet", DataSource), 
         SimulatorSection = ifelse(str_detect(Detail, EliminationRegex), 
                                   "Elimination", SimulatorSection), 
         
         # Interaction details
         DataSource = ifelse(str_detect(Detail, InteractionRegex), 
                             "Input Sheet", DataSource),
         SimulatorSection = ifelse(str_detect(Detail, InteractionRegex), 
                                   "Interaction", SimulatorSection), 
         
         # Transport details   
         DataSource = ifelse(str_detect(Detail, "^Transport"), 
                             "Input Sheet", DataSource),
         SimulatorSection = ifelse(str_detect(Detail, "^Transport"), 
                                   "Transport", SimulatorSection), 
         
         Enzyme = str_extract(Detail, 
                              "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
         
         DetailType = case_when(str_detect(Detail, "^CL|Vmax|Km|Jmax|HalfLife") ~ "CL", 
                                str_detect(Detail, "MBI") ~ "MBI", 
                                str_detect(Detail, "Ind") ~ "Induction", 
                                str_detect(Detail, "Ki") ~ "Inhibition", 
                                .default = "other"), 
         
         # Adding some notes
         Notes = case_when(
            # CL
            str_detect(Detail, "CLadditional_InVivo") ~ "Additional in vivo clearance (L/h)",
            str_detect(Detail, "CLbiliary_InVivoCL") ~ "Additional in vivo biliary clearance (L/h)",
            str_detect(Detail, "CLint_AddHLM") ~ "Additional HLM CLint (uL/min/mg protein)",
            str_detect(Detail, "CLint_biliary") ~ "Additional biliary CLint (uL/min/10^6)",
            
            str_detect(Detail, "CLint_CYP|CLint_UGT|CLint_ENZ.USER[1-9]|CLint_Intestine|CLint_Liver") ~ 
               paste(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|Intestine|Liver"),
                     "CLint", 
                     case_when(str_detect(Detail, "Intestine|Liver") ~ "(uL/min/mg protein)", 
                               .default = "(uL/min/pmol)")),
            
            str_detect(Detail, "^Transporter") ~ 
               paste0(str_extract(Detail, "ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"),
                      " ", 
                      case_when(str_detect(Detail, "Apical") ~ "apical ", 
                                str_detect(Detail, "Basolateral") ~ "basolateral ", 
                                str_detect(Detail, "Canalicular") ~ "canalicular ", 
                                str_detect(Detail, "Sinusoidal") ~ "sinusoidal ", 
                                .default = ""), 
                      
                      case_when(str_detect(Detail, "CLintT") ~ "CLintT ", 
                                str_detect(Detail, "RAFREF") ~ "RAF or REF ", 
                                str_detect(Detail, "fuinc") ~ "fu,inc ", 
                                str_detect(Detail, "Jmax") ~ "Jmax ", 
                                str_detect(Detail, "Km") ~ "Km ", 
                                .default = ""), 
                      
                      "in ", tolower(str_extract(Detail, "Liver|Gut|Kidney")), 
                      case_when(str_detect(Detail, "CLintT") ~ " (uL/min)", 
                                str_detect(Detail, "Jmax") ~ " (pmol/min)", 
                                str_detect(Detail, "Km") ~ " (uM)", 
                                .default = "")), 
            
            str_detect(Detail, "CLiv_InVivo") ~ "In vivo CLiv (L/h)",
            str_detect(Detail, "CLpo_InVivo") ~ "In vivo CLpo (L/h)",
            
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
            
            # fu_mic or inc
            str_detect(Detail, "fu_mic|fu_inc") ~ 
               paste0(case_when(str_detect(Detail, "fu_mic") ~ "fu,mic for ", 
                                str_detect(Detail, "fu_inc") ~ "fu,inc for "), 
                      
                      case_when(str_detect(Detail, "^Ind") ~ "induction of ", 
                                str_detect(Detail, "^MBI") ~ "mechanism-based inactivation of ", 
                                str_detect(Detail, "^Ki") ~ "competitive inhibition of ",
                                .default = ""), 
                      
                      str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP")),
            
            # enzyme kinetics 
            str_detect(Detail, "^Km_") ~ 
               paste0(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
                      " Km", 
                      case_when(str_detect(Detail, "CYP|UGT|ENZ.USER") ~ " (uM)", 
                                .default = "")), 
            
            str_detect(Detail, "^Vmax_") ~ 
               paste0(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
                      " Vmax", 
                      case_when(str_detect(Detail, "CYP|UGT|ENZ.USER") ~ " (pmol/min/pmol)"), 
                      .default = ""), 
            
            # inhibition
            str_detect(Detail, "^Ki_") ~ 
               paste0(str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
                      " competitive inhibition constant", 
                      case_when(str_detect(Detail, "Gut|Kidney|Liver") ~
                                   paste0(" in ", 
                                          tolower(str_extract(Detail, "Gut|Kidney|Liver"))), 
                                .default = "")), 
            
            str_detect(Detail, "MBI_Kapp") ~ 
               paste("Kapp for mechanism-based inactivation of", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP")), 
            
            str_detect(Detail, "MBI_kinact") ~ 
               paste("kinact for mechanism-based inactivation of", 
                     str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP")), 
            
            # Induction
            str_detect(Detail, "^Ind") ~ 
               paste0(case_when(str_detect(Detail, "IndMax|IndC50") ~ str_extract(Detail, "IndMax|IndC50"), 
                                str_detect(Detail, "Ind_gamma") ~ "induction Hill coefficient (gamma)", 
                                str_detect(Detail, "slope") ~ "induction slope"), 
                      " for ", 
                      str_extract(Detail, "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP")),
            
            TRUE ~ Notes),
         
         # Doing a little more cleanup of Notes
         Notes = gsub("MATE2_K", "MATE2-K", Notes), 
         
         # Setting factors for sorting
         SimulatorSection = factor(SimulatorSection, 
                                   levels = c("SimulatorVersion",
                                              "Phys Chem and Blood Binding", 
                                              "Absorption",
                                              "Distribution",
                                              "Elimination",
                                              "Transport",
                                              "Interaction",
                                              "Other", # tumor permeability and breast/milk parameters
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
      arrange(SimulatorSection, DetailType, Enzyme, Detail) %>% 
      select(any_of(c("SimulatorSection", "DataSource", "Notes", "CompoundID",
                      "Compound", "Detail")), everything()) %>% 
      select(-Enzyme, -DetailType)
   
   # Checking for duplicates from, e.g., there being more than one pathway
   PathwayCheck <- MainDetails %>% 
      filter(complete.cases(Value) & complete.cases(Notes)) %>%
      group_by(Notes) %>% 
      summarize(N = length(sort(unique(Detail))), 
                DetailConcat = str_c(unique(Detail), collapse = " | ")) %>% 
      ungroup() %>% 
      filter(N > 1) %>%
      select(-N) %>% 
      left_join(MainDetails %>% select(Notes, Detail), by = "Notes") %>% 
      unique() %>% 
      mutate(Enzyme = str_extract(Detail, 
                                  "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
             Pathway = str_extract(Detail, pattern = paste0(Enzyme, ".*_(sub|inhib|inhib2|met1|met2|secmet|inhib1met)")), 
             Pathway = str_remove(Pathway, paste0(Enzyme, "_")), 
             Pathway = str_remove(Pathway, "_(sub|inhib|inhib2|met1|met2|secmet|inhib1met)"), 
             Pathway = sub("OH", "-OH", Pathway), 
             Pathway = sub("^-", "", Pathway), 
             Pathway = str_replace(Notes, Enzyme, paste0(Enzyme, " (", Pathway, " pathway)"))) %>% 
      # Removing some things that wind up being duplicates here b/c the
      # Simulator codes them oddly
      filter(!str_detect(Detail, "NumDoses|StartDayTimeH"))
   
   if(nrow(PathwayCheck) > 0){
      MainDetails <- MainDetails %>% 
         left_join(PathwayCheck %>% select(Detail, Pathway), by = "Detail") %>% 
         mutate(Notes = case_when(complete.cases(Pathway) ~ Pathway,
                                  .default = Notes))
   }
   
   MainDetails <- MainDetails %>% select(-any_of("Pathway"))
   
   # subfunction starts here ------------------------------------------------
   
   annotate_subfun <- function(item){
      
      item_char <- switch(item, 
                          "MainDetails" = "main set of simulation details", 
                          "Dosing" = "dosing information", 
                          "ConcDependent_BP" = "concentration-dependent B/P information", 
                          "ConcDependent_fup" = "concentration-dependent fup information", 
                          "CustomDosing" = "custom-dosing information", 
                          "DissolutionProfiles" = "dissolution profiles", 
                          "pH_dependent_LuminalDegradation" = "pH-dependent luminal degradation", 
                          "pH_dependent_solubility" = "pH-dependent solubility information", 
                          "ReleaseProfiles" = "release profiles")
      
      if(item == "MainDetails"){
         DF <- MainDetails
      } else {
         DF <- existing_exp_details[[item]]
         
         if(is.null(DF) || nrow(DF) == 0){return(data.frame())}
         
         # If user requested information from a specific Simulator section, then
         # only return custom dosing info if that section was "trial design".
         if(complete.cases(simulator_section) &&
            str_detect(simulator_section, "trial design") == FALSE & 
            item %in% c("CustomDosing", "Dosing")){
            return(data.frame())
         }
         
         suppressMessages(
            DF <- left_join(DF, CompoundNames)
         )
      }
      
      DF <- DF %>% 
         filter(CompoundNameID %in% CompoundNames$CompoundNameID | is.na(CompoundID)) %>% 
         select(-CompoundNameID)
      
      # template simulation might not be present in the particular list item.
      # Checking for that.
      TemplateSim_subfun <- case_when(
         is.na(template_sim) ~ as.character(NA), 
         
         complete.cases(template_sim) ~ 
            intersect(unique(DF$File), template_sim)[1])
      
      ## Filtering as requested -----------------------------------------
      
      # Need everything to be in long format and then will pivot to wide at end.
      # Otherwise, too difficult to filter b/c don't know in advance the names
      # of the files.
      
      ### compoundID ---------------------------------------------------------------
      if(any(complete.cases(compoundID))){
         
         DF <- DF %>% filter(CompoundID %in% sort(unique(compoundID)) |
                                is.na(CompoundID))
      }
      
      if(nrow(DF) == 0){
         return(data.frame())
      }
      
      ### compound -------------------------------------------------------------
      if(complete.cases(compound)){
         
         if(any(str_detect(tolower(DF$Compound), tolower(compound)),
                na.rm = TRUE) == FALSE){
            
            # Only give the warning when item is MainDetails or if they
            # specifically requested this item with the detail_set argument.
            if(item %in% c(detail_set, "MainDetails")){
               if(complete.cases(compoundID)){
                  warning(wrapn(paste0("None of the simulations in the ", 
                                       item_char, 
                                       " included the specific compound and compound ID you requested. You requested a compound with `", 
                                       compound, 
                                       "` in the name as the ", compoundID, 
                                       ", but the compounds present in these simulations for the ", 
                                       compoundID, 
                                       " are: ", 
                                       str_comma(sort(unique(DF$Compound))), ". All the information specific to only those compounds will be omitted from your output.")), 
                          call. = FALSE)   
               } else {
                  warning(wrapn(paste0("None of the simulations in the ", 
                                       item_char, 
                                       " included the specific compound you requested. You requested a compound with `", 
                                       compound, 
                                       "`` in the name, but the compounds present in these simulations are: ", 
                                       str_comma(sort(unique(DF$Compound))), ". All the information specific to only those compounds will be omitted from your output.")), 
                          call. = FALSE)   
               }
            }
            
            return(data.frame())
            
         } 
         
         DF <- DF %>%
            filter(str_detect(tolower(Compound), tolower({{compound}})) |
                      is.na(Compound))
         
         if(nrow(DF) == 0){
            return(data.frame())
         }
         
         if(item == "MainDetails"){
            DF <- DF %>% 
               mutate(Notes = str_trim(gsub("(for|of) (the )?(substrate|primary metabolite 1|primary metabolite 2|secondary metabolite|inhibitor 1|inhibitor 2|inhibitor 1 metabolite)",
                                            "", Notes)), 
                      Detail = sub("_sub$|_inhib$|_met1$|_met2$|_secmet$|_inhib2$|_inhib1met$", 
                                   "", Detail))
         }
      }
      
      ### simulator_section --------------------------------------------------
      
      if(complete.cases(simulator_section) & item == "MainDetails"){
         
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
         
         if(str_detect(simulator_section, "other")){
            MySections <- c(MySections, "Other", "SimulatorVersion")
            
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
            warning(wrapn(paste0("You entered ", simulator_section_orig, 
                                 " for the argument `simulator_section`, but that is not among the acceptable options, which are listed in the help file. We will not filter your results based on simulator section.")), 
                    call. = FALSE)
         }
         
         # The name of the population should show up BOTH for when people want
         # to see the trial design AND when they want population parameters.
         # Hacking things a bit to deal with that.
         if(str_detect(simulator_section, "population")){
            DF <- DF %>% filter(SimulatorSection %in% MySections |
                                   Detail == "Population")
         } else {
            DF <- DF %>% filter(SimulatorSection %in% MySections)
            
         }
         
         if(nrow(DF) == 0){
            return(data.frame())
         }
      }
      
      ### detail_set -------------------------------------------------------------
      
      DetailSet <- c()
      DetailSetSpecial <- c()
      
      if(any(complete.cases(detail_set)) &&
         any(detail_set != "all")){
         
         if(item == "MainDetails"){
            
            if("simcyp inputs" %in% tolower(detail_set)){
               
               DetailSet <- c(
                  DetailSet, (AllExpDetails %>%
                                 filter(complete.cases(CDSInputMatch)) %>%
                                 pull(Detail)), 
                  MainDetails %>% 
                     filter(str_detect(Detail, 
                                       str_c(c(EliminationRegex, InteractionRegex), collapse = "|"))) %>% 
                     pull(Detail),
                  "SimulatorVersion", "Substrate", 
                  "PrimaryMetabolite1", "PrimaryMetabolite2",
                  "SecondaryMetabolite", 
                  "Inhibitor1", "Inhibitor2", "Inhibitor1Metabolite")
            }
            
            if(any(str_detect(tolower(detail_set), "methods|trial design"))){
               DetailSet <- unique(c(
                  DetailSet,  "Substrate", 
                  "PrimaryMetabolite1", "PrimaryMetabolite2",
                  "SecondaryMetabolite", 
                  "Inhibitor1", "Inhibitor2", "Inhibitor1Metabolite",
                  "Dose_sub", "Dose_inhib", 
                  "DoseRoute_sub", "DoseRoute_inhib", "DoseRoute_inhib2",
                  "Regimen_sub", "Regimen_inhib",
                  "FixedTrialDesign", "FixedTrialDesignFile", 
                  "SimulatorVersion", 
                  "NumTrials", "NumSubjTrial", "PercFemale", 
                  "Age_min", "Age_max", "NumDoses", "Regimen", 
                  "SimDuration", "SimStartDayTime", "SimEndDayTime",
                  paste0(rep(c("StartDayTime_", "StartHr_"), 
                             each = 7),
                         c("sub", "inhib", "inhib2", "met1", 
                           "met2", "secmet", "inhib1met"))))
            }
            
            if(any(str_detect(tolower(detail_set), "summary"))){
               DetailSet <- c(DetailSet, 
                              MainDetails %>% 
                                 filter(DataSource %in% c("Input Sheet", 
                                                          "Input Sheet or Summary", 
                                                          "Input Sheet, Summary, or workspace or database",
                                                          "calculated, Summary, or workspace or database")) %>% 
                                 pull(Detail))
            }
            
            if(any(str_detect(tolower(detail_set), "input sheet"))){
               DetailSet <- c(DetailSet, 
                              MainDetails %>% 
                                 filter(DataSource %in% c("Input Sheet", 
                                                          "Input Sheet or Summary", 
                                                          "Input Sheet, Summary, or workspace or database")) %>%
                                 pull(Detail))
            }
            
            if(any(str_detect(tolower(detail_set), "population"))){
               DetailSet <- c(DetailSet, 
                              MainDetails %>% filter(DataSource == "population") %>% 
                                 pull(Detail))
            }
            
            if(any(str_detect(tolower(detail_set), "lactation"))){
               DetailSet <- c(DetailSet, 
                              paste0(rep(c("BreastPerfLimModel", 
                                           "MilkPlasmaRatio", 
                                           "MilkPmk", 
                                           "fu_skimmedmilk_type", 
                                           "Unionised_frac_milk_type", 
                                           "Unionised_frac_plasma_type", 
                                           "SkimToWholeMilk_drugratio_type"), 
                                         each = 7),
                                     c("sub", "inhib", "inhib2", "met1", 
                                       "met2", "secmet", "inhib1met")))
               
            }
            
            # This is when they have requested individual details. This also removes
            # any replicates.
            DetailSet <- unique(c(DetailSet, setdiff(detail_set, DetailSet)))
            
            # Replacing any "_X" details with the compound suffixes
            Xdetails <- DetailSet[str_detect(DetailSet, "_X")]
            Xdetails <- unlist(lapply(c("_sub", "_inhib", "_inhib2", "_inhib1met", 
                                        "_met1", "_met2", "_secmet"),
                                      FUN = function(x) sub("_X", x, Xdetails)))
            DetailSet <- c(DetailSet, Xdetails)
            
            DF <- DF %>% filter(Detail %in% 
                                   switch(as.character(complete.cases(compound)),
                                          "TRUE" = sub("_sub|_inhib|_inhib2|_met1|_met2|_secmet|_inhib1met", 
                                                       "", DetailSet),
                                          "FALSE" = DetailSet))
            
            if(nrow(DF) == 0){
               return(data.frame())
            }
            
         }
         
      } else {
         
         if(any(str_detect(tolower(detail_set), "custom.*?dosing"))){
            DetailSetSpecial <- c(DetailSetSpecial, "CustomDosing")
         }
         
         if(any(str_detect(tolower(detail_set), "dissolu.*?(prof)?"))){
            DetailSetSpecial <- c(DetailSetSpecial, "DissolutionProfiles")
         }
         
         if(any(str_detect(tolower(detail_set), "release.*?(prof)?"))){
            DetailSetSpecial <- c(DetailSetSpecial, "ReleaseProfiles")
         }
         
         if(any(str_detect(tolower(detail_set), "conc.*?fup"))){
            DetailSetSpecial <- c(DetailSetSpecial, "ConcDependent_fup")
         }
         
         if(any(str_detect(tolower(detail_set), "conc.*?b.?p"))){
            DetailSetSpecial <- c(DetailSetSpecial, "ConcDependent_BP")
         }
         
         if(any(str_detect(tolower(detail_set), "ph.*?solubility"))){
            DetailSetSpecial <- c(DetailSetSpecial, "pH_dependent_solubility")
         }
      }
      
      
      ### find_matching_details --------------------------------------------------------
      
      if(item == "MainDetails" & complete.cases(find_matching_details)){
         DF <- DF %>% 
            filter(str_detect(Detail, find_matching_details))
      }
      
      if(nrow(DF) == 0){
         return(data.frame())
      }
      
      ## Cleaning up and removing unnecessary info ----------------------------
      # Removing unnecessary compounds. 
      
      # First, remove any compound IDs for which there are no data. 
      CmpdIDCheck <- DF %>%
         filter(complete.cases(CompoundID)) %>% 
         group_by(CompoundID) %>% 
         summarize(Keep = switch(item, 
                                 "MainDetails" = any(complete.cases(Value)), 
                                 "Dosing" = any(complete.cases(Dose)), 
                                 "CustomDosing" = any(complete.cases(Dose)), 
                                 "ConcDependent_fup" = any(complete.cases(fup)), 
                                 "ConcDependent_BP" = any(complete.cases(BP)),  
                                 "pH_dependent_solubility" = any(complete.cases(pH)), 
                                 "pH_dependent_LuminalDegradation" = any(complete.cases(pH)), 
                                 
                                 # Items that could be empty lists should just
                                 # be set to TRUE here
                                 "DissolutionProfiles" = TRUE, 
                                 "ReleaseProfiles" = TRUE, 
                                 "UserAUCIntervals" = TRUE)) %>% 
         filter(Keep == TRUE) %>% 
         pull(CompoundID) %>% unique() %>% as.character()
      
      DF <- DF %>% 
         filter(is.na(CompoundID) | CompoundID %in% {{CmpdIDCheck}})
      
      if(nrow(DF) == 0){
         return(data.frame())
      }
      
      # Removing compound column if they don't want it
      if(class(show_compound_col) == "logical" & 
         VBEsims == FALSE){
         if(show_compound_col == FALSE){
            DF <- DF %>% select(-Compound)
         }
         
      } else if(show_compound_col == "concatenate" | VBEsims){
         if(complete.cases(compound)){
            
            # Checking whether user has asked to concatenate compounds that
            # occupy multiple positions in the simulator for the same
            # simulation, e.g., asked to concatenate info on the substrate and
            # the inhibitor 1 b/c that would result in multiple values in the
            # same cell.
            
            # NB: This SHOULD be existing_exp_details here and NOT Main or DF.
            # Need it to be in the original format (wide by detail).
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
               
               warning(wrapn(paste0("You have asked to concatenate the compound column and also requested all details that match `", 
                                    compound, 
                                    "`. The problem, though, is that you have requested information for compounds that occupy more than one position in the Simulator, e.g., one is the substrate and one is inhibitor 1 in the same simulation, which means that there would be more than one value for a given detail. This wouldn't be workable in the results, so we cannot concatenate the compound column in this situation.")), 
                       call. = FALSE)
            } else {
               
               AllMyCompounds <- str_comma(sort(unique(DF$Compound)), conjunction = "or")
               DF <- DF %>% mutate(Compound = ifelse(complete.cases(Compound), 
                                                     AllMyCompounds, NA))
               
               DF <- DF %>% select(-CompoundID) %>% 
                  select(any_of(c("SimulatorSection", "DataSource", "Notes",
                                  "Compound", "Detail", "pH", "Conc", 
                                  "Time", "Time_units", "DoseNum", "Dose_units", 
                                  "DoseRoute", "Day", "TimeOfDay", 
                                  "File", "Value", "Solubility", "BP", "fup", 
                                  "Dose", "Dissolution_mean", "Dissolution_CV",
                                  "Release_mean", "Release_CV", everything())))
               
            }
         } else {
            
            AllMyCompounds <- MainDetails %>% 
               select(Compound, CompoundID) %>% 
               filter(complete.cases(CompoundID) & 
                         complete.cases(Compound)) %>% 
               unique() %>% 
               group_by(CompoundID) %>% 
               summarize(
                  Compound = str_comma(sort(unique(Compound)),
                                       conjunction = "or")) %>% 
               ungroup() 
            
            suppressMessages(
               DF <- DF %>% select(-Compound) %>% left_join(AllMyCompounds))
            
         }
      }
      
      ## Pivoting wider again ------------------------------------------------
      
      # Items that will have either template sims or a column where everything
      # is the same. This does NOT include DissolutionProfiles or
      # SolubilityProfiles b/c those have multiple dependent variables
      # (Dissolution_mean and Dissolution_CV, for example). For these others,
      # there is a single dependent variable to go with the specific independent
      # variable or, if there are multiple possible dependent variables (e.g.,
      # CustomDosing), we've picked the one we want to pivot wider by (Dose in
      # that example). Variables for each with *exact* column name:
      
      # MainDetails: IV = Detail, DV = Value
      # Dosing: IV = Time, DV = Dose
      # ConcDependent_BP: IV = Conc, DV = BP
      # ConcDependent_fup: IV = Conc, DV = fup
      # CustomDosing: IV = Time, DV = Dose
      # pH_dependent_LuminalDegradation: IV = pH, DV = LuminalDegradation 
      # pH_dependent_solubility: IV = pH, DV = Solubility
      
      # These two do NOT work b/c they have CV columns, so too many columns to
      # pivot wider by: DissolutionProfiles, ReleaseProfiles
      
      if(item %in% c("MainDetails", "CustomDosing", "Dosing",
                     "ConcDependent_fup", "ConcDependent_BP",
                     "pH_dependent_LuminalDegradation", 
                     "pH_dependent_solubility")){
         
         DF <- DF %>% 
            pivot_wider(names_from = "File", 
                        # values_from calls on the dependent variable listed 
                        values_from = switch(
                           item, 
                           "MainDetails" = "Value", 
                           "Dosing" = "Dose", 
                           "ConcDependent_BP" = "BP", 
                           "ConcDependent_fup" = "fup", 
                           "CustomDosing" = "Dose", 
                           "pH_dependent_LuminalDegradation" = "DegradationRateConstant", 
                           "pH_dependent_solubility" = "Solubility"))
         
         # Need to check again whether template_sim is included b/c it might not
         # be any more if the user has filtered results for a specific compound
         # ID that doesn't exist in template_sim. 
         if(complete.cases(template_sim) & 
            TemplateSim_subfun %in% names(DF) == FALSE){
            
            # Only giving the warning when it's for MainDetails b/c otherwise
            # it's confusing. 
            if(item == "MainDetails"){
               warning(wrapn(paste0("Your template simulation file, `", 
                                    template_sim, 
                                    "`, was originally included in the object you supplied for `existing_exp_details`, but that particular simulation didn't have any of the combination of details or compound IDs or compounds that you requested we filter the results by for the ", 
                                    item_char, 
                                    ". We thus don't have a good template simulation to compare other files to, so we'll have to ignore your input for `template_sim` for this part of the output.")), 
                       call. = FALSE)
            }
            
            template_sim <- NA
         }
         
         # FileOrder should be > 1 if there is more than 1 sim. Also need to
         # check that there is more than one sim in this particular set of data.
         # If there is only 1 sim, no need to adjust order OR to look for
         # differences from template or which are unique.
         if(length(intersect(FileOrder, 
                             unique(existing_exp_details[[item]]$File))) > 1){
            # Checking for details that are the SAME across all files
            GroupingDetails <- switch(item, 
                                      "MainDetails" = "Detail", 
                                      "CustomDosing" = c("Time", "Time_units", "DoseNum",
                                                         "Dose_units", "DoseRoute", 
                                                         "Day", "TimeOfDay"), 
                                      "Dosing" = c("Time", "Time_units", "DoseNum",
                                                   "Dose_units", "DoseRoute"), 
                                      "ConcDependent_fup" = "Conc", 
                                      "ConcDependent_BP" = "Conc", 
                                      "pH_dependent_solubility" = "pH")
            
            suppressMessages(
               AllSame <- DF %>% 
                  select(any_of(c("CompoundID", "Compound", GroupingDetails)), 
                         matches("\\.xlsx|\\.db")) %>% 
                  pivot_longer(cols = matches("\\.xlsx|\\.db"), 
                               names_to = "File",
                               values_to = "Value") %>% 
                  group_by(across(.cols = any_of(c(GroupingDetails,
                                                   "CompoundID",
                                                   "Compound")))) %>% 
                  summarize(Length = length(unique(Value)), 
                            UniqueVal = unique(Value)[1]) %>% 
                  filter(Length == 1) %>% 
                  select(-Length)
            )
            
            suppressMessages(
               DF <- DF %>%
                  left_join(AllSame)
            )
            
            if(complete.cases(template_sim)){
               DF <- DF %>% 
                  select(-any_of("UniqueVal")) %>% 
                  select(any_of(unique(c(
                     "SimulatorSection", "DataSource", "Notes", "CompoundID", "Compound",
                     GroupingDetails, template_sim))), 
                     everything())
               
               TSim <- paste0("TEMPLATE SIMULATION ", 
                              switch(item, 
                                     "MainDetails" = "",
                                     "CustomDosing" = "FOR DOSE ", 
                                     "ConcDependent_fup" = "FOR fu,p ", 
                                     "ConcDependent_BP" = "FOR B/P ", 
                                     "pH_dependent_solubility" = "FOR SOLUBILITY "), 
                              "- ", template_sim)
               names(DF)[names(DF) == template_sim] <- TSim
               
            } else if("UniqueVal" %in% names(DF)){
               DF <- DF %>% 
                  select(any_of(unique(c(
                     "SimulatorSection", "DataSource", "Notes", "CompoundID", "Compound", 
                     GroupingDetails, "UniqueVal"))), 
                     everything())
               
               if("Compound" %in% names(DF)){
                  if(complete.cases(compound)){
                     All_name <- switch(
                        item, 
                        "MainDetails" = "All files have this value for this compound", 
                        "Dosing" = "All files have this dose for this compound", 
                        "CustomDosing" = "All files have this dose for this compound", 
                        "ConcDependent_fup" = "All files have this fu,p for this compound", 
                        "ConcDependent_BP" = "All files have this B/P for this compound",
                        "pH_dependent_LuminalDegradation" = "All files have this luminal degradation rate at this pH for this compound", 
                        "pH_dependent_solubility" = "All files have this solubility at this pH for this compound")
                     
                     names(DF)[names(DF) == "UniqueVal"] <- All_name
                     
                  } else {
                     DF <- DF %>% 
                        mutate(ToOmit = complete.cases(CompoundID) & 
                                  is.na(Compound)) %>% 
                        filter(ToOmit == FALSE) %>% select(-ToOmit)
                     
                     All_name <- switch(
                        item, 
                        "MainDetails" = "All files have this value for this compound ID and compound", 
                        "CustomDosing" = "All files have this dose for this compound ID and compound", 
                        "Dosing" = "All files have this dose for this compound ID and compound", 
                        "ConcDependent_fup" = "All files have this fu,p for this compound ID and compound", 
                        "ConcDependent_BP" = "All files have this B/P for this compound ID and compound", 
                        "pH_dependent_LuminalDegradation" = "All files have this luminal degradation rate at this pH for this compound ID and compound", 
                        "pH_dependent_solubility" = "All files have this solubility at this pH for this compound ID and compound")
                     
                     names(DF)[names(DF) == "UniqueVal"] <- All_name
                  }
               } else {
                  
                  All_name <- switch(
                     item, 
                     "MainDetails" = "All files have this value for this compound ID", 
                     "Dosing" = "All files have this dose for this compound ID", 
                     "CustomDosing" = "All files have this dose for this compound ID", 
                     "ConcDependent_fup" = "All files have this fu,p for this compound ID", 
                     "ConcDependent_BP" = "All files have this B/P for this compound ID", 
                     "pH_dependent_LuminalDegradation" = "All files have this luminal degradation rate at this pH for this compound ID",
                     "pH_dependent_solubility" = "All files have this solubility at this pH for this compound ID")
                  
                  names(DF)[names(DF) == "UniqueVal"] <- All_name
                  
               }
            } 
         }
         
         # Removing anything that was all NA's if that's what user requested
         if(omit_all_missing){
            DF$AllNA <- apply(DF[, names(DF)[str_detect(names(DF), "\\.xlsx|\\.db")]], 
                              MARGIN = 1, FUN = function(.) all(is.na(.)))    
            
            DF <- DF %>% filter(AllNA == FALSE) %>% select(-AllNA)
         }
         
         # Sorting to help organize output
         if(item == "MainDetails"){
            DF <- DF %>%
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
         }
         
         DF <- DF[, c(setdiff(names(DF), FileOrder), # SimulatorSection, etc.
                      intersect(FileOrder, names(DF)))] # files in the order requested
         
         # Checking for differences from template sim
         if(complete.cases(template_sim) & 
            length(FileOrder) > 1){
            Diffs <- list()
            NontempFiles <- setdiff(intersect(FileOrder, names(DF)), 
                                    template_sim)
            
            for(i in 1:length(NontempFiles)){
               Diffs[[i]] <- 
                  list(columns = which(names(DF) == NontempFiles[i]),
                       rows = which(DF[ , NontempFiles[i]] != DF[, TSim] |
                                       (complete.cases(DF[ , NontempFiles[i]]) &
                                           is.na(DF[, TSim])) |
                                       (is.na(DF[ , NontempFiles[i]]) & 
                                           complete.cases(DF[, TSim]))))
            }
         } else if(VBEsims){
            
            Diffs <- list()
            Sets <- existing_exp_details$MainDetails %>% 
               select(File, FileAlone, Treatment)
            Sets <- split(Sets, f = Sets$FileAlone)
            
            for(i in names(Sets)){
               # Making tx 1 the template
               TSim <- Sets[[i]]$File[
                  which(str_detect(Sets[[i]]$File, "Treatment 1$"))]
               
               for(j in which(names(DF) %in% setdiff(Sets[[i]]$File, 
                                                     TSim))){
                  Diffs[[names(DF)[j]]] <- 
                     list(columns = j,
                          rows = which(
                             DF[ , j] != DF[, TSim] |
                                (complete.cases(DF[ , j]) & is.na(DF[, TSim])) |
                                (is.na(DF[ , j]) & complete.cases(DF[, TSim]))))
               }
            }
            
         } else {
            Diffs <- list()
         }
         
      } else {
         Diffs <- list()
      }
      
      return(list(DF = DF, 
                  Diffs = Diffs))
   } 
   
   # subfunction ends here ---------------------------------------------------
   
   Out <- list()
   
   for(i in names(existing_exp_details)){
      Out[[i]] <- annotate_subfun(i)
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
      
      if(Ext == "xlsx"){
         # Using openxlsx to format things. NB: Versions <= 2.12.17 used the
         # xlsx package to save, but openxlsx allows you to insert graphs and
         # also seems to have a much more intuitive interface for saving
         # formatting.
         if(file.exists(FileName)){
            WB <- openxlsx::loadWorkbook(file = FileName)
         } else {
            WB <- openxlsx::createWorkbook()
         }
      }
      
      
      ## subfunction for saving ----------------------------------------------
      
      write_subfun <- function(item, 
                               output_tab_name){
         
         if(nchar(output_tab_name) > 31){
            warning(wrapn(paste0("The tab `", 
                                 output_tab_name, 
                                 "` has more than 31 characters, which is the maximum. This tab will be shortened to `", 
                                 str_sub(output_tab_name, 1, 31), "`.")), 
                    call. = FALSE)
         }
         
         item_char <- switch(item, 
                             "MainDetails" = "main set of simulation details", 
                             "CustomDosing" = "custom-dosing information", 
                             "ConcDependent_fup" = "concentration-dependent fup information", 
                             "ConcDependent_BP" = "concentration-dependent B/P information", 
                             "pH_dependent_solubility" = "pH-dependent solubility information")
         
         MyStyles <- list()
         
         # Only showing differences from template sim if that's what user requested
         if(show_only_diff_from_template){
            RowsWithDiffs <- lapply(Out[[item]][["Diffs"]], FUN = function(x) x[["rows"]])
            RowsWithDiffs <- sort(unique(unlist(RowsWithDiffs)))
            Out[[item]][["DF"]] <- Out[[item]][["DF"]][RowsWithDiffs, ]
         }
         
         # Check for when there isn't any information beyond what the simulator
         # version is, which can happen if the user extracts only some information
         # and then wants information that would not have been extracted. For
         # example, extractExpDetails pulls only the "Summary" tab by default, so
         # it won't have a LOT of useful information.
         if(nrow(Out[[item]][["DF"]]) == 1){
            warning(wrapn(paste0(
               "There is only 1 row in your output for the ",
               item_char, 
               ". When you ran `extractExpDetails` or `extractExpDetails_mult`, did you request all the information you wanted? For example, if you only requested information from the population tab, that won't include any elimination information.")), 
               call. = FALSE)
         }
         
         if(Ext == "csv"){
            write.csv(Out[[item]][["DF"]], FileName, row.names = F)
         } else if(Ext == "xlsx"){
            
            HeaderStyle <- openxlsx::createStyle(textDecoration = "bold",
                                                 wrapText = TRUE, 
                                                 halign = "center", 
                                                 valign = "center")
            
            if(file.exists(FileName) &&
               output_tab_name %in% readxl::excel_sheets(path = FileName)){
               openxlsx::removeWorksheet(wb = WB, 
                                         sheet = output_tab_name)
            }
            
            openxlsx::addWorksheet(wb = WB, 
                                   sheetName = output_tab_name)
            
            openxlsx::writeData(wb = WB, 
                                sheet = output_tab_name,
                                x = Out[[item]][["DF"]], 
                                headerStyle = HeaderStyle)
            
            # Setting column widths. Notes should be 40.
            ColWidths <- guess_col_widths(Out[[item]][["DF"]])
            GoodColWidths <- c("SimulatorSection" = 17, 
                               "DataSource" = 15,
                               "CompoundID" = 15, 
                               "Compound" = 15, 
                               "Notes" = 40, 
                               "Detail" = 20)
            GoodColWidths <- GoodColWidths[intersect(names(GoodColWidths), 
                                                     names(ColWidths))]
            ColWidths[names(GoodColWidths)] <- GoodColWidths
            ColWidths[which(str_detect(names(ColWidths),
                                       "All files have this value|TEMPLATE"))] <- 15
            ColWidths[which(str_detect(names(ColWidths), "xlsx$|wksz$"))] <- 15
            
            openxlsx::setColWidths(wb = WB, 
                                   sheet = output_tab_name, 
                                   cols = 1:ncol(Out[[item]][["DF"]]), 
                                   widths = ColWidths)
            
            # These are all the items that would have either an "all items are
            # the same" column or a template simulation column. Note that this
            # does NOT include DissolutionProfiles or ReleaseProfiles.
            if(item %in% c("MainDetails", "CustomDosing", "ConcDependent_fup", 
                           "ConcDependent_BP", "pH_dependent_solubility")){
               
               NotesColumn <- openxlsx::createStyle(wrapText = TRUE) 
               
               BlueColumn <- openxlsx::createStyle(wrapText = TRUE, 
                                                   fgFill = "#E7F3FF")
               
               BlueColumnHeader <- openxlsx::createStyle(textDecoration = "bold",
                                                         wrapText = TRUE, 
                                                         halign = "center", 
                                                         valign = "center", 
                                                         fgFill = "#E7F3FF")
               
               ProbCells <- openxlsx::createStyle(wrapText = TRUE, 
                                                  fgFill = "#FFC7CE", 
                                                  fontColour = "#9B030C")
               
               
               if(is.na(template_sim) | length(FileOrder) == 1){
                  # This is when there is no template simulation, but we are
                  # including a column noting when a given value was the same for
                  # all simulations.
                  
                  openxlsx::addStyle(wb = WB, 
                                     sheet = output_tab_name, 
                                     style = BlueColumn, 
                                     rows = 2:(nrow(Out[[item]][["DF"]]) + 1), 
                                     cols = which(str_detect(names(Out[[item]][["DF"]]),
                                                             "All files have this")))
                  
                  openxlsx::addStyle(wb = WB, 
                                     sheet = output_tab_name, 
                                     style = BlueColumnHeader, 
                                     rows = 1, 
                                     cols = which(str_detect(names(Out[[item]][["DF"]]),
                                                             "All files have this")))
                  
               } else {
                  # This is when there IS a template simulation. Formatting to
                  # highlight in red all the places where things differ.
                  
                  # making the template sim column blue
                  openxlsx::addStyle(wb = WB, 
                                     sheet = output_tab_name, 
                                     style = BlueColumn, 
                                     rows = 2:(nrow(Out[[item]][["DF"]]) + 1), 
                                     cols = which(str_detect(names(Out[[item]][["DF"]]),
                                                             template_sim)), 
                                     gridExpand = T)
                  
                  openxlsx::addStyle(wb = WB, 
                                     sheet = output_tab_name, 
                                     style = BlueColumnHeader, 
                                     rows = 1, 
                                     cols = which(str_detect(names(Out[[item]][["DF"]]),
                                                             template_sim)))
                  
                  # highlighting mismatches in red
                  if(length(Out[[item]][["Diffs"]]) > 0){
                     for(i in 1:length(Out[[item]][["Diffs"]])){
                        openxlsx::addStyle(wb = WB, 
                                           sheet = output_tab_name, 
                                           style = ProbCells, 
                                           rows = Out[[item]][["Diffs"]][[i]]$rows + 1, 
                                           cols = Out[[item]][["Diffs"]][[i]]$columns)
                     }
                  }
               }
               
               # When VBE sims present, highlighting in red differences between
               # treatments
               if(VBEsims){
                  
                  if(length(Out[[item]][["Diffs"]]) > 0){
                     for(i in 1:length(Out[[item]][["Diffs"]])){
                        openxlsx::addStyle(wb = WB, 
                                           sheet = output_tab_name, 
                                           style = ProbCells, 
                                           rows = Out[[item]][["Diffs"]][[i]]$rows + 1, 
                                           cols = Out[[item]][["Diffs"]][[i]]$columns)
                     }
                  }
               }
               
               # Freezing view 
               UnfrozCol1 <- which(
                  str_detect(names(Out[[item]][["DF"]]),
                             "TEMPLATE|^All files")) + 1
               UnfrozCol1 <- UnfrozCol1[1]
               # Most of the time, column 8 should be the 1st active column,
               # so set that here. Also, the only time we won't have a value
               # here is if there was only 1 simulation, so which column is
               # frozen isn't that important.
               UnfrozCol1 <- ifelse(is.na(UnfrozCol1), 8, UnfrozCol1)
               
               openxlsx::freezePane(wb = WB,
                                    sheet = output_tab_name,
                                    firstActiveRow = 2,
                                    firstActiveCol =  UnfrozCol1)
               
            } else {
               # Adding frozen top row for dissolution and release profiles.
               openxlsx::freezePane(wb = WB,
                                    sheet = output_tab_name,
                                    firstActiveRow = 2,
                                    firstActiveCol =  1)
            }
            
            if(item == "MainDetails"){
               ## MainDetails tab -------------------------------------------
               
               # Adding style for notes column
               openxlsx::addStyle(wb = WB, 
                                  sheet = output_tab_name, 
                                  style = NotesColumn, 
                                  rows = 2:(nrow(Out[[item]][["DF"]]) + 1), 
                                  cols = which(
                                     str_detect(names(Out[[item]][["DF"]]),
                                                "Note")))
               
               
            }
            
            # At this point, we have written the data for all possible items,
            # so, next, we'll add graphs as applicable. Note that, in the code
            # below, we're often using existing_exp_details[[item]] rather than
            # Out[[item]][[DF]]; that's how it should be b/c sometimes we need
            # the data formatted how it was originally.
            
            NumFiles <- length(unique(existing_exp_details[[item]]$File))
            
            if(item == "ConcDependent_fup"){
               
               ## ConcDependent_fup tab ----------------------------------------
               
               plot(ggplot(existing_exp_details[[item]], 
                           aes(x = Conc, y = fup, color = File)) +
                       geom_point() + geom_line() +
                       facet_grid(. ~ Compound, 
                                  scales = "fixed") +
                       xlab("Concentration") +
                       ylab(expression(f[u,p])) +
                       ggtitle("Concentration-dependent fu,p", 
                               subtitle = "Points will overlap perfectly when all simulations have the same values.") +
                       theme_consultancy(border = TRUE))
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth,  
                                    height = PlotHeight,
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
               
            } else if(item == "ConcDependent_BP"){ 
               
               ## ConcDependent_BP tab ----------------------------------------
               
               suppressMessages(
                  plot(ggplot(existing_exp_details[[item]], 
                              aes(x = Conc, y = BP, color = File)) +
                          geom_point() + geom_line() +
                          scale_color_manual(values = rainbow(length(unique(existing_exp_details[[item]]$File)))) +
                          facet_grid(. ~ Compound, 
                                     scales = "fixed") +
                          xlab("Concentration") +
                          ylab("B/P") +
                          ggtitle("Concentration-dependent B/P", 
                                  subtitle = "Points will overlap perfectly when all simulations have the same values.") +
                          theme_consultancy(border = TRUE))
               )
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth,  
                                    height = PlotHeight,
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
            } else if(item %in% c("Dosing", "CustomDosing")){ 
               
               ## Dosing and CustomDosing tabs ----------------------------------
               
               suppressMessages(
                  plot(dosing_regimen_plot(existing_exp_details = existing_exp_details, 
                                   facet1_column = CompoundID, 
                                   colorBy_column = File, 
                                   color_set = "rainbow", 
                                   bar_width = 1) +
                  ggtitle("Dosing regimens", 
                          subtitle = "Lines will overlap perfectly when all simulations have the same dosing regimens.\nIf you have a lot of files and want to see a more-informative version of this graph,\nplease try running dosing_regimen_plot(...) separately."))
               )
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 10
               PlotHeight_g <- length(unique(
                  existing_exp_details[[item]]$CompoundID)) * 3
               PlotHeight_leg <- length(unique(
                  existing_exp_details[[item]]$File)) %/% 10 * 0.5
               PlotHeight <- PlotHeight_g + PlotHeight_leg
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    height = PlotHeight,
                                    width = PlotWidth,
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
            } else if(item == "DissolutionProfiles"){ 
               
               ## DissolutionProfiles tab --------------------------------------
               
               suppressMessages(suppressWarnings(
                  plot(dissolution_profile_plot(existing_exp_details, 
                                                colorBy_column = File, 
                                                line_transparency = 1/(NumFiles * 2), 
                                                facet1_column = Compound) +
                          ggtitle("Dissolution-profile plot", 
                                  subtitle = "Points will overlap perfectly when all simulations have the same values."))
               ))
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth, 
                                    height = PlotHeight, 
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
            } else if(i == "ReleaseProfiles"){ 
               
               ## ReleaseProfiles tab --------------------------------------
               
               suppressMessages(suppressWarnings(
                  plot(release_profile_plot(existing_exp_details, 
                                            colorBy_column = File, 
                                            line_transparency = 1/(NumFiles * 2), 
                                            facet1_column = Compound) +
                          ggtitle("Release-profile plot", 
                                  subtitle = "Points will overlap perfectly when all simulations have the same values."))
               ))
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth, 
                                    height = PlotHeight, 
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
               rm(NumFiles, PlotWidth)
               
            } else if(i == "pH_dependent_solubility"){
               
               ## pH_dependent_solubility tab ----------------------------------
               
               suppressMessages(
                  plot(ggplot(existing_exp_details[[item]], 
                              aes(x = pH, y = Solubility, color = File)) +
                          geom_point() + 
                          geom_line() +
                          facet_grid(Compound ~ ., switch = "y") +
                          scale_color_manual(values = rainbow(length(unique(existing_exp_details[[item]]$File)))) +
                          ylab("Solubility (mg/mL)") +
                          ggtitle("pH-dependent solubility", 
                                  subtitle = "Points will overlap perfectly when all simulations have the same values.") +
                          theme_consultancy(border = TRUE) +
                          theme(strip.placement = "outside")
                  ))
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth,  
                                    height = PlotHeight,
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
            } else if(i == "pH_dependent_LuminalDegradation"){
               ## pH_dependent_LuminalDegradation tab -------------------------
               
               suppressMessages(
                  plot(ggplot(existing_exp_details[[item]], 
                              aes(x = pH, y = DegradationRateConstant,
                                  color = File)) +
                          geom_point() + 
                          geom_line() +
                          facet_grid(Compound ~ ., switch = "y") +
                          scale_color_manual(values = rainbow(length(unique(existing_exp_details[[item]]$File)))) +
                          ylab("Solubility (mg/mL)") +
                          ggtitle("pH-dependent luminal degradation", 
                                  subtitle = "Points will overlap perfectly when all simulations have the same values.") +
                          theme_consultancy(border = TRUE) +
                          theme(strip.placement = "outside")
                  ))
               
               # Seems like ggplot makes not more than 20 items in the legend
               # before going over a column. Will need to consider that when
               # adjusting width.
               PlotWidth <- 8 + (NumFiles %/% 20 + 1) * 6
               PlotHeight <- 0.5 + 
                  length(unique(existing_exp_details[[item]]$Compound)) * 6
               
               openxlsx::insertPlot(wb = WB, 
                                    sheet = output_tab_name, 
                                    width = PlotWidth,  
                                    height = PlotHeight,
                                    fileType = "png", 
                                    units = "in", 
                                    startRow = nrow(Out[[item]][["DF"]]) + 5, 
                                    startCol = 1)
               
            }
         }
         
      } # end subfun for saving xlsx
      
      ## subfunction ends here -----------------------------------------------
      
      GoodItems <- as.logical(
         map(Out, function(x){is.null(x) == FALSE && nrow(x$DF) > 0}))
      
      if(any(tolower(detail_set) == "all")){
         ToWrite <- names(Out)[which(GoodItems)]
      } else {
         ToWrite <- intersect(
            names(Out)[which(GoodItems)], 
            c("MainDetails", 
              ExpDetailListItems[tolower(ExpDetailListItems) %in% 
                                    tolower(detail_set)]))
      }
      
      if(length(ToWrite) > 1){
         if(output_tab_name == "Simulation experimental details"){
            OutputTabs <- ToWrite
            names(OutputTabs) <- ToWrite
            OutputTabs["MainDetails"] <- output_tab_name
         } else {
            OutputTabs <- paste(output_tab_name, "-", ToWrite)
            names(OutputTabs) <- ToWrite
         }
      } else {
         OutputTabs <- output_tab_name
         names(OutputTabs) <- ToWrite
      }
      # }
      
      for(i in ToWrite){
         
         write_subfun(item = i, 
                      output_tab_name = str_sub(OutputTabs[i], 1, 31))
         
      }
      
      openxlsx::saveWorkbook(wb = WB, 
                             file = FileName, overwrite = TRUE)
      
   }
   
   # If they didn't include "MainDetails", then they get a list even if that's
   # not what they wanted.
   if(return_list | "MainDetails" %in% names(Out) == FALSE){
      
      for(i in names(Out)){
         if(length(Out[[i]]) == 0){
            Out[[i]] <- NULL 
         } else if(length(Out[[i]]) > 1){
            Out[[i]] <- Out[[i]][["DF"]]
         }
      }
   } else {
      Out <- Out[["MainDetails"]][["DF"]]
   }
   
   return(Out)
   
}


