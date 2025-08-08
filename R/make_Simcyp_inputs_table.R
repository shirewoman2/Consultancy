#' Summarize a PBPK model using parameters pulled from simulations - UNDER
#' CONSTRUCTION
#'
#' @description \code{make_Simcyp_inputs_table} will make a neatly formatted,
#'   organized, human-readable table of the main Simcyp model inputs based on
#'   information included in your simulation(s). It was designed for use with
#'   writing compound summary pdfs, and, if you supply the references for each
#'   parameter, it will include that information in the table.
#'
#'   This function requires you to first run
#'   \code{\link{extractExpDetails_mult}} to get information about your
#'   simulations in a format this function can use. To see what will be included
#'   in this table and to see the coded names for each of the parameters that
#'   will be included in the table, please run something like:
#'
#'   \code{Details <- extractExpDetails_mult()}
#'
#'   \code{TableCheck <- annotateDetails(existing_exp_details = Details, compoundID =
#'   "substrate", detail_set = "Simcyp inputs")}
#'
#'   and the object Details is what you will provide as input for
#'   \code{existing_exp_details}. The object TableCheck will show you what
#'   information will be included and how to refer to each parameter or detail.
#'   The values to listed for each parameter in your table will come from the
#'   column titled something like "All files have this value for this compound".
#'
#'   Notes: \itemize{\item{This rounds numeric values to the 5th digit.}
#'   \item{There are some parameters that are included in the standard
#'   set of outputs from \code{\link{extractExpDetails_mult}} that we will
#'   ignore: the Simulator version, Ki values set to 1e+06 (the default value in the Simulator for,
#'   really, no inhibition), Indmax values of 1 (also the default value in the
#'   Simulator for no interaction), etc.}}
#'
#' @param existing_exp_details the output from running
#'   \code{\link{extractExpDetails_mult}}
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input: "all" to get
#'   everything or else a character vector of the sim results files you want. If
#'   these are not present in existing_exp_details, they will be ignored.
#' @param compoundID For which compound do you want to summarize the model
#'   parameters? Options are "substrate", "primary metabolite 1", "primary
#'   metabolite 2", "secondary metabolite", "inhibitor 1", "inhibitor 2", or
#'   "inhibitor 1 metabolite".
#' @param compound optionally supply a specific compound name or part of a
#'   specific compound name to get all possible compounds that match that and
#'   \emph{only} compounds that match that. Regular expressions are acceptable
#'   here, e.g., \code{compound = "midaz|keto"} to find any compound with either
#'   "midaz" or "keto" in the name. Not case sensitive. If you request only
#'   information on a specific compound, we will assume what what you care about
#'   is the set of parameters used for that compound and not whether that
#'   compound was the substrate or the inhibitor, so we'll ignore anything you
#'   supply for compoundID.
#' @param references a data.frame or csv file listing the source of all the
#'   parameters used in the model. The columns must include:
#'   \describe{\item{"Parameter" or "Detail" (either is fine)}{the specific
#'   parameter of interest with the coded name for that parameter. How can you
#'   find out the coded name? Run something like this:
#'   \code{annotateDetails(existing_exp_details = Details, compoundID = "substrate", detail_set = "Simcyp inputs")}
#'   and the column "Detail" will
#'   list all of the coded names for the parameters that will be included in
#'   this table. They must match exactly or the reference will be omitted.}
#'   \item{"Reference"}{the reference for that parameter}}
#' @param parameters_to_omit optionally specify any parameters to omit from the
#'   table. For example, maybe you don't want to include that the biliary
#'   clearance was set to 0 because that's just not important. To omit that,
#'   you'll need the exact, coded name of that parameter, and you can list any
#'   parameters to omit as a character vector. How does one obtain the exact,
#'   coded name of a parameter? So glad you asked. Please run something like
#'   this: \code{annotateDetails(existing_exp_details = Details, compoundID =
#'   "substrate", detail_set = "Simcyp inputs")} and the column "Detail" will
#'   list all of the coded names for the parameters that will be included in
#'   this table by default. In our example, you would specify
#'   \code{parameters_to_omit = "CLint_biliary_sub"}
#' @param parameters_to_add optionally add any parameters that weren't included
#'   by default. Acceptable input is a data.frame with the following columns:
#'   \describe{\item{SimulatorSection}{the section of the simulator, e.g.,
#'   "Phys Chem and Blood Binding", "Absorption", "Distribution", "Elimination",
#'   "Interaction", "Transport", "Trial Design", or "Population". These must match those options exactly.}
#'   \item{Detail (also ok to call this column "Parameter")}{the parameter name}
#'   \item{Value}{the value of this parameter}
#'   \item{Reference}{the reference for the parameter (optional)}}
#' @param font font to use. Default is "Arial" and any fonts available on your
#'   machine in either Word or PowerPoint should be acceptable. If you get Times
#'   New Roman in your table when you asked for something else, it means that
#'   that font isn't available or maybe wasn't spelled the way R is expecting
#'   it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
#'   the latter is listed in PowerPoint and Word.
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
#' @param column_widths optionally specify what the widths of the columns should
#'   be with a numeric vector of the widths in inches, e.g., \code{column_widths
#'   = c(1, 2, 1, 1)}
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
#'   slashes, dollar signs, or periods in the file name. If you leave off the
#'   file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#' @param separate_column_for_ADME In the table, do you want a separate column
#'   for ADME headings -- "Absoprtion", "Distribution", etc. -- or would you
#'   like the ADME headings to be included in the "Parameter" column in the
#'   table? Default is FALSE for not having a separate column. 
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # none yet

make_Simcyp_inputs_table <- function(existing_exp_details,
                                     sims_to_include = "all", 
                                     compoundID = NA, 
                                     compound = NA, 
                                     references = NA, 
                                     parameters_to_omit = NA, 
                                     parameters_to_add = NA, 
                                     separate_column_for_ADME = FALSE, 
                                     font = "Palatino Linotype", 
                                     fontsize = 11, 
                                     column_widths = NA, 
                                     save_table = NA,
                                     page_orientation = "portrait"){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   compoundID <- tolower(compoundID)
   
   if(all(is.na(compound)) & all(is.na(compoundID))){
      warning(wrapn("You have not specified anything for either compoundID or compound, so we'll set the compoundID to substrate."),
              call. = FALSE)
      compoundID <- "substrate"
   } else if(any(complete.cases(compoundID)) & 
             any(complete.cases(compound))){
      warning(wrapn("You have specificed something for both compoundID (the position in the Simulator, e.g., 'substrate') and for compound (the compound name), and we won't use both. We will only consider what you supplied for 'compound'."), 
              call. = FALSE)
      compoundID <- NA
   }
   
   if("character" %in% class(references) && str_detect(references, "\\.csv$")){
      references <- read.csv(references, na.strings = c("", "NA"))
      # references should now be a data.frame.
   } 
   
   if("logical" %in% class(references) == FALSE){
      if("data.frame" %in% class(references) == FALSE){
         warning(wrapn("You have supplied something for the argument 'references' other than a data.frame or a .csv file, which are the only options. We will leave off any references from the table."), 
                 call. = FALSE)
         references <- NA
      } else {
         # harmonizing column names
         names(references) <- tolower(names(references))
         names(references)[which(str_detect(names(references), "parameter"))] <- "Detail"
         names(references)[which(str_detect(names(references), "detail"))] <- "Detail"
         names(references)[which(str_detect(names(references), "reference|source"))] <- "Reference"
         
         if(all(c("Detail", "Reference") %in% names(references)) == FALSE){
            warning(wrapn("The data you supplied for the argument 'references' doesn't have the columns we need: 'Parameter' and 'Reference' (not case sensitive). We can't match the references you provided with the parameters from the simulation(s) and will have to omit them from the table."), 
                    call. = FALSE)
            references <- NA
         }
      }
   }
   
   if("logical" %in% class(parameters_to_omit) == FALSE){
      if("character" %in% class(parameters_to_omit) == FALSE){
         warning(wrapn("You must supply either NA or a character vector for the argument 'parameters_to_omit'. We will ignore the parameters supplied otherwise."), 
                 call. = FALSE)
         parameters_to_omit <- NA
      }
   }
   
   if("logical" %in% class(parameters_to_add) == FALSE){
      if("data.frame" %in% class(parameters_to_add) == FALSE){
         warning(wrapn("You have supplied something for the argument 'parameters_to_add' other than a data.frame, which is the only option. We will not be able to add any parameters to the table."), 
                 call. = FALSE)
         parameters_to_add <- NA
      } else {
         # harmonizing column names
         names(parameters_to_add) <- tolower(names(parameters_to_add))
         names(parameters_to_add)[which(str_detect(names(parameters_to_add), "parameter"))] <- "Detail"
         names(parameters_to_add)[which(str_detect(names(parameters_to_add), "detail"))] <- "Detail"
         names(parameters_to_add)[which(str_detect(names(parameters_to_add), "value"))] <- "Value"
         names(parameters_to_add)[which(str_detect(names(parameters_to_add), "reference|source"))] <- "Reference"
         names(parameters_to_add)[which(str_detect(names(parameters_to_add), "simulatorsection|section of model"))] <- "SimulatorSection"
         
         if(all(c("Detail", "Value") %in% names(parameters_to_add)) == FALSE){
            warning(wrapn("The data you supplied for the argument 'parameters_to_add' doesn't have the columns we need: 'Parameter' and 'Value' (not case sensitive), so we can't add them to the table."), 
                    call. = FALSE)
            parameters_to_add <- NA
         }
      }
   }
   
   if("logical" %in% class(column_widths) == FALSE){
      if("numeric" %in% class(column_widths) == FALSE){
         warning(wrapn("You have supplied something other than numeric data for the column widths, so we don't know what you want and will ignore this."), 
                 call. = FALSE)
         column_widths <- NA
      } else {
         # Making sure we have more than enough values
         column_widths <- rep(column_widths, 4)
      }
   } 
   
   if(all(is.na(column_widths))){
      if("logical" %in% class(references) == FALSE){
         if(separate_column_for_ADME){
            column_widths <- c(1, 2, 1.5, 1.5)
         } else {
            column_widths <- c(2, 1.5, 2.5)
         }
      } else {
         if(separate_column_for_ADME){
            column_widths <- c(2, 3, 1.5)
         } else {
            column_widths <- c(3, 2)
         }
      }
   }
   
   
   # Main function ------------------------------------------------------------
   
   if(any(sims_to_include != "all")){
      
      MissingSims <- setdiff(sims_to_include, 
                             existing_exp_details$MainDetails$File)
      
      if(length(MissingSims) > 0){
         
         if(all(sims_to_include %in% MissingSims)){
            stop(paste0(
               wrapn("You requested that we use the following simulation(s) for making your Simcyp inputs table:"),
               str_c(paste0("   '", sims_to_include, "'"), collapse = "\n"), "\n",  
               wrapn("but that/those simulation(s) are not present in your data, so we cannot generate your table.")), 
               call. = FALSE)
         } else {
            warning(paste0(
               wrapn("The following simulation(s), which were included in 'sims_to_include', are not present in your data and will be ignored:"),
               str_c(paste0("   '", sims_to_include, "'"), collapse = "\n")), 
               call. = FALSE)
         }
      }
      
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          sims_to_include, 
                                          "include")
   }
   
   if(nrow(existing_exp_details$MainDetails) == 0){
      stop(wrapn("There are no data present for making a Simcyp inputs table. Please check your input for 'existing_exp_details' and try again."), 
           call. = FALSE)
   }
   
   FT <- annotateDetails(existing_exp_details = existing_exp_details, 
                         compoundID = compoundID, 
                         compound = compound,
                         detail_set = "Simcyp inputs") %>% 
      mutate(Detail = sub(str_c(AllRegCompounds$Suffix, collapse = "|"), "", 
                          Detail))
   
   if(any(complete.cases(parameters_to_omit))){
      FT <- FT %>% 
         filter(!Detail %in% c(parameters_to_omit, 
                               sub(str_c(AllRegCompounds$Suffix, 
                                         collapse = "|"), "", 
                                   parameters_to_omit)))
   }
   
   if("data.frame" %in% class(references)){
      if("character" %in% class(compound)){
         # When compound was supplied, need to remove suffix from detail
         references <- references %>% 
            mutate(Detail = sub(str_c(AllRegCompounds$Suffix, 
                                      collapse = "|"), "", 
                                Detail))
      }
      
      FT <- FT %>% 
         left_join(references %>% select(Detail, Reference), 
                   by = "Detail")
   }
   
   # Dealing with possible differences in column name
   names(FT)[which(str_detect(names(FT), "All files have"))] <- "Value"
   
   # If they only supply 1 sim, then there won't be a column titled "All files
   # have ...", so accounting for that.
   if("Value" %in% names(FT) == FALSE){
      names(FT)[which(str_detect(names(FT), "\\.xlsx"))[1]] <- "Value"
   }
   
   # Removing parameters that really make no sense to include, e.g., b/c they
   # are default values that don't matter s/a interactions that would have no
   # effect.
   FT <- FT %>% 
      mutate(Omit = case_when(str_detect(Detail, "^Ki") & 
                                 !str_detect(Detail, "fu_") &
                                 Value == "1e+06" ~ TRUE, 
                              
                              str_detect(Detail, "Ind_gamma") & 
                                 Value == 1 ~ TRUE, 
                              
                              str_detect(Detail, "rUGTSystem") ~ TRUE,
                              
                              .default = FALSE)) %>% 
      filter(!Omit == TRUE) %>% select(-Omit)
   
   if("data.frame" %in% class(parameters_to_add)){
      FT <- FT %>% 
         mutate(across(.cols = everything(), 
                       .fns = as.character)) %>% 
         bind_rows(parameters_to_add %>% 
                      mutate(across(.cols = everything(), 
                                    .fns = as.character))) %>% 
         mutate(Enzyme = str_extract(Detail, 
                                     "(CYP|UGT)[1-3][ABCDEJ][1-9]{1,2}|ENZ.USER[1-9]|BCRP|OCT[12]|OAT[1-3]|OATP[12]B[1-3]|MATE1|MATE2_K|MRP[1-4]|NTCP"), 
                
                DetailType = case_when(str_detect(Detail, "^CL|Vmax|Km|Jmax|HalfLife") ~ "CL", 
                                       str_detect(Detail, "MBI") ~ "MBI", 
                                       str_detect(Detail, "Ind") ~ "Induction", 
                                       str_detect(Detail, "Ki") ~ "Inhibition", 
                                       .default = "other")) %>% 
         # Need to re-order things we add rows
         left_join(AllExpDetails %>% 
                      select(Detail, SimulatorSection, SortOrder) %>% 
                      mutate(Detail = sub(str_c(AllRegCompounds$Suffix, 
                                                collapse = "|"), "", 
                                          Detail)) %>% 
                      unique(), 
                   by = c("Detail", "SimulatorSection")) %>% 
         mutate(SimulatorSection = factor(SimulatorSection, 
                                          levels = c("Phys Chem and Blood Binding", 
                                                     "Absorption", 
                                                     "Distribution", 
                                                     "Elimination", 
                                                     "Transport", 
                                                     "Interaction", 
                                                     "Trial Design", 
                                                     "Population", 
                                                     "Other"))) %>% 
         arrange(SimulatorSection, SortOrder, DetailType, Enzyme, Detail) %>% 
         mutate(SimulatorSection = as.character(SimulatorSection)) %>% 
         select(-SortOrder, -Enzyme, -DetailType)
   }
   
   
   FT <- FT %>% 
      rename("Section of model" = SimulatorSection) %>% 
      filter(complete.cases(Value) & 
                !Detail %in% c("SimulatorVersion",
                               sort(unique(AllRegCompounds$DetailNames))))
   
   if(nrow(FT) == 0){
      warning(paste0(wrapn("There are no rows in the model summary table. This generally happens when you have different compounds in your simulations, and there are no parameters with the same value for all. To see what data this function is attempting to use for your table, please try running:"), 
                     "\n     annotateDetails(existing_exp_details = ", 
                     deparse(substitute(existing_exp_details)), 
                     ",\n                     sims_to_include = ", 
                     paste0("\"", sims_to_include, "\""), 
                     ",\n                     compoundID = ", 
                     ifelse(is.na(compoundID), compoundID, paste0("\"", compoundID, "\"")), 
                     ",\n                     compound = ", 
                     ifelse(is.na(compound), compound, paste0("\"", compound, "\"")), ")",
                     "\n\n", 
                     wrapn("Model summary tables will *only* include values present in the column titled something like 'All files have...', so if that column contains only NA's, you'll get a table with 0 rows here. We recommend checking which simulations are included and maybe limiting that with the argument 'sims_to_include`.")), 
              call. = FALSE)
      
      return()
      
   }
   
   suppressWarnings(
      FT <- FT %>% 
         left_join(
            AllExpDetails %>% 
               select(Detail, ReportTableText) %>% 
               mutate(Detail = sub(str_c(AllRegCompounds$Suffix, collapse = "|"), 
                                   "", Detail)) %>% unique(), 
            by = "Detail") %>% 
         mutate(Parameter = 
                   case_when(
                      is.na(Notes) ~ Detail,
                      
                      complete.cases(ReportTableText) & 
                         Notes != ReportTableText ~ ReportTableText, 
                      
                      .default = Notes), 
                
                Val_num = as.numeric(Value),  
                Val_num = if_else(Val_num > 100, 
                                  round(Val_num, 1), 
                                  signif(Val_num, 4)), 
                Value = ifelse(complete.cases(Val_num), 
                               as.character(Val_num), Value)) %>% 
         select("Section of model", Parameter, Value, any_of("Reference")) 
   )
   
   
   if(separate_column_for_ADME == FALSE){
      # Need to insert rows for each simulator section just above the 1st row
      # for each section. Also need them to be bold, italics, and 1 pt larger
      # than the main font size.
      ADME <- data.frame(Parameter = c("Phys Chem and Blood Binding", 
                                       "Absorption", 
                                       "Distribution", 
                                       "Elimination", 
                                       "Transport", 
                                       "Interaction", 
                                       "Trial Design", 
                                       "Population")) %>% 
         mutate(OrderSS = 1:8, 
                OrderDetail = 0, 
                # Hacking something other than a completely empty cell for value
                # so that these rows won't get vertically merged with the others
                # if there happens to be no value listed for the parameter.
                Value = "   ") %>% 
         filter(Parameter %in% FT$`Section of model`)
      
      FT <- FT %>% 
         mutate(OrderSS = factor(`Section of model`, 
                                 levels = c("Phys Chem and Blood Binding", 
                                            "Absorption", 
                                            "Distribution", 
                                            "Elimination", 
                                            "Transport", 
                                            "Interaction", 
                                            "Trial Design", 
                                            "Population")), 
                OrderSS = as.numeric(OrderSS), 
                OrderDetail = 1:nrow(.)) %>% 
         bind_rows(ADME) %>% 
         arrange(OrderSS, OrderDetail) %>% 
         select(-`Section of model`, -OrderSS, -OrderDetail) 
      
      if("data.frame" %in% class(references)){
         FT <- FT %>% 
            mutate(Reference = case_when(Parameter %in% ADME$Parameter ~ "   ", 
                                         .default = Reference))
      }
      
      MakeBold <- which(FT$Parameter %in% ADME$Parameter)
      MakeBold <- map(as.list(MakeBold), \(x) c(x, 1))
      MakeBold <- c(list(c(0, NA)), MakeBold)
      
      FT <- FT %>% 
         format_table_simple(font = font, 
                             fontsize = fontsize, 
                             bold_cells = MakeBold) %>% 
         # adding horizontal lines when there is no separate column for
         # simulator section b/c that means that there's also no shading. This
         # makes it clearer.
         flextable::border_inner_h(border = officer::fp_border(width = 0.5))
      
      if(length(MakeBold) > 1){
         for(cells in 2:length(MakeBold)){
            FT <- FT %>% 
               flextable::italic(i = MakeBold[[cells]][1], 
                                 j = MakeBold[[cells]][2]) %>% 
               flextable::fontsize(i = MakeBold[[cells]][1], 
                                   j = MakeBold[[cells]][2], 
                                   size = fontsize + 1)
         }
      }
      
   } else {
      MakeBold <- list(c(0, NA), c(NA, 1)) # default for formatTable_Simcyp
      
      FT <- FT %>% 
         format_table_simple(shading_column = "Section of model", 
                             font = font, 
                             fontsize = fontsize, 
                             bold_cells = MakeBold)
   }
   
   if("data.frame" %in% class(references)){
      if(separate_column_for_ADME){
         FT <- FT %>% 
            flextable::width(j = 1, width = column_widths[1]) %>% 
            flextable::width(j = 2, width = column_widths[2]) %>% 
            flextable::width(j = 3, width = column_widths[3]) %>% 
            flextable::width(j = 4, width = column_widths[4]) %>% 
            flextable::merge_v(j = 1) %>% 
            flextable::merge_v(j = 4) # merge any replicated references
      } else {
         FT <- FT %>% 
            flextable::width(j = 1, width = column_widths[1]) %>% 
            flextable::width(j = 2, width = column_widths[2]) %>% 
            flextable::width(j = 3, width = column_widths[3]) %>% 
            flextable::merge_v(j = 3) # merge any replicated references
      }
   } else {
      if(separate_column_for_ADME){
         FT <- FT %>% 
            flextable::width(j = 1, width = column_widths[1]) %>% 
            flextable::width(j = 2, width = column_widths[2]) %>% 
            flextable::width(j = 3, width = column_widths[3]) %>% 
            flextable::merge_v(j = 1) 
      } else {
         FT <- FT %>% 
            flextable::width(j = 1, width = column_widths[1]) %>% 
            flextable::width(j = 2, width = column_widths[2]) 
      }
   }
   
   FT <- FT %>% 
      flextable::fix_border_issues()
   
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      title_document <- "Model Summary"
      table_caption <- NA
      
      formatTable_Simcyp(DF = FT, 
                         save_table = save_table, 
                         page_orientation = page_orientation, 
                         title_document = title_document, 
                         table_caption = table_caption)
   }
   
   return(FT)
   
}


