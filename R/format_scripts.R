#' INTERNAL: Format common PBPK model parameters to have appropriate subscripts
#' and superscripts.
#'
#' @param DF a data.frame with a column containing, e.g., "ka (h^-1)", "fu,p",
#'   or other PBPK model parameters with syntax that matches what's in the
#'   column "Notes" in the object AllExpDetails, which is included with this
#'   package and is used extensively for figuring out how simulations were set
#'   up.
#' @param parameter_column the name of the column in DF that contains your PK
#'   parameters, unquoted. For example, supply \code{parameter_column = Notes}
#' @param value_column optionally supply the name of the column in DF that
#'   contains values for PK parameters, unquoted.
#' @param remove_compound_suffix TRUE (default) or FALSE to remove the suffix
#'   that shows up in many PBPK elimination and interaction parameters to
#'   indicate which compound ID the parameter refers to. For example, "Vsac_sub"
#'   will be just "Vsac" and "Peff_human_inhib" will become "Peff,human" with
#'   the "eff,human" as a subscript. You should ONLY do this if your whole table
#'   refers to the same compound; otherwise, it will be completely unclear
#'   what's what.
#'
#' @return a flextable
#' @examples
#' DF <- data.frame(Parameter = c("MW", "ka (h^-1)",
#'                                "fu,p", "fu,gut", "fu,mic CYP3A4"),
#'                   Value = c(350, 0.5, 0.01, 0.8, 1))
#'
#' format_scripts(DF = DF)
#' 
format_scripts <- function(DF, 
                           parameter_column, 
                           value_column, 
                           remove_compound_suffix = TRUE){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   
   # Main body of function ----------------------------------------------------
   
   # Setting up for nonstandard evaluation
   parameter_column <- rlang::enquo(parameter_column)
   value_column <- rlang::enquo(value_column)
   
   OrigColNames <- names(DF)
   
   if("Parameter" %in% names(DF) & 
      as_label(parameter_column) != "Parameter"){
      DF <- DF %>% rename(Parameter_orig = Parameter)
   }
   
   
   ## Preliminary text tidying for value column --------------------------------
   
   if(as_label(value_column) != "<empty>"){
      
      if(as_label(value_column) %in% names(DF) == FALSE){
         warning(wrapn(paste0("You specified the value column name to be `", 
                              as_label(value_column), 
                              "`, but that column is not present in DF, so we will skip this.")), 
                 call. = FALSE)
         ValueColIncluded <- FALSE
      } else {
         ValueColIncluded <- TRUE
         DF <- DF %>% mutate(Value = {{value_column}})
      }
   } else {
      ValueColIncluded <- FALSE
   }
   
   
   ## Preliminary text tidying for parameter column ---------------------------
   
   if(as_label(parameter_column) != "<empty>"){
      
      if(as_label(parameter_column) %in% names(DF) == FALSE){
         warning(wrapn(paste0("You specified the parameter column name to be `", 
                              as_label(parameter_column), 
                              "`, but that column is not present in DF. We cannot format the subscripts, superscripts, or special characters in your data.frame and will only be able to return a generic flextable object.")), 
                 call. = FALSE)
         
         return(FT)
      }
      
      DF <- DF %>% mutate(Parameter = {{parameter_column}})
      
      if(remove_compound_suffix){
         DF <- DF %>%
            mutate(Parameter = str_replace(Parameter, "_sub$|_inhib$|_met1$|_met2$|_secmet$|_inhib2$", ""))
      }
      
      DF <- DF %>%
         mutate(Parameter = str_replace_all(Parameter, "_", " "), 
                Parameter = str_replace_all(Parameter, "ABCB1 P(-| )gp MDR1", "P-gp"), 
                Parameter = str_replace_all(Parameter, "\\.\\.1", ""), # sometimes shows up for Km and Vmax
                Parameter = str_replace_all(Parameter, "Pathway[0-9]( )?", ""), 
                Parameter = str_replace_all(Parameter, "star1", "*1"), 
                Parameter = str_replace_all(Parameter, "fu mic", "fu,mic"), 
                Parameter = str_replace_all(Parameter, "fu inc", "fu,inc"), 
                Parameter = str_replace(Parameter, "uL min mg protein", "(uL/min/mg protein)"), 
                Parameter = str_replace(Parameter, "uL min pmol", "(uL/min/pmol)"), 
                Parameter = str_replace(Parameter, "Gut", "gut"), 
                Parameter = str_replace(Parameter, "Kidney", "kidney"), 
                Parameter = str_replace(Parameter, "Liver", "liver"), 
                Parameter = str_replace_all(Parameter, "CYP2B6\\.1", "CYP2B6"), # not sure why this looks like this
                Parameter = case_when(str_detect(Parameter, "Ki fu,mic") ~ 
                                         paste0(str_replace(Parameter, "Ki ", ""), 
                                                " (Ki)"), 
                                      
                                      str_detect(Parameter, "CLint") & 
                                         str_detect(Parameter, "rUGTScalar") ~ 
                                         str_replace(Parameter, "CLint UGT", "UGT"), 
                                      
                                      .default = Parameter), 
                Parameter = str_replace(Parameter, "rUGTScalar", "scalar for"), # This line must come after the one checking for both CLint and rUGTScalar!
                Parameter = case_match(Parameter, 
                                       "CLint AddHLM" ~ "CLint,additional hepatic", 
                                       "CLint biliary" ~ "CLint,biliary", 
                                       .default = Parameter))
      
      # Can't get rename to work w/NSE, so renaming parameter_column back to
      # original value in steps.
      DFtemp <- DF
      names(DFtemp)[which(names(DFtemp) == "Parameter")] <- as_label(parameter_column)
      names(DFtemp)[which(names(DFtemp) == "Parameter_orig")] <- "Parameter"
      
      
      ## Converting to flextable --------------------------------------------
      
      ### Formatting parameter column -----------------------------------------
      
      FT <- flextable::flextable(DFtemp) %>% 
         
         # First, all the places where we replace the ENTIRE cell contents. These
         # don't need to be run in a loop b/c we replace EVERYTHING with the same
         # value, so, even if there were more than one place with, e.g., "ka
         # (h^-1)", ALL of those places would be replaced with the same thing.
         
         flextable::compose(i = which(DF$Parameter == "ka (h^-1)"), 
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "k", flextable::as_sub("a"), " (h", flextable::as_sup("-1"), ")")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "kin,SAC (h-1)"), 
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "k", flextable::as_sub("in,SAC"), " (h", flextable::as_sup("-1"), ")")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "kout,SAC (h-1)"), 
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "k", flextable::as_sub("out,SAC"), " (h", flextable::as_sup("-1"), ")")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "fu,p"), 
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "fu", flextable::as_sub("p"))) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Papp determined in Caco-2 cells (10^-6 cm/s)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "P", flextable::as_sub("app"), 
                               " determined in Caco-2 cells (10",
                               flextable::as_sup("-6"), 
                               " cm/s)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Papp for the reference compound (10^-6 cm/s)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "P", flextable::as_sub("app"), 
                               " for the reference compound (10", 
                               flextable::as_sup("-6"), 
                               " cm/s)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Particle size distribution D10 (um)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "Particle size distribution D10 (\u03BCm)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Particle size distribution D50 (um)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "Particle size distribution D50 (\u03BCm)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Particle size distribution D90 (um)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "Particle size distribution D90 (\u03BCm)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Peff,human (10^-4 cm/s)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "P", flextable::as_sub("eff,human"), 
                               " (10", flextable::as_sup("-4"), 
                               " cm/s)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "tlag (h)"), 
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "t", flextable::as_sub("lag"), " (h)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Vsac volume (L/kg)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "V", flextable::as_sub("SAC"), 
                               " volume (L/kg)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Vss input (L/kg)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "V", flextable::as_sub("SS"), 
                               " input (L/kg)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Vss prediction method used"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "V", flextable::as_sub("SS"), 
                               " prediction method used")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Vss used in the simulation (L/kg)"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "V", flextable::as_sub("SS"), 
                               " used in the simulation (L/kg)")) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Input type for Vss"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "Input type for V", flextable::as_sub("SS"))) %>% 
         
         flextable::compose(i = which(DF$Parameter == "Calibrator compound used for calculating Papp"),
                            j = which(names(DF) == "Parameter"), 
                            part = "body", 
                            value = flextable::as_paragraph(
                               "Calibrator compound used for calculating P",
                               flextable::as_sub("app")))
      
      # Second, all the places where there might be multiple matches b/c we're only
      # matching, e.g., the 1st part of the string and then need to fill in the
      # rest of the string with variable text.
      MultPieceVars <- c("Additional HLM CLint (uL/min/mg protein)", 
                         "fu,gut", "^fu,mic", "^fu,inc", "^Ki fu,mic", "^Ki",
                         "uM", # NB: "uM" MUST come before "Km \\(uM" or it will overwrite the latter.
                         "Km \\(uM", "Vmax", "Jmax", 
                         "^IndC50", "^IndMax", "^Ind gamma", 
                         "^Ind fu,inc", "uL", "Qgut", 
                         "^Kapp", "^kinact", "uL/min/10\\^6")
      
      for(mpv in MultPieceVars){
         
         rows <- which(str_detect(DF$Parameter, mpv))
         
         for(r in rows){
            
            Snips <- str_split_1(DF$Parameter[r], pattern = mpv)
            
            FT <- FT %>% 
               flextable::compose(i = r, 
                                  j = which(names(DF) == "Parameter"), 
                                  part = "body", 
                                  value = switch(
                                     mpv,
                                     
                                     "fu,gut" = 
                                        flextable::as_paragraph(
                                           Snips[1], 
                                           "fu", flextable::as_sub("gut"), 
                                           Snips[2]), 
                                     
                                     "^fu,mic" = 
                                        flextable::as_paragraph(
                                           "fu", flextable::as_sub("mic"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "^fu,inc" = 
                                        flextable::as_paragraph(
                                           "fu", flextable::as_sub("inc"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "^Ki fu,mic" = 
                                        flextable::as_paragraph(
                                           "fu", flextable::as_sub("mic"), 
                                           gsub(mpv, "", 
                                                DF[r, which(names(DF) == "Parameter")]), 
                                           "(K", flextable::as_sub("i"), ")"), 
                                     
                                     "^Ind fu,inc" = 
                                        flextable::as_paragraph(
                                           "fu", flextable::as_sub("inc"), 
                                           gsub("^Ind fu,inc", "", 
                                                DF[r, which(names(DF) == "Parameter")]), 
                                           " (induction)"), 
                                     
                                     "^IndC50" = 
                                        flextable::as_paragraph(
                                           "IndC", flextable::as_sub("50"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "^IndMax" = 
                                        flextable::as_paragraph(
                                           "Ind", flextable::as_sub("max"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "^Ind gamma" = 
                                        flextable::as_paragraph(
                                           "\u03B3 (induction)", 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "^Ki" = 
                                        flextable::as_paragraph(
                                           "K", flextable::as_sub("i"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "Km \\(uM" = 
                                        # flextable::as_paragraph(
                                        #    "K", flextable::as_sub("M"), 
                                        #    sub(mpv, "", 
                                        #        DF[r, which(names(DF) == "Parameter")])),
                                        flextable::as_paragraph(
                                           Snips[1], 
                                           "K", flextable::as_sub("M"), 
                                           " (\u03BCM", Snips[2]), 
                                     
                                     "^Kapp" = 
                                        flextable::as_paragraph(
                                           "K", flextable::as_sub("app"), 
                                           Snips[2], " (\u03BCM)"), 
                                     
                                     "^kinact" = 
                                        flextable::as_paragraph(
                                           "k", flextable::as_sub("inact"), 
                                           Snips[2], " (h", 
                                           flextable::as_sup("-1"), 
                                           ")"), 
                                     
                                     "Qgut" = 
                                        flextable::as_paragraph(
                                           "Q", flextable::as_sub("gut"), 
                                           sub(mpv, "", 
                                               DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "Vmax" = 
                                        # flextable::as_paragraph(
                                        #    "V", flextable::as_sub("max"), 
                                        #    sub(mpv, "", 
                                        #        DF[r, which(names(DF) == "Parameter")])),
                                        flextable::as_paragraph(
                                           Snips[1], 
                                           "V", flextable::as_sub("max"), 
                                           Snips[2]), 
                                    
                                     "Jmax" = 
                                        flextable::as_paragraph(
                                           Snips[1], 
                                           "J", flextable::as_sub("max"), 
                                           Snips[2]), 
                                     
                                     "uL/min/10\\^6" = 
                                        flextable::as_paragraph(
                                           Snips[1], 
                                           "\u03BCL/min/10", flextable::as_sup("6"), 
                                           Snips[2]), 
                                     
                                     # Special characters
                                     "uL" = flextable::as_paragraph(
                                        sub("uL", "\u03BCL", 
                                            DF[r, which(names(DF) == "Parameter")])), 
                                     
                                     "uM" = flextable::as_paragraph(
                                        sub("uM", "\u03BCM", 
                                            DF[r, which(names(DF) == "Parameter")]))
                                  ))
         }
      }
      
      ### Formatting value column -----------------------------------------
      
      if(ValueColIncluded){
         
         FT <- FT %>% 
            flextable::compose(i = which(DF$Value == "1st order"), 
                               j = which(names(DF) == "Value"), 
                               part = "body", 
                               value = flextable::as_paragraph(
                                  "1", flextable::as_sup("st"), 
                                  " order")) 
         
      }
      
   } else {
      FT <- flextable::flextable(DF)
   }
   
   
   ## Column name formatting -----------------------------------------------
   
   ColNames <- names(DF)
   
   ### Special characters for column names ---------------------------------
   
   for(char in c("uL", "umol", "uM")){
      for(cols in which(str_detect(ColNames, char))){
         
         FT <- FT %>% 
            flextable::compose(part = "header",
                               i = 1,
                               j = cols,
                               value = flextable::as_paragraph(
                                  sub(char, 
                                      switch(char, 
                                             "uL" = "\u03BCL", 
                                             "umol" = "\u03BCmol", 
                                             "uM" = "\u03BCM"), 
                                      ColNames[cols])))
         
      }
   }
   
   ### subscripts for column names ------------------------------------------
   
   ColNames <- sub("AUCt( |$)", "AUC~t~ ", ColNames)
   ColNames <- sub("AUCinf( |$)", "AUC~inf~ ", ColNames)
   ColNames <- sub("AUCt$", "AUC~t~", ColNames)
   ColNames <- sub("AUCtau", "AUC~tau~", ColNames)
   ColNames <- sub("AUC24h,ss", "AUC~24h,ss~", ColNames)
   ColNames <- sub("Cmax,ss", "C~max,ss~", ColNames)
   ColNames <- sub("Cmax", "C~max~", ColNames)
   ColNames <- sub("Cmin", "C~min~", ColNames)
   ColNames <- sub("Cavg,u,ss", "C~avg,u,ss~", ColNames)
   ColNames <- sub("tmax", "t~max~", ColNames)
   ColNames <- sub("tlag", "t~lag~", ColNames)
   ColNames <- sub(" ka ", " k~a~ ", ColNames)
   ColNames <- sub("^ka ", "k~a~ ", ColNames)
   ColNames <- sub(" fa ", " f~a~ ", ColNames)
   ColNames <- sub("^fa ", "f~a~ ", ColNames)
   ColNames <- sub(" fh ", " f~h~ ", ColNames)
   ColNames <- sub("^fh ", "f~h~ ", ColNames)
   ColNames <- sub(" fg ", " f~g~ ", ColNames)
   ColNames <- sub("^fg ", "f~g~ ", ColNames)
   ColNames <- sub("Indmax", "Ind~max~", ColNames)
   ColNames <- sub("Emax", "E~max~", ColNames)
   ColNames <- sub("IndC50", "IndC~50~", ColNames)
   ColNames <- sub("EC50", "EC~50~", ColNames)
   
   ColNames_sub <- str_split(ColNames, pattern = "~", simplify = T)
   
   if(ncol(ColNames_sub) == 3){
      for(cols in which(ColNames_sub[,2] != "")){
         FT <- FT %>% 
            flextable::compose(part = "header",
                               i = 1,
                               j = cols,
                               value = flextable::as_paragraph(
                                  ColNames_sub[cols, 1],
                                  flextable::as_sub(ColNames_sub[cols, 2]), 
                                  ColNames_sub[cols, 3]))
      }
   }
   
   ### superscripts for column names --------------------------------------
   
   # These should generally already have x^2^ in the column name to work. 
   ColNames_sup <- str_split(ColNames, pattern = "\\^", simplify = T)
   
   if(ncol(ColNames_sup) == 3){
      for(cols in which(ColNames_sup[,2] != "")){
         FT <- FT %>% 
            flextable::compose(part = "header",
                               i = 1,
                               j = cols,
                               value = flextable::as_paragraph(
                                  ColNames_sup[cols, 1],
                                  flextable::as_sup(ColNames_sup[cols, 2]), 
                                  ColNames_sup[cols, 3]))
      }
   }
   
   
   # Return -----------------------------------------------------------------
   
   return(FT)
   
}


