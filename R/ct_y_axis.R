#' Set up y axis in a conc-time plot
#'
#' This function is specifically for setting options for the y axis in a
#' SimcypConsultancy package concentration-time graph and is NOT meant to be
#' called on its own. The function \code{\link{scale_y_conc}} is more generic
#' and \emph{is} meant to be called on its own, so check that out if you're
#' looking to scale y axes quickly and easily and similar to how they look in
#' SimcypConsultancy concentration-time plots.
#'
#' @param y_compound_info a data.frame containing columns for subsection_ADAM
#'   and Conc_units
#' @param IsADAM T or F for whether ADAM model used
#' @param subsection_ADAM which ADAM-model subsection the data include
#' @param is_enz_plot T or F for whether this was a plot of enzyme abundance
#'
#' @return

ct_y_axis <- function(y_compound_info, 
                      IsADAM = FALSE, 
                      subsection_ADAM = "free compound in lumen", 
                      is_enz_plot = FALSE, 
                      prettify_compound_names = TRUE){
    
    # y axis labels --------------------------------------------------------
    
    if(is_enz_plot){
        ObsConcUnits <- "Relative abundance"
    } else {
        ObsConcUnits <- sort(unique(y_compound_info$Conc_units))
    }
    
    if(any(IsADAM, na.rm = TRUE)){
        
        # # ADAM options available (this is for my reference and was copied from ct_plot.R)
        # ADAMoptions <- c("undissolved compound", "enterocyte concentration",
        #                  "free compound in lumen", "total compound in lumen",
        #                  "Heff", "absorption rate",
        #                  "unreleased compound in faeces", 
        #                  "dissolved compound", "luminal CLint", 
        #                  "dissolution rate of solid state", 
        #                  "cumulative fraction of compound absorbed", 
        #                  "cumulative fraction of compound dissolved")
        
        if(class(prettify_compound_names) == "logical"){
            CompoundLab <- ifelse(prettify_compound_names == TRUE, 
                                  prettify_compound_name(unique(y_compound_info$Compound)), 
                                  unique(y_compound_info$Compound))
        } else {
            CompoundLab <- str_comma(
                unique(
                    prettify_compound_names[
                        prettify_compound_names %in%
                            unique(y_compound_info$Compound[y_compound_info$CompoundID == "substrate"])]), 
                conjunction = "or")
        }
        
        ylab1 <- 
            switch(subsection_ADAM, 
                   "dissolved compound" = 
                       paste("Dissolved", CompoundLab, "in", unique(y_compound_info$Tissue)),
                   "undissolved compound" = 
                       paste0("Undissolved ", CompoundLab, " in ", unique(y_compound_info$Tissue)),
                   "enterocyte concentration" = 
                       # bquote(atop("Enterocyte concentration of", bquote(CompoundLab))), # can't get this to work
                       paste("Enterocyte concentration of", CompoundLab),
                   "free compound in lumen" = 
                       paste0("Free ", CompoundLab, " in lumen"),
                   "total compound in lumen" = 
                       paste0("Total ", CompoundLab, " in lumen"),
                   "Heff" = bquote("Particle"~H[eff]),
                   "absorption rate" = 
                       paste0("Absorption rate of ", CompoundLab, " in ", unique(y_compound_info$Tissue)),
                   "unreleased compound in faeces" = 
                       paste("Unreleased", CompoundLab, "in faeces"),
                   "luminal CLint" = bquote("Luminal"~CL[int]), 
                   "dissolution rate of solid state" = 
                       paste0("Dissolution rate of ", CompoundLab, " in ", unique(y_compound_info$Tissue)), 
                   "cumulative fraction of compound absorbed" =
                       paste0("Cumulative fraction of ", CompoundLab, " absorbed"), 
                   "cumulative fraction of compound dissolved" =
                       paste0("Cumulative fraction of ", CompoundLab, " dissolved")) 
        
        # PossConcUnits is slightly different between ADAM and non-ADAM tissues,
        # so do NOT interchange them in the code.
        PossConcUnits <- list("mg/mL" = "(mg/mL)",
                              "µg/L" = bquote("("*"\u03bc"*g*"/"*L*")"),
                              "µg/mL" = bquote("("*"\u03bc"*g*"/"*mL*")"),
                              "ng/mL" = "(ng/mL)",
                              "ng/L" = "(ng/L)",
                              "µM" = bquote("("*"\u03bc"*M*")"),
                              "µm" = bquote("("*"\u03bc"*m*")"),
                              "nM" = "(nM)",
                              "mM" = "(mM)",
                              "mg" = "(mg)",
                              "µg" = bquote("("*"\u03bc"*"g)"),
                              "ng" = "(ng)",
                              "mg/h" = "(mg/h)",
                              "mg/L" = bquote("("*"\u03bc"*g*"/"*mL*")"),
                              "mL" = "(mL)", 
                              "mmol" = "(mmol)", 
                              "µmol" = bquote("("*"\u03bc"*"mol)"),
                              "nmol" = "(nmol)")
        
        ylab2 <- PossConcUnits[[unique(y_compound_info$Conc_units)]]
        
        if(subsection_ADAM %in% c("cumulative fraction of compound absorbed", 
                                  "cumulative fraction of compound dissolved")){
            ylab <- bquote(bold(.(ylab1)))
            
        } else {
            ylab <- bquote(bold(.(ylab1)) ~ bold(.(ylab2)))
        }
        
    } else {
        
        # PossConcUnits is slightly different between ADAM and non-ADAM tissues,
        # so do NOT interchange them in the code.
        PossConcUnits <- list("mg/mL" = "Concentration (mg/mL)",
                              "µg/L" = bquote(Concentration~"("*"\u03bc"*g*"/"*L*")"),
                              "µg/mL" = bquote(Concentration~"("*"\u03bc"*g*"/"*mL*")"),
                              "ng/mL" = "Concentration (ng/mL)",
                              "ng/L" = "Concentration (ng/L)",
                              "µM" = bquote(Concentration~"("*"\u03bc"*M*")"),
                              "nM" = "Concentration (nM)",
                              "mg" = "Amount (mg)",
                              "mg/h" = "Absorption rate (mg/h)",
                              "mg/L" = bquote(Concentration~"("*"\u03bc"*g*"/"*mL*")"),
                              "mL" = "Volume (mL)",
                              "PD response" = "PD response",
                              "Relative abundance" = "Relative abundance")
        
        ylab <- PossConcUnits[[ObsConcUnits]]
        
    }
    
    Out <- ylab
    
    return(Out)
    
}


