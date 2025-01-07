#' Plot the dosing regimens used in simulations
#'
#' \code{dosing_regimen_plot} creates a graph of the dosing regimen used in the
#' simulations provided. UNDER CONSTRUCTION.
#'
#' @param existing_exp_details output from \code{\link{extractExpDetails}} or
#'   \code{\link{extractExpDetails_mult}}.
#' @param sims_to_include optionally specify which simulation files you'd like
#'   to include in the annotated output. Acceptable input:
#'
#'   \describe{\item{NA (default)}{get all the simulations included in
#'   \code{existing_exp_details}}
#'
#'   \item{a character vector of the file names you want}{The items in the character
#'   vector must \emph{exactly} match file names in the column "File" of the
#'   "MainDetails" item in \code{existing_exp_details}, including the ".xlsx" or ".db"
#'   file extension}}
#' @param break_up_by The output graphs will all be broken up by the simulation
#'   file name and also by either the compound name (specify \code{break_up_by =
#'   "Compound"}, the default) or by the compound ID (specify
#'   \code{break_up_by = "CompoundID"} to break up the graphs by whether the
#'   compound was the substrate, the inhibitor 1, etc. in the simulation).
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' # UNDER CONSTRUCTION 
#' 
dosing_regimen_plot <- function(existing_exp_details, 
                                sims_to_include = NA, 
                                break_up_by = "Compound"){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
           call. = FALSE)
   }
   
   break_up_by <- tolower(break_up_by)
   
   if(break_up_by %in% c("compound", "compoundid") == FALSE){
      warning(SimcypConsultancy:::wrapn("You have supplied something for the argument 'break_up_by' other than Compound or CompoundID, which are the only possible options. We'll set 'break_up_by' to Compound, the default."), 
              call. = FALSE)
      break_up_by <- "compound"
   } 
   
   existing_exp_details <- harmonize_details(existing_exp_details)
   
   if(nrow(existing_exp_details$Dosing) == 0){
      stop(wrapn("There is not the right type of dosing information present to make this graph. Please try re-running 'extractExpDetails_mult' to get all the dosing information you need."), 
           call. = FALSE)
   }
   
   # Main body of function -------------------------------------------------
   
   if(any(complete.cases(sims_to_include))){
      existing_exp_details <- filter_sims(existing_exp_details, 
                                          which_sims = sims_to_include, 
                                          include_or_omit = "include")
   }
   
   if(nrow(existing_exp_details$Dosing) == 0){
      stop(wrapn("We cannot find any dosing information for the simulation you requested, so we cannot make your graph."), 
           call. = FALSE)
   }
   
   Dosing <- existing_exp_details$Dosing
   
   RouteColors <- c("Oral" = "dodgerblue4", 
                    "i.v. bolus" = "#E41A1C", 
                    "iv. infusion" = "#91429D", 
                    "Dermal" = "seagreen", 
                    "Inhaled" = "#5ECCF3", 
                    "Long-Acting-Injectable" = "orange", 
                    "IntraVaginal" = "#08E6D1", 
                    "Rectal" = "#6F4C29", 
                    "Synovial Joint" = "#E0E006", 
                    "Other site" = "gray20", 
                    "Subcutaneous" = "#F51B7E", 
                    "Custom" = "black")
   
   G <- ggplot(Dosing, aes(x = Time, xend = Time,
                           y = 0, yend = Dose, 
                           color = DoseRoute)) +
      geom_segment(linewidth = 1) +
      scale_color_manual(values = RouteColors) +
      labs(color = "Dose route")
   
   if(break_up_by == "compound"){
      G <- G +
         ggh4x::facet_grid2(Compound ~ File, scales = "free", 
                            axes = "all", switch = "y")
   } else if(break_up_by == "compoundid"){
      G <- G + 
         ggh4x::facet_grid2(CompoundID ~ File, scales = "free", 
                            axes = "all", switch = "y")
   }
   
   suppressMessages(
      G <- G + 
         # scale_y_continuous(limits = c(0, max(Dosing$Dose)), 
         #                    expand = expansion(mult = c(0, 0.05))) +
         scale_x_continuous(limits = c(0, max(Dosing$Time))) +
         xlab("Time (h)") +
         ylab("Dose (mg)") +
         ggtitle("Dosing regimens") +
         scale_x_time() +
         theme_consultancy(border = TRUE) +
         theme(legend.position = "bottom", 
               legend.justification = c(0, 0), 
               strip.placement = "outside")
   )
   
   return(G)
   
}



