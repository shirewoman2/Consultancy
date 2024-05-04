#' Make a set of plots showing the demographics used in a set of simulations and
#' optionally compare them to observed demographics. \strong{UNDER
#' CONSTRUCTION}.
#'
#' @param demog_dataframe the output from running \code{\link{extractDemog}}.
#'   Optionally (and we recommend) with added observed demographic data, perhaps
#'   from observed overlay XML files.
#' @param sim_data_file the simulator output Excel file to use. If left as NA,
#'   all the files in demog_dataframe will be included.
#' @param plot_title title to use on the plots
#' @param variables variables to include. We're starting with a limited set:
#'   Age, Weight_kg, Height_cm, HSA_gL, AGP_gL, Sex, BMI_kgm2, and RenalFunction
#' @param obs_alpha how transparent to make the observed data, with 0 being
#'   completely transparent and invisible so I don't know why you'd want that
#'   but, hey, you do you, dude, to 1, which is fully opaque.
#' @param sim_alpha how transparent to make the simulated data. See notes on the
#'   obs_alpha argument.
#'
#' @return a set of graphs. This does not yet save the graphs for you, so you'll
#'   need to run ggsave(...) to do that.
#' @export
#'
#' @examples
#' # none yet
demog_plot <- function(demog_dataframe, 
                       sim_data_file = NA, 
                       plot_title = "Demographics", 
                       variables = NA, 
                       obs_alpha = 0.8, 
                       sim_alpha = 0.4){
   
   if(all(is.na(sim_data_file)) == FALSE){
      Demog_sub <- demog_dataframe %>% filter(File == sim_data_file)
   } else {
      Demog_sub <- demog_dataframe
   }
   
   Demog_sub <- Demog_sub %>% 
      mutate(SorO = ifelse(Simulated == TRUE, 
                           "Simulated", "Observed")) %>% unique()
   
   YLabs <- c("Age" = "Age (years)", 
              "Weight_kg" = "Weight (kg)", 
              "Height_cm" = "Height (cm)", 
              "HSA_gL" = "HSA (g/L)", 
              "AGP_gL" = "AGP (g/L)", 
              "Sex" = "Sex", 
              "BMI_kgm2" = "BMI (kg/m2)", 
              "RenalFunction" = "Renal function")
   
   if(all(is.na(variables)) == FALSE){
      YLabs <- YLabs[YLabs %in% variables]
   }
   
   subplotfun <- function(Var){
      names(Demog_sub)[names(Demog_sub) == Var] <- "MyVar"
      
      ggplot(Demog_sub, aes(x = Age, y = MyVar, color = SorO, 
                            alpha = SorO)) +
         geom_point() +
         scale_alpha_manual(values = c("Observed" = obs_alpha, 
                                       "Simulated" = sim_alpha)) +
         scale_color_brewer(palette = "Set1") +
         labs(color = NULL, alpha = NULL) +
         ylab(YLabs[Var]) +
         xlab("Age (years)") +
         theme_consultancy()  
   }
   
   MyGraphs <- list()
   
   for(yy in names(YLabs)){
      if(yy == "Sex"){
         MyGraphs[[yy]] <- free(
            ggplot(Demog_sub, 
                   aes(x = Age, y = SorO, fill = SorO)) +
               facet_grid(Sex ~ ., switch = "y") +
               geom_violin(alpha = 0.7, show.legend = FALSE) +
               ylab("Sex") +
               xlab("Age (years)") +
               scale_fill_brewer(palette = "Set1") +
               theme_consultancy() + 
               theme(legend.position = "none", 
                     strip.placement = "outside"))
      } else if(yy == "Age"){
         MyGraphs[[yy]] <- ggplot(Demog_sub,
                                  aes(x = Age, fill = SorO)) +
            geom_density(alpha = 0.5, show.legend = FALSE) +
            ylab("Distribution") +
            xlab("Age (years)") +
            scale_fill_brewer(palette = "Set1") +
            theme_consultancy() + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
      } else {
         MyGraphs[[yy]] <- subplotfun(yy)
      }
   }
   
   wrap_plots(MyGraphs) +
      plot_layout(guides = "collect") + 
      plot_annotation(title = plot_title) & 
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5, 
                                      face = "bold"), 
            legend.position = "bottom")
   
}
