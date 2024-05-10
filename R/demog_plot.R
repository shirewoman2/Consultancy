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
#'   "Age", "Weight_kg", "Height_cm", "Height vs Weight", "HSA_gL", "AGP_gL",
#'   "Sex", "BMI_kgm2", and "RenalFunction". If you want only a subset of those,
#'   list them in a character vector, e.g., \code{variables = c("Age",
#'   "Height_cm", "Weight_kg")}. Plots will be in the order listed.
#' @param obs_alpha how transparent to make the observed data, with 0 being
#'   completely transparent and invisible so I don't know why you'd want that
#'   but, hey, you do you, dude, to 1, which is fully opaque.
#' @param sim_alpha how transparent to make the simulated data. See notes on the
#'   obs_alpha argument.
#' @param ncol optionally specify the number of columns. If left as NULL, a
#'   reasonable guess will be used.
#' @param nrow optionally specify the number of rows. If left as NULL, a
#'   reasonable guess will be used.
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
                       sim_alpha = 0.4, 
                       ncol = NULL, 
                       nrow = NULL, 
                       facet_by_sex = FALSE){
   
   if(facet_by_sex & length(unique(demog_dataframe$Sex)) == 1){
      warning("You requested that we facet the graphs by sex, but there's only one sex in your data. We will not be able to do this.\n", 
              call. = FALSE)
   }
   
   if(all(is.na(sim_data_file)) == FALSE){
      Demog_sub <- demog_dataframe %>% filter(File == sim_data_file)
   } else {
      Demog_sub <- demog_dataframe
   }
   
   Demog_sub <- Demog_sub %>% 
      mutate(SorO = ifelse(Simulated == TRUE, 
                           "Simulated", "Observed")) %>% unique()
   
   if("Sex" %in% names(Demog_sub) == FALSE){
      Demog_sub$Sex <- "unknown"
   }
   
   YLabs <- c("Age" = "Age (years)", 
              "Weight_kg" = "Weight (kg)", 
              "Height_cm" = "Height (cm)", 
              "HSA_gL" = "HSA (g/L)", 
              "AGP_gL" = "AGP (g/L)", 
              "Sex" = "Sex", 
              "BMI_kgm2" = "BMI (kg/m2)", 
              "RenalFunction" = "Renal function")
   
   Graphs <- variables
   variables <- setdiff(variables, c("Height vs Weight")) # FIXME - Allow for flexiblity in case for all of these
   
   if(all(is.na(variables)) == FALSE){
      YLabs <- YLabs[variables]
   } else {
      Graphs <- names(YLabs)
   }
   
   subplotfun <- function(Var){
      names(Demog_sub)[names(Demog_sub) == Var] <- "MyVar"
      
      G <- ggplot(Demog_sub, aes(x = Age, y = MyVar, shape = Sex, 
                            color = SorO, alpha = SorO)) +
         geom_point() +
         scale_alpha_manual(values = c("Observed" = obs_alpha, 
                                       "Simulated" = sim_alpha)) +
         scale_color_brewer(palette = "Set1") +
         labs(color = NULL, alpha = NULL) +
         ylab(YLabs[Var]) +
         xlab("Age (years)") +
         theme_consultancy() 
      
      if(length(unique(Demog_sub$Sex)) == 1){
         G <- G + guides(shape = "none") 
      }
      
      return(G)
   }
   
   MyGraphs <- list()
   
   for(yy in Graphs){
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
         MyGraphs[[yy]] <- 
            ggplot(Demog_sub, aes(x = Age, shape = Sex, fill = SorO)) +
            geom_density(alpha = 0.5, show.legend = FALSE) +
            ylab("Distribution") +
            xlab("Age (years)") +
            scale_fill_brewer(palette = "Set1") +
            theme_consultancy() + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
         
         if(length(unique(Demog_sub$Sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
         
      } else if(yy == "Height vs Weight"){
         MyGraphs[[yy]] <- 
            ggplot(Demog_sub, aes(x = Height_cm, y = Weight_kg, shape = Sex, 
                                  color = SorO, alpha = SorO)) +
            geom_point() + 
            scale_alpha_manual(values = c("Observed" = obs_alpha, 
                                          "Simulated" = sim_alpha)) +
            scale_color_brewer(palette = "Set1") +
            labs(color = NULL, alpha = NULL) +
            ylab("Weight (kg)") +
            xlab("Height (cm)") +
            theme_consultancy()
         
         if(length(unique(Demog_sub$Sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
         
      } else {
         MyGraphs[[yy]] <- subplotfun(yy)
      }
      
      if(facet_by_sex & yy != "Sex"){
         MyGraphs[[yy]] <- MyGraphs[[yy]] + 
            facet_grid(Sex ~ ., switch = "y") +
            theme(strip.placement = "outside")
      }
   }
   
   wrap_plots(MyGraphs) +
      plot_layout(guides = "collect", 
                  ncol = ncol, 
                  nrow = nrow) + 
      plot_annotation(title = plot_title) & 
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5, 
                                      face = "bold"), 
            legend.position = "bottom")
   
}
