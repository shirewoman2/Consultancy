#' Make a set of plots showing the demographics used in a set of simulations and
#' optionally compare them to observed demographics. UNDER CONSTRUCTION. 
#'
#' @param demog_dataframe the output from running \code{\link{extractDemog}}.
#'   Optionally (and we recommend) with added observed demographic data, perhaps
#'   from observed overlay XML files.
#' @param sim_data_file the simulator output Excel file to use. If left as NA,
#'   all the files in demog_dataframe will be included.
#' @param graph_title title to use on the plots
#' @param variables variables to include. We're starting with a limited set:
#'   "Age", "Weight_kg" ("Weight" is also fine), "Height_cm" ("Height" is fine),
#'   "Weight vs Height", "Height vs Age", "Weight vs Age", "HSA_gL" ("HSA" is
#'   fine), "AGP_gL" ("AGP" is fine), "Sex", "Sex vs Age", "BMI_kgm2" ("BMI" is
#'   fine), and "RenalFunction". If you want only a subset of those,
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
#' @param facet_by_sex TRUE or FALSE (default) for whether to break up the
#'   graphs into facets based on the sex of the subjects
#' @param border_facets TRUE (default) or FALSE for whether to include a border
#'   around the facets if the graphs are broken up by the sex of the subjects
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs.
#'
#' @return a set of graphs. This does not yet save the graphs for you, so you'll
#'   need to run ggsave(...) to do that.
#' @export
#'
#' @examples
#' # none yet
demog_plot <- function(demog_dataframe, 
                       sim_data_file = NA, 
                       graph_title = "Demographics", 
                       variables = NA, 
                       obs_alpha = 0.8, 
                       sim_alpha = 0.4, 
                       ncol = NULL, 
                       nrow = NULL, 
                       facet_by_sex = FALSE, 
                       border_facets = TRUE, 
                       graph_labels = TRUE){
   
   # Error catching ----------------------------------------------------------
   
   # FIXME: !!!! NOTE TO SELF: This needs some tidying. It replicates needlessly
   # some of the stuff I have set up in demog_plot_sim. Really, I shoudl change
   # this to just use demog_plot_sim with some modification rather than having
   # this be a separate function.
   
   
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   if(facet_by_sex & length(unique(demog_dataframe$Sex)) == 1){
      warning("You requested that we facet the graphs by sex, but there's only one sex in your data. We will not be able to do this.\n", 
              call. = FALSE)
      facet_by_sex = FALSE
   }
   
   if(all(is.na(sim_data_file)) == FALSE){
      Demog_sub <- demog_dataframe %>% filter(File == sim_data_file)
   } else {
      Demog_sub <- demog_dataframe
   }
   
   # Addressing any issues w/case and periods for "vs"
   variables <- tolower(gsub("\\.", "", as.character(variables)))
   names(Demog_sub) <- tolower(names(Demog_sub))
   
   variables <- case_match(variables, 
                           "height vs weight" ~ "weight vs height", 
                           "age vs height" ~ "height vs age", 
                           "age vs weight" ~ "weight vs age", 
                           "age vs sex" ~ "sex vs age", 
                           "weight" ~ "weight_kg",
                           "height" ~ "height_cm",
                           "hsa" ~ "hsa_gl",
                           "agp" ~ "agp_gl",
                           "bmi" ~ "bmi_kgm2",
                           .default = variables)
   
   BadVar <- setdiff(variables, 
                     tolower(c("Age", 
                               "AGP_gL", 
                               "BMI_kgm2", 
                               "Height_cm", 
                               "Height vs Age", 
                               "HSA_gL",
                               "Weight_kg",
                               "Weight vs Age", 
                               "Weight vs Height",
                               "Sex", 
                               "Sex vs Age", 
                               "RenalFunction")))
   
   if(length(BadVar) > 0 && any(complete.cases(BadVar))){
      warning(paste0("The variables ", 
                     str_comma(paste0("`", BadVar, "`")), 
                     " are not among the possible options for variables to graph, so they won't be included. Please check the help file for options.\n"), 
              call. = FALSE)
      
      variables <- setdiff(variables, BadVar)
   }
   
   Demog_sub <- Demog_sub %>% 
      mutate(SorO = ifelse(simulated == TRUE, 
                           "Simulated", "Observed")) %>% unique()
   
   SOcolors <- c("Simulated" = "#377EB8", 
                 "Observed" = "#E41A1C") 
   
   if("sex" %in% names(Demog_sub) == FALSE){
      Demog_sub$Sex <- "unknown"
   }
   
   YLabs <- c("age" = "Age (years)", 
              "weight_kg" = "Weight (kg)", 
              "height_cm" = "Height (cm)", 
              "hsa_gl" = "HSA (g/L)", 
              "agp_gl" = "AGP (g/L)", 
              "sex" = "Sex", 
              "bmi_kgm2" = "BMI (kg/m2)", 
              "renalfunction" = "Renal function")
   
   if(all(is.na(variables))){
      Graphs <- tolower(c("Age", "Weight_kg", "Height_cm", "Weight vs Height",
                          "Height vs Age", "Weight vs Age", "HSA_gL",
                          "AGP_gL", "Sex", "Sex vs Age", "BMI_kgm2", "RenalFunction"))
   } else {
      Graphs <- variables
   }
   
   subfun_density <- function(Var){
      names(Demog_sub)[names(Demog_sub) == Var] <- "MyVar"
      
      suppressWarnings(
         ggplot(Demog_sub, aes(x = MyVar, fill = SorO)) +
            geom_density(alpha = 0.5, show.legend = FALSE) +
            ylab("Distribution") +
            xlab(YLabs[Var]) +
            guides(fill = "none") +
            scale_fill_manual(values = SOcolors) +
            theme_consultancy(border = border_facets) + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
      )
   }
   
   MyGraphs <- list()
   
   for(yy in Graphs){
      if(yy == "sex vs age"){
         MyGraphs[[yy]] <- patchwork::free(
            ggplot(Demog_sub, 
                   aes(x = age, y = SorO, fill = SorO)) +
               facet_grid(sex ~ ., switch = "y") +
               geom_violin(alpha = 0.7, show.legend = FALSE) +
               ylab("Sex") +
               xlab("Age (years)") +
               scale_fill_manual(values = SOcolors) +
               theme_consultancy(border = border_facets) + 
               theme(legend.position = "none", 
                     strip.placement = "outside"))
         
      } else if(yy == "sex"){
         
         PercFemale <- Demog_sub %>% 
            group_by(SorO) %>% 
            summarize(NumF = length(which(sex == "F")), 
                      NumTot = n()) %>% 
            ungroup() %>% 
            mutate(PercFemale = NumF / NumTot)
         
         MyGraphs[[yy]] <-
            ggplot(PercFemale, aes(x = SorO, fill = SorO, y = PercFemale)) +
            geom_bar(stat = "identity", alpha = 0.7) +
            ylab("Percent female") +
            xlab(NULL) +
            scale_fill_manual(values = SOcolors) +
            scale_y_continuous(labels = scales::percent) +
            theme_consultancy(border = border_facets) + 
            theme(legend.position = "none", 
                  strip.placement = "outside")
         
      } else if(yy %in% tolower(c("Weight vs Height", 
                                  "Height vs Age", 
                                  "Weight vs Age"))){
         MyGraphs[[yy]] <- 
            ggplot(
               Demog_sub, 
               switch(yy, 
                      "weight vs height" = aes(y = weight_kg, x = height_cm, shape = sex, 
                                               color = SorO, alpha = SorO), 
                      "height vs age" = aes(y = height_cm, x = age, shape = sex, 
                                            color = SorO, alpha = SorO), 
                      "weight vs age" = aes(y = weight_kg, x = age, shape = sex, 
                                            color = SorO, alpha = SorO))) +
            geom_point() + 
            scale_alpha_manual(values = c("Observed" = obs_alpha, 
                                          "Simulated" = sim_alpha)) +
            scale_color_manual(values = SOcolors) +
            labs(color = NULL, alpha = NULL) +
            ylab(case_match(yy, 
                            "weight vs height" ~ "Weight (kg)", 
                            "height vs age" ~ "Height (cm)", 
                            "weight vs age" ~ "Weight (kg)")) +
            xlab(case_match(yy, 
                            "weight vs height" ~ "Height (cm)", 
                            "height vs age" ~ "Age (years)", 
                            "weight vs age" ~ "Age (years)")) +
            theme_consultancy(border = border_facets) 
         
         if(length(unique(Demog_sub$sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
         
      } else {
         
         MyGraphs[[yy]] <- subfun_density(yy)
         
         if(length(unique(Demog_sub$sex)) == 1){
            MyGraphs[[yy]] <- MyGraphs[[yy]] + guides(shape = "none") 
         }
      }
      
      if(facet_by_sex & yy != "sex"){
         MyGraphs[[yy]] <- MyGraphs[[yy]] + 
            facet_grid(sex ~ ., switch = "y") +
            theme(strip.placement = "outside")
      }
   }
   
   patchwork::wrap_plots(MyGraphs) +
      patchwork::plot_layout(guides = "collect", 
                             ncol = ncol, 
                             nrow = nrow) + 
      patchwork::plot_annotation(title = graph_title, 
                                 tag_levels = switch(as.character(graph_labels), 
                                                     "TRUE" = "A", 
                                                     "FALSE" = NULL)) & 
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5, 
                                      face = "bold"), 
            legend.position = "bottom")
   
}
