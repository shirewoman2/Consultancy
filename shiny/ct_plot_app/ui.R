##                                                                                                                   ^
## You can run the application by clicking 'Run App' -----------------------------------------------------------------^
######################################################################################################################

## Load the required packages
library(SimcypConsultancy)
library(shiny)
library(ggplot2)

## Draw the user interface
function(request) { fluidPage(
  
  tags$head(
    tags$style(".content {background-color: black;}"),
    tags$style("label{font-family: Palatino Linotype;}"),
    tags$style(type='text/css', ".nav-tabs {font-family: Palatino Linotype; font-size: 14px; font-weight:bold;} ")),
  
  
  (fluidRow(
    
    h1(id="big-heading", "Simcyp Consultancy - Excel Output Analysis Tool"),
    tags$style(HTML("#big-heading{font-family: Palatino Linotype; font-size:18px; font-weight:bold;}")),
    
    ## The preview graph will be shown on the top of the user-interface
    # column(12, plotOutput("plotCONC_1")),

    ## Add sliders under the graph
    tabsetPanel(type = "tabs",
                tabPanel("Excel Output File:",
                         br(),
                         fileInput("sim_output_file", "Simcyp Simulator Excel output file (.xlsx):", multiple = FALSE,
                                   width = '60%', buttonLabel = "Browse...",
                                   placeholder = "No file selected", accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')),
                         
                         # fileInput("obs_data_file", HTML("Optional: PE Data entry template (.xlsx), if data not already in Excel file <br/> Note: Limited to plasma concs for a single substrate - minus inhibitor/inducer"), multiple = FALSE,
                         #           width = '60%', buttonLabel = "Browse...",
                         #           placeholder = "No file selected", accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')),
                         # 
                         # fileInput("obs_effector_data_file", HTML("Optional: PE Data entry template (.xlsx), if data not already in Excel file <br/> Note: as input above but in presence of inhibitor/inducer"), multiple = FALSE,
                         #           width = '60%', buttonLabel = "Browse...",
                         #           placeholder = "No file selected", accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')),
                ),

                tabPanel("Plot:",
                         br(),
                         actionButton("plot_generate", "Generate Plot"),
                         br(),
                         br(),
                         column(6, 
                                selectInput("figure_type", "Figure Type", c("Trial Means" = "trial means", "Trial Percentiles" = "trial percentiles", "Means Only" = "means only"),
                                            selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                
                                selectInput("substrate_or_effector", "Substrate/Effector", c("Substrate" = "substrate", "Effector" = "Effector"),
                                            selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                
                                radioButtons("adjust_obs_time", "Adjust Observed Time (to last dose)", c("No" = "FALSE", "Yes" = "TRUE"), inline = TRUE),
                                
                                column(12,
                                       column(6,
                                              numericInput("time_range_start", "Time Range - Start", value = NA, min = 0, max = 10000, step = 0.1)),
                                       column(6,
                                              numericInput("time_range_end", "Time Range - End", value = NA, min = 0, max = 10000, step = 0.1)))),
                         
                         
                         column(6,
                                selectInput("tissue", "Compartment/Tissue", c("Plasma" = "plasma", "Liver" = "liver", "Lung" = "lung", "Brain" = "brain"),
                                            selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                
                                selectInput("mean_type", "Mean Type", c("Arithmetic" = "arithmetic", "Geometric" = "geometric"),
                                            selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                
                                radioButtons("return_data", "Return Data?", c("No" = "FALSE", "Yes" = "TRUE"), inline = TRUE),
                                
                                radioButtons("return_indiv_graphs", "Return Individual Graphs?", c("No" = "FALSE", "Yes" = "TRUE"), inline = TRUE)),
                
                
                column(12, plotOutput("plotCONC_1", width = "80%"))
                ),
                
                tabPanel("Table:",
                         br(),
                         actionButton("table_generate", "Generate Table"),
                         br(),
                         br(),
                         column(6, 
                                selectInput("table_format", "Table Format", c("DDI Substrate w/o Observed" = "DDI_sub_wo_obs"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                         # c("Single Dose Monotherapy w/o Observed" = "sd_mono_wo_obs", "Multiple Dose Monotherapy w/o Observed" = "md_mono_wo_obs",
                         #   "Single Dose Monotherapy with Observed" = "sd_mono_with_obs", "Multiple Dose Monotherapy with Observed" = "md_mono_with_obs",
                         #   "DDI Substrate w/o Observed" = "DDI_sub_wo_obs", "DDI Substrate with Observed" = "DDI_sub_with_obs")
                         
                         
                         column(6,
                                selectInput("auc_choice", "AUC", c("AUCinf" = "auc_inf", "AUCt" = "auc_t"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),

                         
                         column(12, tableOutput("table"))
                         ),
                         
                tabPanel("Additional Options / Bookmarking:", 
                         br(),
                         bookmarkButton())),

    align="center"
    
  )))}