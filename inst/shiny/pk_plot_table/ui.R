##                                                                                                                   ^
## You can run the application by clicking 'Run App' -----------------------------------------------------------------^
######################################################################################################################

## Load the required packages
library(SimcypConsultancy)
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyFiles)
library(ggplot2)

## Draw the user interface
function(request) { fluidPage(
  
  useShinyjs(),  # Include shinyjs
    
  tags$head(
    tags$style(".content {background-color: black;}"),
    tags$style("label{font-family: Palatino Linotype;}"),
    tags$style(type='text/css', ".nav-tabs {font-family: Palatino Linotype; font-size: 14px; font-weight:bold;} ")),
  
  tags$style(HTML("#big-heading{font-family: Palatino Linotype; font-size:18px; font-weight:bold;}")),
  tags$style(HTML("#small-heading{font-family: Palatino Linotype; font-size:14px; font-weight:bold;}")),
  
  (fluidRow(
    
    h1(id="big-heading", "Simcyp Consultancy - Excel Output Tool"),
    # tags$style(HTML("#big-heading{font-family: Palatino Linotype; font-size:18px; font-weight:bold;}")),
    
    ## The preview graph will be shown on the top of the user-interface
    # column(12, plotOutput("plotCONC_1")),

    ## Add sliders under the graph
    tabsetPanel(type = "tabs",
                tabPanel("File Locations:",
                         br(),
                         column(6,
                                tags$h2(id="big-heading", "Input:"),
                                br(),
                                fileInput("sim_output_file", "Simcyp Simulator Excel output file (.xlsx):", multiple = FALSE,
                                          width = '60%', buttonLabel = "Browse...",
                                          placeholder = "No file selected", accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx'))),
                         
                         
                         column(6,
                                tags$h2(id="big-heading", "Output:"),
                                br(),
                                selectInput("output_location_selection", "Output Location:", c("Folder path" = "path", "Modelling Working Directory" = "mwd",
                                                                                               "Local (i.e. C: drive, Onedrive/Documents)" = "local"),
                                            selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                
                                div(id = "output_location_path", 
                                    textInput("output_path", "Enter folder path", value = "", width = NULL, placeholder = "copy and paste folder name")),
                                
                                div(id = "output_location_mwd",
                                    textInput("mwd_od_username", "Username:", value = "", width = NULL, placeholder = "e.g., 'hburt', 'lshireman' etc."),
                                    textInput("mwd_dir", "Modelling Working Directory Name:", value = "", width = NULL, placeholder = "e.g., 'PBPKConsultTEST4'")),
                                
                                div(id = "output_location_local",
                                    tags$h2(id="small-heading", "Select Directory:"),
                                    textInput("local_od_username", "Username:", value = "", width = NULL, placeholder = "e.g., 'hburt', 'lshireman' etc."),
                                    shinyDirButton("shiny_output_dir_local", "Select folder to place outputs:",
                                                   title = "Select Output Directory:", viewtype = "list")),
                                
                                br(),
                                textInput("output_folder_name", "Sub-Folder Name (or path):", value = "Consultancy_Shiny_App_Outputs", width = NULL, placeholder = "e.g., 'Consultancy_Shiny_App_Outputs'"),
                                
                                br(),
                                tags$h2(id="small-heading", "Current Output Location:"),
                                textOutput("output_folder_name")),
                         
                ),
                
                tabPanel("Plot:",
                         br(),
                         actionButton("plot_generate", "Generate Plot"),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Data:",
                                              br(),
                                              column(6, 
                                                     selectInput("figure_type", "Figure Type", c("Trial Means" = "trial means", "Percentiles" = "percentiles", "Means Only" = "means only"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     selectInput("compound", "Compound to Extract", c("Substrate" = "substrate", "Inhibitor 1" = "inhibitor 1", "Inhibitor 2" = "inhibitor 2", 
                                                                                                      "Primary Metabolite 1" = "primary metabolite 1", "Primary Metabolite 2" = "primary metabolite 2", "Inhibitor 1 metabolite" = "inhibitor 1 metabolite",
                                                                                                      "Secondary Metabolite" = "secondary metabolite",
                                                                                                      "Conjugated Protein" = "conjugated protein", "Total Protein" = "total protein", "Released Payload" = "released payload"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     checkboxInput("obs_data_file_option", label =("Define observed data file"), value = FALSE),
                                                     
                                                     div(id = "obs_data_file_input",
                                                         fileInput("obs_data_file", "Observed data file (.xlsx):", multiple = FALSE,
                                                                   width = '60%', buttonLabel = "Browse...",
                                                                   placeholder = "No file selected", accept = c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', '.xlsx')))),
                                              
                                              
                                              column(6,
                                                     selectInput("mean_type", "Mean Type", c("Arithmetic" = "arithmetic", "Geometric" = "geometric", "Median" = "median"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     selectInput("tissue", "Compartment/Tissue", c("Plasma" = "plasma", "Blood" = "blood", "Liver" = "liver", "Lung" = "lung", "Brain" = "brain",
                                                                                                   "Unbound Plasma" = "unbound plasma", "Unbound Blood" = "unbound blood", "Other" = "other"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     div(id = "tissue_input", textInput("tissue_text", "Tissue:", value = "", width = NULL, placeholder = "e.g. 'muscle', or if ADAM: 'stomach', 'duodenum' etc.")))),
                                                     
                                     tabPanel("Formatting (Simple):",
                                              br(),
                                              column(6,
                                                     selectInput("linear_or_log", "Graph Format", c("Both Vertical" = "both vertical", "Semi-Log" = "semi-log", "Linear" = "linear", 
                                                                                                    "Both Horizontal" = "both horizontal"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     numericInput("x_axis_interval", "X-Axis Interval (Default if left blank)", value = NA, min = 0, max = 10000, step = 0.1),
                                                     
                                                     selectInput("time_range_selection", "Time Range", c("Whole Simulation (Default)" = "NA", "Defined Range" = "defined range", "First dose" = "first dose", "Last dose" = "last dose",
                                                                                                         "Penultimate dose" = "penultimate dose", "Defined dose/s" = "defined dose",
                                                                                                         "All observed" = "all obs", "Last dose to last observed" = "last obs"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     div(id = "time_range", 
                                                                numericInput("time_range_start", "Time Range - Start", value = NA, min = 0, max = 10000, step = 0.1),
                                                                numericInput("time_range_end", "Time Range - End", value = NA, min = 0, max = 10000, step = 0.1)),
                                                     
                                                     div(id = "dose_range", textInput("dose_range_text", "Dose Range", value = "", width = NULL, placeholder = "e.g., 'dose 3' for the 3rd dose or 'doses 1 to 4' for doses 1 to 4"))
                                                     
                                                     ),
                                              
                                              column(6,
                                                     selectInput("t0", "Time-Zero", c("Simulation Start" = "simulation start", "Dose 1" = "dose 1", 
                                                                                      "Penultimate dose" = "penultimate dose", "Last dose" = "last dose"),
                                                                 selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     radioButtons("adjust_obs_time", "Adjust Observed Time (to last dose)", c("No" = "FALSE", "Yes" = "TRUE"), inline = TRUE),
                                                     
                                                     selectInput("y_axis_limits_lin", "Linear Y-axis Limits", c("Default" = "default", "Defined" = "defined"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     div(id = "y_axis_limits_lin_input", 
                                                         numericInput("y_axis_limits_lin_min", "Linear Y-axis - Min", value = NA, min = 0, max = 10000, step = 0.1),
                                                         numericInput("y_axis_limits_lin_max", "Linear Y-axis - Max", value = NA, min = 0, max = 10000, step = 0.1)),
                                                     
                                                     selectInput("y_axis_limits_log", "Log Y-axis Limits", c("Default" = "default", "Defined" = "defined"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     div(id = "y_axis_limits_log_input", 
                                                         numericInput("y_axis_limits_log_min", "Log Y-axis - Min", value = NA, min = 0, max = 10000, step = 0.1),
                                                         numericInput("y_axis_limits_log_max", "Log Y-axis - Max", value = NA, min = 0, max = 10000, step = 0.1)),
                                                     
                                                     ),
                                              ),
                                     
                                     tabPanel("Formatting (Advanced):",
                                              br(),
                                              column(12,
                                                     column(6,
                                                            selectInput("line_type", "Line Type", c("Default" = "NA", "Defined" = "defined"),
                                                                        selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                            div(id = "line_type_input", textInput("line_type_text", "Line Type", value = "", width = NULL, placeholder = "e.g. solid, dashed"))),
                                                     column(6,
                                                            selectInput("pad_x_axis", "Pad X-Axis", c("Yes (Default)" = "TRUE", "No" = "FALSE", "Specific" = "specific"),
                                                                        selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                            div(id = "pad_x_axis_input", textInput("pad_x_axis_text", "X-axis Padding", value = "", width = NULL, placeholder = "e.g. 0.02, 0.04")),
                                                            selectInput("pad_y_axis", "Pad Y-Axis", c("Yes (Default)" = "TRUE", "No" = "FALSE", "Specific" = "specific"),
                                                                        selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                            div(id = "pad_y_axis_input", textInput("pad_y_axis_text", "Y-axis Padding", value = "", width = NULL, placeholder = "e.g. 0.02, 0.04"))
                                                     )),
                                              
                                              column(12,
                                                     column(6,
                                                            selectInput("subsection_ADAM", "ADAM Selection", c("Free Compound in Lumen" = "free compound in lumen", "Solid Compound" = "solid compound", "Dissolved Compound" = "dissolved compound",
                                                                                                               "Luminal CLint of Compound" = "luminal CLint of compound", "Heff" = "Heff", "Absorption Rate" = "absorption rate", "Unreleased Substrate in Faeces" = "unreleased substrate in faeces",
                                                                                                               "Unreleased Inhibitor in Faeces" = "unreleased inhibitor in faeces"),
                                                                        selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                            numericInput("fig_height", "Figure Height (cm)", value = 16, min = 5, max = 40, step = 0.5),
                                                            numericInput("fig_width", "Figure Width (cm)", value = 14, min = 5, max = 40, step = 0.5)),
                                                     column(6,
                                                            numericInput("line_transparency", "Line Transparency (Default if left blank)", value = NA, min = 0, max = 1, step = 0.1),
                                                            numericInput("line_width", "Line Width", value = 1, min = 0.1, max = 2, step = 0.1)),
                                                     ),
                                              
                                     )),
                         
                         column(12, 
                                br(),
                                plotOutput("plotCONC_1", width = "80%"))
                ),
                
                tabPanel("Table:",
                         br(),
                         actionButton("table_generate", "Generate Table"),
                         tabsetPanel(type = "tabs",
                                     tabPanel("PK parameters:",
                                              br(),
                                              column(6, 
                                                     selectInput("table_format", "Table Format", c("Monotherapy" = "mono", "DDI" = "ddi", "Custom Table" = "custom"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     selectInput("regimen", "Dosing Regimen", c("Single Dose" = "sd", "Multiple Dose" = "md"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     selectInput("mean_type_table", "Mean Type", c("Geometric" = "geometric", "Arithmetic" = "arithmetic"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                                     
                                                     checkboxInput("define_sheet", "Define sheet for parameters (optional)", value = FALSE),
                                                     checkboxInput("obs_data_inc", "Include row for observed data and calculate simulated/observed ratio", value = FALSE),
                                                     checkboxInput("qc_check", "Output QC table", value = FALSE),
                                                     
                                              ),
                                              column(6, 
                                                     
                                                     div(id = "mono_sd_options_input", 
                                                         checkboxGroupInput("mono_sd_op_params", label =("Optional parameters:"), choices = list("CL" = "cl", "Half-life" = "t12", "tmax" = "tmax"), inline = TRUE),
                                                         selectInput("auc_choice_mono_sd", "AUC choice", c("AUCt" = "AUCt_dose1", "AUCinf" = "AUCinf_dose1"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                                                     
                                                     div(id = "mono_md_options_input", 
                                                         checkboxGroupInput("mono_md_op_params", label =("Optional parameters:"), choices = list("CL" = "cl", "tmax" = "tmax", "Cmin (Last Dose)" = "cmin"), inline = TRUE),
                                                         selectInput("auc_choice_mono_md", "AUC/Cmax choice", c("AUCtau - Last dose" = "AUCtau_last", "AUCtau - First dose" = "AUCt_dose1"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                                                     
                                                     div(id = "ddi_sd_options_input",
                                                         selectInput("auc_choice_ddi_sd", "AUC choice", c("AUCt" = "AUCt_dose1", "AUCinf" = "AUCinf_dose1"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                                                     
                                                     div(id = "ddi_md_options_input",
                                                         checkboxGroupInput("ddi_md_op_params", label =("Optional parameters:"), choices = list("Cmin" = "cmin"), inline = TRUE),
                                                         selectInput("auc_choice_ddi_md", "AUC/Cmax choice", c("AUCtau - Last dose" = "AUCtau_last"), selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                                                     
                                                     div(id = "custom_table_input", textInput("custom_table_text", "Custom Table Parameters:", value = "", width = NULL, placeholder = "e.g., 'AUCinf_dose1', 'Cmax_dose1', 'tmax_dose1'")),
                                                     
                                                     div(id = "define_sheet_input", textInput("define_sheet_text", "Define Sheet (optional)", value = "", width = NULL, placeholder = "e.g., 'AUC', 'AUC0', 'AUCX'")),
                                                     
                                                     div(id = "obs_data_input", textInput("obs_data", "Observed Values (comma separated)", value = "", width = NULL, placeholder = "e.g., 0.125, 114")),
                                                     
                                              ),         
                                     ),
                                     tabPanel("Variability:",
                                              br(),
                                              column(6, 
                                                     checkboxGroupInput("var_options", label =("Variability Options:"), choices = list("CV" = "cv", "90% CI" = "ci", "Percentiles" = "pi", "Trial Range" = "trials"), inline = TRUE),
                                              ),
                                              
                                              column(6, 
                                                     checkboxInput("concat_var", "Concatenate Variability", value = FALSE),
                                                     checkboxInput("pretty_col", "Pretty Columns?", value = TRUE),
                                                     checkboxInput("trial_range_so", "Include Trial Range S/O", value = FALSE),
                                              ),
                                              
                                     ),
                         ),
                         
                         column(12, tableOutput("table"),
                                )),

                tabPanel("Bookmark:", 
                         br(),
                         bookmarkButton())),

    align="center"
    
  )))}