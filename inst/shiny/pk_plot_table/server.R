
# Server code for Simcyp output analysis

## Load the required packages
# library(SimcypConsultancy)
# library(tidyverse)
# library(shiny)
# library(shinyjs)
# library(shinyFiles)
# library(ggplot2)

options(shiny.maxRequestSize=1000*1024^2)

## Define server logic required  
shinyServer(function(input, output, session) {
    
    updateDirChooseLocal <- function(folder, username) {
        od_root <- paste("C:/Users/", username, "/OneDrive - Certara/Documents", sep = "")
        shinyFiles::shinyDirChoose(input, id = "shiny_output_dir_local", 
                                   roots=c("OneDrive-Documents" = od_root,
                                           "C:" = "C:"), 
                                   session=session)
    }
    
    observe({
      
      if (input$output_location_selection == "path") {
        output_directory <- input$output_path
        output_directory <- gsub("\\\\", "/", output_directory) # change slash direction from Windows to R friendly
        if (str_sub(output_directory, start = -1) == "/") {
          output_directory <- str_sub(output_directory, start = 1, end = -2) }
        output_directory<-paste(output_directory, "/", input$output_folder_name, sep="")
        
      } else if (input$output_location_selection == "mwd") {
        output_directory <- paste("C:/Users/", input$mwd_od_username, "/Certara/", input$mwd_dir, " - Modelling Working Directory/", input$output_folder_name, sep="")
        
      } else if (input$output_location_selection == "local") {
        updateDirChooseLocal(input$shiny_output_dir_local, input$local_od_username)
        od_root <- paste("C:/Users/", input$local_od_username, "/OneDrive - Certara/Documents", sep = "")
        output_directory <- paste(parseDirPath(roots=c("OneDrive-Documents" = od_root, "C:" = "C:"), input$shiny_output_dir_local),
                                  "/", input$output_folder_name, sep = "")
      }
      
      output$output_folder_name <- renderText({
        output_directory
      })
      # print(output_directory)
    })
    
    observe({
        shinyjs::toggle(id = "output_location_local", anim = TRUE, condition = input$output_location_selection == 'local')
        shinyjs::toggle(id = "output_location_mwd", anim = TRUE, condition = input$output_location_selection == 'mwd')
        shinyjs::toggle(id = "output_location_path", anim = TRUE, condition = input$output_location_selection == 'path')
    })
    
    observe({
        shinyjs::toggle(id = "obs_data_file_input", anim = TRUE, condition = input$obs_data_file_option == TRUE)
        shinyjs::toggle(id = "time_range", anim = TRUE, condition = input$time_range_selection == 'defined range')
        shinyjs::toggle(id = "dose_range", anim = TRUE, condition = input$time_range_selection == 'defined dose')
        shinyjs::toggle(id = "tissue_input", anim = TRUE, condition = input$tissue == 'other')
        shinyjs::toggle(id = "y_axis_limits_lin_input", anim = TRUE, condition = input$y_axis_limits_lin == 'defined')
        shinyjs::toggle(id = "y_axis_limits_log_input", anim = TRUE, condition = input$y_axis_limits_log == 'defined')
        shinyjs::toggle(id = "fig_dim_input", anim = TRUE, condition = input$fig_dim_selection == 'defined')
        shinyjs::toggle(id = "line_type_input", anim = TRUE, condition = input$line_type == 'defined')
        shinyjs::toggle(id = "pad_x_axis_input", anim = TRUE, condition = input$pad_x_axis == 'specific')
        shinyjs::toggle(id = "pad_y_axis_input", anim = TRUE, condition = input$pad_y_axis == 'specific')
    })

    observe({
        shinyjs::toggle(id = "mono_sd_options_input", anim = TRUE, condition = input$table_format == 'mono' & input$regimen == 'sd')
        shinyjs::toggle(id = "mono_md_options_input", anim = TRUE, condition = input$table_format == 'mono' & input$regimen == 'md')
        shinyjs::toggle(id = "ddi_sd_options_input", anim = TRUE, condition = input$table_format == 'ddi' & input$regimen == 'sd')
        shinyjs::toggle(id = "ddi_md_options_input", anim = TRUE, condition = input$table_format == 'ddi' & input$regimen == 'md')
        shinyjs::toggle(id = "custom_table_input", anim = TRUE, condition = input$table_format == 'custom')
        shinyjs::toggle(id = "define_sheet_input", anim = TRUE, condition = input$define_sheet == TRUE)
        shinyjs::toggle(id = "obs_data_input", anim = TRUE, condition = input$obs_data_inc == TRUE)
        
    })
    
    
    generate_plot <- eventReactive({
        input$plot_generate
    }, {
        
        inFile <- input$sim_output_file
        sim_data_file <- inFile$datapath
        
        if (input$tissue == "other") {
            tissue <- input$tissue_text
        } else {
            tissue <- input$tissue
        }


        if (input$time_range_selection == "NA") {
         time_range <- NA
        } else if (input$time_range_selection == "defined range") {
            time_range <- c(input$time_range_start, input$time_range_end)
        } else if (input$time_range_selection == "defined dose") {
            time_range <- input$dose_range_text
        } else {
            time_range <- input$time_range_selection
        }

        if(input$y_axis_limits_lin == "defined") {
            y_axis_limits_lin <- c(input$y_axis_limits_lin_min, input$y_axis_limits_lin_max)
        } else {
            y_axis_limits_lin <- NA
        }

        if(input$y_axis_limits_log == "defined") {
            y_axis_limits_log <- c(input$y_axis_limits_log_min, input$y_axis_limits_log_max)
        } else {
            y_axis_limits_log <- NA
        }


        if (input$line_type == "defined") {
            line_type <- unlist(strsplit(input$line_type_text,","))
        } else {
            line_type <- NA
        }


        if (input$pad_x_axis == "specific") {
            pad_x_axis <- as.numeric(unlist(strsplit(input$pad_x_axis_text,",")))
        } else if (input$pad_x_axis == "TRUE") {
            pad_x_axis <- TRUE
        } else if (input$pad_x_axis == "FALSE") {
            pad_x_axis <- FALSE}


        if (input$pad_y_axis == "specific") {
            pad_y_axis <- as.numeric(unlist(strsplit(input$pad_y_axis_text,",")))
        } else if (input$pad_y_axis == "TRUE") {
            pad_y_axis <- TRUE
        } else if (input$pad_y_axis == "FALSE") {
            pad_y_axis <- FALSE}
        
        if (input$obs_data_file_option == FALSE) {
            obs_data_file <- NA
        } else {
            obsFile        <- input$obs_data_file
            obs_data_file  <- obsFile$datapath
        }
        
        if (is.na(input$line_type)) {
            line_type <- NA
        } else {
            line_type <- unlist(strsplit(input$line_type_text,","))
        }
        
        sim_obs_df <- extractConcTime(sim_data_file = sim_data_file,
                              obs_data_file = obs_data_file,
                              adjust_obs_time = FALSE,
                              tissue = tissue,
                              compoundToExtract = input$compound,
                              returnAggregateOrIndiv = c("aggregate", "individual"),
                              expdetails = NA,
                              fromMultFunction = FALSE)
        
    
        plot_output <-  ct_plot(ct_dataframe = sim_obs_df,
                                figure_type = input$figure_type,
                                adjust_obs_time = (input$adjust_obs_time == "TRUE"),
                                time_range = time_range,
                                x_axis_interval = input$x_axis_interval,
                                y_axis_limits_lin = y_axis_limits_lin,
                                y_axis_limits_log = y_axis_limits_log,
                                linear_or_log = input$linear_or_log,
                                t0 = input$t0,
                                subsection_ADAM = input$subsection_ADAM,
                                pad_x_axis = pad_x_axis,
                                pad_y_axis = pad_y_axis,
                                line_transparency = input$line_transparency,
                                line_width = input$line_width,
                                line_type = line_type
                                )
        
        return(plot_output)
        
    })
    
    output$plotCONC_1 <- renderPlot({
      
      req(input$sim_output_file)
      
      if (input$output_location_selection == "path") {
        output_directory <- input$output_path
        output_directory <- gsub("\\\\", "/", output_directory) # change slash direction from Windows to R friendly
        if (str_sub(output_directory, start = -1) == "/") {
          output_directory <- str_sub(output_directory, start = 1, end = -2) }
        output_directory<-paste(output_directory, "/", input$output_folder_name, sep="")
        
      } else if (input$output_location_selection == "mwd") {
        output_directory <- paste("C:/Users/", input$mwd_od_username, "/Certara/", input$mwd_dir, " - Modelling Working Directory/", input$output_folder_name, sep="")
        
      } else if (input$output_location_selection == "local") {
        updateDirChooseLocal(input$shiny_output_dir_local, input$local_od_username)
        od_root <- paste("C:/Users/", input$local_od_username, "/OneDrive - Certara/Documents", sep = "")
        output_directory <- paste(parseDirPath(roots=c("OneDrive-Documents" = od_root, "C:" = "C:"), input$shiny_output_dir_local),
                                  "/", input$output_folder_name, sep = "")
      }
      
      if (!dir.exists(output_directory)){
        dir.create(file.path(output_directory))
      }
      
      inFile <- input$sim_output_file
      file_name <- inFile$name
      file_name <- gsub(".xlsx", "", file_name)
      
      if (input$fig_dim_selection == "auto" & input$linear_or_log == "both vertical") {
        fig_height <- 16
        fig_width <- 14
      } else if (input$fig_dim_selection == "auto" & input$linear_or_log == "semi-log" | input$fig_dim_selection == "auto" & input$linear_or_log == "linear") {
        fig_height <- 10
        fig_width <- 14
      } else if (input$fig_dim_selection == "auto" & input$linear_or_log == "both horizontal") {
        fig_height <- 10
        fig_width <- 28  
      } else if (input$fig_dim_selection == "defined") {
        fig_height <- input$fig_height
        fig_width <- input$fig_width
      }
      
      ggsave(filename = paste(output_directory, "/", file_name, ".png", sep=""), plot = generate_plot(), width = fig_width, height = fig_height, units = "cm", dpi = 600)
      
      generate_plot()
    })
    
    
    generate_table <- eventReactive({
        input$table_generate
    }, {
        
        my_3sf = function(x) {
            if (x >= 1000) { out <- round(x, digits = 0) 
            } else { out <- signif(x, digits = 3)}
            return(out)
        }
        
        inFile <- input$sim_output_file
        sim_data_file <- inFile$datapath
        
        
        if (input$table_format == 'mono' & input$regimen == 'sd') {
            AUC <- as.character(input$auc_choice_mono_sd)
            
            if (AUC == "AUCt_dose1") {
                CL <- as.character("CLt_dose1")
            } else if (AUC == "AUCinf_dose1") {
                CL <- as.character("CLinf_dose1")
            }
            
            PKparameters <- c(AUC, CL, "Cmax_dose1", "HalfLife_dose1", "tmax_dose1")
            PKparameters <- PKparameters[c(TRUE, 'cl' %in% input$mono_sd_op_params, TRUE, 't12' %in% input$mono_sd_op_params, 'tmax' %in% input$mono_sd_op_params)]
        }
        
        if (input$table_format == 'mono' & input$regimen == 'md') {
            AUC <- as.character(input$auc_choice_mono_md)
            if (AUC == "AUCtau_last") { Cmax <- "Cmax_last" } else if (AUC == "AUCt_dose1") { Cmax <- "Cmax_dose1" }
            if (AUC == "AUCtau_last") { tmax <- "tmax_last" } else if (AUC == "AUCt_dose1") { tmax <- "tmax_dose1" }
            PKparameters <- c(AUC, "CLtau_last", Cmax, "Cmin_last", tmax)
            PKparameters <- PKparameters[c(TRUE, 'cl' %in% input$mono_md_op_params, TRUE, 'cmin' %in% input$mono_md_op_params, 'tmax' %in% input$mono_md_op_params)]
        }
        
        if (input$table_format == 'ddi' & input$regimen == 'sd') {
            AUC <- as.character(input$auc_choice_ddi_sd)
            if (AUC == 'AUCt_dose1') { 
                PKparameters <- c("AUCt_dose1",
                                  "AUCt_dose1_withInhib",
                                  "Cmax_dose1",
                                  "Cmax_dose1_withInhib",
                                  "AUCt_ratio_dose1",
                                  "Cmax_ratio_dose1")
            } else if (AUC == 'AUCinf_dose1') {
                PKparameters <- c("AUCinf_dose1",
                                  "AUCinf_dose1_withInhib",
                                  "Cmax_dose1",
                                  "Cmax_dose1_withInhib",
                                  "AUCinf_ratio_dose1",
                                  "Cmax_ratio_dose1")   
            }
        }

        if (input$table_format == 'ddi' & input$regimen == 'md') {
            AUC <- as.character(input$auc_choice_ddi_md)
            if (AUC == 'AUCtau_last') { 
                PKparameters <- c("AUCtau_last",
                                  "AUCtau_last_withInhib",
                                  "Cmax_last",
                                  "Cmax_last_withInhib",
                                  "Cmin_last",
                                  "Cmin_last_withInhib",
                                  "AUCtau_ratio_last",
                                  "Cmax_ratio_last",
                                  "Cmin_ratio_last")
                
                PKparameters <- PKparameters[c(TRUE, TRUE, TRUE, TRUE, 'cmin' %in% input$ddi_md_op_params, 'cmin' %in% input$ddi_md_op_params, TRUE, TRUE, 'cmin' %in% input$ddi_md_op_params)]
            }
        }
        
        if (input$table_format == 'custom') {
         PKparameters <- unlist(strsplit(input$custom_table_input,","))   
        }
        
        if (input$define_sheet == TRUE) {
            define_sheet <- as.character(input$define_sheet_text)   
        } else {
            define_sheet <- NA
        }

        table <-  pksummary_table(sim_data_file = sim_data_file,
                                  PKparameters = PKparameters,
                                  sheet_PKparameters = define_sheet,
                                  mean_type = input$mean_type_table,
                                  includeCV = ('cv' %in% input$var_options),
                                  includeConfInt = ('ci' %in% input$var_options),
                                  includePerc = ('pi' %in% input$var_options),
                                  includeTrialMeans = ('trials' %in% input$var_options),
                                  concatVariability = input$concat_var,
                                  prettify_columns = input$pretty_col,
                                  checkDataSource = input$qc_check,
                                  save_table = NA)
    

        if (input$obs_data_inc == TRUE) {
            
            if(input$qc_check == TRUE) {
                table_df <- table[["Table"]]
            } else { table_df <- table }
            
            obs <- c("Observed", unlist(strsplit(input$obs_data,",")))
            obs_n <- as.numeric(unlist(strsplit(input$obs_data,",")))
            sim_n <- as.numeric(table_df[table_df$Statistic == "Simulated",2:ncol(table_df)])
            so <- c("S/O", as.character(sapply(sim_n/obs_n, my_3sf)))
            table_df <- rbind(table_df, obs, so, stringsAsFactors = FALSE)
            
            if(input$qc_check == TRUE) {
                table[["Table"]] <- table_df
            } else { table <- table_df }
            
        }
        
        if (input$obs_data_inc == TRUE & 'trials' %in% input$var_options & input$trial_range_so == TRUE) {
            
            if(input$qc_check == TRUE) {
                table_df <- table[["Table"]]
            } else { table_df <- table }
            
            trial_ranges <- filter(table_df, Statistic == "Range of trial means")
            obs          <- as.numeric(unlist(strsplit(input$obs_data,",")))
            
            trial_min <- as.numeric(sub(" to .*", "", trial_ranges[,-1]))  
            trial_max <- as.numeric(sub(".* to ", "", trial_ranges[,-1]))  
            
            so_min <- round(trial_min/obs, digits = 2)
            so_max <- round(trial_max/obs, digits = 2)
            
            so_range <- c("S/O range", paste(so_min, " to ", so_max, sep=""))
            
            if(input$qc_check == TRUE) {
                table[["Table"]] <- rbind(table_df, so_range)
            } else { table <- rbind(table_df, so_range) }
            
        }
        
        if (input$output_location_selection == "path") {
          output_directory <- input$output_path
          output_directory <- gsub("\\\\", "/", output_directory) # change slash direction from Windows to R friendly
          if (str_sub(output_directory, start = -1) == "/") {
            output_directory <- str_sub(output_directory, start = 1, end = -2) }
          output_directory<-paste(output_directory, "/", input$output_folder_name, sep="")
          
        } else if (input$output_location_selection == "mwd") {
          output_directory <- paste("C:/Users/", input$mwd_od_username, "/Certara/", input$mwd_dir, " - Modelling Working Directory/", input$output_folder_name, sep="")
          
        } else if (input$output_location_selection == "local") {
          updateDirChooseLocal(input$shiny_output_dir_local, input$local_od_username)
          od_root <- paste("C:/Users/", input$local_od_username, "/OneDrive - Certara/Documents", sep = "")
          output_directory <- paste(parseDirPath(roots=c("OneDrive-Documents" = od_root, "C:" = "C:"), input$shiny_output_dir_local),
                                    "/", input$output_folder_name, sep = "")
        }
        
        if (!dir.exists(output_directory)){
            dir.create(file.path(output_directory))
        }
        
        inFile <- input$sim_output_file
        file_name <- inFile$name
        file_name <- gsub(".xlsx", "", file_name)
        
        if (input$qc_check == TRUE) {
            
            write.csv(table[["Table"]], file = paste(output_directory, "/", file_name, ".csv", sep=""))
            write.csv(table[["QC"]], file = paste(output_directory, "/", file_name, "_QC.csv", sep=""))
            return(table)
        } else {
            
            write.csv(table, file = paste(output_directory, "/", file_name, ".csv", sep=""))
            return(list(Table = table, QC = NA))
        }
        
    })
    
    output$table <- renderTable({
        
        req(input$sim_output_file)
        generate_table()[["Table"]]
        
    })
    
    
})