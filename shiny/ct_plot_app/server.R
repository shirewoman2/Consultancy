
# Server code for Simcyp output analysis

## Load the required packages
library(shiny)
library(tidyverse)
library(SimcypConsultancy)

options(shiny.maxRequestSize=1000*1024^2)

## Define server logic required
shinyServer(function(input, output) {

      generate_plot <- eventReactive({
            input$plot_generate
      }, {

            inFile <- input$sim_output_file
            sim_data_file <- inFile$datapath

            if (is.na(input$time_range_start) | is.na(input$time_range_end)) {
                  time_range <- NA
            } else {
                  time_range <- c(input$time_range_start, input$time_range_end)
            }

            plot_output <-  ct_plot(sim_data_file = sim_data_file,
                                    sim_obs_dataframe = NA,
                                    tissue = input$tissue,
                                    figure_type = input$figure_type,
                                    compoundToExtract = input$substrate_or_effector,
                                    adjust_obs_time = (input$adjust_obs_time == "TRUE"),
                                    time_range = time_range,
                                    return_data = (input$return_data == "TRUE"),
                                    return_indiv_graphs = (input$return_indiv_graphs == "TRUE"))

            return(plot_output)

      })

      output$plotCONC_1 <- renderPlot({

            req(input$sim_output_file)

            inFile <- input$sim_output_file
            file_name <- inFile$name
            file_name <- gsub(".xlsx", "", file_name)

            ggsave(filename = paste(file_name, ".png", sep=""), plot = generate_plot(),
                   width = 14, height = 16, units = "cm", dpi = 600)
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

            table <-  extractPK(sim_data_file = sim_data_file,
                                PKparameters = "AUC tab",
                                sheet = NA,
                                returnAggregateOrIndiv = "individual")

            if (input$table_format == "DDI_sub_wo_obs") {

                  if (input$auc_choice == "auc_t") {

                        z <- 1.64

                        table_output_df <- data.frame(AUC_CTRL =  c(my_3sf(exp(mean(log(table$AUCtau_ss)))),NA,NA),
                                                      CMAX_CTRL = c(my_3sf(exp(mean(log(table$Cmax_ss)))),NA,NA),
                                                      AUC_DDI =   c(my_3sf(exp(mean(log(table$AUCtau_ss_withEffector)))),NA,NA),
                                                      CMAX_DDI =  c(my_3sf(exp(mean(log(table$Cmax_ss_withEffector)))),NA,NA),
                                                      AUC_GMR =   c(my_3sf(exp(mean(log(table$AUCtau_ss_withEffector/table$AUCtau_ss)))),
                                                                    my_3sf(exp(mean(log(table$AUCtau_ss_withEffector/table$AUCtau_ss))-z*sd(log(table$AUCtau_ss_withEffector/table$AUCtau_ss))/sqrt(length(table$AUCtau_ss_withEffector/table$AUCtau_ss)))),
                                                                    my_3sf(exp(mean(log(table$AUCtau_ss_withEffector/table$AUCtau_ss))+z*sd(log(table$AUCtau_ss_withEffector/table$AUCtau_ss))/sqrt(length(table$AUCtau_ss_withEffector/table$AUCtau_ss))))),
                                                      CMAX_GMR =  c(my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss)))),
                                                                    my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss))-z*sd(log(table$Cmax_ss_withEffector/table$Cmax_ss))/sqrt(length(table$Cmax_ss_withEffector/table$Cmax_ss)))),
                                                                    my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss))+z*sd(log(table$Cmax_ss_withEffector/table$Cmax_ss))/sqrt(length(table$Cmax_ss_withEffector/table$Cmax_ss))))),
                                                      row.names = c("GeoMean", "CI_lower", "CI_upper"))

                  } else if (input$auc_choice == "auc_inf") {

                        t <- qt(0.1/2, length(table$AUCinf_dose1)-1, lower.tail=FALSE) # this isn't the right way but seems to be the way Simcyp does it for AUCinf
                        z <- 1.64

                        table_output_df <- data.frame(AUC_CTRL =  c(my_3sf(exp(mean(log(table$AUCinf_dose1)))),NA,NA),
                                                      CMAX_CTRL = c(my_3sf(exp(mean(log(table$Cmax_ss)))),NA,NA),
                                                      AUC_DDI =   c(my_3sf(exp(mean(log(table$AUCinf_dose1_withEffector)))),NA,NA),
                                                      CMAX_DDI =  c(my_3sf(exp(mean(log(table$Cmax_ss_withEffector)))),NA,NA),
                                                      AUC_GMR =   c(my_3sf(exp(mean(log(table$AUCinf_dose1_withEffector/table$AUCinf_dose1)))),
                                                                    my_3sf(exp(mean(log(table$AUCinf_dose1_withEffector/table$AUCinf_dose1))-t*sd(log(table$AUCinf_dose1_withEffector/table$AUCinf_dose1))/sqrt(length(table$AUCinf_dose1_withEffector/table$AUCinf_dose1)))),
                                                                    my_3sf(exp(mean(log(table$AUCinf_dose1_withEffector/table$AUCinf_dose1))+t*sd(log(table$AUCinf_dose1_withEffector/table$AUCinf_dose1))/sqrt(length(table$AUCinf_dose1_withEffector/table$AUCinf_dose1))))),
                                                      CMAX_GMR =  c(my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss)))),
                                                                    my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss))-z*sd(log(table$Cmax_ss_withEffector/table$Cmax_ss))/sqrt(length(table$Cmax_ss_withEffector/table$Cmax_ss)))),
                                                                    my_3sf(exp(mean(log(table$Cmax_ss_withEffector/table$Cmax_ss))+z*sd(log(table$Cmax_ss_withEffector/table$Cmax_ss))/sqrt(length(table$Cmax_ss_withEffector/table$Cmax_ss))))),
                                                      row.names = c("GeoMean", "CI_lower", "CI_upper"))
                  }}

            return(table_output_df)
      })

      output$table <- renderTable({

            req(input$sim_output_file)

            inFile <- input$sim_output_file
            file_name <- inFile$name
            file_name <- gsub(".xlsx", "", file_name)

            write.csv(generate_table(), file = paste("my_outputs/", file_name, ".csv", sep=""))

            generate_table()
      })


      # output$text_rpt <- renderPrint({
      #   inFile <- input$sim_output_file
      #   inFile$name
      #   })


})
