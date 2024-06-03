#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
#library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Simcyp consultancy R package index"),
   sidebarLayout(
      sidebarPanel(
         selectInput("category","Category",choices=unique(ConsultancyIndex$Category)),
         selectInput("fsc","First sub category",choices = NULL),
         selectInput("ssc","Second sub category",choices = NULL),
         selectInput("question","How to do",choices = NULL),
         br(),),
      mainPanel(fluidRow(
         column(7,h3("Start your code with:"),
                htmlOutput("start")),
         column(5,h3("Steps to follow"),
                htmlOutput("steps"))),
         fluidRow(
            column(6,h3("Simcyp consultancy R Function"),
                   htmlOutput("Consultfunc")),
            column(6,h3("Need Help ?",offset=8),
                   htmlOutput("docfile"))),
         fluidRow(
            column(6,h3("R code to copy"),
                   htmlOutput("Consultscript")),
            column(6,h3("Argument"),
                   htmlOutput("Consultarg"))),
         fluidRow(
            column(6,h3("Related function"),
                   htmlOutput("Consultrf")),
            column(6,h3("comments"),
                   htmlOutput("Consultcom"))),
         #fluidRow(h3("Code"),
         #        tableOutput("data")),
         
         fluidRow(
            column(10,h3("Output"),
                   imageOutput("figure"))))
   )
)


server <- function(input, output, session) {
   category <- reactive({
      filter(ConsultancyIndex, Category == input$category)
   })
   observeEvent(category(),{
      choices <- unique(category()$FirstSubCategory)
      updateSelectInput(inputId = "fsc",choices = choices)
   })
   
   fsc <- reactive({
      req(input$fsc)
      filter(category (), FirstSubCategory == input$fsc)
   })
   observeEvent(fsc(),{
      choices <- unique(fsc()$SecondSubCategory)
      updateSelectInput(inputId = "ssc",choices = choices)
   })
   
   ssc <- reactive({
      req(input$ssc)
      filter(fsc(), SecondSubCategory == input$ssc)
   })
   observeEvent(ssc(),{
      choices <- unique(ssc()$Question)
      updateSelectInput(inputId = "question",choices = choices)
   })
   
   
   #  output$data <- renderTable({
   #    req(input$question)
   #    category() %>% 
   #      filter (Question == input$question) %>% 
   #      select(Function,Initial_Script, Argument,Related_Function,Comment)
   #  })
   
   
   output$Consultrfp<- renderPrint({
      req(input$question)
      category() %>% 
         filter (Question == input$question) %>% 
         select(Related_Function)
   })
   
   
   output$Consultfunc<- renderUI({
      req(input$question)
      text_Consultfunc <- category() %>% 
         filter (Question == input$question) %>% 
         select(Function)
      tagList(tags$pre(text_Consultfunc))
   })
   output$Consultscript<- renderUI({
      req(input$question)
      text_Consultscript <- category() %>% 
         filter (Question == input$question) %>% 
         select(Initial_Script)
      tagList(tags$pre(text_Consultscript))
   })
   output$Consultarg<- renderUI({
      req(input$question)
      text_Consultarg <- category() %>% 
         filter (Question == input$question) %>% 
         select(Argument)
      tagList(tags$pre(text_Consultarg))
   })
   
   
   output$Consultrf<- renderUI({
      req(input$question)
      text_Consultrf <- category() %>% 
         filter (Question == input$question) %>% 
         select(Related_Function)
      
      tagList(tags$pre(text_Consultrf))
   })
   
   output$Consultcom<- renderUI({
      req(input$question)
      text_Consultcom <- category() %>% 
         filter (Question == input$question) %>% 
         select(Comment)
      tagList(tags$pre(text_Consultcom))
   })
   
   output$start <- renderUI({
      req(input$question)
      text_start <- category() %>% 
         filter (Question == input$question) %>% 
         select(start)
      tagList(tags$pre(text_start))
   })
   
   output$steps <- renderUI({
      req(input$question)
      text_steps<- category() %>% 
         filter (Question == input$question) %>% 
         select(steps)
      tagList(tags$pre(text_steps))
   })
   
   
   output$figure <- renderImage({
      req(input$question)
      name <- category() %>% 
         filter (Question == input$question) %>% 
         select(Figure)
      
      filename <- system.file("images", paste0(name, ".png"), package = "SimcypConsultancy")
      # normalizePath(file.path("./images",paste("", name, ".png", sep="")))
      list(src = filename,
           width = "50%")#, height= "50%")
   },deleteFile=FALSE)
   
   
   output$docfile <- renderUI({
      req(input$question)
      link <- category() %>% 
         filter (Question == input$question) %>% 
         select(HelpFile)
      url <- a("Click here", href=link)
      tagList(url)
      
   })
   
}


# Run the application 
shinyApp(ui = ui, server = server)
