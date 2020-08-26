##*******************************************************
##                                                   ****
##   Unemployment Multi-State Model                  ####
##                                                   ****
##*******************************************************

##*******************************************************
##                                                   ****
##   1. Setup                                        ####
##                                                   ****
##*******************************************************

# This project is run by IFoA ICAT Health & Care working group

# Script by John Ng in July 2020
# Version 1.1

# Multi-State function and parameters sourced from Chris Martin and Scott Reid

##** Environment Details
# R v 3.6.0
# RStudio 1.2.1335

## Disable scientific notation (for example e+09) in R
options(scipen = 999)


##****************************************************
##  ~~ 1.1 Library                                ####
##****************************************************

# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
library(jsonlite)
library(shinyWidgets)
library(plotly)
library(rintrojs)
library(shinycssloaders)


##***************************
##  ~~ 1.2 Functions     ####
##***************************

   ##****************************************************
   ##   ~~ 1.2.1 Scenario.Simulation.function        ####
   ##****************************************************

source("Scenario_Simulation_function.R")

   ##***********************************************************
   ##  ~~ 1.2.2 Download Handler                            ####
   ##***********************************************************

download.data.function <- function(
  name, 
  data
  ){
  
  downloadHandler(
    filename = function() {
      paste(name, "_", Sys.Date(), ".csv", sep = "")
    },
    
    content = function(file) {
      
      write.csv(data, file, row.names = FALSE)
    }
  )
}

##*******************************************************
##                                                   ****
##   2. Preparation                                  ####
##                                                   ****
##*******************************************************

DF_sim_all <- read.csv("DF_simulation_scenarios.csv",stringsAsFactors=F )
DF_sim_all$date <- as.Date(DF_sim_all$date, "%Y-%m-%d")

##*******************************************************
##                                                   ****
##   3. Shiny - ui                                   ####
##                                                   ****
##*******************************************************

ui <- dashboardPage(
  
  skin = "yellow",
  
  ##****************************************************
  ##  ~~ 3.1 Dashboard Header                       ####
  ##****************************************************
  
  dashboardHeader(title = "Unemployment Multi-State Model",
                  titleWidth = 360
                  
                  #, dropdownMenu(
                  #  type = "notifications", 
                  #  icon = icon("question-circle"),
                  #  badgeStatus = NULL,
                  #  headerText = "About:"
                  #)
                  
                  ),
  
  ##****************************************************
  ##  ~~ 3.2 Dashboard Sidebar                      ####
  ##****************************************************
  
  dashboardSidebar(width = 360,
                   sidebarMenu(id = "sidebarmenu",
                               
                               menuItem(text = "Selection of Scenarios", 
                                        tabName = "selection_scenario", icon = icon("chart-line"), 
                                        selected=T),
                               
                               conditionalPanel("input.sidebarmenu === 'selection_scenario'",
                                                
                                                radioButtons("scenario_options", "Scenario options", 
                                                             c(
                                                               "01. OBR Central" = "scenario_1",
                                                               "02. OBR Upside" = "scenario_2",
                                                               "03. OBR Downside" = "scenario_3",
                                                               "04. Bank of England (May)" = "scenario_4",
                                                               "05. Bank of England (August)" = "scenario_5",
                                                               "06. Other forecasters' expectations - Central" = "scenario_6",
                                                               "07. Other forecasters' expectations - Upside" = "scenario_7",
                                                               "08. Other forecasters' expectations - Downside" = "scenario_8"
                                                               ),
                                                             selected = "scenario_1"),
                                                
                                                actionButton("action_plot1", "Plot")

                                                ),
                               
                               menuItem(text = "Custom Scenario", 
                                        tabName = "custom_scenario", 
                                        icon = icon("chart-line"),
                                        badgeLabel = "new", badgeColor = "green"
                               ),
                               
                               conditionalPanel( "input.sidebarmenu === 'custom_scenario'",
                                                
                                                p(#strong("Em = Employed" ),
                                                  #strong("Un = Unemployed" ),
                                                  #strong("Fu = Furlough" ),
                                                  #br(),
                                                  strong("Select Model Parameters"),
                                                  style="white-space: pre-wrap"),

                                                sliderInput("post_FuEm_FuUn_ratio",
                                                            "Ratio of rates of relative transfer from Furloughed to each of the Employed and Unemployed",
                                                            min = 0,
                                                            max = 10,
                                                            step = 0.25,
                                                            value = 1.75),
                                                sliderInput("post_FuEm_FuUn_damp",
                                                            "Dampening of rates of relative transfer from Furloughed to each of the Employed and Unemployed ",
                                                            min = 0,
                                                            max = 600,
                                                            step = 10,
                                                            value= 50),
                                                sliderInput("post_EmUn_ratio",
                                                            "Ratio of rates of reciprocal transfers from Employed to Unemployed",
                                                            min = 0,
                                                            max = 0.1,
                                                            step = 0.001,
                                                            value=0.03),
                                                sliderInput("post_EmUn_UnEm_damp",
                                                            "Dampening of ratio of transfers between Employed and Unemployed",
                                                            min = 0,
                                                            max = 2000,
                                                            step = 100,
                                                            value= 1000),
                                                
                                                actionButton("action_plot2", "Plot")
                               ), 
                               
                           menuItem(text = "Comparison of Unemployment Rates", 
                                        tabName = "comparison_scenario", icon = icon("chart-line") 
                                        ),
                                   
                            menuItem(text = "Model", 
                                     
                                  tabName = "tab_model", 
                                  icon = icon("project-diagram")
                                   ),
                   
                   
                            menuItem(text = "About", 
                                  tabName = "tab_about", 
                                  icon = icon("info-circle")
                                  )
                   
                           
                   )
                   ),

  
  ##***********************************************************
  ##  ~~ 3.3 Dashboard Body                                ####
  ##***********************************************************
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "selection_scenario",
              #h2("Selected Scenario"),
              
              fluidRow(
                div(
                  id = "plot_panel1",
                  column(
                    width = 9,
                    rintrojs::introBox(data.step = 1, data.intro = "test1", uiOutput("box_plot1"))
                  ),
                  column(
                    width = 9,
                    rintrojs::introBox(data.step = 2, data.intro = "test2", uiOutput("box_plot2"))
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "custom_scenario",
              #h2("Custom scenario under flexible parameters"),
              
              fluidRow(
                div(
                  id = "plot_panel2",
                  column(
                    width = 9,
                    rintrojs::introBox(data.step = 3, data.intro = "test3", uiOutput("box_plot3"))
                  ),
                  column(
                    width = 9,
                    rintrojs::introBox(data.step = 4, data.intro = "test4", uiOutput("box_plot4"))
                  )
                )
              )
      ),
      
      
      # Third tab content
      tabItem(tabName = "comparison_scenario",
              #h2("Comparison of scenarios"),
              
              fluidRow(
                div(
                  id = "plot_panel3",
                  column(
                    width = 9,
                    rintrojs::introBox(data.step = 5, data.intro = "test5", uiOutput("box_plot5"))
                  )
                )
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "tab_model",
              
              fluidPage(

                includeMarkdown("Model_Description.Rmd")
                
                #includeMarkdown("Model_Description_1.Rmd"),
                #br(),
                #plotOutput("img1", height=200),
                #includeMarkdown("Model_Description_2.Rmd"),
                #br(),
                #plotOutput("img2", height=200),
                #includeMarkdown("Model_Description_3.Rmd")
              )
      ),
      
      
      # Fifth tab content
      tabItem(tabName = "tab_about",
              fluidPage(
                #br(),
                #h3("About"),
                #br(),
                includeMarkdown("About.Rmd")
      )
      )

    )
    
  )
  
)

##*******************************************************
##                                                   ****
##   4. Shiny - server                               ####
##                                                   ****
##*******************************************************

server <- function(input, output, session){
  
  
  ##***********************************************************
  ##  ~~ 4.1 Selection of scenarios                        ####
  ##***********************************************************

  
  DF_sim <- eventReactive(input$action_plot1, {
    
    req(input$scenario_options)

    ## Generate Projections
    DF_sim <- DF_sim_all[DF_sim_all$scenario_number == input$scenario_options,]
      
  })
  

  ##***********************************************************
  ##  ~~ 4.1.1 Plot Selection of Scenarios                 ####
  ##***********************************************************
  
  ##**************************
  ##  Render Plotly         **
  ##**************************
  output$plot_render_selection <- renderPlotly({
    
    DF_plotly <- DF_sim()
    DF_plotly$Employed <- round(DF_plotly$Employed,2)
    DF_plotly$Furlough <- round(DF_plotly$Furlough,2)
    DF_plotly$Unemployed <-  round(DF_plotly$Unemployed,2)
    
    date_breaks <- c(as.Date("2020-06-22"), seq.Date(as.Date("2020-08-01"), as.Date("2022-01-01"), by="month"))
    relative.size = 1

    ## Generate  Plot **
    gg_box_plot1 <- ggplot() +
      
      geom_line(data = DF_plotly, aes(x = date, y = Unemployed, color = "Unemployed"), linetype = 1, lwd = 1) +
      
      geom_line(data = DF_plotly, aes(x = date, y = Furlough, color = "Furlough"), linetype = 1, lwd = 1) +
      
      geom_line(data = DF_plotly, aes(x = date, y = Employed, color = "Employed"), linetype = 1, lwd = 1) +
      
      scale_color_manual(name = "Legend", 
                         values = c("Unemployed" = "orange",
                                    "Furlough" = "blue",
                                    "Employed" = "green",
                                    "Unemployment Rate" = "red")) +
      
      scale_y_continuous(name = "Population Size (million)") +
      
      scale_x_date(breaks=date_breaks) +
      
      theme(axis.text.x = element_text(angle = 90, size=rel(relative.size)*0.9),
            axis.title.x = element_blank(),
            plot.title = element_text(size=rel(relative.size)),
            axis.text.y = element_text(color = "black", size=rel(relative.size)),
            axis.title.y = element_text(color = "black", size=rel(relative.size)),
            #axis.text.y.right = element_text(color = "red", size=rel(relative.size)),
            #axis.title.y.right = element_text(color = "red", size=rel(relative.size)),
            legend.position="bottom",
            legend.title = element_blank()) 
    
    style(
      ggplotly(gg_box_plot1) %>% config(displayModeBar = FALSE)
      %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.5)),
      hoverlabel = list(bgcolor = "white")
    )  
    
  })
  
  ##**************************
  ##  Render Title          **
  ##**************************
  output$selection_plot_title <- renderText({

      title <- "Multi-State Projections"

  })
  
  ##**************************
  ##  Output to UI          **
  ##**************************
  
  output$box_plot1 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_plot1",
        width = NULL,
        height = 500,
          tabPanel(
            title = htmlOutput("selection_plot_title"),
            div(
              style = "position: absolute; left: 0.5em; bottom: 0.5em;", 
              dropdown( 
                downloadButton(outputId = "button_1", label = "Download Data"),
                size = "s",
                icon = icon("download", class = "opt"), 
                up = FALSE
              )),
            withSpinner(
              plotlyOutput("plot_render_selection", height = 400),
              type = 6,
              color = "#e09200", 
              size = 0.9 
            )
      )
    )
    )
  })

  ##***********************************************************
  ##  ~~ 4.1.2 Plot Percentage                             ####
  ##***********************************************************
  
  ##**************************
  ##  Render Plotly         **
  ##**************************
  
  output$plot_render_percentage <- renderPlotly({
    
    DF_plotly <- DF_sim() 
    DF_plotly$Unemployment_Rate <-  round(DF_plotly$Unemployment_Rate,2)
    
    date_breaks <- c(as.Date("2020-06-22"), seq.Date(as.Date("2020-08-01"), as.Date("2022-01-01"), by="month"))
    relative.size = 1
    
    ## Generate  Plot 
    gg_box_plot2 <- ggplot() +
      
      geom_line(data = DF_plotly, aes(x = date, y = Unemployment_Rate, color = "Unemployment Rate"), linetype = 1, lwd = 1) +
      
      scale_color_manual(name = "Legend", 
                         values = c("Unemployment Rate" = "red")) +
      
      scale_y_continuous(name = "Unemployment Rate (%)") +
      
      scale_x_date(breaks=date_breaks) +
      
      theme(axis.text.x = element_text(angle = 90, size=rel(relative.size)*0.9),
            axis.title.x = element_blank(),
            plot.title = element_text(size=rel(relative.size)),
            axis.text.y = element_text(color = "black", size=rel(relative.size)),
            axis.title.y = element_text(color = "black", size=rel(relative.size)),
            #axis.text.y.right = element_text(color = "red", size=rel(relative.size)),
            #axis.title.y.right = element_text(color = "red", size=rel(relative.size)),
            legend.position="bottom",
            legend.title = element_blank()) 
    
    style(
      ggplotly(gg_box_plot2) %>% config(displayModeBar = FALSE)
      %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.5)),
      hoverlabel = list(bgcolor = "white")
    )  
  
    
  })
  
  ##**************************
  ##  Render Title          **
  ##**************************
  output$selection_percentage_plot_title <- renderText({
    
    title <- "Unemployment Rate"
    
  })
  
  ##**************************
  ##  Output to UI          **
  ##**************************
  
  output$box_plot2 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_plot2",
        width = NULL,
        height = 500,
        tabPanel(
          title = htmlOutput("selection_percentage_plot_title"),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;", 
            dropdown( 
              downloadButton(outputId = "button_2", label = "Download Data"),
              size = "s",
              icon = icon("download", class = "opt"), 
              up = FALSE
            )),
          withSpinner(
            plotlyOutput("plot_render_percentage", height = 400),
            type = 6,
            color = "#e09200", 
            size = 0.9 
          )
        )
      )
    )
  })
  
  ##***********************************************************
  ##  ~~ 4.2 Custom scenario                              ####
  ##***********************************************************
  
  DF_sim2 <- eventReactive(input$action_plot2, {
    
    req(input$post_FuEm_FuUn_ratio)
    req(input$post_FuEm_FuUn_damp)
    req(input$post_EmUn_ratio)
    req(input$post_EmUn_UnEm_damp)
    
    ## Generate Projections
    DF_sim <- Scenario.Simulation.function(
      input$post_FuEm_FuUn_ratio,
      input$post_FuEm_FuUn_damp,
      input$post_EmUn_ratio,
      input$post_EmUn_UnEm_damp
    )
  })
  
  
  ##*************************************************
  ##  ~~ 4.2.1 Plot Custom Scenario              ####
  ##*************************************************
  
  ##**************************
  ##  Render Plotly         **
  ##**************************
  
  output$plot_render_custom <- renderPlotly({
    
    DF_plotly <- DF_sim2()
    DF_plotly$Employed <- round(DF_plotly$Employed,2)
    DF_plotly$Furlough <- round(DF_plotly$Furlough,2)
    DF_plotly$Unemployed <-  round(DF_plotly$Unemployed,2)
    
    date_breaks <- c(as.Date("2020-06-22"), seq.Date(as.Date("2020-08-01"), as.Date("2022-01-01"), by="month"))
    relative.size = 1
    
    ## Generate  Plot **
    gg_box_plot1 <- ggplot() +
      
      geom_line(data = DF_plotly, aes(x = date, y = Unemployed, color = "Unemployed"), linetype = 1, lwd = 1) +
      
      geom_line(data = DF_plotly, aes(x = date, y = Furlough, color = "Furlough"), linetype = 1, lwd = 1) +
      
      geom_line(data = DF_plotly, aes(x = date, y = Employed, color = "Employed"), linetype = 1, lwd = 1) +
      
      scale_color_manual(name = "Legend", 
                         values = c("Unemployed" = "orange",
                                    "Furlough" = "blue",
                                    "Employed" = "green",
                                    "Unemployment Rate" = "red")) +
      
      scale_y_continuous(name = "Population Size (million)") +
      
      scale_x_date(breaks=date_breaks) +
      
      theme(axis.text.x = element_text(angle = 90, size=rel(relative.size)*0.9),
            axis.title.x = element_blank(),
            plot.title = element_text(size=rel(relative.size)),
            axis.text.y = element_text(color = "black", size=rel(relative.size)),
            axis.title.y = element_text(color = "black", size=rel(relative.size)),
            #axis.text.y.right = element_text(color = "red", size=rel(relative.size)),
            #axis.title.y.right = element_text(color = "red", size=rel(relative.size)),
            legend.position="bottom",
            legend.title = element_blank()) 
    
    style(
      ggplotly(gg_box_plot1) %>% config(displayModeBar = FALSE)
      %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.5)),
      hoverlabel = list(bgcolor = "white")
    )  
    
  })
  
  ##**************************
  ##  Render Title          **
  ##**************************
  output$custom_plot_title <- renderText({
    
    title <- "Custom Scenario: Multi-State Projections"
    
  })
  
  ##**************************
  ##  Output to UI          **
  ##**************************
  
  output$box_plot3 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_plot1",
        width = NULL,
        height = 500,
        tabPanel(
          title = htmlOutput("custom_plot_title"),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;", 
            dropdown( 
              downloadButton(outputId = "button_3", label = "Download Data"),
              size = "s",
              icon = icon("download", class = "opt"), 
              up = FALSE
            )),
          withSpinner(
            plotlyOutput("plot_render_custom", height = 400),
            type = 6,
            color = "#e09200", 
            size = 0.9 
          )
        )
      )
    )
  })
  
  ##***********************************************************
  ##  ~~ 4.2.2 Plot Percentage                             ####
  ##***********************************************************
  
  ##**************************
  ##  Render Plotly         **
  ##**************************
  
  output$plot_render_percentage_custom <- renderPlotly({
    
    DF_plotly <- DF_sim2()
    DF_plotly$Unemployment_Rate <-  round(DF_plotly$Unemployment_Rate,2)
    
    date_breaks <- c(as.Date("2020-06-22"), seq.Date(as.Date("2020-08-01"), as.Date("2022-01-01"), by="month"))
    relative.size = 1
    
    ## Generate  Plot 
    gg_box_plot2 <- ggplot() +
      
      geom_line(data = DF_plotly, aes(x = date, y = Unemployment_Rate, color = "Unemployment Rate"), linetype = 1, lwd = 1) +
      
      scale_color_manual(name = "Legend", 
                         values = c("Unemployment Rate" = "red")) +
      
      scale_y_continuous(name = "Unemployment Rate (%)") +
      
      scale_x_date(breaks=date_breaks) +
      
      theme(axis.text.x = element_text(angle = 90, size=rel(relative.size)*0.9),
            axis.title.x = element_blank(),
            plot.title = element_text(size=rel(relative.size)),
            axis.text.y = element_text(color = "black", size=rel(relative.size)),
            axis.title.y = element_text(color = "black", size=rel(relative.size)),
            #axis.text.y.right = element_text(color = "red", size=rel(relative.size)),
            #axis.title.y.right = element_text(color = "red", size=rel(relative.size)),
            legend.position="bottom",
            legend.title = element_blank()) 
    
    style(
      ggplotly(gg_box_plot2) %>% config(displayModeBar = FALSE)
      %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.5)),
      hoverlabel = list(bgcolor = "white")
    )  
    
    
  })
  
  ##**************************
  ##  Render Title          **
  ##**************************
  output$custom_percentage_plot_title <- renderText({
    
    title <- "Custom Scenario: Unemployment Rate"
    
  })
  
  ##**************************
  ##  Output to UI          **
  ##**************************
  
  output$box_plot4 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_plot2",
        width = NULL,
        height = 500,
        tabPanel(
          title = htmlOutput("custom_percentage_plot_title"),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;", 
            dropdown( 
              downloadButton(outputId = "button_4", label = "Download Data"),
              size = "s",
              icon = icon("download", class = "opt"), 
              up = FALSE
            )),
          withSpinner(
            plotlyOutput("plot_render_percentage_custom", height = 400),
            type = 6,
            color = "#e09200", 
            size = 0.9 
          )
        )
      )
    )
  })
  
  ##***********************************************************
  ##  ~~ 4.3 Comparison of scenarios                       ####
  ##***********************************************************
  
  DF_sim3 <- reactive({
    
    ## Generate Projections
    DF_sim3 <- DF_sim_all
  
  })
  
  
  ##***********************************************************
  ##  ~~ 4.3.2 Plot Percentage                             ####
  ##***********************************************************
  
  ##**************************
  ##  Render Plotly         **
  ##**************************
  
  output$plot_render_percentage_comparison <- renderPlotly({
    
    DF_plotly <- DF_sim3()
    DF_plotly$Unemployment_Rate <-  round(DF_plotly$Unemployment_Rate,2)

    date_breaks <- c(as.Date("2020-06-22"), seq.Date(as.Date("2020-08-01"), as.Date("2022-01-01"), by="month"))
    relative.size = 1
    
    ## Generate  Plot 
    gg_box_plot3 <- ggplot() +
      
      geom_line(data = DF_plotly, aes(x = date, y = Unemployment_Rate, color = scenario), linetype = 1, lwd = 1) +
      
      #scale_color_manual(name = "Legend", 
      #                   values = c("Unemployment Rate" = "red")) +
      
      scale_y_continuous(name = "Unemployment Rate (%)") +
      
      scale_x_date(breaks=date_breaks) +
      
      theme(axis.text.x = element_text(angle = 90, size=rel(relative.size)*0.9),
            axis.title.x = element_blank(),
            plot.title = element_text(size=rel(relative.size)),
            axis.text.y = element_text(color = "black", size=rel(relative.size)),
            axis.title.y = element_text(color = "black", size=rel(relative.size)),
            #axis.text.y.right = element_text(color = "red", size=rel(relative.size)),
            #axis.title.y.right = element_text(color = "red", size=rel(relative.size)),
            legend.position="right",
            legend.title = element_blank()) 
    
    style(
      ggplotly(gg_box_plot3,height = 580, width = 580) %>% config(displayModeBar = FALSE)
      %>% layout(legend = list(orientation = "h", x = 0.05, y = -0.3,
                               font = list(
                                 family = "sans-serif",
                                 size = 12,
                                 color = "#000"))),
      hoverlabel = list(bgcolor = "white")
    )  
    
    
  })
  
  ##**************************
  ##  Render Title          **
  ##**************************
  output$comparison_percentage_plot_title <- renderText({
    
    title <- "Comparison of Unemployment Rates"
    
  })
  
  ##**************************
  ##  Output to UI          **
  ##**************************
  
  output$box_plot5 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_plot5",
        width = NULL,
        height = 700,
        tabPanel(
          title = htmlOutput("comparison_percentage_plot_title"),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;", 
            dropdown( 
              downloadButton(outputId = "button_5", label = "Download Data"),
              size = "s",
              icon = icon("download", class = "opt"), 
              up = FALSE
            )),
          withSpinner(
            plotlyOutput("plot_render_percentage_comparison", height = 400),
            type = 6,
            color = "#e09200", 
            size = 0.9 
          )
        )
      )
    )
  })
  
  ##***********************************************************
  ##  ~~ 4.4 Image                                         ####
  ##***********************************************************
  
  #output$img1 <- renderImage({
  #  filename <- normalizePath(file.path('./images',"furlough_states.png"))
  #  list(src = filename)
  #  
  #}, deleteFile = FALSE)
  #
  #
  #output$img2 <- renderImage({
  #  filename <- normalizePath(file.path('./images',"post_furlough_states.png"))
  #  list(src = filename)
  #  
  #}, deleteFile = FALSE)
  
  ##***********************************************************
  ##  ~~ 4.5 Download Handler                              ####
  ##***********************************************************
  
  output$button_1 <- download.data.function("Select_Scenario_data", DF_sim())
  output$button_2 <- download.data.function("Select_Scenario_Percentage_data", DF_sim())
  output$button_3 <- download.data.function("Custom_Scenario_data", DF_sim2())
  output$button_4 <- download.data.function("Custom_Scenario_Percentage_data", DF_sim2())
  output$button_5 <- download.data.function("AllScenarios_Unemployment_data", DF_sim3())
}  


##*******************************************************
##                                                   ****
##   5. Run Shiny                                    ####
##                                                   ****
##*******************************************************

shinyApp(ui = ui, server = server)

