library(ggplot2)
library(readr)
library(dplyr)
library(geosphere)
library(tibble)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(knitr)
library(leaflet)
library(ggmap)
library(maps)
library(mapdata)
library(rvest)
library(magrittr)
library(stringr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(shiny)
library(shinydashboard)
library(gridExtra)


city_data <- read_csv("city_data.csv")
categ_data <- read_csv("categ_data.csv")
allCateg = unique(categ_data$normTitleCategory)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Annual Statistics", tabName = "AnnualStat", icon = icon("th")),
    menuItem("Dynamic Trend by Industry", icon = icon("dashboard"), tabName = "DyTrend1"),
    menuItem("Dynamic Trend by States", icon = icon("institution"), tabName = "DyTrend2"),
    menuItem("Models", icon = icon("graduation-cap"), tabName = "FittedModels"),
    
    br(),
    br(),
    br(),

    actionButton("do", "See Results")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "AnnualStat",
            h2("Annual Information by City"),
            div(style="display:inline-block",selectInput("dataSelect", label = "Variable for city map", 
                                                         choices = c("Annual Salary", "Clicks per job", 
                                                                     "Local clicks ratio"), 
                                                         selected = "Annual Salary")),
            div(style="display:inline-block",selectInput("categSelect", label = "Industry", 
                                                         choices = c("All",allCateg), 
                                                         selected = "All")),
            
            leafletOutput("map", width="100%",height=600)
    ),
    
    tabItem(tabName = "DyTrend1",
            h2("Monthly Changes by Industry"),
            div(style="display:inline-block",selectInput("categDataSort", 
                                                         label = "Way to sort industries", 
                                                         choices = c("By value", "By name"), 
                                                         selected = "By value")),
            div(style="display:inline-block",selectInput("categDataSelect", 
                                                         label = "Variable for Changes in Industry", 
                                                         choices = c("Number of jobs", "Number of clicks"), 
                                                         selected = "Number of jobs")),

            uiOutput("categImage")
    ),
    
    tabItem(tabName = "DyTrend2",
            h2("Monthly Changes by State"),
            selectInput("monthDataSelect", 
                        label = "Variable for monthly trend", 
                        choices = c("Number of jobs", "Number of clicks",
                                    "Number of local clicks", "Proportion of local clicks"), 
                        selected = "Number of jobs"),
            uiOutput("MapTrendImage")
    ),

    tabItem(tabName = "FittedModels",
            h2("Model"),
            selectInput("modelSelect",
                        label = "Select Model",
                        choices = c("Gaussian Process", "Time Series"),
                        selected = "Gaussian Process"),
            uiOutput("ModelImage")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Indeed Job Data Visualization by BrunchLadies",
                  titleWidth = 450),
  sidebar,
  body
)



server = function(input, output, session)
{
  output$map<-renderLeaflet({
    leaflet() %>%
      addTiles()  
  })
  
  output$categImage <- renderUI({
    img(src = "animation_1.gif", width="800")
  })
  
  output$MapTrendImage <- renderUI({
    img(src = "total_jobs.gif", width="1000")
  })
  
  output$ModelImage <- renderUI({
    img(src = "GP_Model.png", width="600")
  })
  
  observeEvent(input$do,{
    
    ## Get Dataset
    if(input$categSelect=="All"){
      data = city_data
      r = data$n/180
    }else{
      data = categ_data %>% 
        filter(normTitleCategory==input$categSelect)
      r = data$n/15
    }
    
    if(input$dataSelect=="Annual Salary"){
      pal <- colorNumeric(palette = c("lightpink", "red4"), domain = data$medianSalary)
      p1 = leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng=data$lng, lat=data$lat, popup=data$SalaryInfo,
                         color = pal(data$medianSalary), stroke = FALSE,
                         radius = r, fillOpacity = 0.8) %>% 
        addLegend("bottomright", pal = pal, values = data$medianSalary,
                  title = "Median Salary",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 0.8)
    }else if(input$dataSelect=="Clicks per job"){
      pal <- colorNumeric(palette = c("lightblue", "darkblue"), domain = data$cityMeanClick)
      p1 = leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng=data$lng, lat=data$lat, popup=data$ClickInfo,
                         color = pal(data$cityMeanClick), stroke = FALSE,
                         radius = r, fillOpacity = 0.8) %>% 
        addLegend("bottomright", pal = pal, values = data$cityMeanClick,
                  title = "Average Click per job",
                  opacity = 0.8)
    }else{
      pal <- colorNumeric(palette = c("lightpink", "red4"), domain = data$localRatio)
      
      p1 = leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng=data$lng, lat=data$lat, popup=data$LocalClickInfo,
                         color = pal(data$localRatio), stroke = FALSE,
                         radius = r, fillOpacity = 0.8) %>% 
        addLegend("bottomright", pal = pal, values = data$localRatio,
                  title = "Local Click Ratio per job",
                  opacity = 0.8)
    }
    
    if(input$categDataSort=="By value"){
      if(input$categDataSelect=="Number of clicks"){
        imageName = "haha_1.gif"
      }else{
        imageName = "animation_1.gif"
      }
    }else{
      if(input$categDataSelect=="Number of clicks"){
        imageName = "haha_0.gif"
      }else{
        imageName = "animation_0.gif"
      }
    }
    
    if(input$monthDataSelect=="Number of jobs"){
      mapName = "total_jobs.gif"
    }else if(input$monthDataSelect=="Number of clicks"){
      mapName = "total_clicks.gif"
    }else if(input$monthDataSelect=="Number of local clicks"){
      mapName = "total_localClicks.gif"
    }else{
      mapName = "localClicks_ratio.gif"
    }

    if(input$modelSelect=="Gaussian Process"){
      modelName = "GP_Model.png"
    }else{
      modelName = "TS_model.png"
    }
    
    output$categImage <- renderUI({
      img(src = imageName, width="800")
    })
    
    output$MapTrendImage <- renderUI({
      img(src = mapName, width="1000")
    })
    
    output$ModelImage <- renderUI({
      img(src = modelName, width="600")
    })
    
    
    output$map<-renderLeaflet({
      p1
    })
  })
}


shinyApp(ui, server)