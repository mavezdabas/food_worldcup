## Library invocations and a couple of globals (Icons, some map-config options, serviceGeoData, and teslaCentersOnly) 
##    are defined in global.R
library(caTools)
library(shiny)
library(shinydashboard)
library(httr)
library(leaflet)
library(dplyr)
library(ggmap)
library(geosphere)
library(geosphere)
library(data.table)
library(scales)
library(plotly)
library(lazyeval)
library(shinyjs)
library(mongolite)


# For Parallel Coordinates
#devtools::install_github("timelyportfolio/parcoords")
library(parcoords)


# Simple Paralle Corrdinates
# Brush On 
# parcoords(head(na.omit(foodData)), 
#           # brush = "1d-axes", # 2d-strums are really neat, 
#           reorderable = TRUE
# )
# data( diamonds, package = "ggplot2" )
# parcoords(
#   na.omit(diamonds)
#   , rownames = F # turn off rownames from the data.frame
#   , brushMode = "2D-strums"
#   , reorderable = T
#   , queue = T
#   , color = RColorBrewer::brewer.pal(4,"BuPu")[4]
# )
# library(dplyr)
# diamonds[sample(1:nrow(diamonds),1000),] %>%
#   mutate( carat = cut(carat, breaks=c(0,1,2,3,4,5), right = T)) %>%
#   select( carat, color, cut, clarity, depth, table, price,  x, y, z) %>%
#   parcoords(
#     rownames = F # turn off rownames from the data.frame
#     , brushMode = "2D-strums"
#     , reorderable = T
#     , queue = T
#     , color = list(
#       colorBy = "carat"
#       ,colorScale = htmlwidgets::JS("d3.scale.category10()")
#     )    
#   )


#####################################
# UI Header #
#####################################


  
  # Team Information
  # tabPanel("Team MJ",
  #          div(class="outer",
  #              tags$head(
  #                # Include our custom CSS
  #                includeCSS("www/custom.css"),
  #                includeScript("gomap.js")
  #              ),
  #              h2("Team Members",align = "center"),
  #              p(),
  #              p(),
  #              h3("1. Mavez Singh Dabas",align = "center"),
  #              p("Masters of Computer Sciene",align = "center"),
  #              p(),
  #              h3("2. Jinni",align = "center"),
  #              p("Masters of Arts Media and Designe",align = "center"),
  #              p(),
  #              p("Story Telling: As we have discussed Characters, Setting,
  #                Plot, Conflict, Resolution")
  #          )
  # ),

#------------------------------------------------------------  
header <- dashboardHeader(title = "Food World Cup")

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(id = "sidebarmenu",
              p(),
              # Exploratory Data Analysis
              menuItem("Exploratory Data Analysis", tabName = "exploratoryTab",
                       icon = icon("bar-chart"), selected = TRUE),
              conditionalPanel("input.sidebarmenu == 'exploratoryTab'",
                               useShinyjs(),  
                               # uiOutput("knowledgeSelectUI"),
                               # Input for the Region Select
                               #  This input will be used for the maps
                               # selectInput("region",label = "Select Region",
                               #             c("All","East North Central",
                               #               "East South Central",
                               #               "Middle Atlantic","Mountain",
                               #               "New England","Pacific",
                               #               "South Atlantic",
                               #               "West North Central",
                               #               "West South Central"),
                               #             multiple = FALSE,selected = c("East North Central"))
                               p()

              ),
              # Cuisine Distribution
              menuItem("Cuisine", tabName = "cuisines",icon = icon("heartbeat"), selected = FALSE),
              conditionalPanel("input.sidebarmenu == 'analysis'",
                               p()
              ),
              menuItem("Team Information",tabName = "teamTab",
                       icon = icon("whatsapp"),selected = FALSE)
              
  )
)



body <- dashboardBody(
  includeScript("www/scripts.js"),
  includeScript('www/spin.min.js'),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Control.Loading.css")
  ),
  tabItems(
    tabItem(tabName = "exploratoryTab",
            includeScript('www/Control.Loading.js'),
          
            # Heading of the Page
            h2("Exploratory Data Analysis",align = "center"),
            # Input Parameter Values
            selectInput("region",label = "Select Region",
                        c("All","East North Central",
                          "East South Central",
                          "Middle Atlantic","Mountain",
                          "New England","Pacific",
                          "South Atlantic",
                          "West North Central",
                          "West South Central"),
                        multiple = FALSE,selected = c("East North Central")),
            fluidRow(
              box(plotlyOutput("mapPlot"),width = 12)),
            
            fluidRow(
              # column(12,box(plotlyOutput("mapPlot"),width = 12)) ,
              box(plotlyOutput("knowledgePlot"),width=4,
                           title="Level of Food Knowledge"),
              box(plotlyOutput("genderPlot"),width=4,
                           title="Gender Distribution"),
              box(plotlyOutput("agePlot"),width=4,
                           title="Age Distribution")
            ),
            
            h2("Regional Percentage"),
            parcoordsOutput("parallelPlot"),
            p(),
            
            p(),
            # Input for the Cuisisne Select
            #  This input will be used for Parallel Coordinate Maps
            h2("Summary Plot"),
            selectInput("cuisine",label = "Select Cuisine",
                        c("American" ,
                          "Mexico","Italian",
                          "Chinese" ,"Japnese" , 
                          "English" ,
                          "French" ,
                          "German" ,
                          "Indian" ,
                          "Greece" ,"Thai" ),
                        multiple = FALSE,selected = c("United_States")),
            parcoordsOutput("parallelPlot_2"),
            DT::dataTableOutput("regionTopUI"),
            p()
            )
  )
)

dashboardPage(header, sidebar, body,skin = "black")

