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
              # # Cuisine Distribution
              # menuItem("Cuisine", tabName = "cuisines",icon = icon("heartbeat"), selected = FALSE),
              # conditionalPanel("input.sidebarmenu == 'analysis'",
              #                  p()
              # ),
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
            h1("Food !!",align = "center"),
            h4("American’s opinion on cuisines across the world. Food-World-Cup dataset released by FivethirtyEight as
                our preliminary dataset to work on. The dataset is a CSV file with 48 columns
                and 1373 rows showing 1373 individuals’ opinions on cusines across the world.",align = "center"),
            p(),
            h4("FivethirtyEight published a series of infographics in 2014 based on this dataset,
               in an attempt to visualize the popularity of world cuisines in U.S. However, the
               infographic, from today’s eyes, is not very intuitive and well designed, so we
               decided to make full use of this multi-dimensional dataset. This visualization can
               provide some insights for school/company cafeteria to improve their dining
               service quality.",align = "center"),
            p(),
            p(),
            # Heading of the Page
            h2("Exploratory Data Analysis",align = "center"),
            br(),
            p("Finding the Percentage of cuisines depending on the region."),
            # Input Parameter Values
            selectInput("region",label = "Select Region",
                        c("East North Central",
                          "East South Central",
                          "Middle Atlantic","Mountain",
                          "New England","Pacific",
                          "South Atlantic",
                          "West North Central",
                          "West South Central"),
                        multiple = FALSE,
                        selected = c("East North Central")
            ),
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
            # This is the Reference Section of the Visualization
            h3("Refrences", align = "center"),
            p("Please find the following links to the references used in designing and programming the application "),
            a("1. Github Link", href="https://github.com/fivethirtyeight/data/tree/master/food-world-cup"),
            br(),
            a("2: R/RStudio",href="https://shiny.rstudio.com/"),
            br(),
            a("3: Shiny Showcase",href="https://www.rstudio.com/products/shiny/shiny-user-showcase/"),
            br(),
            a("4: D3/Plotly",href="https://plot.ly/r/"),
            br(),
            a("5: Parallel Coordinates",href=" http://www.buildingwidgets.com/blog/2015/1/30/week-04-interactive-parallel-coordinates-1"),
            br(),
            a("6: ShinyApps Server", href="https://www.shinyapps.io/"),
            br(),
            p()
    ),
    tabItem(tabName = "teamTab",
            includeScript('www/Control.Loading.js'),
            h2("Team MJ",align = "center"),
            br(),
            h3("1. Mavez Singh Dabas"),
            p("Masters of Computer Sciene"),
            a("Mavez's: Linkedin", href="https://www.linkedin.com/in/mavezdabas"),
            br(),
            h3("2. Jinni(Kini) Luo",align = "center"),
            p("Masters of Arts Media and Designe"),
            a("Jinni's: Linkedin", href="https://www.linkedin.com/in/jinniluo"),
            br(),
            p()
    )
  )
)

dashboardPage(header, sidebar, body,skin = "black")

