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
            h1("Food World Cup !!",align = "center"),
            
            br(),
            h4("How many of you here will call yourself foodie ???",align= "center"),
            h4("What about your favourite cuisine ???",align= "center"),
            #h4("Do you consider yourself as a Expert or Novice when it comes to rating ???",align= "center"),
            h4("Are you happy with what we have ???",align= "center"),
            #h4("What if we have food of our choice for lunch today ???",align= "center"),
            #h3("But !!",align = "center"),
            br(),
            br(),
            h5("The job is not as simple as it may seem.....",align = "center"),
            h5("Entrepreneur/Departments need some more legitimate reasons to 
               change the current infrastructure...",align = "center"),
            h5("Making a decision should be followed by some research providing 
               a backbone to process...",align = "center"),
            p(),
            p(),
            h5("Team MJ to the rescue. Team MJ realized this problem 
               and therefore we decided to work on something similar.
               We used the rating dataset to create this application which is
               similar to the problem addressed above.",align = "center"),
            p(),
            h5("We present some interactive set of Visualization which might be interested
               to both customers and stakeholders",align = "center"),
            # Heading of the Page
            br(),
            br(),
            h5("American’s opinion on cuisines across the world. Food-World-Cup dataset released by FivethirtyEight as
                our preliminary dataset to work on. The dataset is a CSV file with 48 columns
               and 1373 rows showing 1373 individuals’ opinions on cuisines across the world.",align = "center"),
            p(),
            h5("FivethirtyEight published a series of infographic in 2014 based on this dataset,
               in an attempt to visualize the popularity of world cuisines in U.S. However, the
               infographic, from today’s eyes, is not very intuitive and well designed, so we
               decided to make full use of this multi-dimensional dataset. This visualization can
               provide some insights for school/company cafeteria to improve their dining
               service quality.",align = "center"),
            #
            #
            #
            h2("Exploratory Data Analysis",align = "center"),
            br(),
            h5("We have cuisines rating from 1 - 5 with 5 being the highest.. "),
            h5("In the following map we have aggregated the top rated cuisines 
               for each region and calculated the percentage distribution 
               of each cuisine."),
            h5("Out of the percentage we have plotted the best cuisine and 
                the percentage corresponding to the cuisine.",align = "left"),
            h5("Favourite cuisine across US is Italian."),
            h5("Along with the Map View we have also plotted a couple of 
              exploratory plots Knowledge, Gender and Age."),
            p(),
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
              box(plotlyOutput("knowledgePlot"),width=4),
              box(plotlyOutput("genderPlot"),width=4),
              box(plotlyOutput("agePlot"),width=4)
            ),
            br(),
            br(),
            h2("Regional Parallel Distribution",align = "center"),
            h5("To get a better understanding of the percentage distribution
               we have plotted a coordinate plot with percentage of cuisines
               across different regions."),
            parcoordsOutput("parallelPlot"),
            p(),
            
            p(),
            # Input for the Cuisisne Select
            #  This input will be used for Parallel Coordinate Maps
            h2("Summary",align = "center"),
            h5("We wanted to know the distribution not only depending on region wise
               but also along the other features as well."),
            h5("We have the following Parallel Coordinate Graph to view the 
               frequency of each cuisine's spread for other important features "),
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
            h5("Favourite Cuisine Percentage Distribution"),
            DT::dataTableOutput("regionTopUI"),
            br(),
            h5("Complete Dataeset"),
            DT::dataTableOutput("topFoodUI"),
            # This is the Reference Section of the Visualization
            h2("References", align = "center"),
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
            h4("1. Mavez Singh Dabas"),
            p("Masters of Computer Sciene"),
            a("Mavez's: Linkedin", href="https://www.linkedin.com/in/mavezdabas"),
            br(),
            h4("2. Jinni(Kini) Luo"),
            p("Masters of Arts Media and Designe"),
            a("Jinni's: Linkedin", href="https://www.linkedin.com/in/jinniluo"),
            br(),
            p()
    )
  )
)

dashboardPage(header, sidebar, body,skin = "blue")

