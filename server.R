#####################################
# Map Related Globals #
#####################################
zoomLevel = 3
defaultLong = 0.000
defaultLat = 40.7127

tcp_map <- "https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibWF2ZXpkYWJhcyIsImEiOiJjaXExaHRpcGowMHl6ZmhubnlqZzZ1djE5In0.EdKaYkoXHl-u94z64QAB6Q"

mb_attribution <- "© Mavez Singh Dabas © <a href='https://www.linkedin.com/in/mavezdabas'>Linkedin</a>"

#####################################
# Functions #
#####################################
clearMap <- function(){
  leafletProxy("mainMap") %>% clearMarkers() %>% clearShapes() %>% clearMarkerClusters()
}


##############################
# Data Frames #
##############################
# Food Data
foodData <- read.csv("food-world-cup-data.csv",header = TRUE)


##############################
# Processed Data File #
##############################
# Selecting the top food cuisines
topFood <- foodData %>%
  dplyr::select(Level_of_knowledge,
                Interested_cuisines_world,Household_Income,Age,
                Location,Education,Gender,United_States,Mexico,Italy,
                China,Japan,England,France,Germany,India,Greece,Thailand)

##############################
# Server #
##############################
server <- function(input,output,session) {
  rValues <- reactiveValues()

  ## Initial rendering of the map
  output$mainMap <- renderLeaflet({ leaflet() %>% 
      setView(lng = defaultLong, lat = defaultLat, zoom = zoomLevel) %>%
      addTiles(urlTemplate = tcp_map,attribution = mb_attribution)

  })

  
  
  observe({
    # Knowledge Filter
    output$knowledgeSelectUI <- renderUI({ selectizeInput("knowledgeSelect",
                                                          label = "Select Knowldege:",
                                                          levels(topFood$Level_of_knowledge),
                                                          options = list(placeholder = "Select"))})
    # Interest Filter
    output$cuisineInterestUI <- renderUI({ selectizeInput("interestSelect",
                                                          label = "Select Interest:",
                                                          levels(topFood$Interested_cuisines_world),
                                                          options = list(placeholder = "Select"))})
    # Income Filter
    output$incomeUI <- renderUI({ selectizeInput("incomeSelect",
                                                 label = "Select Income:",
                                                 levels(topFood$Household_Income),
                                                 options = list(placeholder = "Select"))})
    
    # Age Filter
    output$ageUI <- renderUI({ selectizeInput("ageSelect",
                                                 label = "Select Age:",
                                                 levels(topFood$Age),
                                                 options = list(placeholder = "Select"))})

    # Education Filter
    output$educationUI <- renderUI({ selectizeInput("educationSelect",
                                                 label = "Select Education:",
                                                 levels(topFood$Education),
                                                 options = list(placeholder = "Select"))})
    
    # Gender Filter
    output$genderUI <- renderUI({ selectizeInput("genderSelect",
                                                 label = "Select Gender:",
                                                 levels(topFood$Gender),
                                                 options = list(placeholder = "Select"))})
  
    
    # Progress Bar with all the reactive Values
    withProgress({
      # Raw Data Table
      output$foodTableUI  <- DT::renderDataTable({foodData})
      # Table for top foods
      # output$topFoodUI <- DT::renderDataTable({
      #   topFood %>%
      #     dplyr::filter(Level_of_knowledge == input$knowledgeSelect)
      #   })
      # Location Based Table
      
      if (input$region == "East North Central") {
        # Table Data
        tableRendered <- topFood %>%
          dplyr::filter(Location == "East North Central")
      }else if(input$region == "East South Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "East South Central")
      }else if(input$region == "Middle Atlantic"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Middle Atlantic")
      }else if(input$region == "Mountain"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Mountain")
      }else if(input$region == "New England"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "New England")
      }else if(input$region == "Pacific"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Pacific")
      }else if(input$region == "South Atlantic"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "South Atlantic")
      }else if(input$region == "West North Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "West North Central")
      }else if(input$region == "West South Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "West South Central")
      }else if(input$region == "All"){
        tableRendered <- topFood
      }else {
        tableRendered <- topFood
      }
        
      
      # Knowledge table for each table
      knowledgeTable <- as.data.frame(table(tableRendered$Level_of_knowledge))
      colnames(knowledgeTable) <- c("Knowledge_Level","Number")
      # Gender Table
      genderTable <- as.data.frame(table(tableRendered$Gender))
      colnames(genderTable) <- c("Gender","Number")
      # Age Table
      ageTable <- as.data.frame(table(tableRendered$Age))
      colnames(ageTable) <- c("Age","Number")

      
      # Plot to show the distribution of level of knowledge
      output$knowledgePlot <- renderPlotly({
        plot_ly(
          data = knowledgeTable,
          x = ~Knowledge_Level,
          y = ~Number,
          type = "bar"
        )
      })
      # Pie Chart to show percentage of male and female in different 
      # regions
      output$genderPlot <- renderPlotly({
        plot_ly(genderTable, labels = ~Gender, values = ~Number, type = 'pie') %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
      })
      
      # Bar Chart to show the distribution of Age as per different region
      output$agePlot <- renderPlotly({
        plot_ly(ageTable, x = ~Age, y = ~Number, type = 'bar',color = ~Age) #%>%
          # layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          #        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
      })

      
    },min = 0, max = 1, value = NULL, message = "Loading", detail= "Loading")

  })
  
  observe({
    
    # Region wise best food Filtering
    region_TopFood <- topFood %>%
      dplyr::group_by(Location) %>%
      dplyr::summarise(American = round((mean(United_States == 5,na.rm = TRUE))*100,2),
                       Mexican = round((mean(Mexico == 5,na.rm = TRUE))*100,2),
                       Italian  = round((mean(Italy == 5,na.rm = TRUE))*100,2),
                       Chinese = round((mean(China == 5,na.rm = TRUE))*100,2),
                       Japnese   = round((mean(Japan == 5,na.rm = TRUE))*100,2),
                       English  = round((mean(England == 5,na.rm = TRUE))*100,2),
                       French  = round((mean(France == 5,na.rm = TRUE))*100,2),
                       German = round((mean(Germany == 5,na.rm = TRUE))*100,2),
                       Indian = round((mean(India == 5,na.rm = TRUE))*100,2),
                       Greece  = round((mean(Greece == 5,na.rm = TRUE))*100,2),
                       Thai = round((mean(Thailand == 5,na.rm = TRUE))*100,2))
    region_TopFood_Max <- region_TopFood %>%
      dplyr::select(-1)
    region_TopFood$Best <- colnames(region_TopFood_Max)[apply(region_TopFood_Max,1,which.max)]
    region_TopFood$BestPercentage <- apply(region_TopFood_Max,1,max)
    region_TopFood <- region_TopFood[1:9,]
    rm(region_TopFood_Max)
    
    # Cuisine Distribution between regions
    # cuisine_Distribution <- topFood %>%
    #   dplyr::filter(United_States == "5",Mexico == "5",Italy,                    
    #                 China == "5",Japan == "5", England == "5", 
    #                 France == "5",Germany == "5",India == "5",
    #                 Greece == "5",Thailand == "5") %>%
    #   dplyr::group_by(rbind(United_States,Mexico)) %>%
    #   dplyr::summarise(East_North_Central = round((mean(Location == "East North Central",na.rm = TRUE))*100,2),
    #                    East_South_Central = round((mean(Location == "East South Central",na.rm = TRUE))*100,2),
    #                    Middle_Atlantic = round((mean(Location == "Middle Atlantic",na.rm = TRUE))*100,2),
    #                    Mountain = round((mean(Location == "Mountain",na.rm = TRUE))*100,2),
    #                    New_England = round((mean(Location == "New England",na.rm = TRUE))*100,2),
    #                    Pacific = round((mean(Location == "Pacific",na.rm = TRUE))*100,2),
    #                    South_Atlantic = round((mean(Location == "South Atlantic",na.rm = TRUE))*100,2),
    #                    West_North_Central = round((mean(Location == "West North Central",na.rm = TRUE))*100,2),
    #                    West_South_Central = round((mean(Location == "West South Central",na.rm = TRUE))*100,2))
    # 
    
    
    # Dataset for parallel coordinate Graph
    
    # Function. This function will take in as a cuisine INput: The
    # input will be the cusisine The input will come from a drop down
    # menu Once we have the cuisine we will create a data frame 
    # Output: Will be a dataframe and thus will show how Cuisine
    # number aggregated with other columns This datafram will work as
    # an input for the parallel coordinate plot We can also work on to
    # change the Value box showing the Cuisine type
    
    if (input$cuisine == "American") {
      parallelData <- as.data.frame(xtabs(United_States ~ 
                                    Age +
                                    Household_Income +
                                    Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(`American` = Freq)
    }else if(input$cuisine == "Mexican"){
      parallelData <- as.data.frame(xtabs(Mexico ~
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Mexican = Freq)
    }else if(input$cuisine == "Italian"){
      parallelData <- as.data.frame(xtabs(Italy ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Italian = Freq)
    }else if(input$cuisine == "Chinese"){
      parallelData <- as.data.frame(xtabs(China ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Chinese = Freq)
    }else if(input$cuisine == "Japnese"){
      parallelData <- as.data.frame(xtabs(Japan ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Japnese = Freq)
    }else if(input$cuisine == "English"){
      parallelData <- as.data.frame(xtabs(England ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(English = Freq)
    }else if(input$cuisine == "French"){
      parallelData <- as.data.frame(xtabs(France ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(French = Freq)
    }else if(input$cuisine == "German"){
      parallelData <- as.data.frame(xtabs(Germany ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(German = Freq)
    }else if(input$cuisine == "Indian"){
      parallelData <- as.data.frame(xtabs(India ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Indian = Freq)
    }else if(input$cuisine == "Greece"){
      parallelData <- as.data.frame(xtabs(Greece ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Greek = Freq)
    }else if(input$cuisine == "Thai"){
      parallelData <- as.data.frame(xtabs(Thailand ~ 
                                            Age +
                                            Household_Income +
                                            Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Thai = Freq)
    }else{
      parallelData <- as.data.frame(xtabs(Mexico ~  
                                    Age +
                                    Household_Income +
                                    Education, topFood))
      parallelData <- parallelData %>%
        dplyr::rename(Mexico = Freq)
    }
    
    
    # Progress Bar with all the reactive Values
    withProgress({
      
      # Location Based Table

      if (input$region == "East North Central") {
        tableRendered <- topFood %>%
          dplyr::filter(Location == "East North Central")
        # Map Data
        stateAbb <- c("IL","IN","MI","OH","WI")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "East North Central") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "East North Central") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
        
      }else if(input$region == "East South Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "East South Central")
        # Map Data
        stateAbb <- c("AL","KY","MS","TN")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "East South Central") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "East South Central") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "Middle Atlantic"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Middle Atlantic")
        # Map Data
        stateAbb <- c("DE","MD","NJ","NY","PA","VA","VI")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "Middle Atlantic") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "Middle Atlantic") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "Mountain"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Mountain")
        # Map Data
        stateAbb <- c("AZ","CO","ID","MT","NV","NM","UT","WY")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "Mountain") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "Mountain") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "New England"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "New England")
        # Map Data
        stateAbb <- c( "CT","ME","MA","NH","RI","VT")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "New England") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "New England") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "Pacific"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "Pacific")
        # Map Data
        stateAbb <- c("AK","CA","HI","OR","WA")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "Pacific") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "Pacific") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "South Atlantic"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "South Atlantic")
        # Map Data
        stateAbb <- c( "DE","FL","GA","MD","NC","SC","VA","WV")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "South Atlantic") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "South Atlantic") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "West North Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "West North Central")
        # Map Data
        stateAbb <- c("IA","KS","MN","MO","NE","ND","SD")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "West North Central") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "West North Central") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "West South Central"){
        tableRendered <- topFood %>%
          dplyr::filter(Location == "West South Central")
        # Map Data
        stateAbb <- c("AR","LA","OK","TX")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "West South Central") %>%
          dplyr::select(Best)
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "West South Central") %>%
          dplyr::select(BestPercentage)
        # Hover
        hover <- (paste("Division: East North Central",'<br>', 
                        "Favourite Cuisine: ", bestCuisine$Best,"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage, "<br>"))
      }else if(input$region == "All"){
        tableRendered <- topFood
        # For All this will show all the States at once
        stateAbb <- c("IL","IN","MI","OH","WI","AK","CA","HI","OR","WA")
        bestCuisine <- region_TopFood %>%
          dplyr::filter(Location == "East North Central" | Location == "Pacific") %>%
          dplyr::select(Best)
        bestCuisine <- bestCuisine[1,1]
        bestP <- region_TopFood %>%
          dplyr::filter(Location == "East North Central" | Location == "Pacific") %>%
          dplyr::select(BestPercentage)
        bestP <- bestP[1,1]
        # Hover
        hover <- (paste("Division: East North Central",'<br>',
                        "Favourite Cuisine: ", bestCuisine$Best[1],"<br>",
                        "Percentage of Rating: ", bestP$BestPercentage[1], "<br>"))
      }else {
        tableRendered <- topFood
      }
      
      
      # Now we will need to have Maps for each of the locations.
      # "East North Central" <- c("IL","IN","MI","OH","WI")
      # "East South Central" <- c("AL","KY","MS","TN")
      # "Middle Atlantic" <- c("DE","MD","NJ","NY","PA","VA","VI")
      # "Mountain" <- c("AZ","CO","ID","MT","NV","NM","UT","WY")
      # "New England" <- c( "CT","ME","MA","NH","RI","VT") 
      # "Pacific" <- c("AK","CA","HI","OR","WA")
      # "South Atlantic" <- c( "DE","FL","GA","MD","NC","SC","VA","WV")
      # "West North Central" <- c("IA","KS","MN","MO","NE","ND","SD")
      # "West South Central" <- c("AR","LA","OK","TX")
      
      
      # This will plot a single US map with dropdown selction
      # different regions
      output$mapPlot <- renderPlotly({
        g <- list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = toRGB('white')
        )
        plot_ly(z = rep(bestP$BestPercentage,length(stateAbb)),
                text = ~hover,
                locations =  stateAbb,
                type = 'choropleth',
                colors = 'Purples',
                showscale = FALSE,
                locationmode = 'USA-states') %>%
          layout(geo = g,title = 'Best Rated Cuisine <br> (Hover for breakdown)')
      })
      
      # Table with Regional Select Best Cuisine
      output$regionTopUI <- DT::renderDataTable(
        region_TopFood,
        options = list(scrollX = TRUE)
      )
      
      
      # This will be the parallel coordinate plot
      # Plot 1 Exampel
      output$parallelPlot <- renderParcoords(
        parcoords(
          region_TopFood
          , rownames = F # turn off rownames from the data.frame
          , brushMode = "2D-strums"
          , reorderable = T
          , queue = T
          , dimensions = list(
            # American = list(
              # tickValues = unique(region_TopFood$American)
              # tickValues = seq(5,60,by = 5)
            # ),
            Mexican = list(
              # tickValues = unique(region_TopFood$Mexican)
              tickValues = seq(5,60,by = 5)
            ),
            Italian = list(
              # tickValues = unique(region_TopFood$Italian)
              tickValues = seq(5,60,by = 5)
            ),
            Chinese = list(
              # tickValues = unique(region_TopFood$Chinese)
              tickValues = seq(5,60,by = 5)
            ),
            Japnese = list(
              # tickValues = unique(region_TopFood$Japnese)
              tickValues = seq(5,60,by = 5)
            ),
            English = list(
              # tickValues = unique(region_TopFood$England)
              tickValues = seq(5,60,by = 5)
            ),
            French = list(
              # tickValues = unique(region_TopFood$Frence)
              tickValues = seq(5,60,by = 5)
            ),
            German = list(
              # tickValues = unique(region_TopFood$Germany)
              tickValues = seq(5,60,by = 5)
            ),
            Indian = list(
              # tickValues = unique(region_TopFood$Indian)
              tickValues = seq(5,60,by = 5)
            ),
            Greece = list(
              # tickValues = unique(region_TopFood$Greece)
              tickValues = seq(5,60,by = 5)
            ),
            Thai = list(
              # tickValues = unique(region_TopFood$Thai)
              tickValues = seq(5,60,by = 5)
            ),
            BestPercentage = list(
              tickValues = unique(region_TopFood$BestPercentage)
            )
            
            )
          ,color = list(
            colorScale = htmlwidgets::JS('d3.scale.category10()' )
          )
        )  
      )
      
      # PLot 2 Example
      output$parallelPlot_2 <- renderParcoords(
        parcoords( parallelData,
          rownames = F # turn off rownames from the data.frame
          , brushMode = "2D-strums"
          , reorderable = T
          , queue = T
          , color = list(
            colorScale = htmlwidgets::JS("d3.scale.category10()")
            ,colorBy = "Age"
          )
          , alpha = 0.55
          , elementId = 1
        )
      )
      
      # output$mySite <- renderUI({
      #   tags$a <- "© Service Data Science 2016 © <a href='http://www.teslamotors.com'>Tesla Motors</a>"
      # })
      
    },min = 0, max = 1, value = NULL, message = "Loading", detail= "Loading")
    
  })
}











