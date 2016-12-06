
The following project is built on the following configuration:
1. R version 3.3.1 Powered on RStudio as IDE
2. Visualization are designed using Plotly, JavaScript and Shiny.
3. Application is running live on shinyapps.io

Executable:
The code base consists of server.R and ui.R which are the executable files for the project.

Libraries Dependency:
1. Install the following libraries from CRAN:
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
2. Third part libraries:
	devtools::install_github("timelyportfolio/parcoords")
	library(parcoords)

Implementation:
To execute the application on local machines we need to execute the RUN command on
the RStudio tab.
ShinyApps.io plugin should be installed to publish the application.




Data set Rating information.

Anwser key for the responses to the "Please rate how much you like the traditional cuisine of X:" questions.

5: I love this country's traditional cuisine. I think it's one of the best in the world.
4: I like this country's traditional cuisine. I think it's considerably above average.
3: I'm OK with this county's traditional cuisine. I think it's about average.
2: I dislike this country's traditional cuisine. I think it's considerably below average.
1: I hate this country's traditional cuisine. I think it's one of the worst in the world.
N/A: I'm unfamiliar with this country's traditional cuisine.