# Load required libraries
#install.packages("DT")
library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)


# Read database credentials
# source("./03_shiny_HW1/credentials_v3.R")
source("./credentials_v4.R")


ui <- fluidPage(
  
  #tags$head(
  # tags$style("label{font-family: BentonSans Book;}")
  #),
  
  # setBackgroundImage(src = "https://www.fillmurray.com/1920/1080", shinydashboard = TRUE), 
  
  
  dashboardPage(
    skin = "black",
    
    dashboardHeader(title = "Sahar's Zomato App" ),
    #Sidebar content
    dashboardSidebar(
      #Add sidebar menus here
      sidebarMenu(
        menuItem("About the App", tabName = "HWSummary", icon = icon("dashboard")),
        menuItem("Votes Based Search", tabName = "dbquery", icon = icon("list")),
        menuItem("Map of Restaurants", tabName = "leaflet", icon = icon("th"))
      )
    ),
    dashboardBody(
      
      
      tabItems(
        # Add contents for first tab
        tabItem(tabName = "HWSummary",
                h3("This HW was submitted by Sahar Khan of ITOM6265"),
                p("Welcome to Zomato App!", style = "color:maroon ; font-size: 30px"),
                
                p("In this app, users can search popular restaurants based on number of votes a restaurant has
              receieved. You can search it by name and votes. The app covers all the combination of names and votes where votes range from 0 to 10,000. I have styled the app by changing the different colors(marron, olive), font-size and adding 400*300 image using src. ",style="color:olive; font-size: 16px"),
                
                p("In 'Map of Restaurants' Tab , you can view resturants on map and city view. It makes use of the leaflet function and map has marker to indicate the location (the non null values are filtered). I have updated the Map tab icon as well via icon function",style="color:Navy; font-size: 16px" )
        ),
        
        # Add contents for second tab
        
        tabItem(tabName = "dbquery",
                fluidRow(
                  column(width = 9,
                         box(width = NULL, solidHeader = TRUE,
                             textInput("rest_names", h3("Tell us Your Favorite Restaurant")),
                             # Copy the line below to make a slider range 
                             sliderInput("rest_votes_slider", label = h3("Range of Votes to Search for"), min = 0, 
                                         max = 10000, value = c(0, 500)),
                             actionButton("Go", "Get results"),
                             h2("The is your search result :"),
                             DT::dataTableOutput("mytable1")
                         )
                  ))
        ),
        #  Add contents for third tab
        tabItem(tabName = "leaflet", h2("Click on Teardrops to View Names of Restaurants"),
                actionButton("map", "Display Map!"),
                leafletOutput("mymap")
        )
      ),
      
      tags$img(
        src = "https://images.unsplash.com/photo-1568605114967-8130f3a36994?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=2070&q=80",
        width = 400,
        height = 300,
        style = 'position: absolute'
      )
      
    )
    
  )
  
  
)




server <- function(input, output)  {
  
  #Develop your server side code (Model) here
  db <- dbConnector(
    server   = getOption("database_server"),
    database = getOption("database_name"),
    uid      = getOption("database_userid"),
    pwd      = getOption("database_password"),
    port     = getOption("database_port")
  )
  on.exit(dbDisconnect(db), add = TRUE)
  
  #query <- "SELECT MIN(votes) AS min, MAX(votes) AS max from zomato_rest"
  
  #data <- dbGetQuery(db, query)
  
  #updateSliderInput(inputId = "rest_votes", min = data$min, max = data$max)
  
  
  observeEvent(input$Go, {
    
    output$mytable1 <- renderDataTable({
      
      #output$range <- renderPrint({ input$rest_votes })
      #output$value <- renderPrint({ input$Go })
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      #query <- paste("select name,votes,city from zomato_rest where name =","'",input$name,"'",  ";" )
      
      #query <- paste0("select name,votes,city from zomato_rest where name like","'","%",input$name,"%","'", "and votes =", input$rest_votes[1],";")
      #"and votes =", input$rest_votes,
      
      
      query <- paste0("select name,Votes,city from zomato_rest where name like '%",
                     input$rest_names,"%' AND  votes  between ", input$rest_votes_slider[1],
                     " AND ",input$rest_votes_slider[2],";",sep="")
      print(query)
      data <- dbGetQuery(db,query)
      
       
      
      output$mytable1 = DT::renderDataTable({
        data
      
    } )
    
  } )
    
  } )
    
  observeEvent(input$map, {
    db2 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    on.exit(dbDisconnect(db2), add = TRUE)
    
    latq <- dbGetQuery(db2, "SELECT longitude,name FROM zomato_rest WHERE longitude is NOT NULL ;"
    )
    longq <- dbGetQuery(db2, "SELECT latitude,name FROM zomato_rest WHERE latitude is NOT NULL ;"
    )
    output$mymap <- renderLeaflet({
      mymap <- leaflet(data = db2)
      mymap <- addTiles(mymap)
      #addWMSTiles(mymap, layers = "nexrad-n0r-900913")
      mymap <- addMarkers(mymap, 
                          lng = latq[,1], 
                          lat = longq[,1],
                          popup=latq[,2])
    })   
    
  }) 
}




shinyApp(ui, server)

