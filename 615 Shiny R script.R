library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(benford.analysis)
library(BenfordTests)
library(dplyr)
library(esquisse)
library(ggplot2)
library(sqldf)
library(tidyr)
library(data.table)
library(arm)
library(knitr)
library(plyr)

#import data 
Boston.airbnb<-read.csv("tomslee_airbnb_boston_0649_2016-11-21.csv")
# replace all N/A with 0 
Boston.airbnb[is.na(Boston.airbnb)] <- 0
# Remove unrelevant columns
Boston.data<-Boston.airbnb[, c(-4,-9)]
#remove 0 review properties
Boston.data<-filter(Boston.data, reviews >0)
Boston.data<-filter(Boston.data, overall_satisfaction >0)

crime<-read.csv("crime_incident_reports.csv")

# replace district code with names
distrName = c(
  A1 = 'Downtown',
  A15= 'Charlestown',
  A7= 'East Boston',
  B2= 'Roxbury',
  B3= 'Mattapan',
  C6= 'South Boston',
  C11= 'Dorchester',
  D4= 'South End',
  D14= 'Brighton',
  E5= 'West Roxbury',
  E13= 'Jamaica Plain',
  E18= 'Hyde Park',
  HTU= 'Human Traffic Unit'
)
crime$ReptDistrName = as.factor(distrName[as.character(crime$DISTRICT)])
crime$DISTRICT = NULL

data.crime=na.omit(crime)
dff2= count(data.crime, 'ReptDistrName')

Boston.data$priceperperson <- (Boston.data$price)/(Boston.data$accommodates)
a1 <- aggregate( priceperperson ~ room_type+overall_satisfaction, Boston.data, mean )
a1 <- as.data.frame(a1)
names(a1) <- c("room_type","ratings","priceperperson")

df= count(Boston.data, 'neighborhood')

knitr::opts_chunk$set(echo = TRUE,out.width="0.9\\linewidth",dev="png",fig.align  = 'center')
pal <- colorQuantile(
  palette = "YlOrRd",
  domain = Boston.data$price
)
leaflet(Boston.data) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
             popup = ~price, radius = 50,
             color = ~pal(price), fillOpacity = 1)

################################ Load Shiny Dashboard ################################


ui <- dashboardPage(
  dashboardHeader(title = "Boston Airbnb Analysis ",titleWidth = 285),
  dashboardSidebar(
    width = 285,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
      menuItem("Dataset Overview", tabName = "dataset overview", icon = icon("list-alt"),
               menuSubItem("Room Type Overview",tabName = "rt"), 
               menuSubItem("Review & Price Distribution",tabName = "dis")
               
      ),
      menuItem("EDA", tabName = "eda", icon = icon("bar-chart-o"),
               menuSubItem("Ratings vs. Reviews",tabName = "rr"),
               menuSubItem("Room Type Distribution",tabName = "pie"),
               menuSubItem("Price per Person vs. Room Type",tabName = "pp"),
               menuSubItem("Graph by neighborhood",tabName = "n"),
               menuSubItem("Neighborhood Rankings",tabName = "nr"),
               menuSubItem("Crime incidents in Boston",tabName = "crime"),
               menuSubItem("Density Plot",tabName = "den"),
               menuSubItem("Relationship between important variables",tabName = "rel")
               
      ),
      menuItem("Benford Analysis", tabName = "benford", icon = icon("signal"),
               menuSubItem("Price Analysis",tabName = "p"),
               menuSubItem("Reviews Analysis",tabName = "r"),
               menuSubItem("Summary Output",tabName = "sum")
      ),
      menuItem("Map",tabName = "map",icon = icon("globe"))
    )
  ),
  
  
  ################################## Dashboard Body #########################################
  dashboardBody(
    # tab for Benford Analysis
    tabItems(
      
      tabItem(tabName = "p",
              fluidRow(
                box(plotOutput("price1"),title = "Benford law for Price (two digits)",width=11),
                box(plotOutput("price2"),title = "Benford law for Price (one digits)",width=11)
              )
              
      ),
      tabItem(tabName = "r",
              fluidRow(
                box(plotOutput("review1"),title = "Benford law for Reviews (two digits)",width=11),
                box(plotOutput("review2"),title = "Benford law for Reviews (one digits)",width=11)
              ) 
              
      ),
      #tab for roomtype overview
      tabItem(tabName = "rt",
              fluidRow(
                box(plotOutput("room"),title = "Room Type Overview",width=10)
              )
      ),
      #tab for distribution
      
      
      tabItem(tabName = "dis",
              fluidRow(
                box(plotOutput("his"),title = "Distribution of Reviews",width=7),
                box(title = "Number of reviews",width = 5,
                    sliderInput("slider", "Number of observations:", 1, 2400, 100)
                )
              ),
              fluidRow(
                box(plotOutput("his2"),title = "Distribution of Ratings",width=7),
                box(title = "Number of properties with ratings",width = 5,
                    sliderInput("slider2", "Number of observations:", 1, 2400, 100)
                )
              )  
              
      ),
      
      #tab for EDA
      # jitter plot 
      tabItem(tabName = "rr",
              fluidRow(
                box(plotOutput("rrpic"),title = "Room Type Overview",width=10)
              )
              
      ),
      #pie chat 
      tabItem(tabName = "pie",
              fluidRow(
                box(plotOutput("piechart"),title = "Room Type Overview",width=8)
              ) 
              
      ),
      
      # price per person
      
      tabItem(tabName ="pp",
              fluidRow(
                box(plotOutput("line"),title = "By Price per person",width=10)
              ),
              fluidRow(
                box(plotOutput("bar"),title = "By Ratings",width=10)
              ) 
              
      ),
      
      tabItem(tabName = "n",
              fluidRow(
                box(title = "Select a variable", status = "primary", solidHeader = TRUE, width = 3,
                    selectInput("selectn","Choose a variable", choices = c("Ratings","Price per Person") )),
                box(plotOutput("facet"),title = "By Ratings",width=9)
              )
              
      ),
      
      #rankings
      
      tabItem(tabName = "nr",
              fluidRow(
                box(plotOutput("ranking1"),title = "Number of properties ranking in Boston",width=11)
              ),
              fluidRow(
                box(h2("Number of properties in Boston's neighborhoods"),width = 8,
                    DT::dataTableOutput("table1")
                )
              )
      ),
      
      #map
      
      tabItem(tabName = "map",
              fluidRow(
                box(title = "All Listed Perperties in Boston Area",
                    collapsible = TRUE,
                    width = "100%",
                    height = "100%",
                    leafletOutput("bostonmap"))
              )
              
      ),
      
      # crime 
      
      tabItem(tabName = "crime",
              fluidRow(
                box(plotOutput("cri"),title = "Crime incidents ranking in Boston neighborhoods"
                    ,width=10)
              )
              
      ),
      
      #density plot
      tabItem(tabName = "den",
              fluidRow(
                tabBox(width=10,
                  tabPanel(plotOutput("rev"),title = "Reviews Density"),
                  tabPanel(plotOutput("rat"),title = "Ratings Density")
                )
              
              )
      ),
      
      # relationships between variables
      tabItem(tabName = "rel",
              fluidRow(
                tabBox(width=9,
                       tabPanel(plotOutput("r1"),title = "Accommodates and Room Type"),
                       tabPanel(plotOutput("r2"),title = "Minstays and Room Type"),
                       tabPanel(plotOutput("r3"),title = "Price and Room Type")
                )
                
              )
      ),
      
      # dashboard
      tabItem(tabName = "dashboard",
              fluidRow( 
                box(width=8,solidHeader = TRUE, status = "primary",
                             h4("In this project, I chose the Airbnb dataset from Boston Area. The methodology of this final project 
                                 is to test the reivews and price variables to see if they follow the Benford Law and explore further 
                                 details of the output. In addition, this project also includes Modelings, EDA, Data Visualizations, 
                                 Leaflet Mapping and Shiny Dashboard Building." )
                         ) 
              )),
      tabItem(tabName = "sum",
              fluidRow(
                tabBox(width=10,
                       tabPanel(verbatimTextOutput("s1"),title = "Price Benford Summary (2 digits)"),
                       tabPanel(verbatimTextOutput("s11"),title = "Price Benford Summary (1 digits)"),
                       tabPanel(verbatimTextOutput("s2"),title = "Review Benford Summary (2 digit)"),
                       tabPanel(verbatimTextOutput("s22"),title = "Price Benford Summary (1 digits)")
                )
              )
      )
  
  
)))



############################### Server ##############################################


server <- function(input, output) { 
  # output for Benford analysis
  output$price1 <- renderPlot({plot(benford(Boston.data$price, number.of.digits=2))})
  output$price2 <- renderPlot({plot(benford(Boston.data$price, number.of.digits=1))})
  output$review1 <- renderPlot({plot(benford(Boston.data$reviews, number.of.digits=2))})
  output$review2 <- renderPlot({plot(benford(Boston.data$reviews, number.of.digits=1))})
  
  #output for roomtype overview
  output$room <- renderPlot({print(ggplot(data = Boston.data) +
                                     aes(x = room_type) +
                                     geom_bar(fill = '#0c4c8a') +
                                     labs(title = 'Overview of Room Type',
                                          x = 'Room type',
                                          y = 'Count ') +
                                     theme_minimal())})
  
  # output for distribution
  output$his <- renderPlot({hist(Boston.data$reviews,breaks = input$slider, main = "Distribution of Reviews", xlab = "Number of reviews")
  })
  output$his2 <- renderPlot({hist(Boston.data$overall_satisfaction,breaks = input$slider2, main = "Distribution of Ratings", xlab = "Number of properties with ratings")
  })
  
  # output for EDA
  # jitter plot 
  output$rrpic <- renderPlot({
    ggplot(data=Boston.data, aes(x=overall_satisfaction, y=reviews))+geom_jitter()+xlab("Ratings")+ylab("Reviews")
  })
  
  #pie chart
  output$piechart <- renderPlot({print(ggplot(data=Boston.data, aes(x=factor(1), fill = factor(room_type))) + geom_bar(width = 1)
                                       +coord_polar(theta = "y") + ggtitle("Room Type Distribution in Boston") + labs(x="Room Type",y="Count") + theme(plot.title = element_text(size=12)))})
  
  #price per person
  output$line <- renderPlot({ggplot(data=a1,aes(x=priceperperson,y=ratings))+geom_line()+facet_wrap(.~room_type)})
  output$bar <- renderPlot({ggplot(data=a1,aes(x=ratings,y=priceperperson))+geom_bar(stat="identity")+facet_wrap(.~room_type)})
  
  #facet 
  output$facet <- renderPlot({
    switch(input$selectn,
           "Ratings"=  ggplot(data=Boston.data,breaks = input$selectn, aes(x=overall_satisfaction))+
             geom_histogram()+facet_wrap(~neighborhood) + ggtitle("Ratings distribution in different neighborhood"),
           "Price per Person"= ggplot(data=Boston.data, aes(x=priceperperson))+
             geom_histogram()+facet_wrap(~neighborhood) + ggtitle("Price per Person in different neighborhood")
           
    )
  })
  
  #rankings
  output$ranking1 <- renderPlot({print(ggplot(df, aes(x = reorder(neighborhood, freq), y = freq,fill=neighborhood)) +
                                         geom_bar(stat = "identity") +
                                         labs(title = 'Overview of Neighborhood',
                                              x = 'Neighborhood',
                                              y = 'Count ') +
                                         theme_minimal()+coord_flip()+
                                         theme(axis.text.x = element_text(angle = 60, hjust = 1)))})
  
  output$table1 <- DT::renderDataTable({count(Boston.data, 'neighborhood')})
  
  #map
  output$bostonmap <- renderLeaflet({
    colorQuantile(
      palette = "YlOrRd",
      domain = Boston.data$price
    )
    leaflet(Boston.data) %>% addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup = ~price, radius = 50,
                 color = ~pal(price), fillOpacity = 1)
    
  })
  
  #crime
  output$cri <- renderPlot({plot(
    
    ggplot(dff2, aes(x = reorder(ReptDistrName, freq), y = freq, fill = ReptDistrName)) +
      geom_bar(stat = "identity") +
      labs(title = 'Overview of Neighborhood',
           x = 'Neighborhood',
           y = 'Count ') +
      theme_minimal()+coord_flip()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
  )})   
  
    # density plot 
  output$rev <- renderPlot({
     plot(ggplot(data=Boston.data, aes(x=reviews, fill=neighborhood))+geom_density()
          + ggtitle(" Reviews Density Plot by Neighborhoods"))
    })
  
  output$rat <- renderPlot({
    plot(ggplot(data=Boston.data, aes(x=overall_satisfaction, fill=neighborhood))+geom_density()
         + ggtitle(" Ratings Density Plot by Neighborhoods"))
  })  
  
   # relationship 
  output$r1 <- renderPlot({
    plot(ggplot(data=Boston.data, aes(x=accommodates, fill=room_type))+geom_bar(position = "fill")
         + ggtitle("Accommodates and Room Type"))
  })
  
  output$r2 <- renderPlot({
    plot(ggplot(data=Boston.data, aes(x=minstay, fill=room_type))+geom_bar(position = "fill")
         + ggtitle("Minstays and Room Type"))
  })
  
  output$r3 <- renderPlot({
    plot(ggplot(data=Boston.data, aes(x=price, fill=room_type))+geom_histogram()
         + ggtitle("Price and Room Type"))
  })
  
  # summary
  output$s1 <- renderPrint({print(benford(Boston.data$reviews, number.of.digits=2))})
  output$s11 <- renderPrint({print(benford(Boston.data$reviews, number.of.digits=1))})
  output$s2 <- renderPrint({print(benford(Boston.data$price, number.of.digits=2))})
  output$s22 <- renderPrint({print(benford(Boston.data$reviews, number.of.digits=1))})
  
  
  
  
  
}

shinyApp(ui,server)
