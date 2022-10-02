library(shiny)
library(maps)
library(mapproj)
library(ggplot2)
library(reshape2) #for melt
library(tidyr) #for separate
library(dplyr) #for separate

convert <- function(x) {
  if (x < 90) {
    as.numeric(x) + 2000
  }
  else{
    as.numeric(x) + 1900
  }
}

MyData <- read.csv(file = "US_shootings.csv", header = TRUE, sep = ",")
dfPoint <- summarise(
  group_by(MyData, City, .drop = FALSE),
  Shootings = NROW(Fatalities),
  Fatalities = sum(Fatalities),
  Wounded = sum(Wounded),
  lng=first(lng),
  lat=first(lat))
dfPoint<-na.omit(dfPoint)
dfPoint<-subset(dfPoint, dfPoint$lng > -140)

dfState <-summarise(
  group_by(MyData, State, .drop = FALSE),
  Shootings = NROW(Fatalities),
  Fatalities = sum(Fatalities),
  Wounded = sum(Wounded))
dfState$region=sapply(dfState$State, tolower)

states_map <- map_data("state")
states<-summarise(
  group_by(states_map, region, .drop=FALSE),
  group=first(group)
)
dfMapStates <- left_join(states_map, dfState, by = "region", )

dfLine <- data.frame(
  Date = MyData$Date,
  School = MyData$School,
  Fatalities = MyData$Fatalities,
  Wounded = MyData$Wounded
)
#separate the date into day, month and year fields
dfLine$Year = separate(dfLine, Date, c("Day", "Month", "Year"))$Year
dfLine$Year = sapply(dfLine$Year, convert)

#Get the number of fatalities and wounded per year
dfSum <-summarise(
  group_by(dfLine, Year, .drop = FALSE),
  Shootings = NROW(Fatalities),
  Fatalities = sum(Fatalities),
  Wounded = sum(Wounded))

dfCumSum <- summarise(dfSum,
                      Year=Year,
                      Shootings = cumsum(Shootings),
                      Fatalities = cumsum(Fatalities),
                      Wounded = cumsum(Wounded))

ui <- fluidPage(
  titlePanel("US School shootings"),
  
  sidebarLayout(sidebarPanel(
      helpText("School shootings, fatalities and wounded people across the US from 1990-2020"),
      
      selectInput("var",
        label = "Choose the variable to display",
        selected = "Fatalities",
        choices = c("Fatalities", "Wounded", "Shootings")
      ),
    ),
    
    mainPanel(plotOutput("map"))
  ),
  sidebarLayout(sidebarPanel(
    selectInput("cumulative", label = "Historical evolution of shootings", selected = "Not cumulative",
                choices = c("Not cumulative", "Cumulative"))),
  
  mainPanel(plotOutput("line_plot"))),
  
  sidebarLayout(sidebarPanel(
    selectInput("type", label = "Choose a grouping", selected = "None",
                choices = c("None", "By school type", "By area type")),  
    
    sliderInput("range",
        label = "Range of years:",
        min = 1990,
        max = 2020,
        value = c(1990, 2020))),
    sidebarLayout(sidebarPanel("Ratio of shootings, fatalities and wounded"),mainPanel(plotOutput("chart")))),
)

server <- function(input, output) {
  school_types <- reactive({
    req(input$type)
    if(input$type=="By area type"){
      df <- data.frame(
        Date = MyData$Date,
        Group = MyData$AreaType,
        Fatalities = MyData$Fatalities,
        Wounded = MyData$Wounded)
    }
    #keep the Date, School, Fatalities, Wounded variables only
    else{
      df <- data.frame(
      Date = MyData$Date,
      Group = MyData$School,
      Fatalities = MyData$Fatalities,
      Wounded = MyData$Wounded)
    }
    #separate the date into day, month and year fields
    df$Year = separate(df, Date, c("Day", "Month", "Year"))$Year
    df$Year = sapply(df$Year, convert)
    req(input$range)
    min_year <- input$range[1]
    max_year <- input$range[2]
    #Filter based on the input
    dfFiltered <- filter(df, min_year <= Year & Year <= max_year)
    #Get the number of fatalities and wounded per school type
    dfSum <-summarise(
        group_by(dfFiltered, Group, .drop = FALSE),
        Shootings = NROW(Fatalities),
        Fatalities = sum(Fatalities),
        Wounded = sum(Wounded)
      )
    #for every entry, there will be a column which contains a categorical variable (Fatalities/Wounded) and the value of the appropriate entry.
    dfMelted <- melt(dfSum, id.vars = "Group")
  })
  
  #the charts for the different types
  output$chart <- renderPlot({
    req(input$type)
    if(input$type =="None"){
      pie <- ggplot(school_types(), aes(x = "", y = value, fill = variable)) + geom_col(width = 1)
      pie + coord_polar(theta = "y")
    }
    else if(input$type == "By school type"){
      g <- ggplot(school_types())
      g + geom_bar(aes(x = Group, y = value, fill = variable),
                   stat = "identity",
                   position = "dodge") +   xlab("Type of school") + ylab("Number of cases")
    }
    else{
      g <- ggplot(school_types())
      g + geom_bar(aes(x = Group, y = value, fill = variable),
                   stat = "identity",
                   position = "dodge") +   xlab("Type of area") + ylab("Number of cases")
    }
    
  })
  
  #the line plot
  output$line_plot <- renderPlot({
    req(input$cumulative)
    if(input$cumulative=="Not cumulative"){
      ggplot(data=dfSum) +
        geom_line(aes(x=Year, y=Fatalities, group=1, colour="Fatalities"),lwd=1) +
        geom_line(aes(x=Year, y=Wounded, group=1, colour="Wounded"),lwd=1)  +
        geom_line(aes(x=Year, y=Shootings, group=1, colour="Shootings"),lwd=1) +
        scale_color_manual(values = c(
          'Fatalities' = 'black',
          'Wounded' = 'red3',
          'Shootings' = 'blue3')) +
        labs(color = 'Legend', y="Number of cases", x="Year")
    }
    else{
      ggplot(data=dfCumSum) +
        geom_line(aes(x=Year, y=Fatalities, group=1, colour="Fatalities"),lwd=1) +
        geom_line(aes(x=Year, y=Wounded, group=1, colour="Wounded"),lwd=1)  +
        geom_line(aes(x=Year, y=Shootings, group=1, colour="Shootings"),lwd=1) +
        scale_color_manual(values = c(
          'Fatalities' = 'black',
          'Wounded' = 'red3',
          'Shootings' = 'blue3')) +
        labs(color = 'Legend', y="Cumulative number of cases", x="Year")
    }
  })
  
  output$map <- renderPlot({
    req(input$var)
    if (input$var=="Fatalities"){
      ggplot(dfMapStates)+
        geom_polygon(aes(long, lat, group = group, fill = Fatalities), color = "grey")+
        scale_fill_gradient(low = "white", high = "black", na.value = "white")+
        geom_point(data=dfPoint, aes(x=lng, y=lat, size=Fatalities), color="red3")
    }
    
    else if (input$var=="Wounded"){
      ggplot(dfMapStates)+
        geom_polygon(aes(long, lat, group = group, fill = Wounded), color = "grey")+
        scale_fill_gradient(low = "white", high = "red3", na.value = "white")+
        geom_point(data=dfPoint, aes(x=lng, y=lat, size=Wounded), color="black")
    }
    else{
      ggplot(dfMapStates)+
        geom_polygon(aes(long, lat, group = group, fill = Shootings), color = "grey")+
        scale_fill_gradient(low = "white", high = "blue2", na.value = "white")+
        geom_point(data=dfPoint, aes(x=lng, y=lat, size=Shootings), color="red3")
    }
    
  })
}

shinyApp(ui = ui, server = server)