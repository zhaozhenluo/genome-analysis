#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}



time_series_covid19_confirmed_global<-
  read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))%>% 
  rename(Country_Region="Country/Region", Province_State= "Province/State")%>%
  pivot_longer(-c(Province_State, Country_Region, Lat, Long), names_to= "Date", values_to= "Confirmed")
time_series_covid19_confirmed_global<-
  time_series_covid19_confirmed_global %>%
  unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)


time_series_covid19_deaths_global<-
  read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>% 
  rename(Country_Region="Country/Region", Province_State= "Province/State")%>%
  pivot_longer(-c(Province_State, Country_Region, Lat, Long), names_to= "Date", values_to= "Death")

time_series_covid19_deaths_global<-
  time_series_covid19_deaths_global %>%
  unite(Key, Province_State, Country_Region, Date, sep = ".") %>%
  select(Key, Death)


time_series_covid19_recovered_global<- 
  read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
  rename(Country_Region="Country/Region", Province_State= "Province/State")%>%
  pivot_longer(-c(Province_State, Country_Region, Lat, Long), names_to= "Date", values_to= "Recovered")
time_series_covid19_recovered_global<-
  time_series_covid19_recovered_global %>%
  unite(Key,Province_State, Country_Region,Date, sep = ".")%>%
  select(Key, Recovered)



time_series_long_joined<-
  full_join(time_series_covid19_confirmed_global, time_series_covid19_death_global, by = c("Key"))
time_series_long_joined<-
  full_join(time_series_long_joined, time_series_covid19_recovered_global, by = c("Key"))%>%
  select(-Key)

time_series_long_joined$Date<- mdy(time_series_long_joined$Date)

time_series_long_joined_counts<-
  time_series_long_joined %>%
  pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date), names_to = "Report_Type", values_to = "Counts")

global_time_series<- time_series_long_joined

first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)

Report_Type = c("Confirmed")

Countries = global_time_series$Country_Region

ui<- fluidPage(
  titlePanel("COVID-19 Confirmed cases in 5 countries"),
  p("Data for this graph comes from John Hopkins", tags$a("GitHub Respository", href = "https://github.com/CSSEGISandData")
  ),
  tags$br(),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select_type", label = "Report_Type", choices = Report_Type, selected = "Confirmed"),
      sliderInput("dates", label = "Date reported", min = first_date, max = last_date, value = c(first_date, last_date))
    ),
    mainPanel(
      plotOutput("Plot_1_")
    )))

server<- function(input, output){
  output$Plot1<- renderPlot({
    pick_country<- global_time_series %>%
      group_by(Country_Region, Date) %>%
      summarise_at(c("Confirmed", "Death", "Recovered"), sum) %>%
      filter(Country_Region %in% c("China", "Japan", "Korea, South", "Iran", "Italy"))
    ggplot(pick_country, aes_string(x = "Date", y = input$select_type, color = "Country_Region")) +
      geom_point() +
      geom_line() +
      xlim(input$dates) +
      ggtitle("COVID-19 Confirmed cases in 5 countries:", input$select_type)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

