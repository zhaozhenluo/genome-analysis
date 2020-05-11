#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)

# Exercises from Lab 11



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

time_series_long_joined_1_ <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))
time_series_long_joined <- full_join(time_series_long_joined_1_,
                                     time_series_recovered_long, by = c("Key")) %>% 
  select(-Key)

time_series_long_joined$Date <- mdy(time_series_long_joined$Date)



first_date = min(time_series_long_joined$Date, na.rm = TRUE)
last_date = max(time_series_long_joined$Date, na.rm = TRUE)


Report_Type = c("Confirmed", "Deaths", "Recovered")
head(Report_Type)


ui <- fluidPage(
  
  
  titlePanel("Exercise 3"),
  p("Reference: Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
    tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
  ),
  tags$br(),
  tags$hr(),  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("select_type", 
                  label = "Report Type", 
                  choices = Report_Type, selected = "Confirmed"),
      
      sliderInput("slider_date", label = "Report Date", min = first_date, 
                  max = last_date, value = first_date)
    ),
    
    
    mainPanel(
      plotOutput("Plot_1_")
    )
  )
)


server <- function(input, output) {
  
  output$Plot1 <- renderPlot({
    
    pick_date <- time_series_long_joined %>% 
      
      mutate(Country_Region = recode(Country_Region, US = "USA")) %>% 
      
      filter(Date == input$slider_date) %>% 
      group_by(Country_Region) %>% 
      summarise_at(c("Confirmed", "Deaths", "Recovered"), sum)
    
    
    world <- map_data("world")
    
    
    country_join <- left_join(world, pick_date, by = c("region" = "Country_Region"))
    
    
    ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.5) + 
      
      geom_polygon(data = country_join, aes_string(fill = input$select_type), color = "black") +
      scale_fill_gradientn(colours = 
                             wes_palette("FantasticFox1", 100, type = "continuous"),
                           trans = "log10") +
      ggtitle("COVID-19 data", input$select_type)
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

