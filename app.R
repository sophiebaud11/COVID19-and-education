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
library(maps)


# load data cleaned in clean_data.Rmd

map_data_covid <- read_csv("raw_data/covid_map.csv")
map_data_salary <- read_csv("raw_data/salary_map.csv")


ui <- navbarPage(
    "COVID-19 Impact on Teachers",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Contextual Maps"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         # create a select box that allows users to choose which
                         # plot to see.
                         
                         selectInput("view", "Choose a View",
                                     c("COVID Cases by State" = "covid",
                                       "Average Teacher Salary" = "salary"))
                         ),
                     mainPanel(
                         plotOutput("map")
                     ))
                )),
    
    # Create a new panel the user can access in the navigation bar which will
    # ultimately discuss the models. I am still collecting data and building 
    # models, so I did not yet feel comfortable filling out this page!
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("TBD")),
    
    # Create another panel for the "About" page. Here, I list introductory 
    # information about my project & myself.
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project aims to examine the experience of teachers 
               during the COVID-19 pandemic, grounded in an understanding of 
               their treatment (in terms of pay) and morale before 2020.
               Teachers have taken on a great deal during this pandemic,
               with added responsibilities for childcare, teaching virtually,
               and more. I hope this project will shed light on what teachers 
               have gone through in this challenging time using data."),
             h3("About Me"),
             p("My name is Sophie Bauder and I study History & Literature,
             Government, and Mandarin Chinese. 
             You can reach me at sophiebauder@college.harvard.edu.")))

# Define server logic required to display selected maps.

server <- function(input, output) {
    
    # create 2 reactive objects containing the map data for each plot. 
    # (map code is commented on in the clean_data.Rmd file!)
    
    covid <- reactive({
        ggplot(map_data_covid, aes(long, lat, group = group))+
            geom_polygon(aes(fill = cases_per_capita), color = "white") + 
            scale_fill_viridis_c(direction = -1, option = "A", 
                                 name = "Total Cases per Capita") + 
            theme_void() + labs(
                title = "Total COVID-19 Cases per Capita by State, as of 10/16", 
                caption = "Source: New York Times")
    })
    
    salary <- reactive({
        ggplot(map_data_salary, aes(long, lat, group = group))+
            geom_polygon(aes(fill = value), color = "white") + theme_void() +
            scale_fill_viridis_c(direction = -1, option = "D", name = "Salary") + 
            labs(title = "Average Annual Teacher Salary by State, 2018-2019", 
                 caption = "Source: National Center for Education Statistics")
    })
    
    # create a reactive object which responds to the user's selection in the 
    # input "view", and calls the relevant reactive object as a result.
    
    graph <- reactive({
        switch(input$view,
               "covid" = covid(),
               "salary" = salary()
        )
    })
    
    # for the outputted plot, call the "graph" reactive object!
    
    output$map<-renderPlot({
       graph()
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
