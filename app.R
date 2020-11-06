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

county_wage_map <- read_csv("raw_data/county_wage_map.csv")
county_covid_map <- read_csv("raw_data/county_covid_map.csv")
county_samples <- read_csv("raw_data/county_samples.csv")
district_samples <- read_csv("raw_data/sample_districts_edited.csv")
district_samples_table <- read_csv("raw_data/sample_districts_table.csv")
ui <- navbarPage(
    "COVID-19 Impact on Teachers",
    tabPanel("Introduction",
             fluidPage(
                 titlePanel("Contextual Maps"),
                 
                 radioButtons("sampled", "Sampled?",
                              c("Yes" = "sample",
                                "No" = "nosample")),
                 
                 fluidRow(
                     
                     column(6,
                            plotOutput("map1")      
                     ),
                     
                     column(6,
                            plotOutput("map2")
                     )
                 ),
                     # show plots in a tabset panel to improve UI!
                )),
    
    # create a panel in which to store a table of all the school districts
    # I've sampled & which counties they are in!
    
    tabPanel("Samples",
             titlePanel("List of Sample Districts and their Counties"),
             dataTableOutput('table')),
    
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
    
    sample1 <- reactive({
            
        # wage plot 
        
        county_wage_map %>%
            mutate(avg_wkly_wage_all = ifelse(state_county %in% 
                                                  district_samples$state_county,
                                              avg_wkly_wage_all, NA)) %>%
            ggplot(aes(long, lat, group = group)) +
            geom_polygon(aes(fill = avg_wkly_wage_all)) + theme_void() +
            scale_fill_viridis_c(direction = -1, option = "D", name = "Weekly Wage",
                                 na.value = "#e3e3e3") + 
            theme(legend.position = "bottom",
                  plot.title = element_text(color = "black", face = "bold"),
                  plot.caption = element_text(color = "black", face = "bold"),
                  legend.text = element_text(color = "black", face = "bold"),
                  legend.title = element_text(color = "black", face = "bold")) +
            labs(title = " Average Weekly Wages for Educators per County", 
                      subtitle = " Limited to Sampled Counties",
                      caption = "Source: Bureau of Labor Statistics ")
    })
    sample2 <- reactive ({
        
        # covid plot 
        
        county_covid_map %>%
            mutate(cases_per_capita = ifelse(state_county %in% 
                                                 district_samples$state_county,
                                             cases_per_capita, NA)) %>%
            ggplot(aes(long, lat, group = group)) + 
            geom_polygon(aes(fill = cases_per_capita)) + theme_void() +
            scale_fill_viridis_c(direction = -1, option = "A", name = 
                                     "Cases per Capita",
                                 na.value = "#e3e3e3") + 
            theme(legend.position = "bottom",
                  plot.title = element_text(color = "black", face = "bold"),
                  plot.caption = element_text(color = "black", face = "bold"),
                  legend.text = element_text(color = "black", face = "bold"),
                  legend.title = element_text(color = "black", face = "bold")) +
            labs(title = 
                   " Total COVID-19 Cases per Capita by County, as of 10/16", 
                      subtitle = " Limited to Sampled Counties",
                      caption = "Source: New York Times ")
        
    })
    
    nosample1 <- reactive({
               
        # wage plot
        
        county_wage_map %>%
            ggplot(aes(long, lat, group = group)) +
            geom_polygon(aes(fill = avg_wkly_wage_all)) + theme_void() +
            scale_fill_viridis_c(direction = -1, option = "D", name = 
                                     "Weekly Wage",
                                 na.value = "#e3e3e3") + 
            theme(legend.position = "bottom",
                  plot.title = element_text(color = "black", face = "bold"),
                  plot.caption = element_text(color = "black", face = "bold"),
                  legend.text = element_text(color = "black", face = "bold"),
                  legend.title = element_text(color = "black", face = "bold")) + 
            labs(title = " Average Weekly Wages for Educators per County", 
                      caption = "Source: Bureau of Labor Statistics ")
    })
    
    nosample2 <- reactive ({
        # covid plot 
        
        county_covid_map %>%
            ggplot(aes(long, lat, group = group)) + 
            geom_polygon(aes(fill = cases_per_capita)) + theme_void() +
            scale_fill_viridis_c(direction = -1, option = "A", name = 
                                     "Cases per Capita",
                                 na.value = "#e3e3e3") + 
            theme(legend.position = "bottom",
                  plot.title = element_text(color = "black", face = "bold"),
                  plot.caption = element_text(color = "black", face = "bold"),
                  legend.text = element_text(color = "black", face = "bold"),
                  legend.title = element_text(color = "black", face = "bold")) +
            labs(title = 
                   " Total COVID-19 Cases per Capita by County, as of 10/16", 
                      caption = "Source: New York Times ")
        
    })
    
    # create 2 reactive objects which responds to the user's selection in the 
    # input "view", and calls the relevant reactive object as a result.
    
    graphs1 <- reactive({
        switch(input$sampled,
               "sample" = sample1(),
               "nosample" = nosample1()
        )
    })
    graphs2 <- reactive({
        switch(input$sampled,
               "sample" = sample2(),
               "nosample" = nosample2()
        )
    })
    # for the outputted plot, call the "graph" reactive objects!
    
    output$map1 <- renderPlot({
       graphs1()
    })
    output$map2 <- renderPlot({
        graphs2()
    }) 
    
    # render a table in the output for the samples panel!
    
    output$table <- renderDataTable(district_samples_table)

}

# Run the application 

shinyApp(ui = ui, server = server)
