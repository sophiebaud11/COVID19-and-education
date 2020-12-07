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
library(broom.mixed)
library(rstanarm)
library(gt)
library(gtsummary)
library(rsample)
library(shinythemes)
library(shinyWidgets)


# load data cleaned in clean_data.Rmd

county_wage_map <- readRDS("county_wage_map.RDS")
county_covid_map <- readRDS("county_covid_map.RDS")
district_samples <- readRDS("district_samples.RDS")
district_samples_table <- readRDS("district_samples_table.RDS")

fit2 <- readRDS("fit.RDS")




ui <- navbarPage("The Impact of COVID-19 on Education",
    tabPanel("Introduction",
             fluidPage(
                 titlePanel("Contextual Maps"),
                 
                 fluidRow(
                     column(3, 
                            selectInput("sampled", "Sampled?",
                              c("Yes", "No"))),
                     column(3, style="margin-top:10px",
                            submitButton(" Generate Maps", 
                                         icon("globe-americas", 
                                              lib = "font-awesome")))
                 
                 ),
                 
                 fluidRow(
                     
                     column(6,
                            plotOutput("map1")
                     ),
                     
                     column(6,
                            plotOutput("map2")
                     )
                 )
                )),
    
    # create a panel in which to store a table of all the school districts
    # I've sampled & which counties they are in!
    
    tabPanel("Samples",
             titlePanel("List of Sample Districts and their Counties"),
             dataTableOutput('table')),
    
    # Create a new panel the user can access in the navigation bar which will
    # ultimately discuss the models. I am still collecting data and building 
    # models, so I did not yet feel comfortable filling out this page!
    
    tabPanel("Model",
             titlePanel("Model Output, Interpretation, and Application"),
             h5("Explore the output and interpretations of this model in the 
             tabs below. Then, use it to predict whether a school district 
             will operate entirely virtually based on daily COVID-19 case per 
             capita rate of change."),
             mainPanel(
                 tabsetPanel(
                     tabPanel("Output", 
                              fluidRow(
                                  column(5, style = "margin:15px",
                                         gt_output("regTable")
                                  ))
                              ),
                     tabPanel("Interpretation", 
                              fluidRow(
                                  column(5, style = "margin:15px",
                                         textOutput("interp")
                                  ))
                              )
                 )
             ),
             sidebarPanel(
                 h3("Application"),
                 h5("Try entering the mean rate of change: 1.28. Then,
                    generate the probability the district will require all
                    classes will be taught virtually."),
                 numericInput("rateInput", "Enter a Daily Rate of Change:", 0),
                 selectInput("region", "Enter a Region:", c("Northeast",
                                                            "Midwest",
                                                            "West",
                                                            "South")),
                 submitButton("Generate Probability", icon("refresh")),
                 h4("Probability the district will be virtual:"),
                 textOutput("value")
             )),

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
             You can reach me at sophiebauder@college.harvard.edu.")),
    theme = shinytheme("flatly"))

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
             "COVID-19 Cases per Capita by County, as of 11/29/20", 
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
             "COVID-19 Cases per Capita by County, as of 11/29/20", 
           caption = "Source: New York Times ")
    
  })
  
  # create 2 reactive objects which responds to the user's selection in the 
  # input "view", and calls the relevant reactive object as a result.
  
  graphs1 <- reactive({
    switch(input$sampled,
           "Yes" = sample1(),
           "No" = nosample1()
    )
  })
  graphs2 <- reactive({
    switch(input$sampled,
           "Yes" = sample2(),
           "No" = nosample2()
    )
  })
  
    # run function to calculate probability based on model output & user input
    
    probability <- reactive({
        if (input$region == "Midwest") {
            beta_zero = -3.81122
          }
        else if (input$region == "Northeast") {
          beta_zero = -1.92707
        }
        else if (input$region == "West") {
          beta_zero = -0.19769
        }
        else {
          beta_zero = -2.86936
        }
        
        x <- 100 * (exp(beta_zero + (-0.22950 * (input$rateInput))) / 
                        (1 + exp(beta_zero + (-0.22950 * (input$rateInput)))))
        x_short <- signif(x, digits = 3)
        paste(x_short, "%", sep = "")
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
    
    output$regTable <- render_gt(
        expr = tbl_regression(fit2, intercept = TRUE) %>% 
            as_gt() %>%
            tab_header(title = "Regression of Public School COVID Restrictions",
                       subtitle = "Impact of Rate of Change for County COVID-19 per 
               Capita on Restrictions") %>% 
            tab_source_note(md("Source: NY Times, local school websites.")),
        height = px(600),
        width = px(600)
    )
    output$sampled1 <- renderText({ input$sampled })
    output$value <- renderText({probability()})
    output$interp <- renderText("This model output is surprising—it implies 
                              that as COVID-19 cases increase locally, schools
                              will be less likely to go fully virtual. I
                              interpret that result to indicate that areas
                              which are succeeding in curtailing COVID-19 (ex. 
                              those with decreasing daily case counts) may be 
                              more likely to see schools operate virtually as 
                              an extension of cautious methods which brought
                              their local case count down. However, my model is
                                 very limited in sample size and may very well
                                 be skewed by that limitation, so this model
                                 should be taken more as an impetus to examine
                                 the conditions under which schools are likely
                                 to reopen in-person learning. As indicated in
                                 the Teachers tab, this decision has enormous
                                 impacts on educators and students alike.")
    
    
    
    }

# Run the application 

shinyApp(ui = ui, server = server)
