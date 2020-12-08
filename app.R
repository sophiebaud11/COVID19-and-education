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
library(gridExtra)


# load data cleaned in clean_data.Rmd

county_wage_map <- readRDS("rds_data/county_wage_map.RDS")
county_covid_map <- readRDS("rds_data/county_covid_map.RDS")
district_samples <- readRDS("rds_data/district_samples.RDS")
district_samples_table <- readRDS("rds_data/district_samples_table.RDS")
pie_data <- readRDS("rds_data/pie_data.RDS")
bar_chart_inperson_data <- readRDS("rds_data/bar_chart_inperson_data.RDS")
bar_chart_remote_data <- readRDS("rds_data/bar_chart_remote_data.RDS")
communication_chart_data <- readRDS("rds_data/communication_chart_data.RDS")

fit2 <- readRDS("rds_data/fit.RDS")




ui <- navbarPage("The Impact of COVID-19 on Education",
    tabPanel("Introduction",
             fluidPage(
                 titlePanel("Introduction"),
                 h3("Teachers and students across the U.S. are currently 
                 facing the impacts of COVID-19 on education."),
                 h5("Even before the pandemic, the experience of educators 
                 (here measured by compensation) varied widely county by 
                 county; the pandemic has only exacerbated that inequity, with 
                 school reopening plans differing between districts. Under these
                 plans, many teachers have had to tackle the challenge of 
                 interacting with students in person as safely as 
                 possible—often without hazard pay."),
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
                     
                     column(12,
                            plotOutput("maps")
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
                                  column(12, style = "margin:15px",
                                         gt_output("regTable")
                                  ))
                              ),
                     tabPanel("Interpretation", 
                              fluidRow(
                                  column(12, style = "margin:15px",
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
                 span(textOutput("value"), style="font-weight: bold")
                 )
             ),
    tabPanel("Educator Survey",
             titlePanel("What do Educators Think?"),
             h3("In an August NPR/Ipsos poll, K-12 teachers were surveyed
                about the upcoming semester."),
             mainPanel(width = 12,
                 tabsetPanel(
                   tabPanel("Reopening Preference", style = "margin:15px",
                            plotOutput("preferencePie")
                   ),
                   tabPanel("In-Person Learning", style = "margin:15px",
                            plotOutput("inpersonConcern")
                   ),
                   tabPanel("Remote Learning", style = "margin:15px", 
                            plotOutput("remoteConcern")
                   ),
                   tabPanel("School District Support", style = "margin:15px",
                            plotOutput("districtComms")
                   )
                 )
              )
    ),

    # Create another panel for the "About" page. Here, I list introductory 
    # information about my project & myself.
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project aims to examine the impacts of the COVID-19 
             pandemic on education, focusing on the experiences of educators.
               Teachers have taken on a great deal during this pandemic,
               with added responsibilities (childcare, teaching virtually,
               and more). I hope this project will shed light on some oft he
               challenges facing public education and educators amidst the 
               pandemic—and how some districts have tackled those 
               challenges—using data."),
             h3("Project Code"),
             p("Check out the code for this project on GitHub:
               https://github.com/sophiebaud11/COVID19-and-education."),
             h3("Further Reading"),
             p("Check out these links to read more about this topic:"),
             uiOutput("myList"),
             h3("About Me"),
             p("My name is Sophie Bauder and I study History & Literature,
             Government, and Mandarin Chinese. 
             You can reach me at sophiebauder@college.harvard.edu.")),
    theme = shinytheme("flatly"))

# Define server logic required to display selected maps.

server <- function(input, output) {
    
  # create 2 reactive objects which responds to the user's selection in the 
  # input "view", and calls the relevant reactive object as a result.
  
  graphs <- reactive({
    sample1 <- county_wage_map %>%
      mutate(avg_wkly_wage_all = ifelse(state_county %in% 
                                          district_samples$state_county,
                                        avg_wkly_wage_all, NA)) %>%
      ggplot(aes(long, lat, group = group)) +
      geom_polygon(aes(fill = avg_wkly_wage_all)) + theme_void() +
      scale_fill_viridis_c(direction = -1, option = "D", name = "Weekly Wage",
                           na.value = "#e3e3e3") + 
      theme(legend.position = "bottom",
            plot.title = element_text(color = "black", face = "bold"),
            plot.caption = element_text(color = "black"),
            legend.text = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold")) +
      labs(title = " Average Weekly Wages for Educators per County", 
           subtitle = " Limited to Sampled Counties",
           caption = "Source: Bureau of Labor Statistics ")
    sample2 <- county_covid_map %>%
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
            plot.caption = element_text(color = "black"),
            legend.text = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold")) +
      labs(title = 
             "COVID-19 Cases per Capita by County, as of 11/29/20", 
           subtitle = " Limited to Sampled Counties",
           caption = "Source: New York Times ")
    
    nosample1 <- county_wage_map %>%
      ggplot(aes(long, lat, group = group)) +
      geom_polygon(aes(fill = avg_wkly_wage_all)) + theme_void() +
      scale_fill_viridis_c(direction = -1, option = "D", name = 
                             "Weekly Wage",
                           na.value = "#e3e3e3") + 
      theme(legend.position = "bottom",
            plot.title = element_text(color = "black", face = "bold"),
            plot.caption = element_text(color = "black"),
            legend.text = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold")) + 
      labs(title = " Average Weekly Wages for Educators per County", 
           caption = "Source: Bureau of Labor Statistics ")
    
    nosample2 <- county_covid_map %>%
      ggplot(aes(long, lat, group = group)) + 
      geom_polygon(aes(fill = cases_per_capita)) + theme_void() +
      scale_fill_viridis_c(direction = -1, option = "A", name = 
                             "Cases per Capita",
                           na.value = "#e3e3e3") + 
      theme(legend.position = "bottom",
            plot.title = element_text(color = "black", face = "bold"),
            plot.caption = element_text(color = "black"),
            legend.text = element_text(color = "black", face = "bold"),
            legend.title = element_text(color = "black", face = "bold")) +
      labs(title = 
             "COVID-19 Cases per Capita by County, as of 11/29/20", 
           caption = "Source: New York Times ")
    
    sample_graphs <- list(sample1, sample2)
    nosample_graphs <- list(nosample1, nosample2)
    switch(input$sampled,
           "Yes" = grid.arrange(grobs = lapply(sample_graphs, ggplotGrob), 
                                nrow = 1),
           "No" = grid.arrange(grobs = lapply(nosample_graphs, ggplotGrob),
                               nrow = 1)
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
    
    output$maps <- renderPlot({
      
      # use ggarrange to get rid of double usage of input$sampled
      
        graphs()
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

    output$value <- renderText({probability()})
    
    output$interp <- 
    renderText("This model output is surprising—it implies that as COVID-19 
    cases increase locally, schools will be less likely to go fully virtual. I
    interpret that result to indicate that areas which are succeeding in 
    curtailing COVID-19 (ex. those with decreasing daily case counts) may be 
    more likely to see schools operate virtually as an extension of cautious 
    methods which brought their local case count down. The differing 
    coefficients for each region imply that school districts in certain parts of 
    the U.S. (such as the South and Midwest) are more likely than those in 
    other regions to keep their school districts open in person.
    However, while my samples were taken randomly, my model is limited in 
    sample size and may be skewed by that limitation, so this model should be 
    taken more as an impetus to examine the conditions under which schools are 
    likely to reopen in-person learning than as a comprehensive predictor for
    school reopenings. As indicated in the Educators Survey tab, this decision 
    has enormous impacts on educators and students alike, so further research
    is necessary to better understand the conditions of school reopenings.")
    
    output$preferencePie <- renderPlot(
      ggplot(pie_data, aes(x = "", y = values, fill = answer)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) + theme_void() +
        scale_fill_viridis_d(direction = -1, begin = 0.5, end = 0.9, 
                             name = "Preference")  + 
        labs(title = "Reopening Preference: Virtual vs. In Person Learning",
             caption = "Source: Ipsos/NPR") +
        theme(plot.title = element_text(size = 15, face = "bold"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
    )
    
    output$inpersonConcern <- renderPlot(
      ggplot(bar_chart_inperson_data,
             aes(x = fct_reorder(as.factor(Question), desc(Agree)), 
                 y = Agree, fill = Agree)) + geom_col() + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
        theme_minimal() + theme(legend.position = "none", 
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 14),
                                plot.title = element_text(size = 15, 
                                                          face = "bold")) + 
        labs(x = "", y = "Agree/Likely", title = "Concerns about In-Person Teaching", 
             caption = "Source: Ipsos/NPR") + 
        scale_fill_viridis_c(direction = -1, begin = 0.5, end = 0.9)
    )
    
    output$remoteConcern <- renderPlot(
      ggplot(bar_chart_remote_data,
             aes(x = Question, y = Agree, fill = Agree)) + geom_col() + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
        theme_minimal() + theme(legend.position = "none", 
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 14),
                                plot.title = element_text(size = 15)) + 
        labs(x = "", title = "Concerns about Remote Teaching", 
             caption = "Source: Ipsos/NPR") + 
        scale_fill_viridis_c(direction = -1, begin = 0.5, end = 0.9)
    )
    
    output$districtComms <- renderPlot(
      ggplot(communication_chart_data, 
             aes(fill = Response, y = Value, x = Question)) + 
        geom_bar(position="fill", stat="identity", width = 0.7) + 
        scale_y_continuous(labels = scales::percent) + theme_minimal() + 
        labs(x = "", y = "Response", title = "Support from School District",
             caption = "Source: Ipsos/NPR") + 
        geom_text(aes(label = paste(ResponseValue, "%", sep = "")), size = 3,
                  position = position_stack(vjust = 0.5), color = "white") +
        theme(legend.position = "none", axis.text = element_text(size = 12,
                                                                 face = "bold"),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 15, face = "bold")) + 
        scale_fill_viridis_d(end = 0.9)
    )
    output$myList <- renderUI(HTML("<ul><li><a href = 
    'https://www.ipsos.com/en-us/news-polls/NPR-teachers-covid-poll-080620'>
    NPR/Ipsos survey.</a></li><li>
    <a href = 'https://nyti.ms/3myxF2I'>
    NYTimes article about teaching during the pandemic.</a></li>
    <li><a 
    href = 'https://files.epi.org/pdf/205622.pdf'>
    EPI study on student performance, equity, and education policy during 
    COVID-19.</a></li></ul>"))
    }

# Run the application 

shinyApp(ui = ui, server = server)
