
# load in necessary libraries

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
                 
    # add UI for intro page
    
    tabPanel("Introduction",
             fluidPage(
                 titlePanel("Introduction"),
                 
                 # add text of varying sizes to introduce the project
                 
                 h3("Teachers and students across the U.S. are currently 
                 facing the impacts of COVID-19 on education."),
                 h5("Even before the pandemic, the experience of educators 
                 (here measured by compensation) varied widely county by 
                 county; the pandemic has only exacerbated that inequality, 
                 with school reopening plans differing between districts. 
                 Under these plans, many teachers have had to tackle the 
                 challenge of interacting with students in person as safely as 
                 possible—often without hazard pay."),
                 
                 # create a row where users can toggle the maps they see based
                 # on whether or not they solely show the sampled counties
                 
                 fluidRow(
                     column(3, 
                            selectInput("sampled", 
                                        "Show only sampled counties?",
                              c("Yes", "No"))),
                     column(3, style="margin-top:10px",
                            submitButton(" Generate Maps", 
                                         icon("globe-americas", 
                                              lib = "font-awesome")))
                 
                 ),
                 
                 fluidRow(
                   
                   # plot the maps below the toggle buttons
                     
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
    
    # create a  panel the user can access in the navigation bar with a
    # presentation & discussion of my model. 
    
    tabPanel("Model",
             titlePanel("Model Output, Interpretation, and Application"),
             h5("Explore the output and interpretations of this model in the 
             tabs below. Then, use it to predict whether a school district 
             will operate entirely virtually based on daily COVID-19 case per 
             capita rate of change."),
             mainPanel(
                 tabsetPanel(
                   
                   # place the regression table highlighting the model output
                   # in the first tab of the tabset panel
                   
                     tabPanel("Output", 
                              fluidRow(
                                  column(12, style = "margin:15px",
                                         gt_output("regTable")
                                  ))
                              ),
                     
                     # place text elucidating the interpretation of the model
                     # in the second tab
                     
                     tabPanel("Interpretation", 
                              fluidRow(
                                  column(12, style = "margin:15px",
                                         textOutput("interp")
                                  ))
                              )
                 )
             ),
             sidebarPanel(
               
               # create a sidebar panel in which the interactive prediction
               # generator is stored, so users can enter a daily rate of change
               # for COVID-19 cases and a region to generate the probability
               # that a school district under those conditions will be virtual.
               
                 h3("Application"),
                 h5("Try entering the mean rate of change: 1.28. Then, select
                    a region to generate the probability the district will 
                    require all classes to be taught virtually."),
                 numericInput("rateInput", "Enter a Daily Rate of Change:", 0),
                 selectInput("region", "Enter a Region:", c("Northeast",
                                                            "Midwest",
                                                            "West",
                                                            "South")),
                 submitButton("Generate Probability", icon("refresh")),
                 h4("Probability the district will operate entirely 
                    virtually:"),
                 span(textOutput("value"), style="font-weight: bold")
                 )
             ),
    
    #  create a panel to store the data visualizations from the educator survey
    
    tabPanel("Educator Survey",
             titlePanel("What do Educators Think?"),
             h3("In an August 2020 NPR/Ipsos poll, K-12 teachers were surveyed
                about the upcoming semester."),
             
             # in a tabset panel, store each graph based on topic
             
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

    # create another panel for the "About" page with additional 
    # information about my project & myself.
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project aims to examine the impacts of the COVID-19 
             pandemic on education, focusing on the experiences of educators.
               Teachers have taken on a great deal during this pandemic,
               with added responsibilities for childcare, teaching virtually,
               and more. I hope this project will shed light on some of the
               challenges facing public education and educators amidst the 
               pandemic—and how some districts have tackled those 
               challenges—using data."),
             h3("Project Code"),
             p("Check out the code for this project on GitHub:
               https://github.com/sophiebaud11/COVID19-and-education."),
             h3("Further Reading"),
             p("Check out these links to read more about this topic:"),
             
             # this uiOutup holds the list of further reading links
             
             uiOutput("myList"),
             h3("About Me"),
             p("My name is Sophie Bauder and I study History & Literature,
             Government, and Mandarin Chinese. 
             You can reach me at sophiebauder@college.harvard.edu.")),
    theme = shinytheme("flatly"))

# Define server logic required to display selected maps.

server <- function(input, output) {
    
  # create 4 graphs (sampled & not) and a switch function which responds to the 
  # user's selection in the input "view" to call the relevant graphs.
  
  graphs <- reactive({
      
    # each of these blocks of code creates a map of the United States (with
    # either only the sampled counties or all counties for which there's data,
    # depending on the user's selection in the "view" input). in each map,
    # set the fill to the data point being presented (either Weekly Wage for
    # those employed in Educational Services or COVID Cases per Capita). change
    # the fill color between the two topics to differentiate them, and style 
    # the text so that it's easily readable.
      
    # map of educational service wages in sampled counties 
      
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
    
    # map of COVID-19 cases per capita as of 11/29/20 in sampled counties 
    
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
    
    # map of weekly wages for educational services in all counties with 
    # available data
    
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
    
    # map of COVID cases per capita as of 11/29/20 in all counties with 
    # available data
    
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
    
    # store these graphs in two lists so they can be accessed in the switch 
    # function
    
    sample_graphs <- list(sample1, sample2)
    nosample_graphs <- list(nosample1, nosample2)
    
    # based on the user input, switch between displaying the graphs of sampled
    # counties versus those of all counties
    
    switch(input$sampled,
           "Yes" = grid.arrange(grobs = lapply(sample_graphs, ggplotGrob), 
                                nrow = 1),
           "No" = grid.arrange(grobs = lapply(nosample_graphs, ggplotGrob),
                               nrow = 1)
    )
  })
  
    # run function to calculate probability based on model output & user input
    
    probability <- reactive({
        
        # store region coefficients in the beta_zero object depending on 
        # user input
        
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
        
        # calculate probability
        
        x <- 100 * (exp(beta_zero + (-0.22950 * (input$rateInput))) / 
                        (1 + exp(beta_zero + (-0.22950 * (input$rateInput)))))
        
        # store a shortened version of the probability in object x_short
        
        x_short <- signif(x, digits = 3)
        
        # add a percentage sign to x_short
        
        paste(x_short, "%", sep = "")
    })
    
    
    # to generate the outputted plot, call the "graphs" reactive object!
    
    output$maps <- renderPlot({
      
        graphs()
    })
  
    # render a table in the output for the samples panel!
    
    output$table <- renderDataTable(district_samples_table)
    
    # render a gt regression table to highlight the model output
    
    output$regTable <- render_gt(
        
        # print the gt regression table of the model output with the specified
        # height, width, and labels
        
        expr = tbl_regression(fit2, intercept = TRUE) %>% 
            as_gt() %>%
            tab_header(title = "Regression of Public School COVID Restrictions",
                       subtitle = "Impact of Rate of Change for County COVID-19 
                       per Capita on Restrictions") %>% 
            tab_source_note(md("Source: NY Times, local school websites.")),
        height = px(600),
        width = px(600)
    )
    
    # call the probability function to generate probability based on user 
    # input

    output$value <- renderText({probability()})
    
    # render text for the model interpretation
    
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
    likely to reopen virtually than as a comprehensive predictor for
    school reopenings. As indicated in the Educators Survey tab, this decision 
    has enormous impacts on educators and students alike, so further research
    is necessary to better understand the conditions of school reopenings.")
    
    # render pie chart for the remote/in person preference graph
    
    output$preferencePie <- renderPlot(
        
        # render a pie chart visualizing the preference data on virtual and 
        # in person reopening for the 2020 fall semester using geom_bar and 
        # coord_polar to make the bars take the form of a circle.
        
        ggplot(pie_data, aes(x = "", y = values, fill = answer)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) + theme_void() +
            scale_fill_viridis_d(direction = -1, begin = 0.5, end = 0.9, 
                                 name = "Preference")  + 
            labs(title = 
                     "Reopening Preference: Virtual vs. In Person Learning",
                 caption = "Source: Ipsos/NPR") +
            theme(plot.title = element_text(size = 15, face = "bold"),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14))
    )
    
    # render bar chart highlighting teacher concerns about in person learning
    
    output$inpersonConcern <- renderPlot(
        
        # create a basic bar chart displaying the data for survey questions
        # regarding in person learning concerns, with the y-axis measured in 
        # percentages.
        
        ggplot(bar_chart_inperson_data,
                 aes(x = fct_reorder(as.factor(Question), desc(Agree)), 
                     y = Agree, fill = Agree)) + geom_col() + 
            scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
            theme_minimal() + theme(legend.position = "none", 
                                    axis.text = element_text(size = 12),
                                    axis.title = element_text(size = 14),
                                    plot.title = element_text(size = 15, 
                                                              face = "bold")) + 
            labs(x = "", y = "Agree/Likely", 
                 title = "Concerns about In-Person Teaching", 
                 caption = "Source: Ipsos/NPR") + 
            scale_fill_viridis_c(direction = -1, begin = 0.5, end = 0.9)
    )
    
    # render bar chart highlighting teacher concerns about remote learning
    
    output$remoteConcern <- renderPlot(
        
        # create a basic bar chart displaying the data for survey questions
        # regarding virtual learning concerns, with the y-axis measured in 
        # percentages.
        
    
        ggplot(bar_chart_remote_data,
             aes(x = Question, y = Agree, fill = Agree)) + geom_col() + 
          scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
          theme_minimal() + theme(legend.position = "none", 
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 14),
                                plot.title = element_text(size = 15,
                                                          face = "bold")) + 
          labs(x = "", title = "Concerns about Remote Teaching", 
             caption = "Source: Ipsos/NPR") + 
          scale_fill_viridis_c(direction = -1, begin = 0.5, end = 0.9)
    )
    
    # render bar chart highlighting teacher concerns about district support
    
    output$districtComms <- renderPlot(
        
        # create a basic bar chart displaying the data for survey questions
        # regarding how teachers felt they were/were not supported by their 
        # district, with the y-axis measured in percentages.
        
        
        ggplot(communication_chart_data, 
             aes(fill = Response, y = Value, x = Question)) + 
            geom_bar(position="fill", stat="identity", width = 0.7) + 
            scale_y_continuous(labels = scales::percent) + theme_minimal() + 
            labs(x = "", y = "Response", 
                 title = "Support from School District",
                 caption = "Source: Ipsos/NPR") + 
            geom_text(aes(label = paste(ResponseValue, "%", sep = "")), 
                      size = 3,
                      position = position_stack(vjust = 0.5), 
                      color = "white") +
            theme(legend.position = "none", 
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 15, face = "bold")) + 
            scale_fill_viridis_d(end = 0.9)
    )
    
    # render unordered list of further reading links
    
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
