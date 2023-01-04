
#### Beginner Data Storytelling Case Study
#### DataCamp Course Submission
#### Presented by Leondra R. Gonzalez
 

#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#Install & load packages

#install.packages("shiny")
#install.packages("readr")
#install.packages("knitr")
#install.packages("tidyverse")
#install.packages("DT")
#install.packages("colourpicker")
#install.packages("plotly")
#install.packages("renv")
#install.packages("htmltools")
#install.packages("rlang")


require(shiny)
require(readr)
require(knitr)
require(tidyverse)
require(DT)
require(shinythemes)
require(ggplot2)
require(colourpicker)
require(plotly)
require(renv)
require(htmltools)
require(rlang)


# Load All Ages data
all_ages = "https://raw.githubusercontent.com/LeondraJames/sme-dle-case-study-datacamp/master/datasets/all-ages.csv" # <- replace url with link to my sme repo
all_ages_data = read.csv(url(all_ages))
all_ages_data = as.data.frame(all_ages_data)
all_ages_data$Total <- as.numeric(unlist(all_ages_data$Total))
all_ages_data$Employed <- as.numeric(unlist(all_ages_data$Employed))
all_ages_data$Employed_full_time_year_round <- as.numeric(unlist(all_ages_data$Employed_full_time_year_round))
all_ages_data$Unemployed <- as.numeric(unlist(all_ages_data$Unemployed))
all_ages_data$Unemployment_rate <- as.numeric(unlist(all_ages_data$Unemployment_rate))
all_ages_data$Media <- as.numeric(unlist(all_ages_data$Median))
all_ages_data$P25th <- as.numeric(unlist(all_ages_data$P25th))


#Load Grad Students data (for later exercises - not needed for this particular EE)
# = "https://raw.githubusercontent.com/LeondraJames/sme-dle-case-study-datacamp/master/datasets/grad-students.csv" # <- replace url with link to my sme repo
#grad_data = read.csv(url(grad))
#grad_data = as.data.frame(grad_data)
#grad_data$Total <- as.numeric(unlist(all_ages_data$Total))


#Load Women in STEM data (for later exercises - not needed for this particular EE)
#wmn = "https://raw.githubusercontent.com/LeondraJames/sme-dle-case-study-datacamp/master/datasets/women-stem.csv" # <- replace url with link to my sme repo
#wmn_data = read.csv(url(wmn))
#wmn_data = as.data.frame(wmn_data)
#wmn_data$Total <- as.numeric(unlist(all_ages_data$Total))

# Clean up unused objects
rm(all_ages)
#rm(grad, wmn)



##########UI##################


# Define UI for application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Learning Objective
      h1("Beginner Data Storytelling Explorable Exercise 1.1"), 
      h2("Learning Objective"),
      h4("Students are presented with an explorable exercise (EE) where they can choose which title, variable, and data transformations (as an example) best answers the given question.
"),
      
      # Context
      h2("Context"),
      h4("Behind nearly every business context is the desire to turn raw data into insights. Insights helps decision makers better understand their business, oftentimes using data visualizations, presentations, reports, and/or KPIs (key performance indicators). Deriving insights from raw data given a business objective is a fundamental skill to business problem understanding, strategizing, and decision making"),
      p(),
      h4("In this exercise, we will explore some basic concepts of what makes information insightful, and how to make insights into a compelling story"),
      
      # Application title
      titlePanel("College Majors Case Study Exercise 1.1: Major Category Post-Graduate Analysis"),
      theme = shinythemes::shinytheme("yeti"),
      
      # Exercise Steps
      h4("In this exercise, you are the Lead Data Scientist for Success University! The school's administration is committed to improving post-graduate outcomes for its students by improving employment rates, and effectively communicating expected outcomes to prospective students. To get started, you have been tasked with analyzing Success University's data."),
      
      
      h4("The dataset is comprised of college majors, major categories, and some statistics on alumni employment. We will need to conduct an exploratory analysis using the following 2 graphs to learn more about graduate outcomes. Both plots represent the distribution of one or more variables"),
      
      h3("Instructions:"),
      h4(" - Regard the boxplot and density plots"),
      h4(" - Use the axis dropdown to change the plotted variable"),
      h4(" - Use the appropriate plot to answer the questions below"),
      
      h4("(**Note: Below are sample multiple choice questions for this exercise)"),
      h4("---"),
      h4("1) What story does the first plot tell?"),
      h4("2) The second plot gives us another view at the data. How does this plot's story differ from the previous graph?"),
      h4("3) What changes would you make to the first graph to improve its narrative?"),
      h4("4) Which major category had the highest median number of employed graduates?"),
      h4("5) Which major category had the highest reported unemployment rate?"),
      h4("6) Which major category needs the most attention for improving employment rates?"),
      h4("7) Some majors were outliers within their major category. What was the highest number of employed students in the Education major category?"),
      h4("8) Which major category had the most variance for year round full time employment?"),
      h4("9) When might you use the 1st graph over the second, and vice versa?"),
      h4("10) What can we NOT tell from these plots?"),
      h4("11) Give the graphs a title"),
      h4("12) What other plot elements might we utilize to better tell our story?"),
      h4("13) Toggle the Transformation box for the boxplot to 'log2'. Observing the plot update, why might data transformations be useful for data storytelling?"), 
      h4("14) Which of the following is a potential narrative clue based on the provided information?")  
      ),
    
    mainPanel(
      
      
      tabPanel(      
        # Select x-axis, y-axis, and major category 
        textInput("title", "Provide plot title:", "<Your Title Here>"),
        
        
        selectInput('Y','Select a variable', choices = list("Employed","Unemployed","Employed_full_time_year_round",
                                                            "Unemployment_rate")),
        
        selectInput('trans',"Boxplot Transformation", c("identity","log2"), selected = "identity"),
        
        
      ),
      # Plot
      
      plotlyOutput("Boxplot"),
      plotlyOutput("Densityplot")
      #,      DT::DTOutput('table')
      
    )
  )
)


##########SERVER##################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  box_data <- reactive({
    
    all_ages_data %>% 
      ggplot(aes(x=Major_category, y=!!rlang::sym(input$Y), fill = Major_category))+
      geom_boxplot()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(input$title) +
      scale_y_continuous(trans = input$trans)
    
  })
  
  dens_data <- reactive({
    
    all_ages_data %>% 
      ggplot(aes(x=!!rlang::sym(input$Y), fill = Major_category))+
      geom_density(alpha = 0.3)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(input$title)
    
  })
  
  output$Boxplot <- renderPlotly({
    ggplotly(box_data())  %>%
      layout(autosize = TRUE)
    
  })
  
  output$Densityplot <- renderPlotly({
    ggplotly(dens_data())  %>%
      layout(autosize = TRUE)
    
  })
 
}


shinyApp(ui = ui, server = server)
