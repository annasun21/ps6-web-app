library(shiny)
library(tidyverse)
data <- read_delim("City_of_Seattle_Staff_Demographics.csv")
transform(data, "Hourly Rate" = as.numeric("Hourly Rate"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seattle Staff Demographics"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
      tabPanel("About", 
               sidebarLayout(
                 sidebarPanel(
                   p("this app uses data from the ", 
                      strong("Seattle Department of Human Resources."),
                      "The dataset contains ", 
                      nrow(data), 
                      " observatons and ",
                      ncol(data),
                      " variables. Here is a small",
                      em("(random)"),
                      "sample of the data."
                    )
                 ),
                 mainPanel(
                   tableOutput("sample")
                 )
               )
      ),
      tabPanel("Plot", 
               sidebarLayout(
                  sidebarPanel(
                    radioButtons("position",
                                 "What position would you like to see the data presented in: ",
                                 choices = c("stack", "dodge"),
                                 selected = "stack"),
                    checkboxGroupInput("plotDepartments",
                                     "What departments would you like to see: ",
                                     choices = unique(data$Department),
                                     selected = unique(data$Department))
                  ),
                  mainPanel(
                    plotOutput("distPlot"),
                    textOutput("plotObservation")
                  )
              )
      ),
      tabPanel("Table", 
               sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("tableDepartments",
                                       "What departments would you like to see: ",
                                       choices = unique(data$Department),
                                       selected = unique(data$Department))
                  ),
                  mainPanel(
                    textOutput("tableObservation"),
                    tableOutput("table")
                  )
              )
       )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sample <- renderTable({
    data %>% 
      sample_n(5)
  })
  
  output$distPlot <- renderPlot({
    data %>% 
      filter(Department %in% input$plotDepartments) %>%
      ggplot(aes(Department, fill=Sex)) +
      geom_bar(position=input$position) +
      labs(title = "Department by Counts") + 
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$plotObservation <- renderPrint({
    num <- data %>% 
      filter(Department %in% input$plotDepartments) %>%
      nrow()
    cat("There are ", num, " observations contributing to this plot")
  })
  
  output$table <- renderTable({
    data %>% 
      select(Department, Age) %>% 
      filter(Department %in% input$tableDepartments) %>%
      group_by(Department) %>% 
      summarize("Average Age" = mean(Age))
  })
  
  output$tableObservation <- renderPrint({
    ages <- data %>% 
      filter(Department %in% input$tableDepartments) %>% 
      select(Age)
    cat("The oldest age over all selected departments is ", max(ages), " and the youngest is ", min(ages))
  })
}

# Run the application 
shinyApp(ui, server)