# Project MLwR

# Importing Libraries
library(shiny)
library(shinydashboard)

# Define UI for application
ui <- dashboardPage(title = "Machine Learning with R", 
                    
                    # -------------- #
                    # Header Content #
                    # -------------- #
                    header = dashboardHeader(title = "MLwR"), 
                    
                    # --------------- #
                    # Sidebar Content #
                    # --------------- #
                    sidebar = dashboardSidebar(
                      sidebarMenu(
                        menuItem("Data", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Exploratory Data Analysis", tabName = "widgets", icon = icon("th")),
                        menuItem("Feature Engineering", tabName = "widgets", icon = icon("th")),
                        menuItem("Building Models", tabName = "build_models", icon = icon("cogs")),
                        menuItem("Report", tabName = "widgets", icon = icon("th"))
                      )
                    ), 
                    
                    # ------------ #
                    # Body Content #
                    # ------------ #
                    body = dashboardBody(
                      
                      # Tab: Dashboard
                      tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(width = 12, dataTableOutput(outputId = "mydata"))
                                )
                        ),
                        # Tab: Building Models
                        tabItem(tabName = "build_models",
                                fluidRow(
                                  tabBox(width = 12,
                                    title = "Building Models",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1",
                                    tabPanel("Task", "First tab content"),
                                    tabPanel("Learner", "Tab content 2")
                                  )
                                )
                        )
                      
                    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mydata <- renderDataTable({
    airquality
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

