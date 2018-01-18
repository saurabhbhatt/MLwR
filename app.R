# Project MLwR

# Importing Libraries
library(shiny)
library(shinydashboard)

# Updating Options
options(shiny.maxRequestSize=120*1024^2)  # Current Upload size = 120 MB. Default = 5 MB

# Define UI for application
ui <- dashboardPage(title = "Machine Learning with R", 
                    
                    # -------------- #
                    # Header Content #
                    # -------------- #
                    header = dashboardHeader(title = "MLwR",
                                             
                                             # Inserting Dropdown menu
                                             dropdownMenuOutput(outputId = "project_status_menu")
                                             ), 
                    
                    # --------------- #
                    # Sidebar Content #
                    # --------------- #
                    sidebar = dashboardSidebar(
                      sidebarMenu(
                        menuItem("Project", tabName = "tab_project", icon = icon("dashboard")),
                        menuItem("Data", tabName = "tab_data", icon = icon("dashboard")),
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
                      
                      # Adding CSS to the page
                      tags$head(
                        tags$link(href='http://fonts.googleapis.com/css?family=Roboto:300', rel='stylesheet', type='text/css'),
                        tags$style("body { font-family: 'Roboto', sans-serif;}")
                      ),
                      
                      
                      tabItems(
                        # Tab >> Project
                        tabItem(tabName = "tab_project",
                                fluidRow(
                                  tabBox(width = 12, title = "Project",
                                         tabPanel(title = "Select"),
                                         tabPanel(title = "Create New",
                                                  fluidRow(
                                                    column(width = 6, offset = 3,
                                                           textInput(inputId = "project_name", label = "Project Name:", width = "100%"),
                                                           selectInput(inputId = "project_source", 
                                                                       label = "Project Source:", 
                                                                       choices = c("Kaggle", "Analytics Vidhya", "HackerEarth", "Fractal Analytics"), 
                                                                       width = "100%"),
                                                           actionButton(inputId = "project_actionbutton", label = "Create Project", width = "100%"))
                                                  )),
                                         tabPanel(title = "Delete")
                                         )
                                  )
                          
                        ), 
                        
                        # Tab: Dashboard
                        tabItem(tabName = "tab_data",
                                fluidRow(
                                  tabBox(width = 12,
                                         title = "Data",
                                         id = "tabset1",
                                         tabPanel("Upload", 
                                                  fluidRow(column(width = 8, offset = 2,
                                                                  fileInput(inputId = "file1", label = "Upload Training data", 
                                                                            accept = c("text/csv", ".csv"),width = "100%")
                                                                  ))),
                                         
                                         tabPanel("Summary", "Tab content 2"),
                                         tabPanel("Preprocessing", "Tab content 2"))
                        )
                      ),
                      
                      # Tab: Building Models
                      tabItem(tabName = "build_models",
                              fluidRow(
                                tabBox(width = 12,
                                       title = "Building Models",
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset2",
                                       tabPanel("Task", "F"),
                                       tabPanel("Learner", "Tab content 2")
                                )
                              )
                      )
                      
                    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Upadting Current Projct status
  output$project_status_menu <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(from = "Current Project", message = input$project_name),
                 messageItem(from = "Project Source", message = input$project_source))
  })
  
  output$mydata <- renderDataTable({
    airquality
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

