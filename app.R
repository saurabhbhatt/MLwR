# Project MLwR

# Importing Libraries
library(shiny)
library(shinydashboard)

# Loading External files
source("functions.R")

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
                        menuItem("Projects", tabName = "tab_project", icon = icon("cubes")),
                        menuItem("Data", tabName = "tab_data", icon = icon("database")),
                        menuItem("Exploratory Data Analysis", tabName = "widgets", icon = icon("bar-chart")),
                        menuItem("Feature Engineering", tabName = "widgets", icon = icon("th")),
                        menuItem("Building Models", tabName = "build_models", icon = icon("cogs")),
                        menuItem("Report", tabName = "widgets", icon = icon("file-text-o"))
                      )
                    ), 
                    
                    # ------------ #
                    # Body Content #
                    # ------------ #
                    body = dashboardBody(
                      
                      # Adding CSS to the page
                      tags$head(
                        tags$link(href='http://fonts.googleapis.com/css?family=Roboto:300', rel='stylesheet', type='text/css'),
                        tags$style("body, h4 { font-family: 'Roboto', sans-serif;}")
                      ),
                      
                      
                      tabItems(
                        # Tab >> Project
                        tabItem(tabName = "tab_project",
                                fluidRow(
                                  tabBox(width = 12, title = "Project",
                                         tabPanel(title = "Select",
                                                  fluidRow(
                                                    column(width = 6,
                                                           selectInput(inputId = "select_project_source", 
                                                                       label = "Project Source:", 
                                                                       choices = c("Kaggle", "Analytics Vidhya", "HackerEarth", "Fractal Analytics"), 
                                                                       width = "100%"),
                                                           htmlOutput(outputId = "sel_project_list")
                                                    ),
                                                    
                                                    column(5,offset = 1, "fsdf")
                                                  )
                                         ),
                                         
                                         tabPanel(title = "Create New",
                                                  fluidRow(
                                                    column(width = 6, offset = 3,
                                                           
                                                           selectInput(inputId = "project_source", 
                                                                       label = "Project Source:", 
                                                                       choices = c("Kaggle", "Analytics Vidhya", "HackerEarth", "Fractal Analytics"), 
                                                                       width = "100%"),
                                                           textInput(inputId = "project_name", label = "Project Name:", width = "100%"),
                                                           textAreaInput(inputId = "project_desctiotion", label = "Project Description:", width = "100%"),
                                                           actionButton(inputId = "createproject_button", label = "Create Project", width = "100%"))
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
                                                  fluidRow(column(width = 6, offset = 3,
                                                                  fileInput(inputId = "file1", label = "Upload Training data (Max upto 100 MB)", 
                                                                            placeholder = "Upload only .csv files",
                                                                            accept = c("text/csv", ".csv"),width = "100%"),
                                                                  
                                                                  textInput(inputId = "train_data_description", 
                                                                                label = "Description", width = "100%"),
                                                                  
                                                                  actionButton(inputId = "uploaddata_button", label = "Upload Data", width = "100%")
                                                  ))),
                                         
                                         tabPanel("Summary", "Tab content 2"),
                                         tabPanel("Preprocessing", "Tab content 3"))
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
  
  # Creating Reactive values
  project_info <- reactiveValues(project_path = "~/Projects", 
                                 current_project_name = "No Project Selected!!", 
                                 current_project_source = NULL,
                                 current_project_source_path = NULL,
                                 current_project_path = NULL, 
                                 current_project_train_data = NULL)
  
  # Creating Project folders
  observe({
    if(!dir.exists(project_info$project_path)) {
      dir.create(project_info$project_path)
      showNotification("Project folder is created", type = "message")
    } else {
      showNotification("Found Project folder", type = "warning")
    }
  })
  
  # Upadting Current Projct status
  output$project_status_menu <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(from = "Current Project", message = project_info$current_project_name, icon = icon("cube")),
                 messageItem(from = "Project Source", message = project_info$current_project_source))
  })
  
  
  # Creating Project
  observeEvent(input$createproject_button, {
    # ================================== #
    # Updating Project Reactive Variable #
    # ================================== #
    project_info$current_project_name <- input$project_name
    project_info$current_project_source <- input$project_source
    
    # ================================= #
    # Checking / Creating source folder #
    # ================================= #
    project_info$current_project_source_path <- paste0(project_info$project_path, "/", project_info$current_project_source)
    
    if(!dir.exists(project_info$current_project_source_path)) {
      dir.create(project_info$current_project_source_path)
      showNotification("Source folder is created", type = "message")
    } else {
      showNotification("Source folder found", type = "warning")
    }
    
    # ======================= #
    # Creating Project Folder #
    # ======================= #
    project_info$current_project_path <- paste0(project_info$current_project_source_path, "/", project_info$current_project_name)
    
    if(!dir.exists(project_info$current_project_path)) {
      dir.create(project_info$current_project_path)
      dir.create(paste0(project_info$current_project_path, "/Data"))
      showNotification(paste(project_info$current_project_name, "Project created"), type = "message")
    } else {
      showNotification("Can't create. Project is already Created!", type = "error")
    }
    
  })
  
  observeEvent(input$uploaddata_button, {
    
  })
  
  
  output$sel_project_list <- renderUI({
    
    # Checking list of directory
    source_path <- paste(project_info$project_path, input$select_project_source, sep = "/")
    
    project_list <- list.files(source_path)
    
    if(length(project_list)>0) {
      div(selectInput(inputId = "selectproject_name", label = "Select Project", choices = project_list, width = "100%"),
          actionButton(inputId = "selectproject_button", label = "Select Project", width = "100%"))
    } else {
      div("No projects found!!")
    }
    
    
    
    
  })
  
  
  
  
  
  
  output$mydata <- renderDataTable({
    airquality
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

