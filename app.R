# Project MLwR

# Importing Libraries
library(shiny)
library(shinydashboard)
library(DT)
library(mlr)
library(shinyFiles)
library(dplyr)
library(data.table)

# Loading External files
source("functions.R")

# Building Project_Meta file
if(!file.exists("~/MLwR_project.csv")) {
  proj_file <- data.frame(Project_Name = NA, Source = NA, Path = NA, Date_Created = NA)
  proj_file <- proj_file[-1, ]
  write.csv(proj_file, file = "~/MLwR_project.csv", row.names = F)
}

# Updating Options
options(shiny.maxRequestSize=100*1024^2)  # Current Upload size = 120 MB. Default = 5 MB

# Define UI for application
ui <- dashboardPage(title = "Machine Learning with R", 
                    
                    ##################
                    # Header Content #
                    ##################
                    header = dashboardHeader(title = "MLwR",
                                             
                                             # Inserting Dropdown menu
                                             dropdownMenuOutput(outputId = "project_status_menu")
                    ),  # End of Header
                    
                    ###################
                    # Sidebar Content #
                    ###################
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
                    
                    ################
                    # Body Content #
                    ################
                    body = dashboardBody(
                      
                      # Adding CSS to the page
                      tags$head(
                        tags$link(href='http://fonts.googleapis.com/css?family=Roboto:300', rel='stylesheet', type='text/css'),
                        tags$style("body, h3, h4 { font-family: 'Roboto', sans-serif;}")
                      ),
                      
                      
                      tabItems(
                        # Tab >> Project
                        tabItem(tabName = "tab_project",
                                fluidRow(
                                  tabBox(width = 12, title = "Projects",
                                         tabPanel(title = "Select",
                                                  fluidRow(
                                                    column(6, dataTableOutput(outputId = "projects_select_projectTable", width = "100%")),
                                                    column(6, wellPanel(htmlOutput(outputId = "project_select_wellpanel")))
                                                  )
                                         ),
                                         
                                         tabPanel(title = "Create New",
                                                  fluidRow(
                                                    column(width = 6, offset = 3,
                                                           shiny_Dir_Button(id = "project_folder_select", 
                                                                            label = "Choose Folder", 
                                                                            title = "Please select a folder"),
                                                           
                                                           selectInput(inputId = "project_source", 
                                                                       label = "Project Source:", 
                                                                       choices = c("Kaggle", "Analytics Vidhya", "HackerEarth", "Fractal Analytics"), 
                                                                       width = "100%"),
                                                           textInput(inputId = "project_name", label = "Project Name:", width = "100%"),
                                                           textAreaInput(inputId = "project_description", label = "Project Description:", width = "100%"),
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
                                                  fluidRow(column(width = 8, offset = 2,
                                                                  fileInput(inputId = "file1", label = "Upload Training data (Max upto 100 MB)", 
                                                                            placeholder = "Upload only .csv files",
                                                                            accept = c("text/csv", ".csv"),width = "100%")),
                                                           
                                                           column(12, 
                                                                  tags$hr(),
                                                                  DT::dataTableOutput(outputId = "see_data"))
                                                                  
                                                                  
                                                                  # textInput(inputId = "train_data_description", 
                                                                  #           label = "Description", width = "100%"),
                                                                  # 
                                                                  # actionButton(inputId = "uploaddata_button", label = "Upload Data", width = "100%")
                                                  )),
                                         
                                         # Tab: Data / Summary #
                                         tabPanel("Summary", 
                                                  fluidRow(column(3, valueBoxOutput(outputId = "summary_totalrows",width = 12)),
                                                           column(3, valueBoxOutput(outputId = "summary_totalcols", width = 12)),
                                                           column(12, hr(),dataTableOutput(outputId = "data_summary")))),
                                         
                                         tabPanel("Visualization", 
                                                  fluidRow(column(6, 
                                                                  selectInput(inputId = "data_viz_inp_dim", 
                                                                              label = "Select Dimension", 
                                                                              choices = c("Univariate Analysis", "Bivariate Analysis"),
                                                                              width = "100%")),
                                                           column(6, 
                                                                  # Using conditional Panel for Univariate
                                                                  conditionalPanel(condition = "input.data_viz_inp_dim == 'Univariate Analysis'",
                                                                                   selectInput(inputId = "data_viz_inp_vartyp1",
                                                                                               label = "Select Variable Type", 
                                                                                               choices = c("Numerical", "Factor"), 
                                                                                               width = "100%")),
                                                                  
                                                                  # Using conditional Panel for Bivariate
                                                                  conditionalPanel(condition = "input.data_viz_inp_dim == 'Bivariate Analysis'",
                                                                                   selectInput(inputId = "data_viz_inp_vartyp2",
                                                                                               label = "Select Variable Type", 
                                                                                               choices = c("Numerical + Numerical", "Numerical + Factor", "Factor + Factor"), 
                                                                                               width = "100%"))
                                                           )
                                                  )),
                                         
                                         # Tab: Data / Preprocessing #
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

# ############################################################################ #
# ####################### Server Functionality ############################### #
# ############################################################################ #
server <- function(input, output, session) {
  
  # ======================================= #
  # Creating Reactive values to the project #
  # ======================================= #
  project_info <- reactiveValues(select_project_path = NULL, 
                                 select_project_name = "Project not found!!", 
                                 select_project_source = "Source Not found!!",
                                 select_project_train_data = NULL)
  
  # ============================ #
  # Getting Project Related File #
  # ============================ # 
  file_project_info <- reactiveFileReader(intervalMillis = 1000, 
                                  session = session, 
                                  filePath = "~/MLwR_project.csv", 
                                  readFunc = read.csv)
  
  
  # ==================================================== #
  # ================ HEADER UPDATION =================== #
  # ==================================================== #
  # Upadting Current Projct status
  output$project_status_menu <- renderMenu({
    dropdownMenu(type = "messages", icon = icon("info-circle"),
                 messageItem(from = "Current Project", message = project_info$select_project_name, icon = icon("cube")),
                 messageItem(from = "Project Source", message = project_info$select_project_source))
  })
  
  
  # =================================================== #
  # ================ Tab = Projects =================== #
  # =================================================== #
  # --------------------------------------------------- #
  # --------------- Sub_Tab = Select ------------------ #
  # --------------------------------------------------- #
  output$projects_select_projectTable <- DT::renderDataTable({
    x <- file_project_info()
    x <- x %>% select(Project_Name, Source)
    x
  },  selection = "single")
  
  react_select_project <- reactive({
    x <- file_project_info()
    
    if(!is.null(input$projects_select_projectTable_rows_selected)) {
      x <- x %>% slice(input$projects_select_projectTable_rows_selected)
      
      # Updating Reactive Values
      project_info$select_project_path <- x$Path
      project_info$select_project_name <- x$Project_Name
      project_info$select_project_source <- x$Source
    } else {
      project_info$select_project_path <- NULL
      project_info$select_project_name <- "Project not found!!"
      project_info$select_project_source <- "Source not found!!"
    }
    
  })
  
  output$project_select_wellpanel <- renderUI({
    # Triggeering React select Project
    react_select_project()
    
    div(h4(project_info$select_project_name),
          tags$hr(),
          tags$p(project_info$select_project_source))
  })
  
  # ------------------------------------------------------- #
  # --------------- Sub_Tab = Create New ------------------ #
  # ------------------------------------------------------- #
  shinyDirChoose(input = input, 
                 id = 'project_folder_select', 
                 root=c(root='~'))
  
  # Creating Project
  observeEvent(input$createproject_button, {
    
    # Initiating Decision Parameters
    create <- T
    
    # =================== #
    # Getting Folder Path #
    # =================== #
    project_folder <- unlist(input$project_folder_select$path)[-1]
    project_folder <- paste0("~/", paste(project_folder, collapse = "/"))
    
    # =================== #
    # Getting Folder Name #
    # =================== #
    project_folder_path <- paste0(project_folder, "/", input$project_name)
    
    # checking if folder exist
    if(dir.exists(project_folder_path)) {
      showNotification("Can't create. Project is already Created!", type = "error")
      create <- F
    }
    
    # ======================= #
    # Creating Project Folder #
    # ======================= #
    if(create) {
      # Generating Files and Folders 
      dir.create(project_folder_path)  # Creating Project Folder
      dir.create(paste0(project_folder_path, "/Data"))  # Creating Data Folder within project folder

      # Writing Description to text file
      write(x = input$project_description, file = paste0(project_folder_path, "/Description.txt"))
      
      # Storing data into project meta file
      create_proj_info <- file_project_info()
      create_proj_info <- rbind(create_proj_info, 
                                data.frame(Project_Name = input$project_name, 
                                           Source = input$project_source, 
                                           Path = project_folder_path, 
                                           Date_Created = Sys.Date()))
      write.csv(create_proj_info, "~/MLwR_project.csv", row.names = F)
      
      
      # Showing Notification for Creating Folder
      showNotification(paste(project_info$current_project_name, "Project created"), type = "message")
    }
    
  })
  
  # =============================================== #
  # ================ Tab = Data =================== #
  # =============================================== #
  # --------------------------------------------------- #
  # --------------- Sub_Tab = Upload ------------------ #
  # --------------------------------------------------- #
  output$see_data <- DT::renderDataTable({
    if(is.null(input$file1)) {
      
      train_file_path <- paste0(project_info$select_project_path, "/Data/train_data.csv")
      
      # Checking if train data exist
      if(file.exists(train_file_path)){
        df <- fread(train_file_path)
      } else {
        df <- data.frame(X = "No Data Found!! Please upload the file.")
      }
      
    } else {
      df <- read.csv(input$file1$datapath)
      fwrite(df, file = paste0(project_info$select_project_path, "/Data/train_data.csv"), row.names = F)
    }
    
    project_info$select_project_train_data <- df
    
    df
  }, options = list(scrollX = TRUE), selection = "none")
  
  # --------------------------------------------------- #
  # --------------- Sub_Tab = Summary ------------------ #
  # --------------------------------------------------- #
  output$summary_totalrows <- renderValueBox({
    total_rows <- nrow( project_info$select_project_train_data)
    valueBox(value = total_rows, subtitle = "Total Rows", icon = icon("list"))
  })
  
  output$summary_totalcols <- renderValueBox({
    total_cols <- ncol( project_info$select_project_train_data)
    valueBox(value = total_cols, subtitle = "Total Columns", icon = icon("sitemap"))
  })
  
  output$data_summary <- renderDataTable({
    summarizeColumns(project_info$select_project_train_data)
  }, options = list(scrollX = TRUE), selection = "multiple")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

