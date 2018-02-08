# ======================================= #
# Project: MLwR (Machine Learning with R) #
# This Project will help on EDA, Model Building etc. for all the competitions. 
# ======================================= #

# ====================== #
# Loading External files #
# ====================== #
source("functions.R")

# ================ #
# Loading Packages #
# ================ #
check_package("shiny")
check_package("shinydashboard")
check_package("shinyjs")
check_package("data.table")
check_package("DT")
check_package("mlr")
check_package("shinyFiles")
check_package("dplyr")
check_package("digest")

# ======================== #
# Initial Files Generation #
# ======================== #
MLwR_start()

# Updating Options
max_size <- 200
options(shiny.maxRequestSize=max_size*1024^2)  # Current Upload size = 120 MB. Default = 5 MB

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
                        menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("bar-chart")),
                        # menuItem("Feature Engineering", tabName = "widgets", icon = icon("th")),
                        menuItem("Building Models", tabName = "build_models", icon = icon("cogs")),
                        menuItem("Report", tabName = "widgets", icon = icon("file-text-o"))
                      )
                    ),   # End of SideBar
                    
                    ################
                    # Body Content #
                    ################
                    body = dashboardBody(
                     
                      # Adding CSS to the page
                      tags$head(
                        tags$link(href='http://fonts.googleapis.com/css?family=Roboto:300', rel='stylesheet', type='text/css'),
                        tags$style("body, h3, h4 { font-family: 'Roboto', sans-serif;}")
                      ),
                      
                      # tagList(useShinyjs(), loading.screens),
                      
                      
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
                                
                        ),   # End of Project Tab
                        
                        # Tab: Dashboard
                        tabItem(tabName = "tab_data",
                                fluidRow(
                                  tabBox(width = 12,
                                         title = "Data",
                                         id = "tabset1",
                                         tabPanel("Upload", 
                                                  fluidRow(column(width = 8, offset = 2,
                                                                  fileInput(inputId = "file1", label = paste("Upload Training data (Max upto", max_size, "MB)"), 
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
                                                  fluidRow(column(12, 
                                                                  valueBoxOutput(outputId = "summary_totalrows",width = 3),
                                                                  valueBoxOutput(outputId = "summary_totalcols",width = 3),
                                                                  valueBoxOutput(outputId = "summary_totalfact",width = 3),
                                                                  valueBoxOutput(outputId = "summary_totalnum", width = 3)),
                                                           column(12, hr(),dataTableOutput(outputId = "data_summary"))))
                                         
                                  )
                                )
                        ),  # End of Data Tab
                        
                        
                        # Tab: EDA
                        tabItem(tabName = "eda",
                                fluidRow(
                                  tabBox(title = "Exploratory Data Analysis", width = 12,
                                         
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
                                         tabPanel("Preprocessing", "Tab content 3")
                                  ))),  # End of EDA Tab
                        
                        
                        
                        # Tab: Building Models
                        tabItem(tabName = "build_models",
                                fluidRow(
                                  tabBox(width = 12,
                                         title = "Building Models",
                                         # The id lets us use input$tabset1 on the server to find the current tab
                                         id = "tabset2",
                                         
                                         # ================= #
                                         # Building Task Tab #
                                         # ================= #
                                         tabPanel("Task", 
                                                  fluidRow(column(width = 6, 
                                                                  textInput(inputId = "task_name", 
                                                                            label = "Enter Task Name", 
                                                                            width = "100%"),
                                                                  htmlOutput(outputId = "task_yvar_out")),
                                                           
                                                           column(width = 6,
                                                                  DT::dataTableOutput(outputId = "task_list")))),
                                         
                                         # =================== #
                                         # Building Trrain Tab #
                                         # =================== #
                                         tabPanel("Learner", "Tab content 2")
                                  )
                                )
                        )
                        
                      ))
)  # End of UI 

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
                                 select_project_train_data = NULL,
                                 select_project_train_data_summary = NULL)
  
  # ============================= #
  # Model Related Reactive values #
  # ============================= #
  mlr_models <- reactiveValues(task = list(train_task = NA, test_task = NA))
  
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
    valueBox(value = total_rows, subtitle = "Total Rows", icon = icon("list"), color = "blue")
  })
  
  output$summary_totalcols <- renderValueBox({
    total_cols <- ncol( project_info$select_project_train_data)
    valueBox(value = total_cols, subtitle = "Total Columns", icon = icon("sitemap"), color = "blue")
  })
  
  output$summary_totalfact <- renderValueBox({
    total_fact <- length(which(project_info$select_project_train_data_summary$type %in% c("character", "factor")))
    valueBox(value = total_fact, subtitle = "Total Factor Variables", icon = icon("quote-right"), color = "blue")
  })
  
  output$summary_totalnum <- renderValueBox({
    total_num <- length(which(project_info$select_project_train_data_summary$type %in% c("integer", "numeric")))
    valueBox(value = total_num, subtitle = "Total Numerical Variables", icon = icon("percent"), color = "blue")
  })
  
  output$data_summary <- renderDataTable({
    project_info$select_project_train_data_summary <- summarizeColumns(project_info$select_project_train_data)
    project_info$select_project_train_data_summary
  }, options = list(scrollX = TRUE), selection = "single")
  
  # ========================================================== #
  # ================ Tab = Building Models =================== #
  # ========================================================== #
  # --------------------------------------------------- #
  # --------------- Sub_Tab = Task ------------------ #
  # --------------------------------------------------- #
  output$task_yvar_out <- renderUI({
    div(selectInput(inputId = "task_yvar",label = "Select Output / Response Variable",
                    choices = project_info$select_project_train_data_summary$name, 
                    width = "100%"),
        textAreaInput(inputId = "task_description", label = "Task Description", width = "100%"),
        actionButton(inputId = "task_createmodel_button", label = "Create Task", width = "100%"))
    
  })
  
  output$task_list <- renderDataTable({
    mtcars
  })
  
  # Creating Task button click
  observeEvent(input$task_createmodel_button, {
    task_dir <- paste0(project_info$select_project_path, "/Task")
    
    
    # Creating Task Directory and task tracker
    if(!dir.exists(task_dir)) {
      dir.create(task_dir)
      
      # Creating Task Tracker
      x <- data.frame(ID = NA, Task_Name = NA, Response = NA, Description = NA, Date_Created = NA)
      x <- x[-1, ]
      write.csv(x, file = paste0(task_dir, "/task_list.csv"), row.names = F)
    }
    
    # Crating Task
    # Getting class of output variable
    output_class <- project_info$select_project_train_data_summary
    output_class <- output_class$type[which(output_class$name==input$task_yvar)]
    if(output_class=="factor" | output_class=="character") {
      task <- makeClassifTask(data = project_info$select_project_train_data, target = input$task_yvar)
      save()
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

