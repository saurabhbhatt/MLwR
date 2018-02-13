# ============================= #
# Checking and Loading packages #
# ============================= #
check_package <- function(pkg_name = "dplyr") {
  if(!require(pkg_name, character.only = T)) {
    install.packages(pkg_name, dependencies = T)
    require(pkg_name, character.only = T)
  }
}

# ============================= #
# Once shiny app start it is necessary to check the basic criterion
# ============================= #
MLwR_start <- function() {
  # Building Project_Meta file
  if(!file.exists("~/MLwR_project.csv")) {
    proj_file <- data.frame(Project_Name = NA, Source = NA, Path = NA, Date_Created = NA)
    proj_file <- proj_file[-1, ]
    write.csv(proj_file, file = "~/MLwR_project.csv", row.names = F)
  }
}

# ==================================== #
# Function for creating Project Folder #
# ==================================== #
create_project_folder <- function(project_path = NULL, project_name = NULL, Desc = NULL) {
  # Creating Folders
  dir.create(project_path)
  dir.create(paste0(project_path,"/Data"))
  dir.create(paste0(project_path,"/Tasks"))
  dir.create(paste0(project_path,"/Models"))
  dir.create(paste0(project_path,"/Output"))
  
  # Creating Files
  if(!is.null(Desc)) {
    # Create Desc fike
  }
  
  # Crating Task FIles
  
  
  
  
}



# ============================= #
# UI Related Functions #
# ============================= #
# Creating a directory Button
shiny_Dir_Button <- function (id, label, title, buttonType = "default", class = "form-control action-button shiny-bound-input") {
  tagList(singleton(tags$head(tags$script(src = "sF/shinyFiles.js"), 
                              tags$link(rel = "stylesheet", type = "text/css", href = "sF/styles.css"), 
                              tags$link(rel = "stylesheet", type = "text/css", href = "sF/fileIcons.css"))), 
          tags$div(class="form-group shiny-input-container", style="width: 100%;",
                   tags$label(class="control-label", title),
                   tags$button(id = id, type = "button", 
                               class = paste(c("shinyDirectories btn", paste0("btn-", buttonType), class), collapse = " "), 
                               `data-title` = title, as.character(label))
                   )
          )
}


# This function is used to resize the text box
textAreaInput <- function(inputId, label, value = "", width = NULL, height = NULL,
                          cols = NULL, rows = NULL, placeholder = NULL) {
  
  value <- restoreInput(id = inputId, default = value)
  
  style1 <- if (!is.null(width))  paste0("width: ",  validateCssUnit(width),  ";")
  
  style <- paste(
    if (!is.null(height)) paste0("height: ", validateCssUnit(height), ";")
  )
  
  # Workaround for tag attribute=character(0) bug:
  #   https://github.com/rstudio/htmltools/issues/65
  if (length(style) == 0) style <- NULL
  if (length(style1) == 0) style1 <- NULL
  
  div(class = "form-group shiny-input-container", style = style1,
      tags$label(label, `for` = inputId),
      tags$textarea(
        id = inputId,
        class = "form-control",
        placeholder = placeholder,
        style = style,
        rows = rows,
        cols = cols,
        value
      )
  )
}





