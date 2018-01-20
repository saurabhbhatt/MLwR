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