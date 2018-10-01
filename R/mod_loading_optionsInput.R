#' @title mod_loading_optionsInput and mod_loading_options
#' @description A shiny module that read a delimited file (with \code{read_delim}) from a path and a filename. It is more a internal module than a module that works on itself.
#'
#' @param id shiny id
#'
#' @importFrom shiny NS tagList fluidRow column radioButtons checkboxInput
#'
#' @export
#'
#' @examples
#' library(shiny)
#' if (interactive()) {
#' ui <- fluidPage(
#'   h4("Choose a path"),
#'   textInput("path", value = getwd(), label = NULL),
#'   h4("Choose a filename (do not forget the extension)"),
#'   textInput("filename", value = "", label = NULL),
#'   h4("The module goes from here............................"),
#'   mod_loading_optionsInput("loadfile"),
#'   h4("..........................................to here"),
#'   h4("You can display the result"),
#'   tableOutput("data")
#' )
#' 
#' server <- function(input, output, session) {
#'   data <- callModule(mod_loading_options,"loadfile", path = reactive(input$path), 
#'   filename = reactive(input$filename))
#'   output$data <- renderTable(data())
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
mod_loading_optionsInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
                    radioButtons(inputId = ns("sep"), label = "Separation", selected = ";",
                                        choices = c(Virgule = ",", 'Point Virgule' = ";", Tabulation = "\t"))
                    
      ),
      column(width = 6,
                    checkboxInput(inputId = ns("header"), label = "Header ?", value = TRUE)
      )
    )
  )
}


#' mod_loading_options server function
#'
#' @param input internal
#' @param output internal
#' @param session inernal
#' @param path reactive string value. The path to the directory where the file is
#' @param filename reactive string value. Name and extension of the file to read
#' @param coltypes see \code{col_types} argument in \code{read_delim}
#'
#' @importFrom readr read_delim
#' @importFrom shiny reactive req
#' 
#' @export
#' @rdname mod_loading_optionsInput
mod_loading_options <- function(input, output, session, path, filename, coltypes = cols(.default = col_character())) {
  reactive({
    req(path())
    do.call(read_delim, args = list(file = paste0(path(), "/", filename()), col_names = input$header,
                                           col_types = coltypes,
                                           delim = input$sep))
  })
}
