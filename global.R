if (!require(shiny)) install.packages('shiny'); library(shiny)
if (!require(shinyFiles)) install.packages('shinyFiles'); library(shinyFiles)
if (!require(readr)) install.packages('readr'); library(readr)
if (!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
#if (!require(shinyjs)) install.packages('shinyjs'); library(shinyjs)

## wdLoad

wdLoadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shinyDirButton(id = ns("dir"), label = "Browse...", title = "Choose a working directory", buttonType = "primary"),     
    verbatimTextOutput(ns("path"), placeholder = TRUE)
  )
}

wdLoad <- function(input, output, session) {
  shinyDirChoose(input, "dir", roots = c(wd = getwd()))
  output$path <- renderText({
    parseDirPath(c(wd = getwd()), input$dir)
  })
  reactive({parseDirPath(c(wd = getwd()), input$dir)})
}

## loadingOptions

loadingOptionsUI <- function(id) {
  
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

loadingOptions <- function(input, output, session, path, filename) {
  reactive({
    do.call(read_delim, args = list(file = paste0(path(), "/", filename), col_names = input$header,
                                   col_types = cols(.default = col_character()),
                                   delim = input$sep))
  })
}

## showData

showDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("data"))
  )
}

showData <- function(input, output, session, data) {
  output$data <- renderDataTable(data())
}



