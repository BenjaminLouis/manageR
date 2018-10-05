#' @title mod_edit_settingsInput and mod_edit_settings
#' @description A shiny module to manage a settings file stored as xml in the www 
#' folder of the package
#'
#' @param id shiny id
#' 
#' @importFrom shiny NS tagList actionButton icon
#' 
#' @export
#'
#' @examples
#' \dontrun(
#' library(shiny)
#' library(xml2)
#' if (interactive()) {
#' ui <- fluidPage(
#'   mod_edit_settingsInput("config"),
#' )
#' 
#' server <- function(input, output, session) {
#'   initsetsdata <- reactive({
#'     read_xml(system.file("www/config.xml", package = "manageR"))
#'   })
#'   setsdata <- callModule(mod_edit_settings, 
#'                          "config", 
#'                          settingsdata = initsetsdata,
#'                          package = "manageR
#'   )
#' }
#' 
#' shinyApp(ui, server)
#' }
#' )
mod_edit_settingsInput <- function(id) {
  ns <- NS(id)
  tagList(actionButton(ns("config"), label = "Settings", icon = icon("settings", lib = "glyphicon")))
}

#' mod_edit_settings server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param settingsdata the reactive xml settings file
#' @param package package where the xml config file is
#'
#' @return the modified reactive xml settings file
#' 
#' @importFrom shiny observeEvent reactive showModal modalDialog uiOutput tagList actionButton icon modalButton renderUI h4 textInput checkboxInput fileInput removeModal reactiveValues 
#' @importFrom xml2 xml_text xml_find_all xml_name write_xml xml_set_text
#' @importFrom purrr pmap walk walk2
#' 
#' @export
#' @rdname mod_edit_settingsInput
mod_edit_settings <- function(input, output, session, settingsdata, package) {
  
  # Initialisation
  # --------------
  rv <- reactiveValues(update = 0)
  newsets <- NULL
  observeEvent(settingsdata, {
    rv$update <- 0
  })
  xml <- reactive({
    if (rv$update != 0) {
      xml <- newsets
    }
    else {
      xml <- settingsdata()
    }
    xml
  })
  
  # Modal dialog to edit settings
  # -----------------------------
  observeEvent(input$config, {
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Edit settings",
        uiOutput(ns("displaysettings")),
        footer = tagList(
          actionButton(ns("savesettings"), "Save changes", icon = icon("ok",lib = "glyphicon")),
          modalButton("Close", icon = icon("eject",lib = "glyphicon"))
        ),
        easyclose = TRUE,
        size = "l"
      )
    )
  })
  
  # UI displaying settings to edit in modal dialog
  # ----------------------------------------------
  output$displaysettings <- renderUI({
    ns <- session$ns
    types <- xml_text(xml_find_all(xml(), ".//@type"))
    names <- xml_name(xml_find_all(xml(), ".//*"))
    labels <- xml_text(xml_find_all(xml(), ".//@label"))
    texts <- xml_text(xml_find_all(xml(), ".//*"))
    ll <- list(types, names, labels, texts)
    tagList(
      pmap(ll, 
           ~switch(..1,
                   title = h4(..3),
                   text = textInput(inputId = ns(..2), label = ..3, value = ..4),
                   logical = checkboxInput(inputId = ns(..2), label = ..3, value = as.logical(..4)),
                   image = fileInput(inputId = ns(..2), label = ..3, placeholder = ..4,
                                    accept = c("image/png", "image/jpeg", "image/tiff", "image/gif", "image/svg+xml"))
           )
      )
    )
  })
  
  
  # Save settings button
  # --------------------
  observeEvent(input$savesettings, {
    ns <- session$ns
    thexml <- xml()
    types <- xml_text(xml_find_all(thexml, ".//@type"))
    names <- xml_name(xml_find_all(thexml, ".//*"))[!types %in% c("title")]
    types2 <- types[!types %in% c("title")]
    walk2(names, types2, function(x, y) {
      node <- xml_find_all(thexml, paste0(".//", x))
      if (y == "image") {
        if (!is.null(input[[x]])) {
          xml_set_text(node, input[[x]]$name)
        }
      } else if (y == "logical") {
        xml_set_text(node, as.character(input[[x]]))
      } else {
        xml_set_text(node, input[[x]])
      }
    })
    newsets <<- thexml
    rv$update <- rv$update + 1
    if ("image" %in% types2) {
      walk(names[types2 %in% c("image")], function(x) {
        if (!is.null(input[[x]])) {
          file.copy(normalizePath(input[[x]]$datapath, mustWork = FALSE, winslash = "/"), 
                    paste(system.file(package = package), "www", input[[x]]$name, sep = "/"),
                    overwrite = TRUE)
        }
      })
    }
    write_xml(thexml, system.file("www/config.xml", package = package))
    removeModal()
  })
  
  # Return
  # ------
  return(xml)
  
}

