editableDTUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      inline(actionButton(ns("delRow"), "Delete", icon = icon("remove", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("addRow"), "Add New", icon = icon("plus", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("editData"), "Edit Data", icon = icon("wrench", lib = "glyphicon")), va = "middle"),
      inline(radioButtons(ns("selection"), "Data Selection", choices = c("single", "multiple"), inline = TRUE), va = "middle"),
      inline(actionButton(ns("savedata"), label = "Save change", icon = icon("save", lib = "glyphicon")), va = "middle")
    ),
    br(),
    DT::dataTableOutput(ns("origTable"))
  )
}


editableDT <- function(input, output, session, data = reactive(NULL), width = 250, path, filename) {
  
  rv <- reactiveValues(no = 1, update = 0)#, newdf = newdf)
  newdf <- NA # pour éviter un erreur avec l'utilisation de '<<-' qui est nécessaire
  df <- reactive({
    if (rv$update != 0) {
      df <- newdf
    }
    else {
      df <- data()
    }
    df
  })
  
  observeEvent(data(),
    rv$update <- 0
  )
  
  # Delete button
  # -------------
  observeEvent(input$delRow, {
    ids <- input$origTable_rows_selected
    if (length(ids) > 0) {
      x <- as.data.frame(df())
      x <- x[-ids, ]
      newdf <<- x
      rv$update <- rv$update + 1
    }
    else {
      showModal(modalDialog(
        title = "Delete Row", "Please Select Row(s) To Delete. Press 'Esc' or Press 'OK' button",
        easyClose = TRUE, footer = modalButton("OK"), size = "l"
      ))
    }
  })
  
  # Add New button
  # --------------
  observeEvent(input$addRow, {
    x <- as.data.frame(df())
    x1 <- tibble::add_row(x)
    newname <- max(as.numeric(rownames(x)), nrow(x), na.rm = TRUE) + 1
    rownames(x1) <- c(rownames(x), newname)
    rv$no <- nrow(x1)
    newdf <<- x1
    rv$update <- rv$update + 1
    editData2()
  })
  
  # Edit Data button
  # ----------------
  observeEvent(input$editData, {
    ids <- input$origTable_rows_selected
    if (length(ids) == 1) {
      rv$no <- ids
    } else if (rv$no > nrow(df())) {
      rv$no <- 1
    }
    editData2()
  })
  
  # Modal dialog to edit data
  # -------------------------
  editData2 <- reactive({
    input$editData
    input$addRow
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Edit Data", 
        uiOutput(ns("displayedit")),
        footer = tagList(
          actionButton(ns("remove"),"Delete",icon = icon("remove", lib = "glyphicon")),
          actionButton(ns("update"), "Update", icon = icon("ok",lib = "glyphicon")),
          modalButton("Close", icon = icon("eject",lib = "glyphicon"))
        ), 
        easyClose = TRUE, 
        size = "l"
      )
    )
  })
  
  # UI displaying data to edit in modal dialog
  # -----------------------------------------
  output$displayedit <- renderUI({
    ns <- session$ns
    ids <- rv$no
    if (length(ids) == 1) {
      mydf <- df()
      mylist <- list()
      myclass <- lapply(mydf, class)
      mylist[[1]] <- inline(actionButton(inputId = ns("home"), label = NULL, icon = icon("backward", lib = "glyphicon")), m = 3)
      mylist[[2]] <- inline(actionButton(inputId = ns("left"), label = NULL, icon = icon("chevron-left", lib = "glyphicon")), m = 3)
      mylist[[3]] <- inline(numericInput(inputId = ns("rowno"), label = NULL, value = rv$no, min = 1, max = nrow(mydf), step = 1, width = 50 + 10 * log10(nrow(mydf))), m = 3)
      mylist[[4]] <- inline(actionButton(inputId = ns("right"), label = NULL, icon = icon("chevron-right",lib = "glyphicon")), m = 3)
      mylist[[5]] <- inline(actionButton(inputId = ns("end"), label = NULL, icon = icon("forward",lib = "glyphicon")), m = 3)
      mylist[[6]] <- inline(actionButton(inputId = ns("new"), label = NULL, icon = icon("plus",lib = "glyphicon")), m = 3)
      mylist[[7]] <- hr()
      addno <- 7
      mydf <- as.data.frame(mydf[rv$no,])
      for (i in 1:ncol(mydf)) {
        myname <- colnames(mydf)[i]
        if ("factor" %in% myclass[[i]]) {
          mylist[[i + addno]] <- inline(selectInput(inputId = ns(myname), label = myname, choices = levels(mydf[[i]]), selected = mydf[1,i], width = width))
        }
        else if ("Date" %in% myclass[[i]]) {
          mylist[[i + addno]] <- inline(dateInput(inputId = ns(myname), label = myname, value = mydf[1, i], width = width))
        }
        else if ("logical" %in% myclass[[i]]) {
          if (is.na(mydf[1, i])) {
            myvalue <- FALSE
          } else {
            myvalue <- mydf[1, i]
          }
          mylist[[i + addno]] <- inline(checkboxInput(inputId = ns(myname), label = myname, value = myvalue, width = width))
        }
        else {
          mylist[[i + addno]] <- inline(textInput(inputId = ns(myname), label = myname, value = mydf[1, i], width = width))
        }
      }
      do.call(tagList, mylist)
    }
    else {
      h4("You can edit data after select one row in datatable.")
    }
  })
  
  # Backward button
  # ----------------
  observeEvent(input$home, {
    rv$no <- 1
  })
  
  # Forward button
  # ----------------
  observeEvent(input$end, {
    rv$no <- nrow(df())
  })
  
  # Left chevron button
  # ----------------
  observeEvent(input$left, {
    value <- ifelse(rv$no > 1, rv$no - 1, 1)
    rv$no <- value
  })
  
  # Right chevron button
  # ----------------
  observeEvent(input$right, {
    value <- ifelse(rv$no < nrow(df()), rv$no + 1, nrow(df()))
    rv$no <- value
  })
  
  # New button
  # ----------
  observeEvent(input$new, {
    x <- as.data.frame(df())
    x1 <- tibble::add_row(x)
    newname <- max(as.numeric(rownames(x)), nrow(x), na.rm = TRUE) + 1
    rownames(x1) <- c(rownames(x), newname)
    rv$no <- nrow(x1)
    newdf <<- x1
    rv$update <- rv$update + 1
  })
  
  # Row number selection
  # ----------------
  observeEvent(input$rowno, {
    maxno <- nrow(df())
    if (input$rowno > maxno) {
      updateNumericInput(session, "rowno", value = maxno)
      rv$no <- maxno
    }
    else {
      rv$no <- input$rowno
    }
  })
  
  # Remove a row from edit modal dialog
  # -----------------------------
  observeEvent(input$remove, {
    x <- as.data.frame(df())
    x <- x[-rv$no, ]
    if (rv$no > nrow(x)) {
      rv$no <- nrow(x)
    }
    newdf <<- x
    rv$update <- rv$update + 1
  })
  
  # Update button
  # --------------
  observeEvent(input$update, {
    ids <- rv$no
    x <- df()
    myname <- colnames(x)
    x <- as.data.frame(x)
    for (i in 1:ncol(x)) {
      x[ids, i] <- input[[myname[i]]]
    }
    for (i in 1:ncol(x)) {
      try(x[ids, i] <- input[[myname[i]]])
      if ("POSIXct" %in% class(x[ids, i])) {
        tz <- ""
        if (!is.null(attr(x[ids, i], "tzone"))) {
          tz <- attr(x[ids, i], "tzone")
        }
        x[ids, i] <- as.POSIXct(input[[myname[i]]], tz = tz, origin = "1970-01-01")
      }
    }
    newdf <<- x
    rv$update <- rv$update + 1
  })
  
  # Action when row number changed
  # ------------------------------
  observeEvent(rv$no, {
    mydf <- df()
    if (!is.null(mydf)) {
      myclass <- lapply(mydf, class)
      mydf <- as.data.frame(mydf[rv$no, ])
      for (i in 1:ncol(mydf)) {
        myname <- colnames(mydf)[i]
        if ("factor" %in% myclass[[i]]) {
          updateSelectInput(session, myname, choices = levels(mydf[[i]]), selected = mydf[1, i])
        }
        else if ("Date" %in% myclass[[i]]) {
          updateDateInput(session, myname, value = mydf[1,i])
        }
        else if ("logical" %in% myclass[[i]]) {
          if (is.na(mydf[1, i])) {
            myvalue <- FALSE
          } else {
            myvalue <- mydf[1, i]
          }
          updateCheckboxInput(session, myname, value = myvalue)
        }
        else {
          updateTextInput(session, myname, value = mydf[1,i])
        }
      }
    }
  })
  
  # Save as csv button
  # ------------------
  observeEvent(input$savedata, {
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Which delimiter should be used ?",
        radioButtons(
          inputId = ns("sep"), label = "Separation", selected = ";",
          choices = c(Comma = ",", "Semi colon" = ";", Tabulation = "\t")
        ),
        footer = tagList(
          actionButton(ns("save"), "Save", icon = icon("save", lib = "glyphicon")),
          modalButton("Close", icon = icon("eject", lib = "glyphicon"))
        ),
        easyClose = TRUE,
        size = "l"
      ))
  })
  
  # Save Button
  # -----------
  observeEvent(input$save, {
    ns <- session$ns
    write_delim(x = df(), path = normalizePath(file.path(path(), filename)), delim = input$sep)
    removeModal()
  })
  
  # Display Datatable
  # -----------------
  output$origTable <- DT::renderDataTable({
    datatable(df(), selection = input$selection, caption = NULL)
  })
  #print(df)
 reactive(df())
}