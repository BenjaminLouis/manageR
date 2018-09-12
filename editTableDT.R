editableDTUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    actionButton(ns("delRow"), "Delete", icon = icon("remove", lib = "glyphicon")), 
    actionButton(ns("addRow"), "Add New", icon = icon("plus", lib = "glyphicon")), 
    actionButton(ns("editData"), "Edit Data", icon = icon("wrench", lib = "glyphicon")),
    radioButtons3(ns("selection"), "Data Selection", choices = c("single", "multiple"), inline = TRUE, labelwidth = 150, align = "center"),
    column(width = 2, actionButton(ns("savedata"), label = "Save as csv", icon = icon("save", lib = "glyphicon")), offset = 10)),
    br(),
    DT::dataTableOutput(ns("origTable")), 
    conditionalPanel(
      condition = "true==false",
      numericInput(ns("width2"), "width2", value = 100),
      textInput(ns("result"), "result", value = ""), numericInput(ns("no"), "no", value = 1)
    )
  )
}


editableDT <- function(input, output, session, dataname = reactive(""), data = reactive(NULL), inputwidth = reactive(250), mode = reactive(2), path, filename) {
  
  deleted <- deleted1 <- edited <- edited1 <- added <- added1 <- updated1 <- updated <- c()
  observe({
    updateTextInput(session, "result", value = dataname())
  })
  observe({
    updateNumericInput(session, "width2", value = inputwidth())
  })
  df <- reactive({
    if (input$result != "") {
      df <- eval(parse(text = input$result))
    }
    else {
      df <- data()
    }
    df
  })
  output$origTable <- DT::renderDataTable({
    if (dataname() != "") {
      validate(need(any(class(try(eval(parse(text = input$result)))) %in%
        c("tbl_df", "tibble", "data.frame")), "Please enter the valid data name"))
    }
    datatable(df(), selection = input$selection, caption = NULL)
  })
  observeEvent(input$delRow, {
    ids <- input$origTable_rows_selected
    if (length(ids) > 0) {
      x <- as.data.frame(df())
      x <- x[-ids, ]
      if (input$result == "deleted") {
        deleted1 <<- x
        updateTextInput(session, "result", value = "deleted1")
      }
      else {
        deleted <<- x
        updateTextInput(session, "result", value = "deleted")
      }
    }
    else {
      showModal(modalDialog(
        title = "Delete Row", "Please Select Row(s) To Delete. Press 'Esc' or Press 'OK' button",
        easyClose = TRUE, footer = modalButton("OK"), size = "l"
      ))
    }
  })
  observeEvent(input$remove, {
    x <- as.data.frame(df())
    x <- x[-input$no, ]
    if (input$result == "deleted") {
      deleted1 <<- x
      updateTextInput(session, "result", value = "deleted1")
    }
    else {
      deleted <<- x
      updateTextInput(session, "result", value = "deleted")
    }
    if (input$no > nrow(x)) {
      updateNumericInput(session, "no", value = nrow(x))
    }
  })
  observeEvent(input$addRow, {
    x <- as.data.frame(df())
    x1 <- tibble::add_row(x)
    newname <- max(as.numeric(rownames(x)), nrow(x), na.rm = TRUE) +
      1
    rownames(x1) <- c(rownames(x), newname)
    if (input$result == "added") {
      added1 <<- x1
      updateTextInput(session, "result", value = "added1")
    }
    else {
      added <<- x1
      updateTextInput(session, "result", value = "added")
    }
    updateNumericInput(session, "no", value = nrow(x1))
    editData2()
  })
  observeEvent(input$new, {
    x <- as.data.frame(df())
    x1 <- tibble::add_row(x)
    newname <- max(as.numeric(rownames(x)), nrow(x), na.rm = TRUE) +
      1
    rownames(x1) <- c(rownames(x), newname)
    if (input$result == "added") {
      added1 <<- x1
      updateTextInput(session, "result", value = "added1")
    }
    else {
      added <<- x1
      updateTextInput(session, "result", value = "added")
    }
    updateNumericInput(session, "no", value = nrow(x1))
  })
  observeEvent(input$update, {
    ids <- input$no
    x <- df()
    myname <- colnames(x)
    status <- ifelse(tibble::has_rownames(x), 1, 0)
    x <- as.data.frame(x)
    rownames(x)[ids] <- input$rowname
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
        x[ids, i] <- as.POSIXct(input[[myname[i]]],
          tz = tz,
          origin = "1970-01-01"
        )
      }
    }
    if (input$result == "updated") {
      updated1 <<- x
      updateTextInput(session, "result", value = "updated1")
    }
    else {
      updated <<- x
      updateTextInput(session, "result", value = "updated")
    }
  })
  observeEvent(input$Close, {
    updateCheckboxInput(session, "showEdit", value = FALSE)
  })
  observeEvent(input$no, {
    mydf <- df()
    if (!is.null(mydf)) {
      myclass <- lapply(mydf, class)
      updateTextInput(session, "rowname", value = rownames(mydf)[input$no])
      updateNumericInput(session, "width", value = input$width)
      mydf <- as.data.frame(mydf[input$no, ])
      for (i in 1:ncol(mydf)) {
        myname <- colnames(mydf)[i]
        if ("factor" %in% myclass[[i]]) {
          updateSelectInput(session, myname,
            choices = levels(mydf[[i]]),
            selected = mydf[1, i]
          )
        }
        else if ("Date" %in% myclass[[i]]) {
          updateDateInput(session, myname, value = mydf[
            1,
            i
          ])
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
          updateTextInput(session, myname, value = mydf[
            1,
            i
          ])
        }
      }
    }
  })
  observeEvent(input$home, {
    updateNumericInput(session, "no", value = 1)
  })
  observeEvent(input$end, {
    updateNumericInput(session, "no", value = nrow(df()))
  })
  observeEvent(input$left, {
    value <- ifelse(input$no > 1, input$no - 1, 1)
    updateNumericInput(session, "no", value = value)
  })
  observeEvent(input$right, {
    value <- ifelse(input$no < nrow(df()), input$no + 1, nrow(df()))
    updateNumericInput(session, "no", value = value)
  })
  observeEvent(input$rowno, {
    maxno <- nrow(df())
    if (input$rowno > maxno) {
      updateNumericInput(session, "rowno", value = maxno)
      updateNumericInput(session, "no", value = maxno)
    }
    else {
      updateNumericInput(session, "no", value = input$rowno)
    }
  })
  output$test2 <- renderUI({
    ns <- session$ns
    ids <- input$no
    if (length(ids) == 1) {
      mydf <- df()
      mylist <- list()
      myclass <- lapply(mydf, class)
      mylist[[1]] <- actionButton(ns("home"), "", icon = icon("backward",
        lib = "glyphicon"
      ))
      mylist[[2]] <- actionButton(ns("left"), "", icon = icon("chevron-left",
        lib = "glyphicon"
      ))
      mylist[[3]] <- numericInput3(ns("rowno"), "rowno",
        value = input$no, min = 1, max = nrow(mydf),
        step = 1, width = 50 + 10 * log10(nrow(mydf))
      )
      mylist[[4]] <- actionButton(ns("right"), "", icon = icon("chevron-right",
        lib = "glyphicon"
      ))
      mylist[[5]] <- actionButton(ns("end"), "", icon = icon("forward",
        lib = "glyphicon"
      ))
      mylist[[6]] <- actionButton(ns("new"), "", icon = icon("plus",
        lib = "glyphicon"
      ))
      mylist[[7]] <- textInput3(ns("rowname"), "rowname",
        value = rownames(mydf)[input$no], width = 150
      )
      mylist[[8]] <- numericInput3(ns("width"), "input width",
        value = input$width2, min = 100, max = 500, step = 50,
        width = 80
      )
      mylist[[9]] <- hr()
      addno <- 9
      mydf <- as.data.frame(mydf[input$no, ])
      for (i in 1:ncol(mydf)) {
        myname <- colnames(mydf)[i]
        if ("factor" %in% myclass[[i]]) {
          mylist[[i + addno]] <- selectInput3(ns(myname),
            myname,
            choices = levels(mydf[[i]]), selected = mydf[
              1,
              i
            ], width = input$width2
          )
        }
        else if ("Date" %in% myclass[[i]]) {
          mylist[[i + addno]] <- dateInput3(ns(myname),
            myname,
            value = mydf[1, i], width = input$width2
          )
        }
        else if ("logical" %in% myclass[[i]]) {
          if (is.na(mydf[1, i])) {
            myvalue <- FALSE
          } else {
            myvalue <- mydf[1, i]
          }
          mylist[[i + addno]] <- checkboxInput3(ns(myname),
            myname,
            value = myvalue, width = input$width2
          )
        }
        else {
          mylist[[i + addno]] <- textInput3(ns(myname),
            myname,
            value = mydf[1, i], width = input$width2
          )
        }
      }
      do.call(tagList, mylist)
    }
    else {
      h4("You can edit data after select one row in datatable.")
    }
  })
  observeEvent(input$width, {
    updateNumericInput(session, "width2", value = input$width)
  })
  observeEvent(input$editData, {
    ids <- input$origTable_rows_selected
    if (length(ids) == 1) {
      updateNumericInput(session, "no", value = ids)
    } else if (input$no > nrow(df())) {
      updateNumericInput(session, "no", value = 1)
    }
    editData2()
  })
  editData2 <- reactive({
    input$editData
    input$addRow
    ns <- session$ns
    showModal(modalDialog(title = "Edit Data", footer = tagList(
      actionButton(ns("remove"),"Delete",icon = icon("remove", lib = "glyphicon")),
      actionButton(ns("update"), "Update", icon = icon("ok",lib = "glyphicon")), 
      modalButton("Close", icon = icon("eject",lib = "glyphicon"))
    ), easyClose = TRUE, uiOutput(ns("test2")), size = "l"))
  })

  observeEvent(input$savedata, {
    ns <- session$ns
    showModal(modalDialog(
      title = "Which delimiter should be used ?",
      footer = tagList(
        actionButton(ns("save"), "Save", icon = icon("disk", lib = "glyphicon")),
        modalButton("Close", icon = icon("eject", lib = "glyphicon"))
      ),
      easyClose = TRUE,
      radioButtons(
        inputId = ns("sep"), label = "Separation", selected = ";",
        choices = c(Comma = ",", "Semi colon" = ";", Tabulation = "\t")
      ),
      size = "l"
    ))
  })

  observeEvent(input$save, {
    ns <- session$ns
    write_delim(x = df(), path = normalizePath(file.path(path(), filename)), delim = input$sep)
    removeModal()
  })

  return(df)
}