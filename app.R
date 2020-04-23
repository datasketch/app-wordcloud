library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)

# falta el download selfcontained

textDocumentInputUI <- function (id, choices = c("pasted", "fileUpload", "sampleData", 
                                                 "googleSheet", "url", "dsLibrary"), selected = "pasted") {
  ns <- NS(id)
  tagList(div(id = ns("textDocumentInput"), class = "tableInput", 
              radioButtons(ns("textDocumentInput"), "", choices = choices, 
                           selected = selected), uiOutput(ns("textDocumentInputControls"))))
}

textDocumentInput <- function (input, output, session, sampleFiles = NULL) 
{
  output$textDocumentInputControls <- renderUI({
    ns <- session$ns
    if (is.reactive(sampleFiles)) 
      sampleFiles <- sampleFiles()
    if (!is.null(input$textDocumentInput) && input$textDocumentInput == 
        "sampleData") {
      if (!all(map_lgl(sampleFiles, file.exists))) 
        stop("All sample files must exist")
    }
    textDocumentInputControls <- list(pasted = textAreaInput(ns("inputDataPasted"), 
                                                             label = "Paste", placeholder = "placeholder", rows = 5), 
                                      fileUpload = fileInput(ns("inputDataUpload"), "Choose text, pdf file", 
                                                             accept = c("text/plain", ".txt", ".docx", ".pdf")), 
                                      sampleData = selectInput(ns("inputDataSample"), 
                                                               "Select sample data", choices = sampleFiles), 
                                      url = textInput(ns("inputURL"), "Page URL"), googleSheet = list(textInput(ns("inputDataGoogleSheet"), 
                                                                                                                "GoogleSheet URL"), numericInput(ns("inputDataGoogleSheetSheet"), 
                                                                                                                                                 "Sheet", 1)))
    if (is.null(input$textDocumentInput)) {
      return()
    }
    else {
      textDocumentInputControls[[input$textDocumentInput]]
    }
  })
  queryData <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    json_str <- query[["json_data"]]
    data <- NULL
    if (!is.null(json_str)) {
      data <- jsonlite::fromJSON(URLdecode(json_str))
    }
    data
  })
  inputData <- reactive({
    if (is.null(input$textDocumentInput)) {
      warning("inputType must be one of pasted, fileUpload, sampleData, url, googlesheet, dsLibrary")
      return()
    }
    inputType <- input$textDocumentInput
    queryData <- queryData()
    if (!is.null(queryData)) {
      return(queryData)
    }
    if (inputType == "pasted") {
      if (is.null(input$inputDataPasted)) 
        return()
      if (input$inputDataPasted == "") 
        return()
      tx <- input$inputDataPasted
    }
    else if (inputType == "fileUpload") {
      if (is.null(input$inputDataUpload)) 
        return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(), input$inputDataUpload$name)
      file.copy(old_path, path)
      tx <- rio::import(path)
    }
    else if (inputType == "sampleData") {
      file <- input$inputDataSample
      tx <- readLines(file) %>% paste(collapse = "<br/>")
    }
    else if (inputType == "url") {
      url <- input$inputURL
      tx <- xml2::read_html(url) %>% xml2::xml_find_all("//p") %>% 
        paste(collapse = "<br/>")
    }
    else if (inputType == "googleSheet") {
      if (is.null(input$inputDataGoogleSheet)) 
        return()
      if (input$inputDataGoogleSheet == "") 
        return()
    }
    else if (inputType == "dsLibrary") {
    }
    return(tx)
  })
  inputData
}




ui <- panelsPage(panel(title = "Upload Data", 
                       width = 400,
                       body = textDocumentInputUI("initial_data",
                                                  choices = list("Muestra" = "sampleData",
                                                                 "Copiar & Pegar" = "pasted",
                                                                 "URL" = "url",
                                                                 "Cargar" = "fileUpload"
                                                                 #"GoogleSheet" = "googleSheet",
                                                                 #"Mi librería" = "dsLibrary"
                                                  ),
                                                  selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 400,
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       width = 400,
                       body = div(uiOutput("controls0"),
                                  uiOutput("controls1"))),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(wordcloud2Output("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Download")),
                                  shinypanels::modalButton(label = "Download plot", modal_id = "test"))))


config_path <- "parmesan"
# Reactive part
input_ids <- parmesan_input_ids(section = NULL, config_path = "parmesan")
input_ids_values <- lapply(input_ids, function(i) {NA})
names(input_ids_values) <- input_ids


server <- function(input, output, session) {
  
  react_env <- new.env()
  
  datasetInput <- callModule(textDocumentInput,
                             "initial_data",
                             sampleFile = list("Montés" = "data/sampleData/nvtm"))
  
  # renderizando los parámetros
  output$controls0 <- renderUI({
    parmesan_render_ui(sections = "Styles", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls1 <- renderUI({
    parmesan_render_ui(sections = "Display", config_path = config_path, input = input, env = react_env)
  })
  
  output$data_preview <- renderUI({
    req(datasetInput())
    HTML(datasetInput())
  })
  
  wd <- reactive({
    dt0 <- datasetInput()
    if (!is.data.frame(dt0)) {
      dt0 <- gsub("\\.|,|;|¡|!|\\?|¿|-|:", "", dt0)
      dt0 <- strsplit(dt0, " ")[[1]]
      dt0 <- table(dt0)
    }
    
    wordcloud2(data = dt0,
               size = input$size,
               minSize = input$min_size,
               gridSize = input$grid_size,
               fontFamily = input$font_family,
               fontWeight = input$font_weight,
               color = input$theme,
               backgroundColor = input$background_color,
               minRotation = pi * input$rotation[1],
               maxRotation = pi * input$rotation[2],
               rotateRatio = input$rotate_ratio,
               shape = input$shape,
               ellipticity = input$ellipticity#,
               # widgetsize = c(paste0(input$width, "px"), input$height)
    )
  })  
  
  output$result <- renderWordcloud2({
    req(wd())
    # withProgress(message = 'Calculation in progress',
    #              detail = 'This may take a while...', value = 0, {
    #                for (i in 1:15) {
    #                  incProgress(1/15)
    #                  Sys.sleep(0.25)
    #                }
    #              })
    # assign("wd", wd(), envir = globalenv())
    wd()
  })    
  
  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = wd(), name = "wordcloud")
  
}


shinyApp(ui, server)