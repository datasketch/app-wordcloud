library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)
library(readtext)

# Internacionalización
# Arreglar código
# Falta el download selfcontained


ui <- panelsPage(panel(title = "Upload Data", 
                       width = 200,
                       body = textDocumentInputUI("initial_data",
                                                  choices = list("Muestra" = "sampleData",
                                                                 "Copiar & pegar" = "pasted",
                                                                 "URL (scraping de párrafos)" = "url",
                                                                 "Cargar" = "fileUpload"
                                                                 #"GoogleSheet" = "googleSheet",
                                                                 #"Mi librería" = "dsLibrary"
                                                  ),
                                                  selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 300,
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(#wordcloud2Output("result"),
                                  uiOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     # dsmodules::downloadImageUI("download_data_button", "Download HTML")),
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Download HTML"))),
                       footer = shinypanels::modalButton(label = "Download plot", modal_id = "test")))

f0 <- function(input, output, session, sampleFiles = NULL, infoList = NULL) {
  
  output$textDocumentInputControls <- shiny::renderUI({
    ns <- session$ns
    
    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()
    
    if (!is.null(input$textDocumentInput) && input$textDocumentInput == "sampleData") {
      if (!all(map_lgl(sampleFiles, file.exists)))
        stop("All sample files must exist")
    }
    
    textDocumentInputControls <- list(pasted = shiny::textAreaInput(ns("inputDataPasted"), label = "Paste", placeholder = "placeholder", rows = 5),
                                      fileUpload = shiny::fileInput(ns("inputDataUpload"), "Choose text, pdf file", accept = c("text/plain", ".txt", ".docx", ".pdf")),
                                      sampleData = shiny::selectInput(ns("inputDataSample"), "Select sample data", choices = sampleFiles),
                                      url = shiny::textInput(ns("inputURL"), "Page URL"),
                                      googleSheet = list(shiny::textInput(ns("inputDataGoogleSheet"), "GoogleSheet URL"), shiny::numericInput(ns("inputDataGoogleSheetSheet"), "Sheet", 1))#,
                                      # dsLibrary = dsDataInputUI(ns("dsFileInput"))
    )
    
    if (is.null(input$textDocumentInput)) {
      return()
    } else {
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
  
  output$textDocumentInputInfo <- renderUI({
    ns <- session$ns
    textDocumentInputInfo <- infoList[[input$textDocumentInput]]
    if (is.null(textDocumentInputInfo)) return()
    textDocumentInputInfo
  })
  
  inputData <- shiny::reactive({
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
    } else if (inputType == "fileUpload") {
      if (is.null(input$inputDataUpload))
        return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(), input$inputDataUpload$name)
      file.copy(old_path, path)
      print(path)
      tx <- readtext::readtext(path)$text
    } else if (inputType == "sampleData") {
      file <- input$inputDataSample
      print("asdf")
      print(file)
      tx <- readLines(file) %>%
        paste(collapse = "<br/>")
    } else if (inputType == "url") {
      url <- input$inputURL
      tx <- xml2::read_html(url) %>%
        xml2::xml_find_all("//p") %>%
        xml2::xml_text() %>%
        paste(collapse = "<br/>")
    } else if (inputType == "googleSheet") {
      if (is.null(input$inputDataGoogleSheet))
        return()
      if (input$inputDataGoogleSheet == "")
        return()
      # url <- input$inputDataGoogleSheet
      # ws <- input$inputDataGoogleSheetSheet
      # s <- gs_url(url)
      # tabs <- gs_ws_ls(s)
      # df <- gs_read_csv(s, ws = ws)
    } else if (inputType == "dsLibrary") { # ADAPTAR PARA IMÁGENES
      # tx <- callModule(dsDataInput, "dsFileInput")
      # tx <- df()
    }
    return(tx)
  })
  inputData
}

server <- function(input, output, session) {
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  output_parmesan("controls", 
                  parmesan = parmesan,
                  input = input, 
                  output = output)
  
  datasetInput <- callModule(f0,
                             "initial_data",
                             sampleFile = list("Montés" = "data/sampleData/nvtm"),
                             infoList = list("pasted" = "",
                                             "fileUpload" = "Importar archivos de texto (.doc, .txt, .pdf)",
                                             "sampleData" = "",
                                             "url" = "Se extraen los párrafos (el contenido de los HTML tags p) de la página web"))
  
  output$data_preview <- renderUI({
    req(datasetInput())
    HTML(datasetInput())
  })
  
  # n_palabras <- reactive({
  #   req(datasetInput())
  #   nrow(datasetInput())
  # })
  observe({
    assign("dt", datasetInput(), envir = globalenv())
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
               rotateRatio = input$rotation_ratio,
               shape = input$shape,
               ellipticity = input$ellipticity,
               widgetsize = c(input$width, input$height)
    )
  })  
  
  output$result <- renderUI({
    session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
    wordcloud2Output("wc", width = input$width, height = input$height)
  })
  
  output$wc <- renderWordcloud2({
    session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
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
  callModule(downloadHtmlwidget, "download_data_button", widget = reactive(wd()), name = "wordcloud")
  # callModule(downloadImage, "download_data_button", graph = wd(), lib = "highcharter", formats = "html")
}


shinyApp(ui, server)