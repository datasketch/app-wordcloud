library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)

# falta el download selfcontained


ui <- panelsPage(panel(title = "Upload Data", 
                       width = 400,
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
                       width = 400,
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       width = 400,
                       body = uiOutput("controls")),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(#wordcloud2Output("result"),
                                  uiOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     # dsmodules::downloadImageUI("download_data_button", "Download HTML")),
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Download"))),
                       footer = shinypanels::modalButton(label = "Download plot", modal_id = "test")))


server <- function(input, output, session) {
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("#controls", parmesan = parmesan,
                  input = input, output = output,
                  env = parmesan_env)
  
  datasetInput <- callModule(textDocumentInput,
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
  
  wd <- reactive({
    dt0 <- datasetInput()
    if (!is.data.frame(dt0)) {
      dt0 <- gsub("\\.|,|;|¡|!|\\?|¿|-|:", "", dt0)
      dt0 <- strsplit(dt0, " ")[[1]]
      dt0 <- table(dt0)
    }
    
    assign("dt0", dt0, envir = globalenv())
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
    wordcloud2Output("wc", width = input$width, height = input$height)
  })
  
  output$wc <- renderWordcloud2({
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
  # callModule(downloadImage, "download_data_button", graph = wd(), lib = "highcharter", formats = "html")
}


shinyApp(ui, server)