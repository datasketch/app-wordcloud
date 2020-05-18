library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)
library(tm)
library(readtext)

# Internacionalización
# Arreglar código
# Falta el download selfcontained


ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("text_input")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       # body = uiOutput("data_preview", style = "box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); padding: 12px 10px;")),
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       color = "chardonnay",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  #wordcloud2Output("result"),
                                  uiOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = ui_("download_plot"),
                                                     # dsmodules::downloadImageUI("download_data_button", "Download HTML")),
                                                     # dsmodules::downloadHtmlwidgetUI("download_data_button", "Download HTML"))),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download plot", modal_id = "test")))


server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })  
  
  
  output$text_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "url")
    names(choices) <- i_(c("sample", "paste", "upload", "url"), lang = lang())
    textDocumentInputUI("initial_data",
                        choices = choices,
                        selected = ifelse(is.null(input$`initial_data-textDocumentInput`), "sampleData", input$`initial_data-textDocumentInput`))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input, 
                  output = output,
                  env = environment())
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadHtmlwidgetUI("download_data_button", paste(dw, "HTML"))
  })
  
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = list("Montés" = "data/sampleData/nvtm"),
         pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         urlLabel = i_("url_lb", lang()),
         infoList = list("pasted" = "",
                         "fileUpload" = "Importar archivos de texto (.doc, .txt, .pdf)",
                         "sampleData" = "",
                         "url" = "Se extraen los párrafos (el contenido de los HTML tags p) de la página web"))
  })
  
  observeEvent(labels(),{
  # observe({
  # datasetInput <- eventReactive(labels(), {
  datasetInput <<- do.call(callModule, c(textDocumentInput,
                          # do.call(callModule, c(t0,
                          "initial_data",
                          labels()))
  })
  
  output$data_preview <- renderUI({
    req(datasetInput())
    HTML(paste0("<div style = 'box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); padding: 12px 10px;'>", datasetInput(), "</div>"))
  })
  
  tb <- reactive({
    dt0 <- datasetInput()
    if (!is.data.frame(dt0)) {
      dt0 <- gsub("\\.|,|;|¡|!|\\?|¿|-|:", "", dt0)
      dt0 <- strsplit(dt0, " ")[[1]]
      dt0 <- table(dt0)
      dt0 <- as.data.frame(dt0)
    }
    if (input$stop_words) {
      dt1 <- setdiff(dt0$dt0, tm::stopwords(input$words_language))
      dt2 <- data.frame(pl = dt1) %>%
        left_join(dt0, by = c("pl" = "dt0")) 
      dt0 <- dt2
    }
    dt0 %>%
      arrange(desc(Freq))
  })
  
  n_palabras <- reactive({
    nrow(tb())
  })
  
  wd <- reactive({
    req(tb(), input$top_n)
    
    wordcloud2(data = tb()[1:input$top_n, ], 
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