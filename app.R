library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)
library(tm)
library(readtext)
library(webshot)



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
                                  uiOutput("result"),
                                  shinypanels::modal(id = "download",
                                                     title = ui_("download_plot"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download plot", modal_id = "download")))



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
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input, 
                  output = output,
                  env = environment())
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadImageUI("download_data_button", dw, formats = c("jpeg", "png", "pdf"))
    # downloadHtmlwidgetUI("download_data_button", paste(dw, "PNG"))
  })
  
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = list("Smithsonian -- Tigers" = "data/sampleData/nvtm"),
         pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         urlLabel = i_("url_lb", lang()),
         infoList = list("pasted" = "",
                         "fileUpload" = "",#"Importar archivos de texto (.doc, .txt, .pdf)",
                         "sampleData" = "",
                         "url" = i_("in_url", lang())))
  })
  
  observeEvent(lang(), {
    ch0 <- as.character(parmesan$words$inputs[[3]]$input_params$choices)
    names(ch0) <- i_(ch0, lang())
    ch1 <- as.character(parmesan$styles$inputs[[5]]$input_params$choices)
    names(ch1) <- i_(ch1, lang())
    ch2 <- as.character(parmesan$sizes$inputs[[1]]$input_params$choices)
    names(ch2) <- i_(ch2, lang())
    updateSelectInput(session, "words_language", choices = ch0, selected = input$words_language)
    updateSelectInput(session, "background_color", choices = ch1, selected = input$background_color)
    updateSelectInput(session, "shape", choices = ch2, selected = input$shape)
  })
  
  td <- reactiveValues(text_uploaded = NULL)
  
  observe({
    td$text_uploaded <- do.call(callModule, c(textDocumentInput,
                                              "initial_data",
                                              labels()))
  })
  
  output$data_preview <- renderUI({
    HTML(paste0("<div style = 'box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); padding: 12px 10px;'>", td$text_uploaded(), "</div>"))
  })
  
  tb <- reactive({
    dt0 <- td$text_uploaded()
    dt0 <- gsub("<br/>", "", dt0)
    
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
    thm <- input$theme
    if (input$theme == "datasketch") {
      input$top_n
      thm <- rep(c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1"),
                 ceiling(input$top_n / 9))
    }
    wordcloud2(data = tb()[1:input$top_n, ],
               size = input$size,
               # minSize = input$min_size,
               gridSize = input$grid_size,
               fontFamily = input$font_family,
               fontWeight = input$font_weight,
               color = thm,
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
    lapply(c("jpeg", "png", "pdf"), function(z) {
      buttonId <- paste0("download_data_button-DownloadImg", z)
      session$sendCustomMessage("setButtonState", c("none", buttonId))
    })
    wordcloud2Output("wc", width = input$width, height = input$height)
  })
  
  output$wc <- renderWordcloud2({
    session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadImage"))
    req(wd())
    wd()
  })
  
  lapply(c("jpeg", "png", "pdf"), function(z) {
    buttonId <- paste0("download_data_button-DownloadImg", z)
    
    output[[paste0("download_data_button-DownloadImg", z)]] <- downloadHandler(
      filename = function() {
        session$sendCustomMessage("setButtonState", c("loading", buttonId))
        paste0("wordcloud-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        widget <- wd()
        saveWidget(widget, file = "tmp.html", selfcontained = FALSE)
        webshot::webshot("tmp.html", file, cliprect = c(0, 0, 905, 705), delay = 3.5)
        session$sendCustomMessage('setButtonState', c('done', buttonId))
      }
    )
  })
  
}



shinyApp(ui, server)