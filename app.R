library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(dplyr)
library(wordcloud2)
library(htmlwidgets)
library(tm)
library(webshot)
library(shinyjs)
library(shinycustomloader)

# arreglar tamaño wordcloud (tigres, castores...)

webshot::install_phantomjs()

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) {
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  } else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  } else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}



ui <- panelsPage(useShi18ny(),
                 useShinyjs(),
                 showDebug(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("text_input")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       # body = uiOutput("data_preview", style = "box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); padding: 12px 10px;")),
                       body = uiOutput("data_preview")),
                 panel(title = ui_("options"),
                       color = "chardonnay",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  output$text_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "url")
    names(choices) <- i_(c("sample", "paste", "upload", "url"), lang = lang())
    textDocumentInputUI("initial_data",
                        choices = choices,
                        selected = ifelse(is.null(input$`initial_data-textDocumentInput`), "sampleData", input$`initial_data-textDocumentInput`))
  })
  
  labels <- reactive({
    sm_f <- i_(c("sample_ch_0", "sample_ch_1", "sample_ch_2"), lang())
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1", "sample_ch_nm_2"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = sm_f,
         # sampleSelected = sm_s,
         
         pasteLabel = i_("paste", lang()), 
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         urlLabel = i_("url_lb", lang()),
         
         infoList = list("pasted" = "",
                         "fileUpload" = "",#"Importar archivos de texto (.doc, .txt, .pdf)",
                         "sampleData" = "",
                         "url" = i_("in_url", lang())))
  })
  
  inputData <- eventReactive(list(labels(), input$`initial_data-textDocumentInput`), {
    do.call(callModule, c(textDocumentInput, "initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    req(inputData())
    t0 <- gsub("<br/>", "", inputData()())
    HTML(paste0("<div style = 'box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); padding: 12px 10px; overflow-wrap: anywhere;'>", t0, "</div>"))
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
  
  # traduciendo "a mano" named choices de inputs
  observe({
    ch0 <- as.character(parmesan$words$inputs[[3]]$input_params$choices)
    names(ch0) <- i_(ch0, lang())
    sl0 <- ch0[which(c("en", "pt_BR", "es") %in% lang())]
    ch1 <- as.character(parmesan$styles$inputs[[2]]$input_params$choices)
    names(ch1) <- i_(ch1, lang())
    ch2 <- as.character(parmesan$sizes$inputs[[1]]$input_params$choices)
    names(ch2) <- i_(ch2, lang())
    
    # updateNumericInput(session, "top_n", value = 14)
    # updateNumberInput(session, "top_n", value = 14)
    updateSelectizeInput(session, "words_language", choices = ch0, selected = sl0)
    updateRadioButtons(session, "font_weight", choices = ch1, selected = input$font_weight)
    updateSelectInput(session, "shape", choices = ch2, selected = input$shape)
  })
  
  tb <- reactive({
    dt0 <- inputData()()
    if (is.null(dt0)) {
      # data.frame(word = c("w", "d"), freq = c(2, 2))
    } else {
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
      names(dt0)[1] <- "pl"
      dt0 %>%
        filter(!pl %in% "") %>%
        arrange(desc(Freq))
    }
  })
  
  n_palabras <- reactive({
    if (is.null(tb())) {
      0
    } else {
      nrow(tb())
    }
  })
  
  wd <- reactive({
    req(tb(), input$top_n)
    thm <- input$theme
    if (input$theme == "datasketch") {
      input$top_n
      thm <- rep(c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1"),
                 ceiling(input$top_n / 9))
    }
    if (!is.null(tb())) {
      safe_wordcloud <- purrr::safely(wordcloud2a)
      safe_wordcloud(data = tb()[1:input$top_n, ],
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
                     widgetsize = c(input$width, input$height))
    }
  })
  
  output$download <- renderUI({
    lb <- i_("download_plot", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    # mb <- list(textInput("name", i_("gl_name", lang())),
    #            textInput("description", i_("gl_description", lang())),
    #            selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
    #            selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
    #            selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    # downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("jpeg", "png", "pdf"),
    #              display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
    #              modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
    #              modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
    lb <- i_("download_plot", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    downloadImageUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("jpeg", "png", "pdf"), 
                    display = "dropdown", dropdownWidth = 160)
  })
  
  output$result <- renderUI({
    res <- wd()
    if (is.null(res$result)) {
      infomessage(p(res$error$message))
    } else {
      wd <- input$width
      if (input$width > 800) {
        wd <- "auto"
      }
      wordcloud2Output("wc", width = wd, height = input$height)
    }
  })
  
  output$wc <- renderWordcloud2({
    wd()$result
  })
  
  lapply(c("jpeg", "png", "pdf"), function(z) {
    buttonId <- paste0("download_data_button-DownloadImg", z)
    
    output[[paste0("download_data_button-DownloadImg", z)]] <- downloadHandler(
      filename = function() {
        paste0("wordcloud-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
      },
      content = function(file) {
        owd <- getwd()
        setwd(tempdir())
        on.exit(setwd(owd))
        # owd <- tempdir()
        # on.exit(setwd(owd))
        widget <- wd()$result
        saveWidget(widget, file = "tmp.html", selfcontained = FALSE)
        webshot::webshot("tmp.html", file, cliprect = c(0, 0, 905, 705), delay = 3.5)
      })
  })
  
  # # url params
  # par <- list(user_name = "brandon", org_name = NULL)
  # url_par <- reactive({
  #   url_params(par, session)
  # })
  # 
  # # prepare element for pining (for htmlwidgets or ggplots)
  # # función con user board connect y set locale
  # pin_ <- function(x, bkt, ...) {
  #   x <- dsmodules:::eval_reactives(x)
  #   bkt <- dsmodules:::eval_reactives(bkt)
  #   nm <- input$`download_data_button-modal_form-name`
  #   if (!nzchar(input$`download_data_button-modal_form-name`)) {
  #     nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
  #     updateTextInput(session, "download_data_button-modal_form-name", value = nm)
  #   }
  #   dv <- dsviz(x,
  #               name = nm,
  #               description = input$`download_data_button-modal_form-description`,
  #               license = input$`download_data_button-modal_form-license`,
  #               tags = input$`download_data_button-modal_form-tags`,
  #               category = input$`download_data_button-modal_form-category`)
  #   dspins_user_board_connect(bkt)
  #   Sys.setlocale(locale = "en_US.UTF-8")
  #   pin(dv, bucket_id = bkt)
  # }
  # 
  # # descargas
  # observe({
  #   downloadDsServer("download_data_button", element = reactive(wd()$result), formats = "",
  #                    errorMessage = NULL,#i_("gl_error", lang()),
  #                    modalFunction = pin_, reactive(wd()$result),
  #                    bkt = url_par()$inputs$user_name)
  # })
  
  
}


shinyApp(ui, server)