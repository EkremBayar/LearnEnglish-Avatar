# Packages
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(forcats)
library(shinyWidgets)

source("functions.R")

data(transcripts_atla)
data(transcripts_korra)
data(transcripts_lost)
data(transcripts_breaking_bad)
data(transcripts_himym)
data(transcripts_the_office)
data(transcripts_friends)
data(transcripts_spiderman_tas)
data(phrasal_verbs)

ui <- page_sidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML(
      ".dropup .dropdown-menu {
      max-height: 200px !important;
      overflow-y: auto !important;
    }"
    ))
  ),
  # Uygulamanın genel temasını ve stilini belirliyoruz.
  # "superhero" veya "cosmo" gibi farklı temaları deneyebilirsiniz.
  # primary ve secondary renklerini de özelleştirebilirsiniz.
  theme = bs_theme(
    version = 5, # Bootstrap 5 sürümünü kullanıyoruz
    bootswatch = "flatly", # Şık bir Bootswatch teması
    fg = "#333333", # Ön plan (metin) rengi
    bg = "#ffffff", # Arka plan rengi
    primary = "#007bff", # Ana vurgu rengi (örneğin butonlar)
    secondary = "#6c757d", # İkincil vurgu rengi
    info = "#002749" # Bilgi mesajları için renk
  ),

  # Sidebar içeriği
  sidebar = sidebar(

    title = tagList(tags$img(
      src = "logo.png",
      alt = "Uygulama Logosu", # Resim yüklenmezse gösterilecek alternatif metin
      style = "max-width: 50%; height: auto; display: block; margin: auto;"
    ), h4("Avatar Transcripts",style="text-align:center")
    ), # Sidebar başlığı
    width = 300, # Sidebar genişliği (isteğe bağlı)
    open = "desktop", # Masaüstünde varsayılan olarak açık, mobilde kapalı

    hr(),

    #selectInput("serie_type", label = "Avatar", choices = c("Aang", "Korra", "Lost"), selected = "Aang"),

    selectInput("type_process", label = "Type",
                choices = c("Pattern", "Participles", "Detect", "Phrasal Verbs"),
                selected = "Pattern"),

    uiOutput("type_process_select"),

    selectizeInput(
      inputId = "remove_eng_word",
      label = "Remove word",
      choices = NULL,
      # choices parametresi HİÇ KULLANILMADI veya choices = NULL olarak ayarlandı
      multiple = TRUE, # Birden fazla etiket seçilebilir/eklenebilir
      options = list(
        create = TRUE, # Kullanıcının kendi metnini yazıp yeni etiket oluşturmasına izin ver
        placeholder = "Type word to delete and press Enter..." # Placeholder metni
      )
    ),

    hr(), # Yatay çizgi

    # Ek bilgi veya butonlar
    p("Water, Earth, Fire, Air and English!"),
    actionButton("reset_button", "Reset", icon = icon("rotate"), class = "btn-secondary"),
    tags$img(
      src = "iroh.png",
      alt = "Uygulama Logosu" # Resim yüklenmezse gösterilecek alternatif metin
    ),
    # pickerInput("page_select", "Pagination", choices = NULL,options = list(
    #   `live-search` = TRUE,                   # Arama kutusu
    #   `live-search-placeholder` = "Search page number..."  # Arama kutusu yer tutucu
    # ))
    numericInput("page_select", "Pagination", value = 1),
    #downloadButton("download_script", "Download", icon = icon("file-excel")),
    actionButton("download_excel", "Download",icon = icon("file-excel")),
    uiOutput("js_download")
  ),

  # Ana panel içeriği
  card(
    card_header(
      div(
        style="display: flex; justify-content: space-between; align-items: center;",
        div(
          style="display:inline-block;",
          div(style="display:inline-block;",selectInput("serie_type", label = "Series", choices = c("Aang", "Korra", "Spiderman", "Lost", "Friends", "HIMYM", "Breaking Bad", "The Office"), selected = "Aang")),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_season", label = "Seasons", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_episode", label = "Episodes", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_character", label = "Characters", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",actionButton("serie_submit", label = "Submit")),
        ),
        div(HTML('<a href="https://translate.google.com/?sl=en&tl=tr&op=translate" target="blank_" style="margin-right:10px">Google Translate</a>'),
        input_dark_mode(id = "dark_mode_toggle", mode = "light"))

    )),


    dataTableOutput("dt_transcript"),
    datatable_header_ui("dt_transcript"),
    card_footer("Only the Avatar can save your English skills!")
  )
)


server <- function(input, output, session) {

  # Dynamic Series Inputs
  observe({
    req(input$serie_type)

    transcript_season <- switch(input$serie_type,
           "Aang" = unique(transcripts_atla$book),
           "Korra" = unique(transcripts_korra$book),
           "Spiderman" = unique(transcripts_spiderman_tas$book),
           "Lost" = unique(transcripts_lost$book),
           "Friends" = unique(transcripts_friends$book),
           "HIMYM" = unique(transcripts_himym$book),
           "Breaking Bad" = unique(transcripts_breaking_bad$book),
           "The Office" = unique(transcripts_the_office$book)
           )
    updateSelectInput(session, "serie_season", choices = c("All", transcript_season), selected = "All")
  })

  observe({
    req(input$serie_type)
    req(input$serie_season)

    transcript_season <- switch(input$serie_type,
                                "Aang" = distinct(transcripts_atla[, c("book", "chapter")]),
                                "Korra" = distinct(transcripts_korra[, c("book", "chapter")]),
                                "Spiderman" = distinct(transcripts_spiderman_tas[, c("book", "chapter")]),
                                "Lost" = distinct(transcripts_lost[, c("book", "chapter")]),
                                "Friends" = distinct(transcripts_friends[, c("book", "chapter")]),
                                "HIMYM" = distinct(transcripts_himym[, c("book", "chapter")]),
                                "Breaking Bad" = distinct(transcripts_breaking_bad[, c("book", "chapter")]),
                                "The Office" = distinct(transcripts_the_office[, c("book", "chapter")])
    )

    transcript_episodes <- switch(as.character(input$serie_season == "All"),
                                "TRUE" = transcript_season %>% pull(chapter),
                                "FALSE" = transcript_season %>% filter(book == input$serie_season) %>% pull(chapter)
    )

    updateSelectInput(session, "serie_episode", choices = c("All", transcript_episodes), selected = "All")
  })

  observe({
    req(input$serie_type)
    req(input$serie_season)
    req(input$serie_episode)

    transcript_season <- switch(input$serie_type,
                                "Aang" = distinct(transcripts_atla[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "Korra" = distinct(transcripts_korra[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "Spiderman" = distinct(transcripts_spiderman_tas[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "Lost" = distinct(transcripts_lost[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "Friends" = distinct(transcripts_friends[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "HIMYM" = distinct(transcripts_himym[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "Breaking Bad" = distinct(transcripts_breaking_bad[, c("book", "chapter", "character")] %>% filter(character != "Scene Description")),
                                "The Office" = distinct(transcripts_the_office[, c("book", "chapter", "character")] %>% filter(character != "Scene Description"))

    )

    transcript_episodes <- switch(as.character(input$serie_season == "All"),
                                  "TRUE" = transcript_season,
                                  "FALSE" = transcript_season %>% filter(book == input$serie_season)
    )

    transcript_characters <- switch(as.character(input$serie_episode == "All"),
                                  "TRUE" = transcript_episodes %>% pull(character) %>% unique %>% sort,
                                  "FALSE" = transcript_episodes %>% filter(chapter == input$serie_episode) %>% pull(character) %>% unique %>% sort
    )

    updateSelectInput(session, "serie_character", choices = c("All", transcript_characters), selected = "All")
  })


  # Reactive Data -----------------------------------------------------------
  rvList <- reactiveValues()

  observeEvent(input$serie_type, {
    rvList$raw_df <- switch(
      input$serie_type,
      "Aang" = transcripts_atla,
      "Korra" = transcripts_korra,
      "Spiderman" = transcripts_spiderman_tas,
      "Lost" = transcripts_lost,
      "Friends" = transcripts_friends,
      "HIMYM" = transcripts_himym,
      "Breaking Bad" = transcripts_breaking_bad,
      "The Office" = transcripts_the_office
    )
  })


  observeEvent(input$serie_submit, {
    temp <- rvList$raw_df
    if(input$serie_season != "All"){temp <- temp %>% filter(book == input$serie_season)}
    if(input$serie_episode != "All"){temp <- temp %>% filter(chapter == input$serie_episode)}
    if(input$serie_character != "All"){temp <- temp %>% filter(character == input$serie_character)}
    rvList$df <- temp
  })

  # Reactive Data
  temp <- reactive({

    input$type_process
    input$eng_word
    input$remove_eng_word
    input$serie_type

    if(is.null(rvList$df)){
      temp <- rvList$raw_df
    }else{
      temp <- rvList$df
    }


    df <- temp %>%
      mutate(
        character = factor(character),
        book = fct_reorder(paste0(book_num,". ",  book), book_num),
        chapter = fct_reorder(paste0(book_num,".",chapter_num,". ",  chapter), chapter_num)
      ) %>%
      filter(!is.na(character_words), character != "Scene Description") %>%
      select(book, chapter, character, character_words) #"full_text", "character_words"))

    if(length(input$eng_word) > 0){

      if(input$type_process == "Pattern" | input$type_process == "Phrasal Verbs"){
        temp <- df %>% filter(find_structure(character_words, word = input$eng_word))
      }else if(input$type_process == "Detect"){
        temp <- df %>% filter(find_detect(character_words, word = input$eng_word))
      }else if(input$type_process == "Participles"){
        temp <- df %>% filter(find_participles(character_words, type = input$eng_word)) %>% suppressWarnings()
      }

    }else{
      temp <- df
    }

    if(length(input$remove_eng_word) > 0){
      temp <- temp %>% filter(!str_detect(stringr::str_to_lower(character_words), paste0(stringr::str_to_lower(input$remove_eng_word),collapse="|")))
    }


    return(temp)
  })

  observeEvent(input$reset_button,{
    updateSelectizeInput(session, "eng_word", choices = character(), selected = character())
  })


  observeEvent(input$eng_word, {
    if(input$type_process == "Pattern" | input$type_process == "Detect"){
      rvList$last_pattern <- input$eng_word
      rvList$last_pattern_select <- input$eng_word
    }
  })

  observe({
    req(input$type_process)
    if(input$type_process == "Pattern" | input$type_process == "Detect"){
      if(length(input$eng_word) == 0 | is.null(input$eng_word)){
        rvList$last_pattern_select <- character()
      }
    }
  })



  observe({
    req(input$type_process)

    if(input$type_process == "Pattern" | input$type_process == "Detect"){
      choices_temp <- NULL
      create_temp <- TRUE
      label_temp <- "Find pattern"
      multiple_temp <- TRUE
      updateSelectizeInput(session, "eng_word", choices = rvList$last_pattern, selected = rvList$last_pattern_select)
    }else if(input$type_process == "Participles"){
      choices_temp <- c("ing", "ed")
      create_temp <- FALSE
      label_temp <- "Find participles"
      multiple_temp <- FALSE
      updateSelectizeInput(session, "eng_word", choices = c("ing", "ed"), selected = "ing")
    }else if(input$type_process == "Phrasal Verbs"){
      choices_temp <- phrasal_verbs %>% filter(str_detect(PV, " ")) %>% pull(PV)
      create_temp <- FALSE
      label_temp <- "Find phrasal verbs"
      multiple_temp <- TRUE
      updateSelectizeInput(session, "eng_word", choices = choices_temp, selected = character())

    }

    output$type_process_select <- renderUI({
      selectizeInput(
        inputId = "eng_word",
        label = label_temp,
        choices = choices_temp,
        # choices parametresi HİÇ KULLANILMADI veya choices = NULL olarak ayarlandı
        multiple = multiple_temp, # Birden fazla etiket seçilebilir/eklenebilir
        options = list(
          create = create_temp, # Kullanıcının kendi metnini yazıp yeni etiket oluşturmasına izin ver
          placeholder = "Type word and press Enter..." # Placeholder metni
        )
      )
    })
  })




  current_page_length <- reactiveVal(10)  # başlangıç varsayımı
  observeEvent(input$dt_page_length, {
    current_page_length(input$dt_page_length)
  })
  observe({
    req(temp())
    n_rows <- nrow(temp())
    page_length <- current_page_length()
    total_pages <- ceiling(n_rows / page_length)
    #updatePickerInput(session, "page_select", choices = 1:total_pages)
    updateNumericInput(session, "page_select", min = 1, value = 1, max = total_pages)
  })

  observeEvent(input$page_select, {
    session$sendCustomMessage("change_page", as.numeric(input$page_select))
  })

  # Datatable
  output$dt_transcript <- renderDataTable({

    js_target_index <- which(names(temp() %>% select(-c("book", "chapter"))) == "character_words")

    highlight_terms <- input$eng_word[input$eng_word != "" & !is.na(input$eng_word)]

    if (!is.null(highlight_terms) && length(highlight_terms) > 0) {
      if (input$type_process == "Pattern" | input$type_process == "Phrasal Verbs") {
        # --- Pattern için JS kodu ---
        # Kelime sınırları ile tam eşleşme, regex özel karakterlerini kaçırarak
        escaped_words_r <- sapply(highlight_terms, function(word) {
          # Bu pattern, tüm özel regex karakterlerini yakalar ve her birinin önüne \\ ekler
          gsub("([.\\+*?\\[\\^\\]$(){}\\|/:!<>=-])", "\\\\\\1", word, perl = TRUE)
        })

        word_patterns_js <- paste0("\\\\b", escaped_words_r, "\\\\b", collapse = "|")

        render_js <- list(
          targets = js_target_index,
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            paste0("    var targetWordPattern = '", word_patterns_js, "';"),
            "    var wordRegex = new RegExp(targetWordPattern, 'gi');",
            "    var highlightedData = data.replace(wordRegex, '<span class=\"highlight-red\">$&</span>');",
            "    var sentenceRegex = new RegExp('([^.!?]*<span class=\"highlight-red\">[^<]*?</span>[^.!?]*[.!?]?)', 'gi');",
            "    var finalHtml = highlightedData.replace(sentenceRegex, '<span class=\"highlight-blue\">$&</span>');",
            "    return finalHtml;",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      } else if (input$type_process == "Detect") {
        # --- Detect için YENİ JS kodu ---
        # Kelime sınırları olmadan, girilen metni doğrudan ara
        escaped_detect_terms <- sapply(highlight_terms, function(term) {
          # Regex özel karakterlerini kaçır
          # Boşluklar da dahil olmak üzere tam olarak girilen metni arayacağız
          gsub("([.\\+*?\\[\\^\\]$(){}\\|/:!<>=-])", "\\\\\\1", tolower(str_replace_all(term, "\\'", "\\\\'")), perl = TRUE)
        })

        # Kelimeleri veya kelime öbeklerini '|' ile birleştir
        # Burada kelime sınırı (\b) kullanmıyoruz
        full_detect_pattern <- paste0(escaped_detect_terms, collapse = "|")

        render_js <- list(
          targets = js_target_index,
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            paste0("    var targetDetectPattern = '", full_detect_pattern, "';"),
            "    var detectRegex = new RegExp(targetDetectPattern, 'gi');",
            "    var highlightedData = data.replace(detectRegex, '<span class=\"highlight-red\">$&</span>');",

            # Cümleyi vurgulayan kısım aynı kalabilir
            "    var sentenceRegex = new RegExp('([^.!?]*<span class=\"highlight-red\">[^<]*?</span>[^.!?]*[.!?]?)', 'gi');",
            "    var finalHtml = highlightedData.replace(sentenceRegex, '<span class=\"highlight-blue\">$&');",

            "    return finalHtml;",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      } else if (input$type_process == "Participles") {
        # --- Participles için JS kodu ---
        # Sadece "ing" veya "ed" ekinin kelime sonunda olmasını arayacağız.
        # Örneğin, "ing" için: \b\w+ing\b
        # "ed" için: \b\w+ed\b

        # Seçili olan "ing" ve/veya "ed" için regex desenleri oluştur.
        participle_patterns <- sapply(highlight_terms, function(p_type) {
          if (p_type == "ing") {
            return("\\\\b\\\\w+ing\\\\b") # Kelime sonunda 'ing'
          } else if (p_type == "ed") {
            return("\\\\b\\\\w+ed\\\\b")  # Kelime sonunda 'ed'
          } else {
            return("") # Tanımsız tip için boş desen
          }
        })

        # Boş olmayan desenleri filtrele ve birleştir
        participle_patterns <- participle_patterns[participle_patterns != ""]
        if (length(participle_patterns) > 0) {
          full_participle_pattern <- paste0(participle_patterns, collapse = "|")
        } else {
          full_participle_pattern <- "" # Eğer hiç geçerli partikül tipi seçilmediyse
        }

        render_js <- list(
          targets = js_target_index,
          render = JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            # Participles için özel regex deseni
            paste0("    var targetParticiplePattern = '", full_participle_pattern, "';"),
            "    var participleRegex = new RegExp(targetParticiplePattern, 'gi');",
            "    var highlightedData = data.replace(participleRegex, '<span class=\"highlight-red\">$&</span>');",

            # Cümleyi vurgulayan kısım (kırmızı vurgulanan kelimeyi içeren cümleyi mavi yapar)
            "    var sentenceRegex = new RegExp('([^.!?]*<span class=\"highlight-red\">[^<]*?</span>[^.!?]*[.!?]?)', 'gi');",
            "    var finalHtml = highlightedData.replace(sentenceRegex, '<span class=\"highlight-blue\">$&</span>');",

            "    return finalHtml;",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        )
      } else {
        render_js <- list() # Tanımsız type_process için boş
      }
    } else {
      render_js <- list() # input$eng_word boşsa
    }

    datatable(
      tryCatch({temp() %>% select(-c("book", "chapter"))},error=function(e){NULL}),
      #colnames = c("Book", "Chapter", "Character", "Text"),
      colnames = c("Character", "Text"),
      selection = "none",
      #filter = "top",
      escape = FALSE, # HTML etiketlerinin yorumlanmasını sağlar
      options = list(
        # "Sentence" sütununu hedeflemek için columnDefs kullanıyoruz.
        # index = 1 (R'da 2. sütun, JS'de 1. sütun)
        columnDefs = list(
          render_js
        ),
        paginationType = 'full_numbers'
      ),
      callback = JS(paste(
        # Sayfa uzunluğu değişimini Shiny'a bildir
        "table.on('length.dt', function(e, settings, len) {",
        "  Shiny.setInputValue('dt_page_length', len, {priority: 'event'});",
        "});",

        # Sayfa bilgisi her draw'da güncellenir
        "table.on('draw', function() {",
        "  var pageInfo = table.page.info();",
        "  Shiny.setInputValue('current_page', pageInfo.page + 1, {priority: 'event'});",
        "});",

        # Sayfa değiştirici
        "Shiny.addCustomMessageHandler('change_page', function(page) {",
        "  table.page(page - 1).draw(false);",
        "});",

        # Çift tıklama ile Google Translate
        "table.on('dblclick', 'td', function() {",
        "  var cell = $(this);",
        "  var character_words = cell.text();",
        "  var encodedText = encodeURIComponent(character_words);",
        "  var googleTranslateUrl = 'https://translate.google.com/?sl=en&tl=tr&text=' + encodedText;",
        "  window.open(googleTranslateUrl, '_blank');",
        "  return false;",
        "});",
        sep = "\n"
      ))

    )
  })


  # output$download_script <- downloadHandler(
  #   filename = function() {
  #     paste0("transcript_",stringr::str_to_lower(input$serie_type),".xlsx")
  #   },
  #   content = function(file) {
  #     # wb <- openxlsx::createWorkbook()
  #     # openxlsx::addWorksheet(wb, "Transcript")
  #     # openxlsx::writeDataTable(wb, x = temp(), sheet = 1)
  #     # openxlsx::addStyle(wb, "Transcript", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(temp()) + 1), cols = 1:4, gridExpand = TRUE)
  #     # openxlsx::setColWidths(wb, "Transcript", cols = 1:3, widths = 15)
  #     # openxlsx::setColWidths(wb, "Transcript", cols = 4, widths = 100)
  #     # openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  #     openxlsx::write.xlsx(temp(), file)
  #   }
  # )

  observeEvent(input$download_excel, {
    # 1. R'da Excel dosyasını geçici bir konuma oluştur
    # tempfile() fonksiyonu güvenli ve benzersiz bir geçici dosya yolu sağlar.
    temp_excel_file <- tempfile(fileext = ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Transcript")
    openxlsx::writeDataTable(wb, "Transcript", temp()) # temp() yerine temp_data() kullanıldı
    openxlsx::addStyle(wb, "Transcript", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(temp()) + 1), cols = 1:4, gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Transcript", cols = 1:3, widths = 15)
    openxlsx::setColWidths(wb, "Transcript", cols = 4, widths = 100)

    # saveWorkbook'u geçici dosyaya yaz
    openxlsx::saveWorkbook(wb, file = temp_excel_file, overwrite = TRUE)

    # 2. Geçici dosyanın içeriğini raw (ham) olarak oku
    excel_raw_data <- readBin(temp_excel_file, what = "raw", n = file.info(temp_excel_file)$size)

    # 3. Geçici dosyayı sil (isteğe bağlı ama iyi pratik)
    unlink(temp_excel_file)

    # 4. Raw veriyi base64 string'e dönüştür
    excel_base64 <- base64enc::base64encode(excel_raw_data)

    # 5. JavaScript fonksiyonunu çağırarak indirmeyi tetikle
    session$sendCustomMessage(type = "downloadFile", message = list(
      filename = paste0("transcript_",stringr::str_to_lower(input$serie_type),".xlsx"),
      content = excel_base64,
      mimeType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ))
  })

  # JavaScript fonksiyonunu tanımla
  output$js_download <- renderUI({
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadFile', function(message) {
        // Base64 kodlu içeriği ikili (binary) veriye dönüştür
        const byteCharacters = atob(message.content);
        const byteNumbers = new Array(byteCharacters.length);
        for (let i = 0; i < byteCharacters.length; i++) {
          byteNumbers[i] = byteCharacters.charCodeAt(i);
        }
        const byteArray = new Uint8Array(byteNumbers);

        const blob = new Blob([byteArray], { type: message.mimeType });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = message.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });
    "))
  })


}

shinyApp(ui, server)
