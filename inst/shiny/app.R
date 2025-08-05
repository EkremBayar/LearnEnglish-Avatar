source("library.R")
source("functions.R")
source("ui.R")

server <- function(input, output, session) {

  # Reactive Data -----------------------------------------------------------
  rvList <- reactiveValues()

  observeEvent(input$serie_type, {
    if(length(ls())>0){suppressWarnings(rm(ls()[str_detect(ls(), pattern = "transcripts_")]))}
    selected_data_path <- switch(
      input$serie_type,
      "Avatar: The Last Airbender" = "data/transcripts_atla.rda",
      "The Legend of Korra" = "./data/transcripts_korra.rda",
      "Spiderman" = "./data/transcripts_spiderman_tas.rda",
      "Lost" = "data/transcripts_lost.rda",
      "Friends" = "data/transcripts_friends.rda",
      "HIMYM" = "data/transcripts_himym.rda",
      "Breaking Bad" = "data/transcripts_breaking_bad.rda",
      "The Office" = "data/transcripts_the_office.rda"
    )
    if (!is.null(selected_data_path) && file.exists(selected_data_path)) {
      rvList$raw_df <- get(load(selected_data_path))
    } else {
      rvList$raw_df <- NULL # Dosya bulunamazsa veya geçersiz seçimse boşaltın
      print("Veri dosyası bulunamadı veya geçersiz seçim!")
    }
  })


  # Dynamic Inputs ----------------------------------------------------------
  observe({
    req(input$serie_type)
    req(rvList$raw_df)
    if(!is.null(rvList$raw_df)){
      transcript_season <- c("All", rvList$raw_df %>% select(book, book_num) %>% distinct() %>% arrange(book_num) %>% pull(book))
    }else{
      transcript_season <- NULL
    }
    updateSelectInput(session, "serie_season", choices = transcript_season, selected = transcript_season[1])
  })

  observe({
    req(input$serie_type)
    req(input$serie_season)
    req(rvList$raw_df)
    if(!is.null(rvList$raw_df)){

      transcript_episodes <- switch(
        as.character(input$serie_season == "All"),
        "TRUE" = c("All", rvList$raw_df %>% select(book, book_num, chapter, chapter_num) %>% distinct() %>% arrange(book_num, chapter_num) %>% pull(chapter)),
        "FALSE" = rvList$raw_df %>% filter(book == input$serie_season)%>% select(chapter, chapter_num)%>% arrange(chapter_num) %>% distinct() %>% pull(chapter)
      )
    }else{
      transcript_episodes <- NULL
    }
    updateSelectInput(session, "serie_episode", choices = transcript_episodes, selected = transcript_episodes[1])
  })

  observe({
    req(input$serie_type)
    req(input$serie_season)
    req(input$serie_episode)
    req(rvList$raw_df)
    if(!is.null(rvList$raw_df)){

      transcript_characters <- switch(
        as.character(input$serie_season == "All"),
        "TRUE" = c("All", rvList$raw_df %>% pull(character) %>% unique() %>% sort()),
        "FALSE" = rvList$raw_df %>% filter(chapter == input$chapter)%>% pull(character) %>% unique() %>% sort()
      )
    }else{
      transcript_characters <- NULL
    }
    updateSelectInput(session, "serie_character", choices = transcript_characters, selected = transcript_characters[1])
  })


  # Submit Data -------------------------------------------------------------
  observeEvent(input$serie_submit, {
    temp <- rvList$raw_df
    if(input$serie_season != "All"){temp <- temp %>% filter(book == input$serie_season)}
    if(input$serie_episode != "All"){temp <- temp %>% filter(chapter == input$serie_episode)}
    if(input$serie_character != "All"){temp <- temp %>% filter(character == input$serie_character)}
    rvList$df <- temp %>%
      mutate(
        character = factor(character),
        book = fct_reorder(paste0(book_num,". ",  book), book_num),
        chapter = fct_reorder(paste0(book_num,".",chapter_num,". ",  chapter), chapter_num)
      ) %>%
      filter(!is.na(character_words), character != "Scene Description") %>%
      select(book_num, book, chapter_num, chapter, character, character_words)
  })

  # Find Pattern ------------------------------------------------------------
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



  # Find & Remove Pattern & Reactive Data -----------------------------------
  temp <- reactive({

    if(is.null(rvList$df)){
      temp <- NULL
    }else{
      temp <- rvList$df

      df <- temp %>%
        mutate(
          character = factor(character),
          book = fct_reorder(book, book_num),#fct_reorder(paste0(book_num,". ",  book), book_num),
          chapter = fct_reorder(chapter, chapter_num)#fct_reorder(paste0(book_num,".",chapter_num,". ",  chapter), chapter_num)
        ) %>%
        filter(!is.na(character_words), character != "Scene Description") %>%
        select(book, chapter, character, character_words) #"full_text", "character_words"))

      if(length(input$eng_word) > 0){

        if(input$type_process == "Pattern" | input$type_process == "Phrasal Verbs"){
          temp <- df %>% filter(find_structure(character_words, word = input$eng_word))
        }else if(input$type_process == "Detect"){
          temp <- df %>% filter(find_detect(character_words, word = input$eng_word))
        }else if(input$type_process == "Participles"){
          temp <- tryCatch({df %>% filter(find_participles(character_words, type = input$eng_word)) %>% suppressWarnings()},error=function(e){NULL})
        }

      }else{
        temp <- df
      }

      if(length(input$remove_eng_word) > 0){
        temp <- temp %>% filter(!str_detect(stringr::str_to_lower(character_words), paste0(stringr::str_to_lower(input$remove_eng_word),collapse="|")))
      }
    }
    return(temp)
  })


  # Reset Inputs ------------------------------------------------------------
  observeEvent(input$reset_button,{
    updateSelectizeInput(session, "eng_word", choices = character(), selected = character())
    updateSelectizeInput(session, "remove_eng_word", choices = character(), selected = character())
  })


  # Last Pattern ------------------------------------------------------------
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


  # Datatable Page Change ---------------------------------------------------
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


  # Datatable ---------------------------------------------------------------
  output$dt_transcript <- renderDataTable({

    if(!is.null(temp())){
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
    }


  })


  # Download Transcript -----------------------------------------------------
  observeEvent(input$download_excel, {
    if(!is.null(temp())){
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
    }else{
      sendSweetAlert(session, type = "warning", title = "Transcript Warning!", text = "Please, submit a transcript!")
    }

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
