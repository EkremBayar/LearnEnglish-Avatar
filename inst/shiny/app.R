library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(forcats)

# functions
find_structure <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word))
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0(paste0("\\b", word_pattern, "\\b"),collapse = "|")))
}

find_participles <- function(text_column, type = c("ing", "ed")){

  if(type == "ing" | type == "ed"){
    pattern <- paste0("\\b.*",type,"\\b")
  }else{
    stop("type argument must be ing or ed!")
  }
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = pattern))
}

find_detect <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word))
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0(paste0(word_pattern),collapse = "|")))
}


datatable_header_ui <- function(datatable_id){
  res <- tagList(
    tags$style(type = "text/css",".noUi-connect {background: #39a642;}"), #lacivert 002749
    tags$style(HTML(paste0("#", datatable_id, ' table.dataTable tbody tr.selected>* {box-shadow: inset 0 0 0 9999px #C6E0B4; color: black;}'))),
    tags$head(tags$style(paste0("#", datatable_id," thead th{background-color: #002749; color: white;}"))),
    #tags$head(tags$style("#responses_table tbody td {border-top: 0.1px solid #002749;border-left: 0.1px solid #002749;border-right: 0.1px solid #002749;}")),
    tags$head(tags$style("#", datatable_id, " .dataTables_length {float:right;}"))
  )
  return(res)
}

data(transcripts_atla)

df <- transcripts_atla %>%
  mutate(
    character = factor(character),
    book = fct_reorder(paste0(book_num,". ",  book), book_num),
    chapter = fct_reorder(paste0(book_num,".",chapter_num,". ",  chapter), chapter_num)
  ) %>%
  filter(!is.na(character_words)) %>%
  select(book, chapter, character, character_words) #"full_text", "character_words"))
js_target_index <- which(names(df) == "character_words")


ui <- page_sidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
    ), h4("ATLA Transcripts",style="text-align:center")
    ), # Sidebar başlığı
    width = 300, # Sidebar genişliği (isteğe bağlı)
    open = "desktop", # Masaüstünde varsayılan olarak açık, mobilde kapalı

    hr(),

    selectInput("type_process", label = "Type", choices = c("Pattern", "Participles", "Detect"), selected = "Pattern"),

    uiOutput("type_process_select"),

    hr(), # Yatay çizgi

    # Ek bilgi veya butonlar
    p("Water, Earth, Fire, Air and English!"),
    actionButton("reset_button", "Reset", icon = icon("rotate"), class = "btn-secondary"),
    tags$img(
      src = "iroh.png",
      alt = "Uygulama Logosu" # Resim yüklenmezse gösterilecek alternatif metin
    )
  ),

  # Ana panel içeriği
  card(
    card_header(HTML(
      '<div style="display: flex; justify-content: space-between; align-items: center;">',
      '  <span>Transcripts</span>', # Sola hizalanacak metin
      '  <a href="https://translate.google.com/?sl=en&tl=tr&op=translate" target="blank_">Google Translate</a>', # Sağa hizalanacak hyperlink
      '</div>'
    )),
    dataTableOutput("dt_transcript"),
    datatable_header_ui("dt_transcript"),
    card_footer("Only the Avatar can save your English skills!")
  )
)

server <- function(input, output, session) {

  rvList <- reactiveValues()

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
    }else{
      choices_temp <- c("ing", "ed")
      create_temp <- FALSE
      label_temp <- "Find participles"
      multiple_temp <- FALSE
      updateSelectizeInput(session, "eng_word", choices = c("ing", "ed"), selected = "ing")
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


  # Reactive Data
  temp <- reactive({

    input$type_process
    input$eng_word

    if(length(input$eng_word) > 0){

      if(input$type_process == "Pattern"){
        temp <- df %>% filter(find_structure(character_words, word = input$eng_word))
      }else if(input$type_process == "Detect"){
        temp <- df %>% filter(find_detect(character_words, word = input$eng_word))
      }else if(input$type_process == "Participles"){
        temp <- df %>% filter(find_participles(character_words, type = input$eng_word)) %>% suppressWarnings()
      }

    }else{
      temp <- df
    }
    return(temp)
  })

  # Datatable
  output$dt_transcript <- renderDataTable({

    highlight_terms <- input$eng_word[input$eng_word != "" & !is.na(input$eng_word)]

    if (!is.null(highlight_terms) && length(highlight_terms) > 0) {
      if (input$type_process == "Pattern") {
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
          gsub("([.\\+*?\\[\\^\\]$(){}\\|/:!<>=-])", "\\\\\\1", tolower(term), perl = TRUE)
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
      tryCatch({temp()},error=function(e){NULL}),
      colnames = c("Book", "Chapter", "Character", "Text"),
      selection = "none", filter = "top",
      escape = FALSE, # HTML etiketlerinin yorumlanmasını sağlar
      options = list(
        # "Sentence" sütununu hedeflemek için columnDefs kullanıyoruz.
        # index = 1 (R'da 2. sütun, JS'de 1. sütun)
        columnDefs = list(
          render_js
        )
      ),
      callback = JS(
        "table.on('dblclick', 'td', function() {",
        "  var cell = $(this);",
        "  var character_words = cell.text();",
        "  var encodedText = encodeURIComponent(character_words);",
        "  var googleTranslateUrl = 'https://translate.google.com/?sl=en&tl=tr&text=' + encodedText;", # <<-- Sorun burada olabilir
        "  window.open(googleTranslateUrl, '_blank');",
        "  return false;",
        "});"
      )
    )
  })
}

shinyApp(ui, server)
