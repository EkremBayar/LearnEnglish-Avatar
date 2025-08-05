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
      src = "united-kingdom.png",#"logo.png",
      alt = "Uygulama Logosu", # Resim yüklenmezse gösterilecek alternatif metin
      style = "max-width: 50%; height: auto; display: block; margin: auto;"
    ), h4("Learn English with transcripts",style="text-align:center")
    ), # Sidebar başlığı
    width = 300, # Sidebar genişliği (isteğe bağlı)
    open = "desktop", # Masaüstünde varsayılan olarak açık, mobilde kapalı

    hr(),

    #selectInput("serie_type", label = "Avatar", choices = c("Avatar: The Last Airbender", "The Legend of Korra", "Lost"), selected = "Avatar: The Last Airbender"),

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
    #p("Water, Earth, Fire, Air and English!"),
    actionButton("reset_button", "Reset", icon = icon("rotate"), class = "btn-secondary"),
    # tags$img(
    #   src = "iroh.png",
    #   alt = "Uygulama Logosu" # Resim yüklenmezse gösterilecek alternatif metin
    # ),
    # pickerInput("page_select", "Pagination", choices = NULL,options = list(
    #   `live-search` = TRUE,                   # Arama kutusu
    #   `live-search-placeholder` = "Search page number..."  # Arama kutusu yer tutucu
    # ))
    numericInput("page_select", "Pagination", value = 1),
    #downloadButton("download_script", "Download", icon = icon("file-excel")),
    actionButton("download_excel", "Download",icon = icon("file-excel")),
    uiOutput("js_download"),
    HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="ekrembayar" data-color="#5F7FFF" data-emoji="🇬🇧"  data-font="Comic" data-text="Support Ekrem" data-outline-color="#000000" data-font-color="#ffffff" data-coffee-color="#FFDD00" ></script>')
  ),

  # Ana panel içeriği
  card(
    card_header(
      div(
        style="display: flex; justify-content: space-between; align-items: center;",
        div(
          style="display:inline-block;",
          div(style="display:inline-block;",selectInput("serie_type", label = "Series", choices = c("Avatar: The Last Airbender", "The Legend of Korra", "Spiderman", "Lost", "Friends", "HIMYM", "Breaking Bad", "The Office"), selected = "Avatar: The Last Airbender")),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_season", label = "Seasons", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_episode", label = "Episodes", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",selectInput("serie_character", label = "Characters", choices = NULL, selected = NULL)),
          div(style="display:inline-block;margin-left:10px;",actionButton("serie_submit", label = "Submit")),
        )


      )),


    dataTableOutput("dt_transcript"),
    datatable_header_ui("dt_transcript"),
    #card_footer("Only the Avatar can save your English skills!")
    card_footer(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        tags$div(
          "Created by ",
          tags$a("Ekrem Bayar", href = "https://www.linkedin.com/in/ekrem-bayar/", target = "blank_")
        ),
        tags$div(
          HTML('<a href="https://translate.google.com/?sl=en&tl=tr&op=translate" target="blank_" style="margin-right:10px">Google Translate</a>'),
          input_dark_mode(id = "dark_mode_toggle", mode = "dark")
        )
      )
    )
  )
)
