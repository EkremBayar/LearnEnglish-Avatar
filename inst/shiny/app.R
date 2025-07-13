library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(LearnEnglishWithAvatar)

df <- transcripts_atla %>%
  select(book, chapter, character, character_words) #"full_text", "character_words"))
js_target_index <- which(names(df) == "character_words")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  textInput("eng_word", "Find word"),
  actionButton("eng_submit", "Submit"),
  dataTableOutput("dt_transcript")
)

server <- function(input, output, session) {

  observeEvent(input$eng_submit, {

  })

  output$dt_transcript <- renderDataTable({

    datatable(
      df %>%
        filter(LearnEnglishWithAvatar::find_structure(character_words, word = input$eng_word)),
      colnames = c("Book", "Chapter", "Character", "Text"),

      escape = FALSE, # HTML etiketlerinin yorumlanmasını sağlar
      options = list(
        # "Sentence" sütununu hedeflemek için columnDefs kullanıyoruz.
        # index = 1 (R'da 2. sütun, JS'de 1. sütun)
        columnDefs = list(
          list(
            targets = js_target_index,
            render = JS(
              # JavaScript kodu her satırı tırnak işaretleri içinde olacak şekilde yazılmıştır.
              # R'da bu satırların her biri geçerli bir stringdir.
              # HTML içindeki çift tırnaklar (") dikkatlice kaçırılmıştır (\").
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              # R'dan gelen değeri burada JavaScript değişkenine atıyoruz.
              # Bu satır, renderDT çalıştırıldığında R tarafından oluşturulur.
              paste0("    var targetWord = '", input$eng_word, "';"),
              "    var wordRegex = new RegExp(targetWord, 'gi');",
              "    var highlightedData = data.replace(wordRegex, '<span class=\"highlight-red\">$&</span>');",

              # Cümleyi ve içindeki kelimeyi eşleştiren daha basit bir regex.
              # Burada 'highlight-red' sınıfını içeren metni arıyoruz.
              "    var sentenceRegex = new RegExp('([^.!?]*<span class=\"highlight-red\">[^<]*?</span>[^.!?]*[.!?]?)', 'gi');",

              "    var finalHtml = highlightedData.replace(sentenceRegex, '<span class=\"highlight-blue\">$&</span>');",

              "    return finalHtml;",
              "  } else {",
              "    return data;",
              "  }",
              "}"
            )



          )
        )



      )
    )




  })



}

shinyApp(ui, server)
