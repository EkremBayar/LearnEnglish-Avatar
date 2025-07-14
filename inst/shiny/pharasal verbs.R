library(pdftools)
library(dplyr)
library(stringr)

# PDF dosyanızın yolu
pdf_file_path <- "phrasal.pdf" # Bu kısmı kendi dosyanızın yolu ile değiştirin

# PDF'den metni sayfalar halinde çıkarın
text_from_pdf <- pdf_text(pdf_file_path)

# Metni tek bir dizeye birleştirin veya sayfalar arasında yineleyin
full_text <- paste(text_from_pdf, collapse = "\n")

# Şimdi 'full_text' üzerinde desen tanıma ve ayrıştırma yapmanız gerekecek.
# "Phrasal Verb", "Meaning", "Example" gibi kalıpları kullanarak satırları veya bölümleri ayırabilirsiniz.
# Bu adım, PDF'in tam formatına bağlı olarak karmaşık olabilir.

# Örnek olarak, satır satır ayırma ve sonra bunları bir veri çerçevesine dönüştürme:
lines <- str_split(full_text, "\\n")[[1]]

# Daha sonra bu 'lines' vektörünü kendi "Phrasal Verb", "Meaning", "Example"
# sütunlarınıza göre ayrıştırmanız gerekecektir.
# Bu genellikle düzenli ifadeler (regex) kullanmayı içerir.
# Örneğin, her satırın formatına bağlı olarak:
# df_phrasal_verbs_manual <- data.frame(
#   PhrasalVerb = character(),
#   Meaning = character(),
#   Example = character(),
#   stringsAsFactors = FALSE
# )
# for (line in lines) {
#   # Her satırı ayrıştırmak için karmaşık regex desenleri gerekebilir.
#   # Bu kısım, PDF'inizdeki gerçek veri formatına göre özelleştirilmelidir.
#   # Örneğin, virgülle ayrılmış bir CSV benzeri yapıysa:
#   # parts <- str_split(line, ",")[[1]]
#   # if (length(parts) == 3) {
#   #   df_phrasal_verbs_manual <- rbind(df_phrasal_verbs_manual, data.frame(PhrasalVerb = parts[1], Meaning = parts[2], Example = parts[3]))
#   # }
# }

print("pdftools ile metin çıkarıldı. Şimdi manuel ayrıştırma yapmanız gerekiyor.")
print(head(lines)) # İlk birkaç satırı görüntüleyin


a <- lines[1:length(lines)]

head(a)

str_count(a[2])

a2 <- data.frame(
  PV = a
) %>%
  mutate(
    Example = sapply(PV, function(i){str_sub(i, 59, str_count(i))}),
    PV = sapply(PV, function(i){str_sub(i, 1, 58)}),
    Meaning = sapply(PV, function(i){str_sub(i, 13)}),
    PV = sapply(PV, function(i){str_sub(i, 1, 12)})
  ) %>% mutate_all(str_squish) %>%
  mutate(PV = ifelse(PV == "", NA_character_, PV)) %>%
  fill(PV, .direction = "down") %>%
  mutate(PV = ifelse(PV == "Verb", NA_character_, PV)) %>%
  fill(PV, .direction = "up") %>%
  group_by(PV) %>%
  summarise(
    Meaning = paste0(Meaning, collapse = " "),
    Example = paste0(Example, collapse = " ")
  ) %>%
  ungroup() %>%
  mutate(
    PV = sapply(PV, function(i){ifelse(str_count(str_squish(str_sub(i, -2)))==1,str_sub(i, 1, (str_count(i)-2)) ,i)})
  ) %>% mutate_all(str_squish) %>%
  filter(Meaning != "", str_count(PV) > 1) %>%
  mutate(
    PV = sapply(PV, function(i){ifelse(str_count(str_squish(str_sub(i, -2)))==1,str_sub(i, 1, (str_count(i)-2)) ,i)})
  ) %>% mutate_all(str_squish) %>%
  filter(Meaning != "", str_count(PV) > 1) %>%
  select(PV) %>%
  distinct()

a2 <- a2[4:nrow(a2),]
a2 <- a2[1:2143,]

View(a2)

phrasal_verbs <- a2


a2[5:nrow(a2),] %>% View


str_sub(a2[1,], 14)
str_sub(a2[6,], 13)




x <- data.frame(
  Pharasal = sapply(a, function(i){str_sub(i, 1, 58)}),
  Example = sapply(a, function(i){str_sub(i, 59, str_count(i))}),
)









