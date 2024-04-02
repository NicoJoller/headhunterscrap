library(httr)
library(tidyverse)

# Funktion zum Extrahieren des Textes von einer URL
extract_text <- function(url) {
  response <- httr::GET(url)
  text <- httr::content(response, "text")
  return(text)
}

# Liste der URLs der Firmen-Homepages
urls <- c("https://jobimpuls.ch", "https://www.art-of-work.ch")

# Keywords, nach denen gesucht werden soll
keywords <- c("Mandat", "Erfolgsbasis", "Executive Search", "Assessment", "Temporär")

# DataFrame für die Ergebnisse
results <- data.frame(Firma = character(),
                      Keyword = character(),
                      Präsenz = logical(),
                      stringsAsFactors = FALSE)

# Durchlaufe jede URL und überprüfe das Vorhandensein von Keywords
for (url in urls) {
  text <- extract_text(url)
  for (keyword in keywords) {
    presence <- grepl(keyword, text, ignore.case = TRUE) # Überprüfe das Vorhandensein des Keywords
    results <- rbind(results, data.frame(Firma = url, Keyword = keyword, Präsenz = presence))
  }
  Sys.sleep(0.5) # Kurze Verzögerung zwischen den Anfragen
}

# Visualisierung der Ergebnisse
ggplot(results, aes(x = Firma, fill = Präsenz)) +
  geom_bar() +
  facet_wrap(~Keyword) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Präsenz von Keywords auf Firmen-Homepages", x = "Firma", y = "Anzahl")
