library(rvest)
library(tidyverse)

# Funktion zum Extrahieren des Textes von einer URL mit Fehlerbehandlung und Verzögerung
extract_text <- function(url) {
  Sys.sleep(0.2)  # Pause von 0,2 Sekunden einfügen
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Fehler beim Öffnen der Seite ", url, ": ", conditionMessage(e))
    return(NULL)
  })
  
  if (!is.null(webpage)) {
    text <- html_text(webpage)
    return(text)
  } else {
    return(NULL)
  }
}

# Liste der URLs der Firmen-Homepages
urls <- c("https://www.jobimpuls.ch", "https://www.art-of-work.ch")

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
  if (!is.null(text)) {
    for (keyword in keywords) {
      presence <- grepl(keyword, text, ignore.case = TRUE) # Überprüfe das Vorhandensein des Keywords
      results <- rbind(results, data.frame(Firma = url, Keyword = keyword, Präsenz = presence))
    }
  }
}

# Ersetzen von logischen Werten durch "Found" und "Not Found"
results$Präsenz <- ifelse(results$Präsenz, "Found", "Not Found")

# Visualisierung der Ergebnisse
ggplot(results, aes(x = Keyword, fill = Präsenz)) +
  geom_bar() +
  facet_wrap(~Firma, scales = "free") +
  scale_fill_manual(values = c("Found" = "green", "Not Found" = "red"), guide = "none") +
  labs(title = "Präsenz von Keywords auf Firmen-Homepages", x = "Keyword", y = "Anzahl")
