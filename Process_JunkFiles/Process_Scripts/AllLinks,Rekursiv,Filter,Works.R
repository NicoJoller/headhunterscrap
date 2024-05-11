# Auch die Unterseiten, die auf der Startseite verlinkt sind, werden heruntergeladen

library(rvest)
library(tidyverse)

# Aktualisierte Funktion zum Extrahieren des Textes und der Links von einer URL mit Fehlerbehandlung und Verzögerung
extract_text_and_links <- function(url) {
  Sys.sleep(0.2)  # Pause von 0,2 Sekunden einfügen
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Fehler beim Öffnen der Seite ", url, ": ", conditionMessage(e))
    return(NULL)
  })
  
  if (!is.null(webpage)) {
    # Extrahiere den Text der Hauptseite
    text <- html_text(webpage)
    
    # Extrahiere die Links der Hauptseite
    links <- html_attr(html_nodes(webpage, "a"), "href")
    
    # Filtere nur HTTP/HTTPS-Links
    links <- links[grep("^https?://", links)]
    
    # Vollständige URLs für relative Links erstellen
    links <- purrr::map_chr(links, ~ ifelse(startsWith(.x, "http"), .x, paste0(url, .x)))
    
    # Rückgabe von Text und Links
    return(list(text = text, links = links))
  } else {
    return(NULL)
  }
}
# Funktion zur Filterung relevanter Links basierend auf einer ähnlichen Pfadstruktur
filter_relevant_links <- function(links, parent_url) {
  relevant_links <- character(0)
  for (link in links) {
    # Prüfe, ob der Link eine ähnliche Pfadstruktur wie die Hauptseite hat
    if (startsWith(link, parent_url)) {
      relevant_links <- c(relevant_links, link)
    }
  }
  return(relevant_links)
}
# Liste der URLs der Firmen-Homepages
urls <- c("https://www.jobimpuls.ch", "https://www.art-of-work.ch", 
          "https://www.abena.li/", "https://www.accurity.ch/", 
          "https://addexpert.ch/", "https://www.albedis.com/", 
          "https://www.almoag.ch/", "https://www.manpower.ch/", "https://www.prime21.ch")

# Keywords, nach denen gesucht werden soll
keywords <- c("Mandat", "Erfolgsbasis", "Festvermittlung", 
              "Personalverleih", "Executive Search", "Headhunting", 
              "Assessment", "Temporär")

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
}

# Ersetzen von logischen Werten durch "Found" und "Not Found"
results$Präsenz <- ifelse(results$Präsenz, "Found", "Not Found")

# Visualisierung der Ergebnisse
ggplot(results, aes(x = Keyword, fill = Präsenz)) +
  geom_bar() +
  facet_wrap(~Firma, scales = "free") +
  scale_fill_manual(values = c("Found" = "green", "Not Found" = "red"), guide = "none") +
  labs(title = "Präsenz von Keywords auf Firmen-Homepages", x = "Keyword", y = "Anzahl")
