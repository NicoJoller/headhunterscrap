# Laden der benötigten Bibliotheken
library(rvest)

# Funktion zum Durchsuchen einer Seite nach dem Keyword
search_keyword_on_page <- function(url, keyword) {
  tryCatch({
    # Die Webseite herunterladen und analysieren
    webpage <- read_html(url)
    
    # Die gesamte Textinhalt der Seite abrufen
    page_text <- html_text(webpage)
    
    # Überprüfen, ob das gesuchte Wort auf der Seite vorhanden ist
    keyword_found <- sum(gregexpr(keyword, page_text)[[1]] >= 0) > 0
    
    if (keyword_found) {
      print(paste("Das Wort", keyword, "wurde auf der Seite", url, "gefunden."))
      return(TRUE)
    } else {
      print(paste("Das Wort", keyword, "wurde nicht auf der Seite", url, "gefunden."))
      return(FALSE)
    }
  }, error = function(e) {
    print(paste("Fehler beim Öffnen der Seite", url, ":", e$message))
    return(FALSE)
  })
}

# Funktion zum Durchsuchen aller Unterseiten einer Webseite
search_keyword_on_all_pages <- function(url, keyword) {
  # Die Webseite herunterladen und analysieren
  webpage <- read_html(url)
  
  # Alle Links auf der Seite finden
  links <- html_attr(html_nodes(webpage, "a"), "href")
  
  # Vollständige Links erstellen
  full_links <- ifelse(startsWith(links, "http"), links, paste0(url, links))
  
  # Variable zum Speichern des Status des Keywords auf der Startseite und allen Unterseiten
  keyword_status <- search_keyword_on_page(url, keyword)
  
  # Alle Links durchgehen und nach dem Keyword suchen
  for (link in full_links) {
    keyword_found <- search_keyword_on_page(link, keyword)
    keyword_status <- c(keyword_status, keyword_found)
    Sys.sleep(0.02)  # Pause von 0,02 Sekunden einfügen
  }
  
  # Zusammenfassung generieren
  if(all(keyword_status)) {
    print(paste("Das Keyword", keyword, "wurde auf der Seite", url, "und allen Unterseiten gefunden."))
  } else {
    print(paste("Das Keyword", keyword, "wurde auf der Seite", url, "oder allen Unterseiten nicht gefunden."))
  }
}

# URL der Webseite, die Sie durchsuchen möchten
url <- "https://jobimpuls.ch/"

# Das gewünschte Wort, nach dem Sie suchen möchten
keyword <- "Vermittlung"

# Durchsuchen aller Unterseiten der Webseite
search_keyword_on_all_pages(url, keyword)
