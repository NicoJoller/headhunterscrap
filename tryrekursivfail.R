library(rvest)

# Funktion zum Durchsuchen einer Seite nach dem Keyword
search_keyword_on_page <- function(url, keyword) {
  # Die Webseite herunterladen und analysieren
  webpage <- read_html(url)
  
  # Die gesamte Textinhalt der Seite abrufen
  page_text <- html_text(webpage)
  
  # Überprüfen, ob das gesuchte Wort auf der Seite vorhanden ist
  matches <- gregexpr(keyword, page_text)
  num_matches <- sum(matches[[1]] >= 0)
  
  if (num_matches > 0) {
    print(paste("Das Wort", keyword, "wurde auf der Seite", url, "gefunden."))
    print(paste("Es wurde insgesamt", num_matches, "Mal gefunden."))
  } else {
    print(paste("Das Wort", keyword, "wurde nicht auf der Seite", url, "gefunden."))
  }
}

# Funktion zum Durchsuchen aller Unterseiten einer Webseite
search_keyword_on_all_pages <- function(url, keyword, searched_links = character()) {
  # Überprüfen, ob der Link bereits durchsucht wurde
  if (url %in% searched_links) {
    return(invisible())
  }
  
  # Die Webseite herunterladen und analysieren
  webpage <- read_html(url)
  
  # Alle Links auf der Seite finden
  links <- html_attr(html_nodes(webpage, "a"), "href")
  
  # Nur gültige HTTP- oder HTTPS-Links berücksichtigen
  valid_links <- grep("^https?://", links, value = TRUE, ignore.case = TRUE)
  
  # Vollständige Links erstellen und nur gültige Links berücksichtigen
  full_links <- unique(url_absolute(valid_links, base = url))
  
  # Alle Links durchgehen und nach dem Keyword suchen
  for (link in full_links) {
    print(paste("Durchsuche:", link))
    search_keyword_on_page(link, keyword)
    # Rekursiv alle Unterseiten durchsuchen
    search_keyword_on_all_pages(link, keyword, c(searched_links, url))
    Sys.sleep(0.25)  # Pause von 0,25 Sekunden einfügen
  }
}

# URLs der Webseiten, die Sie durchsuchen möchten
urls <- c("https://jobimpuls.ch/", "https://www.abena.li/", "https://www.accurity.ch/", "https://addexpert.ch/", "https://www.albedis.com/", "https://www.almoag.ch/")

# Das gewünschte Wort, nach dem Sie suchen möchten
keyword <- "Vermittlung"

# Durchsuchen aller Unterseiten der Webseiten
for (url in urls) {
  search_keyword_on_all_pages(url, keyword)
}
