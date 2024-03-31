library(tidyverse)
# Laden der benötigten Bibliotheken
library(rvest)

# URL der Webseite, die Sie durchsuchen möchten
url <- "https://jobimpuls.ch"

# Die Webseite herunterladen und analysieren
webpage <- read_html(url)

# Das gewünschte Wort, nach dem Sie suchen möchten
keyword <- "Vermittlung"

# Die gesamte Textinhalt der Seite abrufen
page_text <- html_text(webpage)

# Überprüfen, ob das gesuchte Wort auf der Seite vorhanden ist
matches <- gregexpr(keyword, page_text)
num_matches <- sum(matches[[1]] >= 0)

if (num_matches > 0) {
  print(paste("Das Wort", keyword, "wurde auf der Seite gefunden."))
  print(paste("Es wurde insgesamt", num_matches, "Mal gefunden."))
} else {
  print(paste("Das Wort", keyword, "wurde nicht auf der Seite gefunden."))
}


