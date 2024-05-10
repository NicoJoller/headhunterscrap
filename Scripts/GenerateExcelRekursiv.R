library(rvest)
library(dplyr)
library(openxlsx)
library(purrr)
library(httr)  # Sicherstellen, dass httr auch geladen ist

# Hilfsfunktion zur Extraktion der Basis-URL
extract_base_url <- function(url) {
  domain_info <- parse_url(url)
  paste0(domain_info$scheme, "://", domain_info$hostname)
}

# Funktion zum rekursiven Extrahieren von Text und Links von einer URL
extract_text_recursively <- function(url, base_url, depth = 2, max_links = 10) {
  if (depth == 0) return(list(text = "", links = character()))
  
  Sys.sleep(0.2)  # Kurze Pause
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Fehler beim Öffnen der Seite ", url, ": ", conditionMessage(e))
    return(list(text = "", links = character()))
  }, warning = function(w) {
    message("Warnung beim Öffnen der Seite ", url, ": ", conditionMessage(w))
    return(list(text = "", links = character()))
  })
  
  if (is.null(webpage) || length(webpage) == 0) return(list(text = "", links = character()))
  
  # Text der Hauptseite extrahieren
  main_text <- tryCatch({
    html_text(webpage, trim = TRUE)
  }, error = function(e) {
    message("Fehler beim Lesen des Texts von ", url, ": ", conditionMessage(e))
    return("")
  })
  
  # Extrahiere und bereinige Links sicher
  links <- tryCatch({
    html_attr(html_nodes(webpage, "a"), "href")
  }, error = function(e) {
    message("Fehler beim Extrahieren von Links von ", url, ": ", conditionMessage(e))
    return(character())
  })
  
  links <- na.omit(links)
  links <- unique(links)
  links <- links[grep("^https?://", links)]
  links <- map_chr(links, ~ifelse(startsWith(.x, base_url), .x, url_absolute(.x, url)))
  links <- links[grepl(base_url, links)]  # Filtere Links, die zur gleichen Basis-URL gehören
  
  # Beschränke die Anzahl der zu verfolgenden Links
  if (length(links) > max_links) {
    set.seed(123)  # Für Reproduzierbarkeit
    links <- sample(links, max_links)
  }
  
  # Rekursive Verarbeitung von Links
  recursive_data <- if (depth > 1) {
    map_df(links, ~extract_text_recursively(.x, base_url, depth - 1, max_links))
  } else {
    data.frame(text = character(), links = character())
  }
  
  # Zusammenführen der Texte
  full_text <- paste(main_text, recursive_data$text, collapse = " ")
  return(list(text = full_text, links = links))
}

# Liste der URLs der Firmen-Homepages
urls <- c(
  "https://www.abena.li/",
  "https://www.accurity.ch/",
  "https://addexpert.ch/",
  "https://www.albedis.com/",
  "https://www.almoag.ch/",
  "https://alprojob.ch/",
  "https://www.amstad-personal.ch/",
  "https://www.aris-personal.ch/",
  "https://www.art-of-work.ch/",
  "https://asanti.ch/",
  "https://asspro.ch/",
  "https://www.avalect.ch/",
  "https://www.axia.ch/",
  "https://www.axosag.ch/",
  "https://www.beenux.com/",
  "https://boxtop.ch/",
  "https://bromsolutions.com/",
  "https://canaj-recruiting.ch/",
  "https://clickjob.ch/",
  "https://www.coni-partner.com/de/",
  "https://consultandpepper.com/",
  "https://cyberunity.io/",
  "https://www.deligo.ch/",
  "https://www.expresspersonal.ch/",
  "https://flexsuisse.com/de/",
  "https://www.frankgroup.com/de/",
  "https://www.freestar.ch/home.html",
  "https://www.gassmann-recruiting.ch/",
  "https://www.gbnag.ch/",
  "https://www.rueegsegger-consulting.ch/",
  "https://www.homberger-ag.ch/",
  "https://huk-ag.ch/",
  "https://humanpro.ch/",
  "https://www.impact-recruitment.eu/",
  "https://insearch.swiss/",
  "https://www.inside-personal.ch/",
  "https://www.instapeople.ch/",
  "https://brettirwin.ch/",
  "https://www.isg.com/de/standort/schweiz/",
  "https://www.workfinder.ch/",
  "https://www.jobimpuls.ch/",
  "https://www.jobtalente.ch/",
  "https://jokerpersonal.ch/",
  "https://joerg-lienert.ch/",
  "https://www.kellyservices.ch/de",
  "https://www.kesslervogler.ch/",
  "https://lagea.ch/",
  "https://www.lavoris.ch/",
  "https://manexperts.com/",
  "https://www.manpower.ch/de",
  "https://www.markritter.ch/",
  "https://medicareers.ch/",
  "https://medici-sprecher.ch/home",
  "https://mosaikconsulting.de/",
  "https://www.myitjob.ch/",
  "https://www.nemensis.com/",
  "https://www.nexus.ch/",
  "https://www.oliverjames.de/",
  "https://www.oneagency.ch/",
  "https://onyx-circle.com/",
  "https://www.parcon.ch/",
  "https://www.peopal.ch/",
  "https://www.peopleconcept.ch/",
  "https://www.personal-sigma.ch/",
  "https://portfolia.ch/",
  "https://www.prime21.ch/",
  "https://www.proacademics.ch/",
  "https://www.propers.ch/",
  "https://rentaperson.ch/",
  "https://www.robertwalters.ch/",
  "https://scherler4.ch/",
  "https://skybris.ch/",
  "https://www.smartheads.ch/",
  "https://smiti.ch/",
  "https://sommeritconsulting.ch/",
  "https://www.sourcegroupinternational.com/de/",
  "https://www.summitag.ch/",
  "https://www.swisscarecompany.ch/",
  "https://www.swisselect.ch/",
  "https://swisspromed.ch/",
  "https://techsearch.ch/",
  "https://www.tempox.ch/",
  "https://www.thoma-partner.ch/de/",
  "https://biber-associates.com/",
  "https://www.timejob.ch/",
  "https://www.twincap.ch/",
  "https://universaljob.ch/",
  "https://www.ur-capital.com/en/",
  "https://vistoria.ch/",
  "https://www.workmanagement.ch/",
  "https://www.workselection.com/"
)

# Keywords, nach denen gesucht wird
keywords <- c("Mandat", "Erfolgsbasis", "Festvermittlung", "Personalverleih", "Executive Search", "Headhunting", "Assessment", "Temporär")

# Ergebnisse sammeln
results <- data.frame(Firma = character(), Keyword = character(), Präsenz = logical(), stringsAsFactors = FALSE)

# Durchlaufe jede URL und überprüfe das Vorhandensein von Keywords
for (url in urls) {
  base_url <- extract_base_url(url)  # Basis-URL extrahieren
  data <- extract_text_recursively(url, base_url, depth = 2, max_links = 10)  # Tiefe der Rekursion und maximale Links definieren
  for (keyword in keywords) {
    presence <- grepl(keyword, data$text, ignore.case = TRUE)  # Überprüfe das Vorhandensein des Keywords im Text
    results <- rbind(results, data.frame(Firma = url, Keyword = keyword, Präsenz = presence))
  }
}

# Excel-Datei erstellen
wb <- createWorkbook()
addWorksheet(wb, "Keyword Analysis")
writeData(wb, "Keyword Analysis", results)

# Speichern der Excel-Datei
saveWorkbook(wb, "Keyword_Analysis.xlsx", overwrite = TRUE)

print("Die Excel-Datei wurde erstellt und gespeichert.")
