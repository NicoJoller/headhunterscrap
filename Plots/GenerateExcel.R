# Notwendige Pakete laden und installieren, falls nicht vorhanden
if (!require("rvest")) install.packages("rvest", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)
if (!require("purrr")) install.packages("purrr", dependencies = TRUE)

library(rvest)
library(dplyr)
library(openxlsx)
library(purrr)

# Funktion zum Extrahieren des Textes und der Links von einer URL mit Fehlerbehandlung und Verzögerung
extract_text_and_links <- function(url) {
  Sys.sleep(0.2)  # Kurze Pause zur Schonung des Servers
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Fehler beim Öffnen der Seite ", url, ": ", conditionMessage(e))
    return(NULL)
  })
  
  if (!is.null(webpage)) {
    text <- html_text(webpage)  # Text extrahieren
    links <- html_attr(html_nodes(webpage, "a"), "href")  # Links extrahieren
    links <- links[grep("^https?://", links)]  # Nur HTTP/HTTPS-Links behalten
    links <- purrr::map_chr(links, ~ifelse(startsWith(.x, "http"), .x, paste0(url, .x)))  # Relative Links umwandeln
    return(list(text = text, links = links))
  } else {
    return(NULL)
  }
}

# URLs und Keywords definieren
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
keywords <- c("Mandat", "Erfolgsbasis", "Festvermittlung", 
              "Personalverleih", "Executive Search", "Headhunting", 
              "Assessment", "Temporär")

# Daten sammeln
results <- data.frame(URL = character(), Keyword = character(), Found = logical(), stringsAsFactors = FALSE)

# Überprüfung jeder URL auf Vorhandensein der Keywords
for (url in urls) {
  data <- extract_text_and_links(url)
  if (!is.null(data)) {
    text <- data$text
    for (keyword in keywords) {
      presence <- grepl(keyword, text, ignore.case = TRUE)
      results <- rbind(results, data.frame(URL = url, Keyword = keyword, Found = presence))
    }
  }
}

# Konvertieren der logischen Werte in "Found" und "Not Found"
results$Found <- ifelse(results$Found, "Found", "Not Found")

# Excel-Datei erstellen
wb <- createWorkbook()
addWorksheet(wb, "Keyword Analysis")
writeData(wb, "Keyword Analysis", results)

# Speichern der Excel-Datei
saveWorkbook(wb, "Keyword_Analysis.xlsx", overwrite = TRUE)

print("Die Excel-Datei wurde erstellt und gespeichert.")
