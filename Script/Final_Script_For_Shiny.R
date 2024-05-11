library(rvest)
library(dplyr)
library(openxlsx)
library(purrr)
library(httr)
library(parallel)
library(stringr)

# Hilfsfunktion zur Extraktion der Basis-URL
extract_base_url <- function(url) {
  domain_info <- httr::parse_url(url)
  paste0(domain_info$scheme, "://", domain_info$hostname)
}

# Funktion zum rekursiven Extrahieren von Text und Links von einer URL
extract_text_recursively <- function(url, depth = 2, max_links = 10) {
  base_url <- extract_base_url(url)
  
  if (depth == 0) return(data.frame(URL = url, Keyword = NA, TextSnippet = NA))
  
  Sys.sleep(runif(1, 0.1, 0.5))
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Fehler beim Öffnen der Seite ", url, ": ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(webpage)) return(data.frame(URL = url, Keyword = NA, TextSnippet = NA))
  
  main_text <- tryCatch({
    html_text(webpage, trim = TRUE)
  }, error = function(e) {
    message("Fehler beim Lesen des Texts von ", url, ": ", conditionMessage(e))
    return("")
  })
  
  # Beispiel für Keywords
  keywords <- c("Mandat", "Erfolgsbasis", "Festvermittlung", "Personalverleih", "Executive Search", "Headhunting", "Assessment", "Temporär")
  
  # Finde Keywords im Text und extrahiere den Kontext
  results <- lapply(keywords, function(kw) {
    if (grepl(kw, main_text, ignore.case = TRUE)) {
      snippets <- str_extract_all(main_text, regex(paste0("\\b.{0,30}", kw, ".{0,30}\\b"), ignore_case = TRUE))[[1]]
      data.frame(URL = url, Keyword = kw, TextSnippet = snippets)
    } else {
      return(data.frame(URL = url, Keyword = kw, TextSnippet = NA))
    }
  }) %>% bind_rows()
  
  return(results)
}

# Cluster-Konfiguration und Ausführung des parallelen Prozesses
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(rvest)
  library(dplyr)
  library(purrr)
  library(httr)
  library(stringr)
})
clusterExport(cl, list("extract_text_recursively", "extract_base_url", "read_html", "html_text", "str_extract_all", "regex", "grepl", "Sys.sleep"))

# URLs definieren und Verarbeitung durchführen
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

results <- parLapply(cl, urls, extract_text_recursively)
stopCluster(cl)

# Ergebnisse verarbeiten und in Excel speichern
results_df <- do.call(rbind, results)
wb <- createWorkbook()
addWorksheet(wb, "Results")
writeData(wb, "Results", results_df)
saveWorkbook(wb, "Keyword_Results.xlsx", overwrite = TRUE)
message("Die Excel-Datei wurde erstellt und gespeichert.")
