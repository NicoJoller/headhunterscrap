library(rvest)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)

# Funktion zum Extrahieren des Textes und der Links von einer URL mit Fehlerbehandlung und Verzögerung
extract_text_and_links <- function(url) {
  Sys.sleep(0.2)  # Kurze Pause, um die Serverlast zu minimieren
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
    links <- purrr::map_chr(links, ~ ifelse(startsWith(.x, "http"), .x, paste0(url, .x)))  # Relative Links umwandeln
    return(list(text = text, links = links))
  } else {
    return(NULL)
  }
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
  data <- extract_text_and_links(url)
  if (!is.null(data)) {
    text <- data$text
    for (keyword in keywords) {
      presence <- grepl(keyword, text, ignore.case = TRUE)
      results <- rbind(results, data.frame(Firma = url, Keyword = keyword, Präsenz = presence))
    }
  }
}

# Konvertieren der logischen Werte in Zeichenketten "Found" und "Not Found"
results$Präsenz <- ifelse(results$Präsenz, "Found", "Not Found")

# Shiny UI
ui <- fluidPage(
  titlePanel("Übersicht der Keyword-Präsenz auf Firmen-Homepages"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedKeyword", "Wählen Sie ein Keyword:", choices = unique(results$Keyword)),
      actionButton("searchButton", "Suchen")
    ),
    mainPanel(
      tableOutput("resultsTable")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Reaktive Funktion, die URLs basierend auf ausgewähltem Keyword und Buttonklick aktualisiert
  observeEvent(input$searchButton, {
    keyword_selected <- input$selectedKeyword
    
    # Filtert Ergebnisse basierend auf dem ausgewählten Keyword
    filtered_urls <- results %>%
      filter(Keyword == keyword_selected, Präsenz == "Found") %>%
      distinct(Firma) %>%
      arrange(Firma)  # Sortiert die URLs
    
    # Anzeigen der URLs in einer Tabelle
    output$resultsTable <- renderTable({
      filtered_urls
    }, rownames = TRUE)
  })
}

# App starten
shinyApp(ui = ui, server = server)