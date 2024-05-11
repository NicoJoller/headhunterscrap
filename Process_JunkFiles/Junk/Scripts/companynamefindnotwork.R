# Lade benötigte Bibliotheken
library(httr)
library(jsonlite)

# Google Places API Details
api_key <- "AIzaSyC0H7QwGV2hmavab0qzLpLhJyigbSYdJjc"
base_url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"

# Funktion zum Suchen der Homepage-URL einer Firma mit der Google Places API
get_company_website <- function(company_name) {
  # Erstelle die Anfrage-URL
  query_url <- modify_url(base_url, query = list(
    key = api_key,
    input = company_name,
    inputtype = "textquery",
    fields = "website"
  ))
  
  # Führe die Anfrage durch
  response <- GET(query_url)
  
  # Parse die Antwort
  if (http_status(response)$category == "Success") {
    result <- content(response, "parsed")
    if (result$status == "OK" && !is.null(result$candidates) && length(result$candidates) > 0) {
      website <- result$candidates[[1]]$website
      return(website)
    }
  }
  
  return(NULL)
}

# Liste von Firmennamen
company_names <- c("ABENA Personalmanagement Anstalt",
                   "Accurity GmbH",
                   "addexpert GmbH",
                   "AdPro Consulting AG",
                   "Albedis SA",
                   "Almojob GmbH",
                   "Alprojob Litscher",
                   "Amstad Personal",
                   "ARIS Personalberatung GmbH",
                   "Art of Work Personalberatung AG",
                   "Asanti AG",
                   "ASSpro AG",
                   "Avalect HR-Executive Consulting",
                   "Axia AG",
                   "Beenux AG",
                   "Boxtop AG",
                   "BROMsolutions AG",
                   "Canaj Recruiting",
                   "Clickjob Meyer AG",
                   "Coni + Partner AG",
                   "Consult & Pepper AG",
                   "cyberunity AG",
                   "Deligo DAG AG",
                   "EXPRESS PERSONAL AG",
                   "Flex Suisse AG",
                   "Frank Recruitment Group (FRG)",
                   "Freestar People AG",
                   "Gassmann Consulting",
                   "Guggenbühl & Partner AG",
                   "Haussener Consulting",
                   "Homberger Personalberatung für Kommunikation",
                   "HUK",
                   "Human Professional Personalberatung AG",
                   "Impact Recruitment GmbH",
                   "InSearch AG",
                   "inside Personaldienstleistungs AG",
                   "InstaPeople GmbH",
                   "Irwin AG",
                   "ISG Personalmanagement Schweiz",
                   "IT Recruitment GmbH",
                   "job impuls ag",
                   "Job Talente GmbH",
                   "Joker Personal AG",
                   "Jörg Lienert AG",
                   "Kelly Services",
                   "kessler.vogler gmbh",
                   "Lage A AG",
                   "Lavoris (Schwyz) AG",
                   "Manexperts (Schweiz) GmbH",
                   "Manpower",
                   "Mark Ritter Executive Search & Consulting",
                   "Medicareers GmbH",
                   "Medici & Sprecher AG",
                   "Mosaik Consulting GmbH",
                   "myitjob gmbh",
                   "Nemensis AG",
                   "Nexus",
                   "Oliver James Associates AG",
                   "Oneagency",
                   "Onyx Circle AG",
                   "PARCON PERSONALTREUHAND AG",
                   "Peopal AG",
                   "People Concept",
                   "Personal Sigma AG",
                   "Portfolia GmbH",
                   "Prime 21",
                   "Pro Academics",
                   "ProPers Vermittlungen AG",
                   "Rent a Person AG",
                   "Robert Walters",
                   "Scherler People Management AG",
                   "Skybris GmbH",
                   "smart.heads",
                   "smiti AG",
                   "Sommer IT Consulting GmbH",
                   "Source Group International GmbH",
                   "Summit AG",
                   "Swiss Care Company",
                   "Swisselect aim AG",
                   "SwissPromed",
                   "TechSearch AG",
                   "tempoX Personal AG",
                   "Thoma & Partner Management Consulting AG",
                   "Thomas Biber & Partner Schweiz GmbH",
                   "Timejob AG",
                   "Twincap",
                   "Universal-Job AG",
                   "UR-Capital, Geneva",
                   "Vistoria Group",
                   "Woma Work Management AG",
                   "Work Selection AG"
)

# Suche nach den Homepages der Firmen
for (company_name in company_names) {
  # Suche nach der Homepage der Firma
  website <- get_company_website(company_name)
  
  # Speichere die URL, wenn gefunden
  if (!is.null(website)) {
    cat("Homepage von", company_name, ":", website, "\n")
    # Hier kannst du Code hinzufügen, um die URL zu speichern oder weitere Aktionen auszuführen
  } else {
    cat("Homepage von", company_name, "nicht gefunden.\n")
  }
}
