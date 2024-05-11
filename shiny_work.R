library(shiny)
library(DT)        # Für interaktive Tabellen
library(openxlsx)  # Zum Einlesen der Excel-Datei
library(dplyr)     # Für Datenmanipulation

# UI-Definition
ui <- fluidPage(
  titlePanel("Analyseergebnisse anzeigen"),
  sidebarLayout(
    sidebarPanel(
      selectInput("keyword", "Wähle ein Keyword:", choices = NULL), # Wird dynamisch gefüllt
      downloadButton("downloadData", "Daten herunterladen")
    ),
    mainPanel(
      DTOutput("resultsTable")  # Anzeige der Ergebnisse als interaktive Tabelle
    )
  )
)

# Server-Logik
server <- function(input, output, session) {
  # Daten beim Start der App laden
  data <- read.xlsx("Keyword_Results.xlsx", sheet = 1) %>%
    mutate(URL = as.character(URL), Keyword = as.character(Keyword)) %>%
    group_by(URL, Keyword) %>%
    summarise(Presence = any(!is.na(TextSnippet)), .groups = 'drop') %>%
    arrange(URL, Keyword)
  
  # Fülle die Dropdown-Liste mit einzigartigen Keywords
  observe({
    updateSelectInput(session, "keyword",
                      choices = unique(data$Keyword))
  })
  
  # Reaktive Datenquelle für die Tabelle basierend auf der Keyword-Auswahl
  filteredData <- reactive({
    req(input$keyword)  # Stellt sicher, dass ein Keyword ausgewählt wurde
    data %>%
      filter(Keyword == input$keyword) %>%
      select(URL, Presence) %>%
      mutate(Presence = if_else(Presence, "Ja", "Nein"))
  })
  
  # Rendere die DataTable
  output$resultsTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10, scrollX = TRUE), 
              escape = FALSE)
  })
  
  # Ermögliche das Herunterladen der gefilterten Daten
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Ergebnisse-", input$keyword, ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filteredData(), file)
    }
  )
}

# App starten
shinyApp(ui = ui, server = server)
