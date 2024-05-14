library(shiny)
library(shinyauthr)
library(DT)
library(openxlsx)
library(dplyr)

# Benutzerdatenbank definieren
user_base <- data.frame(
  user = c("suva"),
  password = c("PDL2024"),  # Sicherheitshinweis: Passwörter sollten verschlüsselt gespeichert werden
  stringsAsFactors = FALSE
)

# UI-Definition
ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),  # Login UI
  uiOutput("app_ui")  # UI für die App, die nach dem Login erscheint
)

# Server-Logik
server <- function(input, output, session) {
  # Authentifizierungslogik
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    log_out = reactive({ NULL })
  )
  
  # Schützt die UI-Elemente der App
  output$app_ui <- renderUI({
    req(credentials()$user)  # Stellt sicher, dass ein Benutzer eingeloggt ist
    
    fluidPage(
      titlePanel("Analyseergebnisse anzeigen"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selectedUrl", "Wähle eine URL:", choices = NULL),
          selectInput("keyword", "Wähle ein Keyword:", choices = NULL),
          checkboxInput("showOnlyYes", "Nur 'Ja' anzeigen", value = FALSE),
          downloadButton("downloadData", "Daten herunterladen")
        ),
        mainPanel(
          DTOutput("resultsTable")
        )
      )
    )
  })
  
  # Daten beim Start der App laden
  data <- reactive({
    df <- read.xlsx("Keyword_Results.xlsx", sheet = 1) %>%
      mutate(
        URL = as.character(URL),
        Keyword = as.character(Keyword),
        Presence = !is.na(TextSnippet),
        Link = paste0('<a href="', URL, '" target="_blank">', URL, '</a>')
      ) %>%
      group_by(URL, Keyword) %>%
      summarise(Presence = any(Presence), Link = first(Link), .groups = 'drop')
    df
  })
  
  # Initialisiere die Dropdown-Optionen
  observe({
    req(credentials()$user)  # Stellt sicher, dass ein Benutzer eingeloggt ist
    df <- data()
    url_choices <- unique(c("Alle", df$URL))
    keyword_choices <- unique(c("Alle", df$Keyword))
    updateSelectInput(session, "selectedUrl", choices = url_choices, selected = input$selectedUrl)
    updateSelectInput(session, "keyword", choices = keyword_choices, selected = input$keyword)
  })
  
  # Reaktive Datenquelle für die Tabelle basierend auf den Dropdown-Auswahlen
  filteredData <- reactive({
    df <- data()
    if (input$selectedUrl != "Alle") {
      df <- df %>% filter(URL == input$selectedUrl)
    }
    if (input$keyword != "Alle") {
      df <- df %>% filter(Keyword == input$keyword)
    }
    if (input$showOnlyYes) {
      df <- df %>% filter(Presence == TRUE)
    }
    df
  })
  
  # Rendere die DataTable
  output$resultsTable <- renderDT({
    df <- filteredData()
    datatable(df, escape = FALSE,
              options = list(
                pageLength = nrow(df),  # Setzt pageLength auf die Anzahl der Zeilen in df
                scrollX = TRUE
              ),
              class = "display"
    )
  })
  
  # Ermögliche das Herunterladen der gefilterten Daten
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Ergebnisse-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- filteredData()
      data$Link <- gsub("<a href='(.*?)' target='_blank'>(.*?)</a>", "\\2", data$Link)
      write.xlsx(data, file)
    }
  )
}

# App starten
shinyApp(ui = ui, server = server)
