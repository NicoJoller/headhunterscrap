library(shiny)
library(DT)
library(openxlsx)
library(dplyr)

# UI-Definition
ui <- fluidPage(
  titlePanel("Analyseergebnisse anzeigen"),
  sidebarLayout(
    sidebarPanel(
      selectInput("keyword", "Wähle ein Keyword:", choices = NULL),
      checkboxInput("showOnlyYes", "Nur 'Ja' anzeigen", value = FALSE),
      downloadButton("downloadData", "Daten herunterladen")
    ),
    mainPanel(
      DTOutput("resultsTable")
    )
  )
)

# Server-Logik
server <- function(input, output, session) {
  # Daten beim Start der App laden
  data <- reactive({
    df <- read.xlsx("Keyword_Results.xlsx", sheet = 1) %>%
      mutate(
        URL = as.character(URL),
        Keyword = as.character(Keyword),
        Presence = !is.na(TextSnippet)  # Direkt true/false basierend auf der TextSnippet-Spalte
      ) %>%
      group_by(URL, Keyword) %>%
      summarise(Presence = any(Presence), .groups = 'drop')  # Aggregieren, um Duplikate zu entfernen
    df
  })
  
  # Fülle die Dropdown-Liste mit einzigartigen Keywords, sobald Daten geladen sind
  observe({
    df <- data()
    updateSelectInput(session, "keyword", choices = unique(df$Keyword))
  })
  
  # Reaktive Datenquelle für die Tabelle basierend auf der Keyword-Auswahl
  filteredData <- reactive({
    req(input$keyword)  # Stellt sicher, dass ein Keyword ausgewählt wurde
    df <- data() %>%
      filter(Keyword == input$keyword)
    if (input$showOnlyYes) {
      df <- df %>% filter(Presence == TRUE)
    }
    df
  })
  
  # Rendere die DataTable
  output$resultsTable <- renderDT({
    datatable(filteredData(), 
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                rowCallback = JS(
                  "function(row, data, dataIndex) {",
                  "  if (data[2] === true) {",
                  "    $(row).addClass('yes');",
                  "  }",
                  "}"
                )
              ), 
              class = "display"
    )
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
