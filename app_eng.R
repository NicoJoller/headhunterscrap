library(shiny)
library(shinyauthr)
library(DT)
library(openxlsx)
library(dplyr)

# Define user database
user_base <- data.frame(
  user = c("suva"),
  password = c("PDL2024"),  # Security notice: Passwords should be stored encrypted
  stringsAsFactors = FALSE
)

# UI definition
ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),  # Login UI
  uiOutput("app_ui")  # UI for the app that appears after login
)

# Server logic
server <- function(input, output, session) {
  # Authentication logic
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    log_out = reactive({ NULL })
  )
  
  # Protect the app's UI elements
  output$app_ui <- renderUI({
    req(credentials()$user)  # Ensure a user is logged in
    
    fluidPage(
      titlePanel("Display Analysis Results"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selectedUrl", "Select a URL:", choices = NULL),
          selectInput("keyword", "Select a Keyword:", choices = NULL),
          checkboxInput("showOnlyYes", "Show only 'Yes'", value = FALSE),
          downloadButton("downloadData", "Download Data")
        ),
        mainPanel(
          DTOutput("resultsTable")
        )
      )
    )
  })
  
  # Load data when the app starts
  data <- reactive({
    # Add error handling
    tryCatch({
      df <- read.xlsx("Keyword_Results.xlsx", sheet = 1)
      print("File successfully loaded")
      df <- df %>%
        mutate(
          URL = as.character(URL),
          Keyword = as.character(Keyword),
          Presence = !is.na(TextSnippet),
          Link = paste0('<a href="', URL, '" target="_blank">', URL, '</a>')
        ) %>%
        group_by(URL, Keyword) %>%
        summarise(Presence = any(Presence), Link = first(Link), .groups = 'drop')
      print("Data successfully processed")
      return(df)
    }, error = function(e) {
      print(paste("Error loading data:", e))
      return(data.frame())  # Return empty DataFrame if an error occurs
    })
  })
  
  # Initialize dropdown options
  observe({
    req(credentials()$user)  # Ensure a user is logged in
    df <- data()
    print("Data loaded for dropdown")
    url_choices <- unique(c("Alle", df$URL))
    keyword_choices <- unique(c("Alle", df$Keyword))
    updateSelectInput(session, "selectedUrl", choices = url_choices, selected = input$selectedUrl)
    updateSelectInput(session, "keyword", choices = keyword_choices, selected = input$keyword)
  })
  
  # Reactive data source for the table based on dropdown selections
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
  
  # Render the DataTable
  output$resultsTable <- renderDT({
    df <- filteredData()
    datatable(df, escape = FALSE,
              options = list(
                pageLength = nrow(df),  # Set pageLength to the number of rows in df
                scrollX = TRUE
              ),
              class = "display"
    )
  })
  
  # Enable downloading of the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Results-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- filteredData()
      data$Link <- gsub("<a href='(.*?)' target='_blank'>(.*?)</a>", "\\2", data$Link)
      write.xlsx(data, file)
    }
  )
}

# Start the app
shinyApp(ui = ui, server = server)
