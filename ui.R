source("global.R")


ui <- fluidPage(
  # Application title
  titlePanel("Converter"),
  
  # Sidebar with inputs and main panel with table output
  sidebarLayout(
    sidebarPanel(
      # Upload 
      fileInput(
        inputId = "upload",
        label = "Upload a file",
        multiple = FALSE,
        accept = ".xlsx",
        buttonLabel = "Browse...",
        placeholder = "No file selected..."
      ),
      
      # Type selection
      selectizeInput(
        inputId = "type",
        label = "Type",
        choices = c("MT103", "MT109", "MT540"),
        selected = NULL,
        multiple = FALSE,
        options = list(placeholder = "Select a type")
      ),
      actionButton(
        inputId = "run",
        label = "Run!",
        icon = icon(name = "glyphicon glyphicon-play", lib = "glyphicon")
      ),
      textOutput("result"),
      textOutput("status"),
    
      hr(),
      
      # Download button
      downloadButton(
        outputId = "downloadData",
        label = "Download"
      )
    ),
    
    # Main panel with table output
    mainPanel(
      gt_output("table")
    )
  )
)