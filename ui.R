source("script.R")


ui <- fluidPage(
  # Application title
  titlePanel("Converter"),
  
  # Sidebar with inputs and main panel with table output
  sidebarLayout(
    sidebarPanel(
      # Reset button
      actionButton(
        inputId = "reset",
        label = "Reset"
      ),
      
      hr(),
      # MsgId  MessageIdentification
      textInput(
        inputId = "MsgId",
        label = "Message Identification",
        value = "",
        placeholder = "Enter text here"
      ),
      # CreDtTm  CreationDateTime
      dateInput(
        inputId = "CreDtTm",
        label = "Creation Date Time",
        value = Sys.time(),
        format = "yyyy-mm-dd"
      ),
      # NbOfTxs  NumberOfTransactions
      textInput(
        inputId = "NbOfTxs",
        label = "Number Of Transactions",
        value = "",
        placeholder = "Enter text here"
      ),
      # CtrlSum  ControlSum
      autonumericInput(
        inputId = "CtrlSum",
        label = "Control Sum",
        min = 0,
        value = NULL,
        decimalPlaces = 2,
        digitGroupSeparator = ",",
        decimalCharacter = "."
      ),
      #### InitgPty  InitiatingParty -> list of MessageElement
      
      hr(),
      
      # Type selection
      selectizeInput(
        inputId = "type",
        label = "Type",
        choices = c("MT103", "MT109", "Other"),
        selected = NULL,
        multiple = FALSE,
        options = list(placeholder = "Select a type")
      ),
      
      # Currency selection
      selectizeInput(
        inputId = "currency",
        label = "Currency",
        choices = c("EUR", "USD", "GBP", "CHF"), # infor_currencies() to check
        selected = "€",
        multiple = FALSE
      ),
      
      
      br(),
      
      # Confirm button
      actionButton(
        inputId = "confirm",
        label = "Confirm"
      ),
      
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