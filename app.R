library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gt)
library(thematic)
library(scales)
library(writexl)
library(rsconnect)

ui <- fluidPage(
    # Application title
    titlePanel("Converter"),
    
    # Sidebar with inputs and main panel with table output
    sidebarLayout(
        sidebarPanel(
            # Clear button
            actionButton(
                inputId = "clear",
                label = "Clear"
            ),
            
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
            
            # Amount input with currency formatting
            autonumericInput(
                inputId = "amount",
                label = "Amount",
                min = 0,
                value = NULL,
                currencySymbolPlacement = "p",
                decimalPlaces = 2,
                digitGroupSeparator = ",",
                decimalCharacter = "."
            ),
            
            # Currency selection
            selectizeInput(
                inputId = "currency",
                label = "Currency",
                choices = c("EUR", "USD", "GBP", "CHF"), # infor_currencies() to check
                selected = "€",
                multiple = FALSE
            ),
            
            # Date input
            dateInput(
                inputId = "date",
                label = "Date",
                value = Sys.Date(),
                format = "yyyy-mm-dd",
                language = "en",
                autoclose = TRUE
            ),
            
            # Text input
            textInput(
                inputId = "text",
                label = "Text",
                value = "",
                placeholder = "Enter text here"
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

# Define server logic required to manage data and outputs
server <- function(input, output, session) {
    
    # Reactive tibble with input validation
    data <- reactive({
        # Ensure inputs exist
        req(input$type, input$date, input$amount, input$text)
        
        # Validate amount is numeric
        validate(
            need(is.numeric(input$amount), "Amount must be a number"),
            need(is.Date(input$date), "Amount must be a date")
        )
        
        tibble(
            Type = input$type,
            Date = as.Date(input$date),  # Ensure proper date format
            Amount = as.numeric(input$amount),
            Text = trimws(input$text)  # Remove leading/trailing whitespace
        )
    })
    
    # Reactive for currency input to impute to amount in gt table
    currency_ <- reactive({input$currency})
    
    # Render the gt table
    output$table <- render_gt({
        data() %>%
            gt() %>%
            cols_align(align = "center") %>%
            fmt_currency(
                columns = c("Amount"),
                currency = currency_(), # static value from reactive one
                decimals = 2,
                dec_mark = ".",
                sep_mark = "," 
            ) %>%
            opt_interactive()  # Make the table interactive
    })
}

# Run the application
shinyApp(ui = ui, server = server)
