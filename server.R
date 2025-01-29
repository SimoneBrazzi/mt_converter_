source("script.R")

# Define server logic required to manage data and outputs
server <- function(input, output, session) {
    
    # default empty data and type instantiation
    data <- reactiveVal(
        tibble(
            MsgId = character(),
            CreDtTm = as_datetime(character()),
            NbOfTxs = character(),
            CtrlSum = numeric(),
            Type = character()
        )
    )
    
    # 
    observeEvent(
        input$confirm,
        {
            req(input$MsgId, input$CreDtTm, input$NbOfTxs, input$CtrlSum, input$type)
            
            # Create new row
            new_row <- tibble(
                Type = input$type,
                MsgId = input$MsgId,
                CreDtTm = input$CreDtTm, 
                NbOfTxs = input$NbOfTxs,
                CtrlSum = input$CtrlSum
            )
            
            # Update data by binding new row
            data(bind_rows(data(), new_row))
        }
    )
    

    # Reactive for currency input to impute to amount in gt table
    currency_ <- reactive({input$currency})
    
    # Render the gt table
    output$table <- render_gt({
        data() %>%
            gt() %>%
            cols_align(align = "center") %>%
            opt_interactive()
    })
    
    # reset button to empty tibble
    observeEvent(input$reset, {
        data(tibble(
            MsgId = character(),
            CreDtTm = as.Date(character()),
            NbOfTxs = character(),
            CtrlSum = numeric(),
            Type = character()
        ))
    })
    
    # download button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(data(), file)
        }
    )
    
    
}

# Run the application
# sshinyApp(ui = ui, server = server)
