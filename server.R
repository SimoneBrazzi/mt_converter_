server <- function(input, output, session) {
    # default empty data and type instantiation
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               xlsx = readxl::read_xlsx(input$upload$datapath),
               validate("Invalid file; Please upload a .xlsx file")
        )
    })
    
    # Reactive value to store the XML content
    xmlContent <- reactiveVal(NULL)
    
    # Render the gt table
    output$table <- render_gt({
        data() %>%
            gt() %>%
            cols_align(align = "center") %>%
            opt_interactive()
    })
    
    # observe type selection
    observeEvent(input$run, {
        if (input$type == "MT540") {
            # Update result text
            output$result <- renderText("Running code to convert MT540 to semt.017...")
            
            # Convert to XML
            xml <- convert_to_xml(data())
            xmlContent(xml) # Store in reactive value
            
            # Update status text
            output$status <- renderText("Finished!")
        } else {
            output$result <- renderText("Not yet implemented!")
        } 
    })
    
    # download button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".xml", sep = "")
        },
        content = function(file) {
            write_xml(xmlContent(), file)
        }
    )
}
