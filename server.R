source("global.R")


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
            opt_interactive(
                page_size_default = 50,
                page_size_values = c(10, 25, 50, 100)
            )
    })
    
    # observe type selection
    observeEvent(input$run, {
        if (input$type == "MT540") {
            # Update result text
            output$result <- renderText("Running code to convert MT540 to semt.017...")
            
            # Convert to XML
            xml <- convert_tibble_to_xml_mt540(data())
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
