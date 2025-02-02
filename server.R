convert_to_xml <- function(tibble) {
    root <- xml_new_root("SctiesTxPstngRpt")
    
    # Add pagination
    pgntn <- xml_add_child(root, "Pgntn")
    xml_add_child(pgntn, "PgNb", tibble$PgNb)
    xml_add_child(pgntn, "LastPgInd", tolower(as.character(tibble$LastPgInd)))
    
    # Add statement general details
    stmt_gnl_dtls <- xml_add_child(root, "StmtGnlDtls")
    rpt_nb <- xml_add_child(stmt_gnl_dtls, "RptNb")
    xml_add_child(rpt_nb, "Shrt", tibble$RptNb)
    stmt_prd <- xml_add_child(stmt_gnl_dtls, "StmtPrd")
    fr_dt_to_dt <- xml_add_child(stmt_prd, "FrDtToDt")
    xml_add_child(fr_dt_to_dt, "FrDt", as.character(tibble$FrDt))
    xml_add_child(fr_dt_to_dt, "ToDt", as.character(tibble$ToDt))
    xml_add_child(stmt_gnl_dtls, "Frqcy") %>% xml_add_child("Cd", tibble$Frqcy)
    xml_add_child(stmt_gnl_dtls, "UpdTp") %>% xml_add_child("Cd", tibble$UpdTp)
    xml_add_child(stmt_gnl_dtls, "StmtBsis") %>% xml_add_child("Cd", tibble$StmtBsis)
    xml_add_child(stmt_gnl_dtls, "ActvtyInd", tolower(as.character(tibble$ActvtyInd)))
    xml_add_child(stmt_gnl_dtls, "SubAcctInd", tolower(as.character(tibble$SubAcctInd)))
    
    # Add safekeeping account
    sfkpg_acct <- xml_add_child(root, "SfkpgAcct")
    xml_add_child(sfkpg_acct, "Id", tibble$SfkpgAcct)
    
    # Add financial instrument details
    fin_instrm_dtls <- xml_add_child(root, "FinInstrmDtls")
    fin_instrm_id <- xml_add_child(fin_instrm_dtls, "FinInstrmId")
    xml_add_child(fin_instrm_id, "ISIN", tibble$ISIN)
    
    # Add transaction details
    tx <- xml_add_child(fin_instrm_dtls, "Tx")
    xml_add_child(tx, "AcctOwnrTxId", tibble$AcctOwnrTxId)
    tx_dtls <- xml_add_child(tx, "TxDtls")
    xml_add_child(tx_dtls, "TxActvty") %>% xml_add_child("Cd", tibble$TxActvty)
    sttlm_tx_or_corp_actn_evt_tp <- xml_add_child(tx_dtls, "SttlmTxOrCorpActnEvtTp")
    scties_tx_tp <- xml_add_child(sttlm_tx_or_corp_actn_evt_tp, "SctiesTxTp")
    xml_add_child(scties_tx_tp, "Cd", tibble$SctiesTxTp)
    xml_add_child(tx_dtls, "SctiesMvmntTp", tibble$SctiesMvmntTp)
    xml_add_child(tx_dtls, "Pmt", tibble$Pmt)
    pstng_qty <- xml_add_child(tx_dtls, "PstngQty")
    xml_add_child(pstng_qty, "Qty") %>% xml_add_child("FaceAmt", tibble$FaceAmt)
    fctv_sttlm_dt <- xml_add_child(tx_dtls, "FctvSttlmDt")
    xml_add_child(fctv_sttlm_dt, "Dt", as.character(tibble$FctvSttlmDt))
    
    # Add delivering settlement parties
    dlvrg_sttlm_pties <- xml_add_child(tx_dtls, "DlvrgSttlmPties")
    dpstry <- xml_add_child(dlvrg_sttlm_pties, "Dpstry")
    xml_add_child(dpstry, "Id") %>% xml_add_child("AnyBIC", tibble$Dpstry_BIC)
    pty1_dlvrg <- xml_add_child(dlvrg_sttlm_pties, "Pty1")
    prtry_id_dlvrg <- xml_add_child(pty1_dlvrg, "Id") %>% xml_add_child("PrtryId")
    xml_add_child(prtry_id_dlvrg, "Id", tibble$DlvrgPty1_Id)
    xml_add_child(prtry_id_dlvrg, "Issr", tibble$DlvrgPty1_Issr)
    xml_add_child(dlvrg_sttlm_pties, "Pty2") %>% xml_add_child("Id") %>% xml_add_child("AnyBIC", tibble$DlvrgPty2_BIC)
    
    # Add receiving settlement parties
    rcvg_sttlm_pties <- xml_add_child(tx_dtls, "RcvgSttlmPties")
    pty1_rcvg <- xml_add_child(rcvg_sttlm_pties, "Pty1")
    prtry_id_rcvg <- xml_add_child(pty1_rcvg, "Id") %>% xml_add_child("PrtryId")
    xml_add_child(prtry_id_rcvg, "Id", tibble$RcvgPty1_Id)
    xml_add_child(prtry_id_rcvg, "Issr", tibble$RcvgPty1_Issr)
    xml_add_child(rcvg_sttlm_pties, "Pty2") %>% xml_add_child("Id") %>% xml_add_child("AnyBIC", tibble$RcvgPty2_BIC)
    pty3_rcvg <- xml_add_child(rcvg_sttlm_pties, "Pty3")
    xml_add_child(pty3_rcvg, "Id") %>% xml_add_child("AnyBIC", tibble$RcvgPty3_BIC)
    xml_add_child(pty3_rcvg, "SfkpgAcct") %>% xml_add_child("Id", tibble$RcvgSfkpgAcct)
    
    return(root)
}


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
