library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gt)
library(thematic)
library(scales)
library(writexl)
library(rsconnect)
library(usethis)
library(rmarkdown)
library(knitr)
library(reticulate)
library(xml2)
library(XML)

library(xml2)
library(tidyverse)

xml <- '<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:sese.023.001.09"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <SctiesSttlmTxInstr>
        <TxId>20240201123456</TxId>
        <SttlmTpAndAddtlParams>
            <SttlmTp>FOP</SttlmTp>
            <AddtlParams>
                <MsgFctn>NEWM</MsgFctn>
            </AddtlParams>
        </SttlmTpAndAddtlParams>
        <TxDtls>
            <TradDt>2024-02-01</TradDt>
            <SttlmDt>2024-02-05</SttlmDt>
            <FinInstrmId>
                <ISIN>DE000DB9VWC0</ISIN>
                <Desc>Deutsche Post AG Express-Zertifikat</Desc>
            </FinInstrmId>
            <FinInstrmQty>
                <Unit>1000</Unit>
            </FinInstrmQty>
            <SfkpgAcct>9876543210</SfkpgAcct>
        </TxDtls>
        <SttlmParams>
            <!-- Cedente: Deutsche Bank AG -->
            <SfkpgPlc>
                <BIC>DEUTDEFFXXX</BIC>
            </SfkpgPlc>
            <DlvrgPty>
                <BIC>DEUTDEFFXXX</BIC>
                <Nm>Deutsche Bank AG</Nm>
                <Acct>XXXXX12345</Acct>
            </DlvrgPty>
            <!-- Beneficiario: Minotti Fabio Pietro (Fineco Bank) -->
            <RcvgPty>
                <BIC>FEBIITM2XXX</BIC>
                <Nm>Fineco Bank S.P.A.</Nm>
                <Acct>2547926</Acct>
            </RcvgPty>
        </SttlmParams>
        <RgltryRptg>
            <RptgTp>TRAF</RptgTp>
            <RptgDtls>
                <Id>TRAF-2024-002</Id>
            </RptgDtls>
        </RgltryRptg>
    </SctiesSttlmTxInstr>
</Document>
'

# first function
# convert_to_xml <- function(tibble) {
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



convert_xml_to_tibble_mt540 <- function(xml){
  
  # read the xml file using the path
  xml <- xml |> read_xml()
  # convert the xml file to a list
  lst <- as_list(xml)
  # map lst to a dfr and unlist the iteration
  dfr <- map_dfr(lst, unlist)
  # assign the full names to a variable
  full_names <- dfr |> names()
  # iterate over full names to find the location of the last tag in the tree for each tag path
  locations <- map(full_names,
                   ~ .x |> 
                     str_locate_all("\\.") |>
                     unlist() |>
                     max() + 1)|>
    unlist()
  # map tags using names and location to substring them to a list
  tags <- map2(full_names, locations,
               ~ .x |> 
                 str_sub(start = .y)
  ) |> 
    unlist()
  # pivot longer to better display it and add later the path column
  dfr <- dfr |> 
    pivot_longer(
      cols = everything(),
      names_to = "tree",
      values_to = "value"
    )
  # add full tree path
  dfr$tags = tags
  dfr <- dfr |> 
    select(tree, tags, value)
  
  return(dfr)
  
}
tbl <- convert_xml_to_tibble_mt540(xml)
# write_xlsx(tbl, path = "~/R/mt_converter_/mt540_test.xlsx")


convert_tibble_to_xml_mt540 <- function(tbl, file_path) {
  # Create root document
  doc <- xml_new_root("Document", 
                      `xmlns` = "urn:iso:std:iso:20022:tech:xsd:sese.023.001.09",
                      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance")
  
  # Function to add nodes recursively
  add_nodes <- function(path, value) {
    elements <- str_split(path, "\\.", simplify = TRUE)
    
    # Traverse and add nodes using reduce()
    last_node <- reduce(elements, 
                        .init = doc, 
                        .f = ~ {
                          existing <- xml_find_first(.x, .y)
                          if (inherits(existing, "xml_missing")) {
                            xml_add_child(.x, .y)
                          } else {
                            existing
                          }
                        })
    
    # Set text value for the last node
    xml_set_text(last_node, value)
  }
  
  # Apply function for each row using walk2
  walk2(tbl$tree, tbl$value, add_nodes)
  
  # Save the XML document to file
  write_xml(doc, file_path)
  
  return(doc)
}

# Example usage: save to "output.xml"
# xml_output <- convert_tibble_to_xml(tbl, "~/R/mt_converter_/output.xml")
