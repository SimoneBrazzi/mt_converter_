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

# Sample XML string
xml_string <- '
<SctiesTxPstngRpt>
    <Pgntn>
        <PgNb>1</PgNb>
        <LastPgInd>true</LastPgInd>
    </Pgntn>
    <StmtGnlDtls>
        <RptNb>
            <Shrt>124</Shrt>
        </RptNb>
        <StmtPrd>
            <FrDtToDt>
                <FrDt>2021-01-14</FrDt>
                <ToDt>2021-01-15</ToDt>
            </FrDtToDt>
        </StmtPrd>
        <Frqcy>
            <Cd>DAIL</Cd>
        </Frqcy>
        <UpdTp>
            <Cd>COMP</Cd>
        </UpdTp>
        <StmtBsis>
            <Cd>SETT</Cd>
        </StmtBsis>
        <ActvtyInd>true</ActvtyInd>
        <SubAcctInd>false</SubAcctInd>
    </StmtGnlDtls>
    <SfkpgAcct>
        <Id>222S</Id>
    </SfkpgAcct>
    <FinInstrmDtls>
        <FinInstrmId>
            <ISIN>GB0000987654</ISIN>
        </FinInstrmId>
        <Tx>
            <AcctOwnrTxId>FRTJ123REC1</AcctOwnrTxId>
            <TxDtls>
                <TxActvty>
                    <Cd>SETT</Cd>
                </TxActvty>
                <SttlmTxOrCorpActnEvtTp>
                    <SctiesTxTp>
                        <Cd>TRAD</Cd>
                    </SctiesTxTp>
                </SttlmTxOrCorpActnEvtTp>
                <SctiesMvmntTp>RECE</SctiesMvmntTp>
                <Pmt>FREE</Pmt>
                <PstngQty>
                    <Qty>
                        <FaceAmt>6000000</FaceAmt>
                    </Qty>
                </PstngQty>
                <FctvSttlmDt>
                    <Dt>2021-01-15</Dt>
                </FctvSttlmDt>
                <DlvrgSttlmPties>
                    <Dpstry>
                        <Id>
                            <AnyBIC>CRSTGB22</AnyBIC>
                        </Id>
                    </Dpstry>
                    <Pty1>
                        <Id>
                            <PrtryId>
                                <Id>456</Id>
                                <Issr>CRST</Issr>
                            </PrtryId>
                        </Id>
                    </Pty1>
                    <Pty2>
                        <Id>
                            <AnyBIC>CPFIDEFF</AnyBIC>
                        </Id>
                    </Pty2>
                </DlvrgSttlmPties>
                <RcvgSttlmPties>
                    <Pty1>
                        <Id>
                            <PrtryId>
                                <Id>123</Id>
                                <Issr>CRST</Issr>
                            </PrtryId>
                        </Id>
                    </Pty1>
                    <Pty2>
                        <Id>
                            <AnyBIC>DRESDEFF</AnyBIC>
                        </Id>
                    </Pty2>
                    <Pty3>
                        <Id>
                            <AnyBIC>MGTCDE55</AnyBIC>
                        </Id>
                        <SfkpgAcct>
                            <Id>111S</Id>
                        </SfkpgAcct>
                    </Pty3>
                </RcvgSttlmPties>
            </TxDtls>
        </Tx>
    </FinInstrmDtls>
</SctiesTxPstngRpt>'

# Read the XML string into an XML document
xml_doc <- read_xml(xml_string)

# Convert XML to a list of named vectors
data <- list(
  PgNb = xml_text(xml_find_first(xml_doc, "//PgNb")),
  LastPgInd = as.logical(xml_text(xml_find_first(xml_doc, "//LastPgInd"))),
  RptNb = as.numeric(xml_text(xml_find_first(xml_doc, "//RptNb/Shrt"))),
  FrDt = as.Date(xml_text(xml_find_first(xml_doc, "//StmtPrd/FrDtToDt/FrDt"))),
  ToDt = as.Date(xml_text(xml_find_first(xml_doc, "//StmtPrd/FrDtToDt/ToDt"))),
  Frqcy = xml_text(xml_find_first(xml_doc, "//Frqcy/Cd")),
  UpdTp = xml_text(xml_find_first(xml_doc, "//UpdTp/Cd")),
  StmtBsis = xml_text(xml_find_first(xml_doc, "//StmtBsis/Cd")),
  ActvtyInd = as.logical(xml_text(xml_find_first(xml_doc, "//ActvtyInd"))),
  SubAcctInd = as.logical(xml_text(xml_find_first(xml_doc, "//SubAcctInd"))),
  SfkpgAcct = xml_text(xml_find_first(xml_doc, "//SfkpgAcct/Id")),
  ISIN = xml_text(xml_find_first(xml_doc, "//FinInstrmId/ISIN")),
  AcctOwnrTxId = xml_text(xml_find_first(xml_doc, "//Tx/AcctOwnrTxId")),
  TxActvty = xml_text(xml_find_first(xml_doc, "//TxActvty/Cd")),
  SctiesTxTp = xml_text(xml_find_first(xml_doc, "//SctiesTxTp/Cd")),
  SctiesMvmntTp = xml_text(xml_find_first(xml_doc, "//SctiesMvmntTp")),
  Pmt = xml_text(xml_find_first(xml_doc, "//Pmt")),
  FaceAmt = as.numeric(xml_text(xml_find_first(xml_doc, "//PstngQty/Qty/FaceAmt"))),
  FctvSttlmDt = as.Date(xml_text(xml_find_first(xml_doc, "//FctvSttlmDt/Dt"))),
  Dpstry_BIC = xml_text(xml_find_first(xml_doc, "//DlvrgSttlmPties/Dpstry/Id/AnyBIC")),
  DlvrgPty1_Id = xml_text(xml_find_first(xml_doc, "//DlvrgSttlmPties/Pty1/Id/PrtryId/Id")),
  DlvrgPty1_Issr = xml_text(xml_find_first(xml_doc, "//DlvrgSttlmPties/Pty1/Id/PrtryId/Issr")),
  DlvrgPty2_BIC = xml_text(xml_find_first(xml_doc, "//DlvrgSttlmPties/Pty2/Id/AnyBIC")),
  RcvgPty1_Id = xml_text(xml_find_first(xml_doc, "//RcvgSttlmPties/Pty1/Id/PrtryId/Id")),
  RcvgPty1_Issr = xml_text(xml_find_first(xml_doc, "//RcvgSttlmPties/Pty1/Id/PrtryId/Issr")),
  RcvgPty2_BIC = xml_text(xml_find_first(xml_doc, "//RcvgSttlmPties/Pty2/Id/AnyBIC")),
  RcvgPty3_BIC = xml_text(xml_find_first(xml_doc, "//RcvgSttlmPties/Pty3/Id/AnyBIC")),
  RcvgSfkpgAcct = xml_text(xml_find_first(xml_doc, "//RcvgSttlmPties/Pty3/SfkpgAcct/Id"))
)

# Convert the list to a tibble
data_tibble <- as_tibble(data)
# write_xlsx(data_tibble, path = "/Users/simonebrazzi/Downloads/test.xlsx")


# Convert tibble back to XML
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

# Create the XML structure from the tibble
xml_root <- convert_to_xml(data_tibble)

# Print the XML
cat(as.character(xml_root))













