

coag_prots <- read_excel("coagulation_proteins.xlsx")
coag_olink <- read_excel("coagulation_proteins.xlsx", sheet = "OLINK_ID")
coag_soma <- read_excel("coagulation_proteins.xlsx", sheet = "SOMA_ID")
coag_soma$SEQID <- as.character(coag_soma$SEQID)

## Olink statistics
{
data = D0_olink %>%
    dplyr::select(1, (coag_olink$OlinkID)) %>%
    merge(clinical[c("study_id","Severity", "COVID", "VTE", "Age", "Sex", "Ethnicity", 
                     "HEART", "KIDNEY", "LUNG", "HTN", "DIABETES", "LIVER")], by.x = "SampleID", 
          by.y = "study_id") %>%
    subset(COVID == 1) %>%
    # subset(Severity == 1) %>%
    relocate(c("VTE","Severity", "COVID", "Age", "Sex", "Ethnicity", 
               "HEART", "KIDNEY", "HTN","LUNG", "DIABETES", "LIVER"), .after = 1)
  for (i in olink_assay$OlinkID) {
    colnames(data) = gsub(i, olink_assay[olink_assay$OlinkID==i,2], colnames(data))}
  
  table1 <- data[,c(2,14:ncol(data))]
  ## Filter patients
  # table1 <- subset(table1, COVID == 1)
  # table1 <- table1[,-1]
  
  table1 <- tbl_summary(table1,  by = VTE, missing = "no") %>%
    add_n() %>% 
    add_p() %>% 
    modify_header(label = "VTE") %>% 
    bold_p(t = 0.05) %>%
    bold_labels()
  table1
}

##Somalogic statistics
{data = D0_soma %>%
    dplyr::select(1, starts_with(coag_soma$SEQID)) %>%
    merge(clinical[c("study_id","Severity", "COVID", "VTE", "Age", "Sex", "Ethnicity",
                     "HEART", "KIDNEY", "HTN", "DIABETES", "LIVER")],
          by.x = "patient", by.y = "study_id") %>%
    subset(COVID == 1) %>%
    # subset(Severity == 1) %>%
    relocate(c("VTE", "Severity", "COVID", "Age", "Sex", "Ethnicity",
               "HEART", "KIDNEY", "HTN", "DIABETES", "LIVER"),
             .after = 1)
  for (i in 13:ncol(data)) {
    j = i-12
    colnames(data)[i] = coag_soma$GENE[j]
  }
  # stringi::stri_sub(colnames(data)[13:ncol(data)], 1, 0) <- "SID"
  
  table1 <- data[,c(2,13:ncol(data))]
  ## Filter patients
  # table1 <- subset(table1, COVID == 1)
  # table1 <- table1[,-1]
  
  table1 <- tbl_summary(table1,  by = VTE, missing = "no") %>%
    add_n() %>% 
    add_p() %>% 
    modify_header(label = "VTE") %>% 
    bold_p(t = 0.05) %>%
    bold_labels()
  table1
}
