install.packages("easypackages")
library("easypackages")
libraries("readr", "tidyr", "data.table", "dplyr", "gtsummary", "magrittr", "plotROC", "pROC", "openxlsx", "readxl", "MASS")
libraries("extrafont", "viridis", "hrbrthemes", "ggplot2", "stringi")


#### SOMA ######
somalogic_data <- read_excel("somalogic-data.xlsx", col_names=TRUE)
somalogic_raw <- read_excel("Novartis.BMD062_MGHCOV_human_plasmaedta_rpmi_785_Jennilo2_2020June15_vF.06Jul2020.HybNormPlateScaleMedNormRefCalibrateMedNormSMP.adat.xlsx")
SOMAscan_annotations <- read_excel("SOMAscan_Assay_v4_Annotations_version3.3.2.xlsx" ,sheet = "SOMAscanAssayv4_Annotations3.3")

somalogic <- merge(somalogic_data[,1:3], somalogic_raw, by.x = "sample_barcode", by.y = "SampleId")
colnames(somalogic)[4:ncol(somalogic)] <- gsub("-", "", colnames(somalogic)[4:ncol(somalogic)]) ##Get rid of the "-" symbol in SeqID names
SOMAscan_annotations$SeqId <- gsub("-", "", SOMAscan_annotations$SeqId) ##Get rid of "-" symbol
SOMAscan_annotations$Target <- gsub("[[:punct:]]", "", SOMAscan_annotations$Target)

##Log2 transformation
somalogic[,4:ncol(somalogic)] <- log2(somalogic[4:ncol(somalogic)])

## Somalogic analysis - formally prepare data in the patient_ID column
somalogic$patient = gsub("c", "C", somalogic$patient)
somalogic$temp = (gsub("C4-", "", somalogic$patient)) %>%
  as.double() %>%
  formatC(flag=0,width=3) ## Makes '001' from '1' and so son 
somalogic$patient = paste0("C4-", somalogic$temp) ##Adds "C4-"
somalogic$temp <- NULL

## Daily subsets
D0_soma = (subset(somalogic, day == 0)) %>%
  dplyr::select(2,4:ncol(somalogic))
D3_soma = (subset(somalogic, day == 3)) %>%
  dplyr::select(2,4:ncol(somalogic))
D7_soma = (subset(somalogic, day == 7)) %>%
  dplyr::select(2,4:ncol(somalogic))



##### OLINK #####
## olink is the Olink proteome file ()
olink <- read_csv("~/Dropbox (Olink_Data_Wide.csv")
## olink_assay is the dictionary for protein names

olink <- olink %>%
  separate(SampleID, c("SampleID", "day"), sep = "_")

#Daily sample subsets
olink$SampleID = gsub("c", "C", olink$SampleID)
D0_olink = subset(olink, day == "D0") %>%
  subset(, -2)
D3_olink = subset(olink, day == "D3") %>%
  subset(, -2)
D7_olink = subset(olink, day == "D7") %>%
  subset(, -2)
