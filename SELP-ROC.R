install.packages("easypackages")
library("easypackages")
libraries("readr", "data.table", "dplyr", "gtsummary", "magrittr", "plotROC", "pROC", "openxlsx", "readxl", "MASS")


outcome = "VTE"

## Olink
{data = D0_olink %>%
    dplyr::select(1, contains(coag_olink$OlinkID)) %>%
    merge(clinical[c("study_id","Severity", "VTE", "COVID", "Age", "Sex", "Ethnicity", 
                     "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER",
                     "ddimer_0")], by.x = "SampleID", by.y = "study_id") %>%
    subset(COVID == 1) %>%
    # subset(Severity == 1) %>%
    relocate(c("VTE","Severity", "COVID", "VTE", "Age", "Sex", "Ethnicity", 
               "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER",
               "ddimer_0"), .after = 1)
  for (i in olink_assay$OlinkID) {
    colnames(data) = gsub(i, olink_assay[olink_assay$OlinkID==i,2], colnames(data))}
  
}

{
glm1 = glm(VTE ~ ddimer_0, data = data)
glm2 = glm(VTE ~ SELP, data = data)
glm3 = glm(VTE ~ ddimer_0 + SELP, data = data)

pred1 = predict(glm1, newdata = data, type = "response") ## Create a prediction 
pred2 = predict(glm2, newdata = data, type = "response") ## Create a prediction 
pred3 = predict(glm3, newdata = data, type = "response") ## Create a prediction 

roc1 = roc(data$VTE ~ pred1, plot = TRUE, print.auc = TRUE) ## Plot the predictive capability of the model on a ROC 
roc2 = roc(data$VTE ~ pred2, plot = TRUE, print.auc = TRUE) ## Plot the predictive capability of the model on a ROC 
roc3 = roc(data$VTE ~ pred3, plot = TRUE, print.auc = TRUE) ## Plot the predictive capability of the model on a ROC 
}
