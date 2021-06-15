
clinical <- read_excel("clinical_data.xlsx")

data <- clinical

##All patients by COVID status  
table1 <- data[,c("COVID", "VTE", 
                  "Ethnicity", "Age", "Sex", "BMI", "HEART", "KIDNEY", "LIVER", "LUNG", "DIABETES", "HTN",
                  "Severity", "Vaso",
                  "plts_0", "plts_3", "plts_7", 
                  "crp_0", "crp_3", "crp_7",
                  "ddimer_0", "ddimer_3", "ddimer_7", 
                  "fibrinogen_0", "fibrinogen_3", "fibrinogen_7")]

  table1 <- tbl_summary(table1,  by = COVID, missing = "no") %>%
    add_n() %>% 
    add_p() %>% 
    modify_header(label = "COVID") %>% 
    bold_p(t = 0.05) %>%
    bold_labels()
  table1


##COVID-positive patients by VTE complications
table1 <- data[,c("COVID", "VTE", 
                  "Ethnicity", "Age", "Sex", "BMI", "HEART", "KIDNEY", "LIVER", "LUNG", "DIABETES", "HTN",
                  "Severity", "Vaso",
                  "plts_0", "plts_3", "plts_7", 
                  "crp_0", "crp_3", "crp_7",
                  "ddimer_0", "ddimer_3", "ddimer_7", 
                  "fibrinogen_0", "fibrinogen_3", "fibrinogen_7")] %>%
  subset(COVID == 1)
table1 <- table1[,-1]

  table1 <- tbl_summary(table1,  by = VTE, missing = "no") %>%
    add_n() %>% 
    add_p() %>% 
    modify_header(label = "VTE") %>% 
    bold_p(t = 0.05) %>%
    bold_labels()
  table1