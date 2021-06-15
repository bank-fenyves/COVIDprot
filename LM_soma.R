
library("easypackages")
libraries("readr", "data.table", "dplyr", "gtsummary", "magrittr", "plotROC", "pROC", "openxlsx", "readxl", "MASS")
libraries("extrafont", "viridis", "hrbrthemes", "ggplot2", "stringi")
BiocManager::install("qvalue")
library(qvalue)

##Soma
{data = D0_soma %>%
    dplyr::select(1, starts_with(coag_soma$SEQID)) %>%
    merge(clinical[c("study_id","Severity", "COVID", "VTE", "CTPE_US", "Age", "Sex", "Ethnicity",
                     "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER")],
          by.x = "patient", by.y = "study_id") %>%
    subset(COVID == 1) %>%
    # subset(Severity == 1) %>%
    # subset(CTPE_US == 1) %>%
    relocate(c("VTE", "Severity", "COVID", "Age", "Sex", "Ethnicity",
               "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER", "CTPE_US"),
             .after = 1)
  stringi::stri_sub(colnames(data)[15:ncol(data)], 1, 0) <- "SID"
}

## Multivariable adjusted GLM/LM analysis - iterated for multiple outcome variables
summaries <- lapply(colnames(data[,15:ncol(data)]),
                    
        function(var) {
          formula    <- as.formula(paste(var, " ~ VTE + Severity + Age + Sex + Ethnicity + HEART + LUNG + KIDNEY + DIABETES + HTN + LIVER")) #Iterate the dependent variable (outcome), and keep the independent variable and covariate(s)
          res.logist <- lm(formula, data = data)
          summary(res.logist)
          # tbl_regression(res.logist, exponentiate = TRUE) %>%      ##Either use summary as output, or use the tbl_regression.
          #   bold_p(t = 0.05) %>%
          #   modify_header(label = paste(var, " ~"))
        })

##confidence intervals (batch calculation)
summaries_ci <- lapply(colnames(data[,15:ncol(data)]),
                       
         function(var) {
           formula    <- as.formula(paste(var, " ~ VTE + Severity + Age + Sex + Ethnicity + HEART + LUNG + KIDNEY + DIABETES + HTN + LIVER")) #Iterate the dependent variable (outcome), and keep the independent variable and covariate(s)
           res.logist <- lm(formula, data = data)
           confint(res.logist)
           # tbl_regression(res.logist, exponentiate = TRUE) %>%      ##Either use summary as output, or use the tbl_regression.
           #   bold_p(t = 0.05) %>%
           #   modify_header(label = paste(var, " ~"))
         })

{dt1 = data.table() ##Data for the first variable
  dt1$ID = colnames(data[,15:ncol(data)])
  stringi::stri_sub(dt1$ID, 1, 3) <- ""
  dt1$GENE <- coag_soma$GENE
  dt1$estimate = 1
  dt1$P = 1
  varnum = length(summaries[[1]]$aliased) - 1
  for (i in 1:length(dt1$P)) {
    dt1$estimate[i] <- summaries[[i]][["coefficients"]][2] #the 2nd element is the estimate for the first variable IF there are two variables
    dt1$P[i] <- summaries[[i]][["coefficients"]][3*varnum + 5] # = 3*number of variables + 5
    dt1$CI_low[i] <- summaries_ci[[i]][2] ##is 2nd element for the first variable
    dt1$CI_up[i] <- summaries_ci[[i]][varnum + 3] ## 13 if there are 10 variables (= 3 + number of variables)
  }         ##Only works if summaries is the summary() table and not the tbl_regression
  dt1$adjP <- p.adjust(dt1$P, "BH")
  # dt1$q <- qvalue(dt1$P)$qvalues
}

##Volcano plot for dt1
{plot(dt1$estimate, -log10(dt1$P), type = "p", cex =0.4, xlim = c(min(dt1$CI_low),max(dt1$CI_up)))
  text(dt1$estimate, -log10(dt1$P), labels = dt1$GENE, cex = 0.5, adj = c(0.5,-0.6))
  abline(h = -log10(0.05), col = "red")
  abline(v = 0)
  for (i in 1:length(dt1$estimate)) {
    if (dt1$CI_low[i]*dt1$CI_up[i] > 0) {
      lines(c(dt1$CI_low[i], dt1$CI_up[i]), c(-log10(dt1$P[i]), -log10(dt1$P[i])), col = "red", lwd = 0.4)
    }
    else {}}
}

