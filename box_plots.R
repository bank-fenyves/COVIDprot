


##### SOMA or OLINK #####
## Full data for trend plotting (Soma)
data = somalogic %>%
  dplyr::select(2, 3, starts_with(coag_soma$SEQID)) %>%
  merge(clinical[c("study_id","Severity", "COVID", "VTE")], by.x = "patient", by.y = "study_id") %>%
  subset(COVID == 1) %>%
  # subset(day == "0") %>%
  # subset(Severity == 1) %>%
  subset(day %in% c("0", "3", "7")) %>%
  relocate(c("VTE","Severity", "COVID"), .after = 1)
for (i in coag_soma$SEQID) {
  colnames(data) = gsub(i, coag_soma[coag_soma$SEQID==i,1], colnames(data))
}
## Full data for trend plotting (Olink)
data = olink %>%
  dplyr::select(1, 2, coag_olink$OlinkID) %>%
  merge(clinical[c("study_id","Severity", "COVID", "VTE")], by.x = "SampleID", by.y = "study_id") %>%
  subset(COVID == 1) %>%
  # subset(day == "D0") %>%
  # subset(Severity == 1) %>%
  subset(day %in% c("D0", "D3", "D7")) %>%
  relocate(c("VTE","Severity", "COVID"), .after = 1)
for (i in coag_olink$OlinkID) {
  colnames(data) = gsub(i, coag_olink[coag_olink$OlinkID==i,1], colnames(data))
}


## Rename values box plotting 
{ data$Severity <- data$Severity %>%        ## This modified data, rewrites Severity
    replace(data$Severity == 1, "Severe") %>%
    replace(data$Severity == 0, "Non-severe")
  data$VTE <- data$VTE %>%        ## This modified data, rewrites VTE
    replace(data$VTE == 1, "VTE") %>%
    replace(data$VTE == 0, "No VTE")
  data$COVID <- data$COVID %>%        ## This modified data, rewrites VTE
    replace(data$COVID == 1, "C+") %>%
    replace(data$COVID == 0, "C-")
}

## Grouped Box Plots
pdf(file = "boxplots_all_coag.pdf")
for (i in colnames(data[6:ncol(data)])) {
  give.n <- function(x){
    return(data.frame(y = median(x)*1.05, label = paste0("n = ", length(x)))) }
  x <- ggplot(data, aes(x=VTE, y=data[,i])) +  ## x= contrast groups
    geom_boxplot(
      aes(fill=VTE), ## fill= contrast groups
      position = position_dodge(0.9)
    ) +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
    # scale_x_discrete(limits=c("0", "3", "7")) +
    scale_fill_manual(values = c("#819DFC", "#B9FA6F", "#FA7855")) +
    theme(legend.position = "bottom") +
    labs(y="Level") +
    facet_wrap(~day, nrow = 6, ncol = 6) + ## ~day == group by "day"
    ggtitle(i)
  print(x)
}
dev.off()


## Multivariable Linear model (adjust for confounders) for P-selectin
{
  data = olink %>%
  dplyr::select(1, 2, coag_olink$OlinkID) %>%
  merge(clinical[c("study_id","Severity", "COVID", "VTE", "CTPE_US", "Age", "Sex", "Ethnicity",
                   "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER")],
        by.x = "SampleID", by.y = "study_id") %>%
  subset(COVID == 1) %>%
  subset(day == "D3") %>%
  # subset(Severity == 1) %>%
  relocate(c("SampleID","Severity", "COVID", "VTE", "CTPE_US", "Age", "Sex", "Ethnicity",
             "HEART", "KIDNEY", "HTN", "LUNG", "DIABETES", "LIVER"), .after = 1)
for (i in coag_olink$OlinkID) {
  colnames(data) = gsub(i, coag_olink[coag_olink$OlinkID==i,1], colnames(data))
  }
lmt = lm(SELP ~ VTE + Severity + Age + Sex + Ethnicity + HEART + KIDNEY + LUNG + DIABETES + HTN + LIVER,
         data = data)
tbl_regression(lmt, exponentiate = FALSE) %>%
  bold_p(t = 0.05) %>%
  modify_header(label = "day0 soma c+. selp")
}