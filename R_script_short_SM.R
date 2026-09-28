survey_data<-read.csv("Survey_results_359_19122025.csv")

grep("aller", colnames(survey_data))
grep("obesity", colnames(survey_data),value = TRUE)
grep("weight", colnames(survey_data))

grep("mental", colnames(survey_data),value = TRUE)
grep("mental", colnames(survey_data))

table(survey_data[,27])
sum(table(survey_data[,27]))

tbl<-matrix(c(length(which(survey_data[,57] == "Yes" & survey_data[,37] == "Yes")),
length(which(survey_data[,57] == "Yes" & survey_data[,37] == "No")),
length(which(survey_data[,57] == "No" & survey_data[,37] == "Yes")),
length(which(survey_data[,57] == "No" & survey_data[,37] == "No"))),nrow=2,byrow = TRUE,dimnames=list("Self Alergies"=c("Yes","No"),"Family Alergies"=c("Yes","No")))

chisq.test(tbl)$p.value
sqrt(chisq.test(tbl)$statistic)

### Obesity frequency and plot ### 
survey_data_endo<-survey_data[which(survey_data[,3] == "Yes, confirmed by laparoscopic surgery"),]

tbl<-matrix(c(length(which(survey_data_endo[,25] == "Yes, I am/have been overweight" & survey_data_endo[,40] == "Yes")),
              length(which(survey_data_endo[,25] == "Yes, I am/have been overweight" & survey_data_endo[,40] == "No")),
              length(which(survey_data_endo[,25] != "Yes, I am/have been overweight" & survey_data_endo[,40] == "Yes")),
              length(which(survey_data_endo[,25] != "Yes, I am/have been overweight" & survey_data_endo[,40] == "No"))),nrow=2,byrow = TRUE,dimnames=list("Self Obesity"=c("Yes","No"),"Family Obesity"=c("Yes","No")))

p_val <- chisq.test(tbl)$p.value
p_label <- sprintf("p = %.2e", p_val)
z_score<-sqrt(chisq.test(tbl)$statistic)
z_label<- sprintf("z = %.2f", z_score)

library(ggplot2)

#tbl_percent <- 100 * tbl / sum(tbl)
df <- as.data.frame(as.table(tbl))

#df_percent <- as.data.frame(as.table(tbl_percent))
#names(df_percent) <- c("Self.Obesity", "Family.Obesity", "Freq")

ggplot(df, aes(x = Family.Obesity, y = Self.Obesity, fill = Freq)) +
  geom_tile(color="black") +
  geom_text(aes(label = sprintf("%.0f", Freq)), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal(base_size = 16) +
  labs(title = "Obesity Percentage")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )+
  annotate("text", x = 0.7, y = 2.3, label = p_label, size = 6, fontface = "bold")+
  annotate("text", x = 0.7, y = 2.15, label = z_label, size = 6, fontface = "bold")


## mental health frequency and plot ###
tbl<-matrix(c(125,65,18,40),nrow=2,byrow = TRUE,dimnames=list("Self Mental Health"=c("Yes","No"),"Family Mental Health"=c("Yes","No")))
df <- as.data.frame(as.table(tbl))

p_val <- chisq.test(tbl)$p.value
p_label <- sprintf("p = %.2e", p_val)
z_score<-sqrt(chisq.test(tbl)$statistic)
qnorm(p_val / 2, lower.tail = FALSE)
z_label<- sprintf("z = %.2f", z_score)

ggplot(df, aes(x = Family.Mental.Health, y = Self.Mental.Health, fill = Freq)) +
  geom_tile(color="black") +
  geom_text(aes(label = sprintf("%.0f", Freq)), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal(base_size = 16) +
  labs(title = "Mental Health Percentage")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )+
  annotate("text", x = 0.75, y = 2.3, label = p_label, size = 6, fontface = "bold")+
  annotate("text", x = 0.75, y = 2.15, label = z_label, size = 6, fontface = "bold")

## allergies frequency and plot ###
tbl<-matrix(c(78,54,76,100),nrow=2,byrow = TRUE,dimnames=list("Self Mental Health"=c("Yes","No"),"Family Mental Health"=c("Yes","No")))
df <- as.data.frame(as.table(tbl))

p_val <- chisq.test(tbl)$p.value
p_label <- sprintf("p = %.2e", p_val)
z_score<-sqrt(chisq.test(tbl)$statistic)
qnorm(p_val / 2, lower.tail = FALSE)
z_label<- sprintf("z = %.2f", z_score)

ggplot(df, aes(x = Family.Mental.Health, y = Self.Mental.Health, fill = Freq)) +
  geom_tile(color="black") +
  geom_text(aes(label = sprintf("%.0f", Freq)), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal(base_size = 16) +
  labs(title = "Mental Health Percentage")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )+
  annotate("text", x = 0.75, y = 2.3, label = p_label, size = 6, fontface = "bold")+
  annotate("text", x = 0.75, y = 2.15, label = z_label, size = 6, fontface = "bold")

########### Comparing medians over time (not repeated measures) ##############
#### When order matters #########
library(clinfun)

jonckheere.test(delayed$ ..Delay.of.Diagnosis, as.numeric(as.factor(delayed$Year_bin)), alternative = "decreasing")

pairwise.wilcox.test(delayed$ ..Delay.of.Diagnosis, as.factor(delayed$Year_bin), p.adjust.method = "bonferroni")

library(ggplot2)

ggplot(delayed, aes(x = as.factor(delayed$Year_bin), y =  ..Delay.of.Diagnosis)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  labs(
    x = "Year of Onset (the preceeding 5 years)",
    y = "Delay of Diagnosis",
    title = "Diagnosis Delay by Symptom Onset"
  )

#### quantile regression
library(quantreg)

model <- rq(delayed$ ..Delay.of.Diagnosis ~ as.numeric(as.factor(delayed$Year_bin)), tau = 0.5, data = df)
summary(model, se = "boot")

#############
tbl<-matrix(c(6,14,52,7,42,33,18,33,38),nrow=3,byrow = TRUE,dimnames=list("Appetite vs Weight Issues"=c("under","none","over"),"Appetite Change"=c("Increase","No Change","Decrease")))
tbl
fisher.test(tbl)

library(rcompanion)

pairwiseNominalIndependence(
  tbl,
  fisher=TRUE,
  method = "BH",
  compare="row",
 )

#tab <- table(df$`Appetite vs weight`, df$`Appetite Change`)

rows <- rownames(tbl)
cols <- colnames(tbl)

results <- list()
pvals <- c()

for (r in combn(rows, 2, simplify = FALSE)) {
  for (c in combn(cols, 2, simplify = FALSE)) {
    
    sub_tab <- tbl[r, c]
    
    test <- fisher.test(sub_tab)
    
    name <- paste(r[1], r[2], "|", c[1], c[2], sep = " vs ")
    
    results[[name]] <- test
    pvals[name] <- test$p.value
  }
}

# View raw p-values
pvals

or_mat <- matrix(NA,
                 nrow = length(rows),
                 ncol = length(cols),
                 dimnames = list(rows, cols))

for (r in combn(rows, 2, simplify = FALSE)) {
  for (c in combn(cols, 2, simplify = FALSE)) {
    
    sub_tab <- tbl[r, c]
    
    test <- fisher.test(sub_tab)
    or <- test$estimate  # odds ratio
    
    # Assign OR to the "top-left" cell of the 2x2 combination
    or_mat[r[1], c[1]] <- or
  }
}

library(dplyr)

results <- data.frame()

for (r in combn(rows, 2, simplify = FALSE)) {
  for (c in combn(cols, 2, simplify = FALSE)) {
    
    sub_tab <- tbl[r, c]
    test <- fisher.test(sub_tab)
    
    results <- rbind(results, data.frame(
      row1 = r[1],
      row2 = r[2],
      col1 = c[1],
      col2 = c[2],
      odds_ratio = as.numeric(test$estimate),
      p_value = test$p.value
    ))
  }
}

results

library(ggplot2)

results$comparison <- paste(results$row1, "vs", results$row2)
results$cols <- paste(results$col1, "vs", results$col2)

ggplot(results, aes(x = cols, y = comparison, fill = log(odds_ratio))) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "log(OR)"
  ) +
  labs(
    x = "Column Comparison",
    y = "Row Comparison",
    title = "Heatmap of Odds Ratios (2 2 Comparisons)"
  ) +
  theme_minimal()

########################
rows <- rownames(tbl)
cols <- colnames(tbl)

results <- data.frame()

for (r in combn(rows, 2, simplify = FALSE)) {
  for (c in combn(cols, 2, simplify = FALSE)) {
    
    sub_tab <- tbl[r, c]
    
    ft <- fisher.test(sub_tab)
    
    results <- rbind(results, data.frame(
      row_pair = paste(r, collapse = " vs "),
      col_pair = paste(c, collapse = " vs "),
      OR = as.numeric(ft$estimate),
      logOR = log(as.numeric(ft$estimate)),
      p = ft$p.value
    ))
  }
}

library(ggplot2)

ggplot(results, aes(x = col_pair, y = row_pair, fill = logOR)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "log(OR)"
  ) +
  geom_text(aes(label = ifelse(p < 0.05, "*", "")), size = 6) +
  labs(
    x = "Appetite Change (pairwise)",
    y = "Appetite vs weight (pairwise)",
    title = "Fisher-based Odds Ratio Heatmap (3 3 table)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(reshape2)

chisq <- chisq.test(tab)
res <- chisq$stdres   # key object

df_res <- melt(res)

ggplot(df_res, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-3, 3),
    name = "Std Residual"
  ) +
  geom_text(aes(label = round(value, 1)), size = 4) +
  labs(
    x = "Appetite Change",
    y = "Appetite vs weight",
    title = "Chi-square Standardised Residual Heatmap"
  ) +
  theme_minimal()

################# Global Fisher test with chisq residuals #### for visualisation

tbl<-matrix(c(6,14,52,7,42,33,18,33,38),nrow=3,byrow = TRUE,dimnames=list("Weight"=c("underweight","no weight issues","overweight"),"Appetite"=c("Increase","No Change","Decrease")))
tbl
fisher.test(tbl)

chisq <- chisq.test(tbl) # structure
res <- chisq$stdres

library(ggplot2)
library(reshape2)

df_res <- melt(res)
df_res$Appetite <- factor(df_res$Appetite, levels = rev(levels(df_res$Appetite)))


ggplot(df_res, aes(Weight, Appetite, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-5, 5),
    name = "Z residual"
  ) +
  theme_minimal()+
  labs(
    x = "Weight Issues",
    y = "Appetite Change",
    title = "Appetite Change vs Weight Issues"
  )

##
gyny_tbl<-read.csv("Gyny_heatmap.csv",header = TRUE)

rownames(gyny_tbl) <- gyny_tbl[, 1]
gyny_tbl <- gyny_tbl[, -1]

fisher.test(gyny_tbl, simulate.p.value = TRUE, B = 1e6)

chisq <- chisq.test(gyny_tbl) # structure
res <- chisq$stdres

library(ggplot2)
library(reshape2)

df_res <- melt(res)
df_res$Var1 <- factor(df_res$Var1, levels = rev(levels(df_res$Var1)))

ggplot(df_res, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-3.5, 3.5),
    name = "Z residual"
  ) +
  theme_minimal()+
  labs(
    x = "Family Condition(s)",
    y = "Repsondant Condition(s)",
    title = "Repsondant conditions vs Family Conditions"
  )+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

###
conditions_tbl<-read.csv("Conditions.csv",header = TRUE)

rownames(conditions_tbl) <- conditions_tbl[, 1]
conditions_tbl <- conditions_tbl[, -1]

fisher.test(conditions_tbl, simulate.p.value = TRUE, B = 1e6)

chisq <- chisq.test(conditions_tbl) # structure
res <- chisq$stdres

library(ggplot2)
library(reshape2)

df_res <- melt(res)
df_res$Var1 <- factor(df_res$Var1, levels = rev(levels(df_res$Var1)))

ggplot(df_res, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-4, 10),
    name = "Z residual"
  ) +
  theme_minimal()+
  labs(
    x = "Family History (Categories)",
    y = "Combined (self + details)",
    title = "Family History vs Combined"
  )+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

############## performing checks ############
tbl<-matrix(c(33,91,94,92),nrow=2,byrow = TRUE,dimnames=list("Self Conditions (Yes/No)"=c("yes","no"),"Family history (Yes/No)"=c("No","Yes")))
tbl
fisher.test(tbl)

z_score<-sqrt(chisq.test(tbl)$statistic)
z_score
