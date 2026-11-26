# Survey paper complementary data analyses-Rstudio

# Load libraries needed
library(ggplot2)
library(reshape2)
library(clinfun)
library(quantreg)
library(rcompanion)

survey_data<-read.csv("Survey_results_359_19122025.csv")  

#########################################################################

### OBESITY-PLOT and p-VALUES
survey_data_endo<-survey_data[which(survey_data[,3] == "Yes, confirmed by laparoscopic surgery"),]

tbl<-matrix(c(length(which(survey_data_endo[,25] == "Yes, I am/have been overweight" & survey_data_endo[,40] == "Yes")),
              length(which(survey_data_endo[,25] == "Yes, I am/have been overweight" & survey_data_endo[,40] == "No")),
              length(which(survey_data_endo[,25] != "Yes, I am/have been overweight" & survey_data_endo[,40] == "Yes")),
              length(which(survey_data_endo[,25] != "Yes, I am/have been overweight" & survey_data_endo[,40] == "No"))),nrow=2,byrow = TRUE,dimnames=list("Self Obesity"=c("Yes","No"),"Family Obesity"=c("Yes","No")))

p_val <- chisq.test(tbl)$p.value
p_label <- sprintf("p = %.2e", p_val)
z_score<-sqrt(chisq.test(tbl)$statistic)
z_label<- sprintf("z = %.2f", z_score)


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

############################################################################
## MENTAL HEALTH-PLOT and p-VALUE
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


############################################################################
## ALLERGIES- PLOT and p-VALUE
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
  labs(title = "Allergies Percentage")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )+
  annotate("text", x = 0.75, y = 2.3, label = p_label, size = 6, fontface = "bold")+
  annotate("text", x = 0.75, y = 2.15, label = z_label, size = 6, fontface = "bold")


####################################################################################
### DIAGNOSIS GRAPH and p-values
########### Comparing medians over time (not repeated measures) ##############
#### When order matters #########

delayed<-read.csv("Delayed_diagnosis.csv") # read file

# p-value calculation
jonckheere.test(delayed$Delay_of_Diagnosis, as.numeric(as.factor(delayed$Year_bin)), alternative = "decreasing")
pairwise.wilcox.test(delayed$Delay_of_Diagnosis, as.factor(delayed$Year_bin), p.adjust.method = "bonferroni")

# Plot
ggplot(delayed, aes(x = as.factor(delayed$Year_bin), y =Delay_of_Diagnosis )) +
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

# Quantile regression
model <- rq(delayed$Delay_of_Diagnosis ~ as.numeric(as.factor(delayed$Year_bin)), tau = 0.5, data = df)
summary(model, se = "boot")

##########################################################################################################

## APPETITE VS WEIGHT-p-VALUES and GRAPH
tbl<-matrix(c(6,14,52,7,42,33,18,33,38),nrow=3,byrow = TRUE,dimnames=list("Weight"=c("underweight","no weight issues","overweight"),"Appetite"=c("Increase","No Change","Decrease")))
tbl
fisher.test(tbl)

chisq <- chisq.test(tbl) # structure
res <- chisq$stdres

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

##########################################################################

### GYN CONDITIONS:p-VALUES and GRAPH
gyny_tbl<-read.csv("Gyny_heatmap.csv",header = TRUE)

rownames(gyny_tbl) <- gyny_tbl[, 1]
gyny_tbl <- gyny_tbl[, -1]

fisher.test(gyny_tbl, simulate.p.value = TRUE, B = 1e6)

chisq <- chisq.test(gyny_tbl) # structure
res <- chisq$stdres

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
    title = "Respondent conditions vs Family Gyn Conditions"
  )+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


### p-value of if stratified into YES and NO
tbl<-matrix(c(80,36,123,71),nrow=2,byrow = TRUE,dimnames=list("Self Conditions (Yes/No)"=c("yes","no"),"Family history (Yes/No)"=c("No","Yes")))
tbl
fisher.test(tbl)

z_score<-sqrt(chisq.test(tbl)$statistic)
z_score



###################################################################################

### CHRONIC INFL CONDITIONS-p-VALUES and GRAPH
conditions_tbl<-read.csv("Inflammatory_conditions.csv",header = TRUE)

rownames(conditions_tbl) <- conditions_tbl[, 1]
conditions_tbl <- conditions_tbl[, -1]

fisher.test(conditions_tbl, simulate.p.value = TRUE, B = 1e6)

chisq <- chisq.test(conditions_tbl) # structure
res <- chisq$stdres

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


### p-value of if stratified into YES and NO
tbl<-matrix(c(33,91,94,92),nrow=2,byrow = TRUE,dimnames=list("Self Conditions (Yes/No)"=c("yes","no"),"Family history (Yes/No)"=c("No","Yes")))
tbl
fisher.test(tbl)

z_score<-sqrt(chisq.test(tbl)$statistic)
z_score
