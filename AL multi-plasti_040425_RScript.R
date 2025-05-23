# PACKAGES
library(tidyverse);library(writexl);library(dplyr);library(tidyr);library(ggplot2); library(ggpubr)

rm(list=ls())
dodge.posn <- position_dodge(.9)

# PAPERS
theme_angele_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 16,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 16,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour="black"),
                         axis.title.x = element_text(size = 20, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 20, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 16, vjust = 3.3, family="sans", margin = margin(t = 15)),
                         legend.title = element_text(size = 17, vjust = 2, family="sans", margin = margin(t = 11.5)),
                         legend.key = element_blank(),
                         legend.position = "right",
                         legend.spacing.x = unit(0.2, 'cm'),
                         title = element_text(size = 16, family="sans"),
                         strip.text = element_text(size = 15))
# --------------------------------------------------------------------------------------------------------
# Prepare data ---------------------------------------------------------------------------------------

require(openxlsx)
Data <- read.xlsx(xlsxFile="C:\\Users\\alombrey\\Documents\\STATS\\Article 1\\Data.xlsx", sheet=1, colNames=TRUE)

Data$SexRec <- str_sub(Data$SexCombinaison, start = 2)
Data <- Data %>% unite("SexSetting", c(Sex,Setting), sep = "/", remove = FALSE)

# Take out the lines "no focal subject"
Data <- Data[Data$Subject != "No focal subject", ]  
# Take out duplicated lines
Datareduit <- Data[Data$Duplicated != "T", ]  
# Take out the Keepers-directed signals, None and NA in Recipient
Datareduit <- Datareduit[Datareduit$Recipient != "Keepers", ]; Datareduit <- Datareduit[Datareduit$Recipient != "NA", ]; Datareduit <- Datareduit[Datareduit$Recipient != "None", ]
Datareduit <- subset(Datareduit, !is.na(Recipient))

# Create a "Dyad" column ensuring that dyads are treated identically
Datareduit$Dyad <- apply(Datareduit[, c("Subject", "Recipient")], 1, function(row) {
  paste(sort(row), collapse = "/")
})

# In the Data file, the variable for "Multicomponent acts" is called "Multimodality_YN"
# and the variable for "Multisensory acts" is called "ReceptionMultimodality_YN"

# --------------------------------------------------------------------------------------------------------
# Proportion of multimodal signals ---------------------------------------------------------------------------------------

table(Datareduit$Multimodality_YN)
table(Datareduit$ReceptionMultimodality_YN)

datM <- data.frame(datMulti %>% group_by(Subject,Multimodality_YN==1) %>% summarize(count=n())) 
datM <- subset(datM, datM$Multimodality_YN....1=="TRUE") ; datM <- datM[,-2]; colnames(datM) <- c("Subject","MultimodalityCount")
nodatR <- data.frame(datMulti %>% group_by(Subject,Multimodality_YN==0) %>% summarize(count=n())) 
nodatR <- subset(nodatR, nodatR$Multimodality_YN....0=="TRUE") ; nodatR <- nodatR[,-2]; colnames(nodatR) <- c("Subject","NoMultimodalityCount")
datM <- merge(datM, nodatR, by.x = 'Subject', by.y = 'Subject', all.x = TRUE, all.y = TRUE)
datM[is.na(datM)] <- 0
datM$Proportion <- datM$MultimodalityCount*100/(datM$MultimodalityCount+datM$NoMultimodalityCount); colnames(datM) <- c("Subject","MultimodalityCount","NoMultimodalityCount","MultimodalityProp")

datR <- data.frame(datRecMod %>% group_by(Subject,ReceptionMultimodality_YN==1) %>% summarize(count=n())) 
datR <- subset(datR, datR$ReceptionMultimodality_YN....1=="TRUE") ; datR <- datR[,-2]; colnames(datR) <- c("Subject","ReceptionMultimodalityCount")
nodatR <- data.frame(datRecMod %>% group_by(Subject,ReceptionMultimodality_YN==0) %>% summarize(count=n())) 
nodatR <- subset(nodatR, nodatR$ReceptionMultimodality_YN....0=="TRUE") ; nodatR <- nodatR[,-2]; colnames(nodatR) <- c("Subject","NoReceptionMultimodalityCount")
datR <- merge(datR, nodatR, by.x = 'Subject', by.y = 'Subject', all.x = TRUE, all.y = TRUE)
datR[is.na(datR)] <- 0
datR$Proportion <- datR$ReceptionMultimodalityCount*100/(datR$ReceptionMultimodalityCount+datR$NoReceptionMultimodalityCount); colnames(datR) <- c("Subject","ReceptionMultimodalityCount","NoReceptionMultimodalityCount","ReceptionMultimodalityProp")


# --------------------------------------------------------------------------------------------------------
# Nb of communicative acts per setting and per Context ---------------------------------------------------------------------------------------

# Prepare dataset
behaviors_SocialContext <- data.frame(Datareduit %>% group_by(SocialContext,Setting, Context) %>% summarize(count=n())) 
behaviors_SocialContext$Setting[behaviors_SocialContext$Setting == "Captive"] <- "Zoo"
behaviors_SocialContext <- behaviors_SocialContext[behaviors_SocialContext$SocialContext != "None", ]; behaviors_SocialContext <- behaviors_SocialContext[behaviors_SocialContext$SocialContext != "NA", ]; behaviors_SocialContext <- behaviors_SocialContext[behaviors_SocialContext$SocialContext != "other", ] # Take out the lines "None", "NA", "other" dans SocialContext

# Count nb of communicative acts per Context
nb.signals.per.SocialContextm <- data.frame(Datareduit %>% group_by(SocialContext) %>% dplyr::summarize(count=n())) ; colnames(nb.signals.per.SocialContextm) <- c("SocialContext","NsignalsTot") # Compte le nombre total de signaux (=lignes) par SocialContext)
nb.signals.per.SocialContextm$SocialContext <- reorder(nb.signals.per.SocialContextm$SocialContext, -nb.signals.per.SocialContextm$NsignalsTot)
nb.signals.per.SocialContextm <- nb.signals.per.SocialContextm[nb.signals.per.SocialContextm$SocialContext != "NA", ]; nb.signals.per.SocialContextm <- nb.signals.per.SocialContextm[nb.signals.per.SocialContextm$SocialContext != "None", ]; nb.signals.per.SocialContextm <- nb.signals.per.SocialContextm[nb.signals.per.SocialContextm$SocialContext != "other", ]

behaviors_SocialContext <- merge(behaviors_SocialContext, nb.signals.per.SocialContextm, by.x = c('SocialContext'), by.y = c('SocialContext'), all.x = TRUE, all.y = TRUE)
behaviors_SocialContext$SocialContext <- reorder(behaviors_SocialContext$SocialContext, -behaviors_SocialContext$NsignalsTot)

# Split the data into two categories for Social and Non Social 
behaviors_SocialContext$Context <- factor(behaviors_SocialContext$Context, levels = c("Social", "Non social"))

# Plot
ggplot(behaviors_SocialContext, aes(SocialContext, count)) + 
  geom_bar(stat = "identity", position = "stack", aes(fill = Setting)) + 
  theme_angele_ss + 
  scale_x_discrete("Context") + 
  scale_y_continuous("Number of communicative acts", breaks = seq(0, 10000, by = 200)) + 
  scale_fill_manual(values = c("#ed6651", "#007ABB")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Context, scales = "free_x", strip.position = "bottom") + # Use free_x to control x axis labels independently
  theme_angele_ss + 
  theme(strip.background = element_rect(fill = "white", color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        )


#----------------------------------------------------------------------------------------------------------
# Multicomponent acts combinations/types -------------------------------------------------------------------

MultiCombi <- read.xlsx(xlsxFile="C:\\Users\\alombrey\\Documents\\STATS\\Article 1\\MultiCombi.xlsx", sheet=1, colNames=TRUE)

# Take out the lines "no focal subject"
MultiCombi <- MultiCombi[MultiCombi$Subject != "No focal subject", ]  
# Take out the lines "None", "NA" dans Multimodality
MultiCombi <- MultiCombi[MultiCombi$Multimodality != "None", ]; MultiCombi <- MultiCombi[MultiCombi$Multimodality != "NA", ] 
MultiCombi <- subset(MultiCombi, !is.na(Multimodality))
# Take out duplicated lines
MultiCombireduit <- MultiCombi[MultiCombi$Duplicated != "T", ]  
# Take out the Keepers-directed signals
MultiCombireduit <- MultiCombireduit[MultiCombireduit$Recipient != "Keepers", ]
MultiCombireduit <- subset(MultiCombireduit, !is.na(Recipient))

MultiCombireduit$Setting[MultiCombireduit$Setting == "Captive"] <- "Zoo"

# Omit all levels of Subject that contributed fewer than 2 cases 
nb.signals.per.ind <- data.frame(MultiCombireduit %>% group_by(Subject) %>% dplyr::summarize(count=n())) ; colnames(nb.signals.per.ind) <- c("Subject","NsignalsTot") # Compte le nombre total de signaux (=lignes) par individu)
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$NsignalsTot>=2) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
MultiCombireduit <- subset(MultiCombireduit, Subject %in% nb.signals.per.ind$Subject)

# Subset data 
MultiCombiSub <- subset(MultiCombireduit, select = c("ObservationID", "Subject", "SocialContext","Body","Body 2", "Facial", "Manual", "Manual 2", "Vocal","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison"))
MultiCombiSub$Body= ifelse(MultiCombiSub$Body== "T", TRUE, FALSE)
MultiCombiSub$`Body 2`= ifelse(MultiCombiSub$`Body 2`== "T", TRUE, FALSE)
MultiCombiSub$Facial= ifelse(MultiCombiSub$Facial== "T", TRUE, FALSE)
MultiCombiSub$Manual= ifelse(MultiCombiSub$Manual== "T", TRUE, FALSE)
MultiCombiSub$`Manual 2`= ifelse(MultiCombiSub$`Manual 2`== "T", TRUE, FALSE)
MultiCombiSub$Vocal= ifelse(MultiCombiSub$Vocal== "T", TRUE, FALSE)

# Create a new column for combinations
MultiCombiSub$Combination <- apply(MultiCombiSub[, c("Body", "Body 2", "Facial", "Manual", "Manual 2", "Vocal")], 1, function(row) {
  names <- c("Body", "Body 2", "Facial", "Manual", "Manual 2", "Vocal")
  true_columns <- names[row]
  if (length(true_columns) > 1) {
    paste(true_columns, collapse = "+")
  } else {NA}})
# Remove rows with NA in the Combination column
MultiCombiSub <- MultiCombiSub[complete.cases(MultiCombiSub), ]
# Count occurrences for each combination by Subject
combination_counts <- table(MultiCombiSub$Subject, MultiCombiSub$Combination)
# View the resulting counts
print(combination_counts)
# Calculate total combinations for each subject
total_combinations <- rowSums(combination_counts)
# Create proportion columns for each combination
proportion_columns <- apply(combination_counts, 2, function(combination_count) {combination_count / total_combinations})
proportion_columns <- as.data.frame(proportion_columns)

proportion_columns$Subject <- rownames(proportion_columns) # put the row names as a column (Subject)
proportion_columns <- proportion_columns %>% relocate(Subject, .before = `Body+Body 2`)

# Put the combination values in long format (combination --> column)
library(data.table) ; library(stringr)
setDT(proportion_columns)
proportion_columns = melt(proportion_columns, id.vars=c("Subject"), variable.name = "Combination", value.name="Proportion")

IDinfo <- MultiCombiSub[,c("Subject","Group","Setting","Sex","Age","Rank")] # Extract individual info from dataset
IDinfo <- IDinfo[!duplicated(IDinfo), ] # Take out the duplicated lines
proportion_columns <- merge(proportion_columns, IDinfo, by.x = "Subject", by.y = "Subject", all.x = TRUE, all.y = TRUE)
total_combinations <- as.data.frame(total_combinations)
total_combinations$Subject <- rownames(total_combinations)
proportion_columns <- merge(proportion_columns, total_combinations, by.x = c('Subject'), by.y = c('Subject'), all.x = TRUE, all.y = TRUE)

# Set order
proportion_columns$Combination <- factor(proportion_columns$Combination, levels = c("Body+Body 2","Body+Facial","Body+Manual","Body+Vocal","Facial+Manual","Facial+Vocal","Manual+Manual 2","Manual+Vocal","Body+Facial+Manual","Body+Facial+Vocal","Body+Manual+Vocal","Facial+Manual+Vocal","Body+Facial+Manual+Vocal"))

PointSizeM <- sqrt(proportion_columns$total_combinations) / max(sqrt(proportion_columns$total_combinations)) * 5

install.packages("viridis")
library("viridis")

MSet <- ggplot(proportion_columns, aes(x = Setting, y = Proportion)) + 
  stat_summary(aes(fill = Combination, color = Combination),
               fun = mean, geom = "point", shape = 21,
               size = 5, position = position_dodge(width = 0.92)) +
  scale_fill_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  scale_color_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  stat_summary(aes(fill = Combination), fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.5, 
               position = position_dodge(width = 0.92)) +
  theme_angele_ss + 
  ggtitle("(b)") +
  scale_x_discrete("Setting", expand = expansion(mult = c(0.5, 0.5))) +
  scale_y_continuous("", breaks = seq(0, 1, by = 0.1)) +
  guides(fill = guide_legend(title = "Types of signal combination", ncol = 4), color = "none") +
  theme(
    legend.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")) +
  geom_point(aes(size = PointSizeM, fill = Combination),position= position_dodge(0.92), shape = 1, colour = "black", alpha = 0.25, show.legend = FALSE)

MSex <- ggplot(proportion_columns, aes(Sex, Proportion)) + 
  stat_summary(aes(fill = Combination, color = Combination),
               fun = mean, geom = "point", shape = 21,
               size = 5, position = position_dodge(width = 0.92)) +
  stat_summary(aes(fill = Combination), fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.5, 
               position = position_dodge(width = 0.92)) +
  theme_angele_ss + 
  ggtitle("(a)") +
  scale_x_discrete("Sex", labels=c("Females","Males"), expand = expansion(mult = c(0.5, 0.5))) +
  scale_y_continuous("Proportion of MC acts", breaks = seq(0, 1, by=0.1)) +
  scale_fill_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  scale_color_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  guides(fill=guide_legend(title="Types of signal combination", ncol=4), color = "none") +
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +  
  geom_point(aes(size = PointSizeM, fill = Combination),position= position_dodge(0.92), shape = 1, colour = "black", alpha = 0.25, show.legend = FALSE)

library(gridExtra)
library(grid)
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- get_legend(MSex)
grid.arrange(arrangeGrob(MSex + theme(legend.position="none"),
                         MSet + theme(legend.position="none"), ncol=2, widths = c(2, 2)), mylegend, nrow = 3, heights = c(4, 2, 1))

# Extract values
# Count occurrences for each combination
combination_counts_tot <- as.data.frame(table(MultiCombiSub$Combination)); colnames(combination_counts_tot) <- c("Combination","Count")
sum(combination_counts_tot$Count)
combination_counts_tot$Prop <- combination_counts_tot$Count/1336

# Mean individual prop of each combination
data.frame(proportion_columns %>% group_by(Combination) %>% summarize(meanprop = mean(Proportion)))

# Mean/Max/Min individual prop per group for each combination
Minfo <- data.frame(proportion_columns %>% group_by(Combination,Group) %>% summarize(meanprop = mean(Proportion))) 
Mmax <- data.frame(proportion_columns %>% group_by(Combination,Group) %>% summarize(maxprop = max(Proportion)))
Mmin <- data.frame(proportion_columns %>% group_by(Combination,Group) %>% summarize(minprop = max(Proportion)))
Minfo <- merge(Minfo, Mmax, by.x = c('Combination','Group'), by.y = c('Combination','Group'), all.x = TRUE, all.y = TRUE)
Minfo <- merge(Minfo, Mmin, by.x = c('Combination','Group'), by.y = c('Combination','Group'), all.x = TRUE, all.y = TRUE)

# --------------------------------------------------------------------------------------------------------
# Multisensory acts combinations ---------------------------------------------------------------------------------------

# Subset data 
DatareduitSub <- subset(Datareduit, select = c("ObservationID", "Subject", "SocialContext","ReceptionMultimodality_YN","Auditory", "Seismic", "Tactile", "Visual","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison"))
DatareduitSub$Auditory= ifelse(DatareduitSub$Auditory== "T", TRUE, FALSE)
DatareduitSub$Seismic= ifelse(DatareduitSub$Seismic== "T", TRUE, FALSE)
DatareduitSub$Tactile= ifelse(DatareduitSub$Tactile== "T", TRUE, FALSE)
DatareduitSub$Visual= ifelse(DatareduitSub$Visual== "T", TRUE, FALSE)

# Take out lines with "None", "NA" in ReceptionMultimodality_YN
table(DatareduitSub$ReceptionMultimodality_YN, useNA="ifany")
DatareduitSub <- DatareduitSub[DatareduitSub$ReceptionMultimodality_YN != "NA", ]
DatareduitSub <- subset(DatareduitSub, !is.na(ReceptionMultimodality_YN))

# Omit all levels of Subject that contributed fewer than 2 cases 
nb.signals.per.ind <- data.frame(DatareduitSub %>% group_by(Subject) %>% dplyr::summarize(count=n())) ; colnames(nb.signals.per.ind) <- c("Subject","NsignalsTot") # Compte le nombre total de signaux (=lignes) par individu)
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$NsignalsTot>=2) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
DatareduitSub <- subset(DatareduitSub, Subject %in% nb.signals.per.ind$Subject)

DatareduitSub$Setting[DatareduitSub$Setting == "Captive"] <- "Zoo"

# Create a new column for combinations
DatareduitSub$Combination <- apply(DatareduitSub[, c("Auditory", "Seismic", "Tactile", "Visual")], 1, function(row) {
  names <- c("Auditory", "Seismic", "Tactile", "Visual")
  true_columns <- names[row]
  if (length(true_columns) > 1) {
    paste(true_columns, collapse = "+")
  } else {NA}})
# Remove rows with NA in the Combination column
DatareduitSub <- DatareduitSub[complete.cases(DatareduitSub), ]
# Count occurrences for each combination by Subject
RMcombination_counts <- table(DatareduitSub$Subject, DatareduitSub$Combination)
# View the resulting counts
print(RMcombination_counts)
# Calculate total combinations for each subject
total_RMcombinations <- rowSums(RMcombination_counts)
# Create proportion columns for each combination
RMproportion_columns <- apply(RMcombination_counts, 2, function(combination_count) {combination_count / total_RMcombinations})
RMproportion_columns <- as.data.frame(RMproportion_columns)

RMproportion_columns$Subject <- rownames(RMproportion_columns) # put the row names as a column (Subject)
RMproportion_columns <- RMproportion_columns %>% relocate(Subject, .before = `Auditory+Seismic+Visual`)

# Put the combination values in long format (combination --> column)
library(data.table) ; library(stringr)
setDT(RMproportion_columns)
RMproportion_columns = melt(RMproportion_columns, id.vars=c("Subject"), variable.name = "Combination", value.name="Proportion")
RMproportion_columns[, Combination:=str_replace(Combination, ".Close", "")]

IDinfo <- DatareduitSub[,c("Subject","Group","Setting","Sex","Age","Rank")] 
IDinfo <- IDinfo[!duplicated(IDinfo), ] # Take out the lines en double
RMproportion_columns <- merge(RMproportion_columns, IDinfo, by.x = "Subject", by.y = "Subject", all.x = TRUE, all.y = TRUE)
total_RMcombinations <- as.data.frame(total_RMcombinations)
total_RMcombinations$Subject <- rownames(total_RMcombinations)
RMproportion_columns <- merge(RMproportion_columns, total_RMcombinations, by.x = c('Subject'), by.y = c('Subject'), all.x = TRUE, all.y = TRUE)

# Set order
RMproportion_columns$Combination <- factor(RMproportion_columns$Combination, levels = c("Auditory+Tactile","Auditory+Visual","Seismic+Tactile","Seismic+Visual","Tactile+Visual","Auditory+Seismic+Visual","Auditory+Tactile+Visual","Seismic+Tactile+Visual"))

PointSizeRM <- sqrt(RMproportion_columns$total_RMcombinations) / max(sqrt(RMproportion_columns$total_RMcombinations)) * 5

RMSet <- ggplot(RMproportion_columns, aes(x = Setting, y = Proportion)) + 
  stat_summary(aes(fill = Combination, color = Combination),
               fun = mean, geom = "point", shape = 21,
               size = 5, position = position_dodge(width = 0.92)) +
  scale_fill_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  scale_color_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  stat_summary(aes(fill = Combination), fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.5, 
               position = position_dodge(width = 0.92)) +
  theme_angele_ss + 
  ggtitle("(b)") +
  scale_x_discrete("Setting", expand = expansion(mult = c(0.5, 0.5))) +
  scale_y_continuous("", breaks = seq(0, 1, by = 0.1)) +
  guides(fill = guide_legend(title = "Types of signal combination", ncol = 2),color = "none") +
  theme(
    legend.title = element_text(hjust = 0.5),
    legend.spacing.x = unit(2, "cm"),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")) +
  geom_point(aes(size = PointSizeRM, fill = Combination),position= position_dodge(0.92), shape = 1, colour = "black", alpha = 0.25, show.legend = FALSE)

RMSex <- ggplot(RMproportion_columns, aes(Sex, Proportion)) + 
  stat_summary(aes(fill = Combination, color = Combination),
               fun = mean, geom = "point", shape = 21,
               size = 5, position = position_dodge(width = 0.92)) +
  stat_summary(aes(fill = Combination), fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.5, 
               position = position_dodge(width = 0.92)) +
  theme_angele_ss + 
  ggtitle("(a)") +
  scale_x_discrete("Sex", labels=c("Females","Males"), expand = expansion(mult = c(0.5, 0.5))) +
  scale_y_continuous("Proportion of MC acts", breaks = seq(0, 1, by=0.1)) +
  scale_fill_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  scale_color_viridis(option = "C", discrete = "TRUE", direction = -1, begin = 0.3) +
  guides(fill=guide_legend(title="Types of signal combination", ncol=2)) +
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +  
  geom_point(aes(size = PointSizeRM, fill = Combination),position= position_dodge(0.92), shape = 1, colour = "black", alpha = 0.25, show.legend = FALSE)

get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- get_legend(RMSet)
grid.arrange(arrangeGrob(RMSex + theme(legend.position="none"),
                         RMSet + theme(legend.position="none"), ncol=2, widths = c(2, 2)), mylegend, nrow = 2, heights = c(4, 2))

# Extract values
# Count occurrences for each combination
RMcombination_counts_tot <- as.data.frame(table(DatareduitSub$Combination)); colnames(RMcombination_counts_tot) <- c("Combination","Count")
sum(RMcombination_counts_tot$Count)
RMcombination_counts_tot$Prop <- RMcombination_counts_tot$Count/2234

# Mean individual prop of each combination
data.frame(RMproportion_columns %>% group_by(Combination) %>% summarize(meanprop = mean(Proportion)))

# Mean/Max/Min individual prop per group of each combination
RMinfo <- data.frame(RMproportion_columns %>% group_by(Combination,Group) %>% summarize(meanprop = mean(Proportion))) 
RMmax <- data.frame(RMproportion_columns %>% group_by(Combination,Group) %>% summarize(maxprop = max(Proportion)))
RMmin <- data.frame(RMproportion_columns %>% group_by(Combination,Group) %>% summarize(minprop = max(Proportion)))
RMinfo <- merge(RMinfo, RMmax, by.x = c('Combination','Group'), by.y = c('Combination','Group'), all.x = TRUE, all.y = TRUE)
RMinfo <- merge(RMinfo, RMmin, by.x = c('Combination','Group'), by.y = c('Combination','Group'), all.x = TRUE, all.y = TRUE)

#---------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------
# BLUP MULTICOMPONENT ACTS
#---------------------------------------------------------------------------------------------------------------------------------------------

# PACKAGES
library(tidyverse); library(dplyr);library(writexl);library(tidyr);library(ggplot2)
library(lme4);library(arm);library(MuMIn)
library(broom);library(coda);library(grid)
library(gridExtra);library(brms); library(broom.mixed); library(merTools);library(rptR)
library(tidybayes);library(parallel);library(car);library(RColorBrewer)

dodge.posn <- position_dodge(.9)

col.MCaptive <- "#004bbb" #brewer.pal(n = 10, name = "RdBu")[1]
col.FCaptive <- "#02b2e8" #brewer.pal(n = 10, name = "RdBu")[5]
col.MSanctuary <- "#941501" #brewer.pal(n = 10, name = "RdBu")[6]
col.FSanctuary <- "#ed6651" #brewer.pal(n = 10, name = "RdBu")[7]

contr <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

# extract legend function
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# --------------------------------------------------------------------------------------------------------
# Multicomponent acts ---------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# Prepare data ---------------------------------------------------------------------------------------

# Subset data for Multimodality (= Multicomponent acts)--- 
datMulti <- subset(Datareduit, select = c("ObservationID", "Subject", "Context", "SocialContext","Multimodality_YN","RecAttentionState_YN","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison","SexRec","SexSetting","Dyad"))
# Take out lines with "None", "NA" in Multimodality_YN
table(datMulti$Multimodality_YN, useNA="ifany")
datMulti <- datMulti[datMulti$Multimodality_YN != "NA", ]
datMulti <- subset(datMulti, !is.na(Multimodality_YN))
# Take out the lines "None", "NA" dans Context
table(datMulti$Context, useNA="ifany")
datMulti <- datMulti[datMulti$Context != "NA", ]
datMulti <- subset(datMulti, !is.na(Context))
# Take out the lines "NA" dans RecAttentionState_YN
table(datMulti$RecAttentionState_YN, useNA="ifany")
datMulti <- datMulti[datMulti$RecAttentionState_YN != "NA", ]
datMulti <- subset(datMulti, !is.na(RecAttentionState_YN))
# Take out the lines "None", "NA" dans ASO
table(datMulti$ASO, useNA="ifany")
datMulti <- datMulti[datMulti$ASO != "NA", ]; datMulti <- datMulti[datMulti$ASO != "None", ]
datMulti <- subset(datMulti, !is.na(ASO))

datMulti$Multimodality_YN <- as.numeric(datMulti$Multimodality_YN)

# Omit Subjects that contribute less than 2 cases to each context
nb.signals.per.ind <- datMulti %>%
  group_by(Subject, Context) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Context, values_from = Count, values_fill = list(Count = 0))
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$Social>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$`Non social`>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
datMulti <- subset(datMulti, Subject %in% nb.signals.per.ind$Subject) 

table(datMulti$Multimodality_YN,datMulti$Group)
#      A    B   C2   C4    L
# 0 1194  270 2305  627  971
# 1  209   46  709   44  118
table(datMulti$Multimodality_YN,datMulti$Context)
#   Non social Social
# 0       3822   1545
# 1        967    159

# switch chr to factors
datMulti$Subject <- as.factor(datMulti$Subject)
datMulti$Recipient <- as.factor(datMulti$Recipient)
datMulti$Sex <- as.factor(datMulti$Sex)
datMulti$Group <- as.factor(datMulti$Group)
datMulti$Setting <- as.factor(datMulti$Setting)
datMulti$Dyad <- as.factor(datMulti$Dyad)
#datMulti$Context <- ifelse(datMulti$Context=="Social",0,1)
#datMulti$Context <- as.numeric(datMulti$Context)
datMulti$Context <- as.factor(datMulti$Context)
datMulti$RecAttentionState_YN <- as.factor(datMulti$RecAttentionState_YN)
datMulti$ASO <- as.factor(datMulti$ASO)

# Check collinearity 
vif(lm(Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN, data = datMulti))
# scale(Age)                  Sex              Setting              Context RecAttentionState_YN 
#   1.183073             1.126997             1.110915             1.075583             1.058408 

# Random intercept model
ri_M <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                (1|Subject) + (1|Recipient) + (1|ASO), family = binomial, data = datMulti, control = contr)   # random effects

length(residuals(ri_M)) # 6493 ok 

# Fit "null models" without random intercepts & slope for individual identity
ri_M.null <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                     (1|Recipient) + (1|ASO), family = binomial, data = datMulti, control = contr)

summary(ri_M)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -2.72050    0.34612  -7.860 3.84e-15 ***
# scale(Age)             0.06563    0.07299   0.899  0.36854    
# SexM                  -0.33731    0.16183  -2.084  0.03713 *  
# SettingSanctuary       0.49729    0.17608   2.824  0.00474 ** 
# ContextSocial          0.34557    0.12428   2.781  0.00543 ** 
# RecAttentionState_YNT  0.14939    0.13396   1.115  0.26476    
  

as.data.frame(anova(ri_M.null, ri_M, test="LTR")) # test the full (ri_M) against the null (ri_M.null) model using a likelihood ratio test
#          npar      AIC      BIC    logLik deviance    Chisq Df  Pr(>Chisq)
# ri_M.null    8 5103.876 5158.104 -2543.938 5087.876      NA NA           NA
# ri_M         9 5041.326 5102.332 -2511.663 5023.326 64.5505  1 9.408963e-16 --> p-value<0.05 so ri_M is better than ri_M.null 

# Calculate repeatability using rptR --- i.e. variance standardized individual variation in focal behavior
##install.packages("devtools");devtools::install_github("mastoffel/rptR", build_vignettes = TRUE);library(rptR)
rep_ri <- rpt(Multimodality_YN ~ scale(Age) + Sex + Setting + Context  + RecAttentionState_YN +
                (1|Subject) + (1|Recipient) + (1|ASO), grname = c("Subject","Recipient","ASO"), data = datMulti, datatype = "Binary", nboot = 1000, 
              npermut = 0, adjusted = FALSE)
print(rep_ri)
#Repeatability estimation using the glmm method and logit link 

#Repeatability for Subject
#--------------------------------
#  Link-scale approximation:
#R  = 0.025
#SE = 0.008
#CI = [0.008, 0.037]
#P  = 4.7e-16 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.024
#SE = 0.007
#CI = [0.007, 0.035]
#P  = 4.7e-16 [LRT]
#NA [Permutation]

#Repeatability for Recipient
#--------------------------------
#  Link-scale approximation:
#  R  = 0.023
#SE = 0.007
#CI = [0.006, 0.036]
#P  = 7.1e-13 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.022
#SE = 0.007
#CI = [0.006, 0.033]
#P  = 7.1e-13 [LRT]
#NA [Permutation]

#Repeatability for ASO
#--------------------------------
#  Link-scale approximation:
#  R  = 0.156
#SE = 0.058
#CI = [0.038, 0.257]
#P  = 7.28e-83 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.147
#SE = 0.051
#CI = [0.038, 0.227]
#P  = 7.28e-83 [LRT]
#NA [Permutation]



# Plot individual differences ---
randomSimsM <- REsim(ri_M, n.sims = 1000)

# Add info on the individuals
randomSimsM <- merge(randomSimsM[randomSimsM$groupFctr=="Subject",],
                     datMulti[!duplicated(datMulti$Subject),c("Subject","Group","Setting","Age","Sex","SexSetting")], # + Rank
                     by.x = "groupID",by.y="Subject")

# Add identifier to color individuals uniquely
randomSimsM$SexSetting <- factor(randomSimsM$SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary"))
randomSimsM$COL <- ifelse(randomSimsM$SexSetting=="M/Captive",col.MCaptive,ifelse(randomSimsM$SexSetting=="F/Captive",col.FCaptive,ifelse(randomSimsM$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(randomSimsM$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
cols <- c("M/Captive" = col.MCaptive, "F/Captive" = col.FCaptive, "M/Sanctuary" = col.MSanctuary, "F/Sanctuary" = col.FSanctuary)

# Add population intercept and site specific differences for easier interpretation of realized sequence use
randomSimsM[randomSimsM$Setting == "Captive",]$mean <- randomSimsM[randomSimsM$Setting == "Captive",]$mean + fixef(ri_M)["(Intercept)"]
randomSimsM[randomSimsM$Setting == "Sanctuary",]$mean <- randomSimsM[randomSimsM$Setting == "Sanctuary",]$mean + (fixef(ri_M)["(Intercept)"] + fixef(ri_M)["SettingSanctuary"])

# Sort data by value and group
table(randomSimsM$SexSetting)
sort.order <- c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")
randomSimsM <- arrange(randomSimsM, mean)
randomSimsM <- arrange(randomSimsM, factor(SexSetting, levels = sort.order))

# Tell ggplot that you have an ordered factor already
randomSimsM$groupID <- factor(randomSimsM$groupID, levels = randomSimsM$groupID) #for ordering: [order(randomSimsM$mean)]

theme_angele_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 16,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 16,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour="black"),
                         axis.title.x = element_text(size = 20, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 20, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 16, vjust = 3.3, family="sans", margin = margin(t = 15)),
                         legend.title = element_text(size = 17, vjust = 2, family="sans", margin = margin(t = 11.5)),
                         legend.key = element_blank(),
                         legend.position = "left",
                         legend.spacing.x = unit(0.2, 'cm'),
                         title = element_text(size = 16, family="sans"),
                         strip.text = element_text(size = 15))
# Set y-axis limits and breaks
y_limits <- c(0, 0.25)
y_breaks <- seq(0, 1, by = 0.1)

BLUPm <- ggplot() +
  geom_errorbar(data = randomSimsM, aes(x = groupID, ymin = plogis(mean-sd), ymax = plogis(mean+sd), col=SexSetting)) +
  geom_point(data = randomSimsM, aes(x = groupID, y = plogis(mean), col=SexSetting), size = 2) +
  # geom_vline(xintercept = c(4.5,12.5,17.5), linetype=2) + 
  ggtitle("(b)") +
  theme_angele_ss +
  scale_y_continuous("Multicomponent acts", limits = y_limits, breaks = y_breaks) +
  xlab("Subject") +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  # annotate("text",x = 3, y = 0.4, label = "R = 0.1", size = 4) +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary")) +
  theme(plot.title = element_text(face = "bold"))

print(BLUPm)


#---------------------------------------------------------------------------------------------------------------------------------------------------
# BLUP MULTISENSORY ACTS
#---------------------------------------------------------------------------------------------------------------------------------------------
# Multisensory acts ---------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# Prepare data ---------------------------------------------------------------------------------------

# Subset data for ReceptionMultimodality_YN (= Multisensory acts)--- 
datRecMod <- subset(Datareduit, select = c("ObservationID", "Subject", "Context", "SocialContext","ReceptionMultimodality_YN","RecAttentionState_YN","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison","SexRec","SexSetting","Dyad"))
# Take out lines with "None", "NA" in ReceptionMultimodality_YN
table(datRecMod$ReceptionMultimodality_YN, useNA="ifany")
datRecMod <- datRecMod[datRecMod$ReceptionMultimodality_YN != "NA", ]
datRecMod <- subset(datRecMod, !is.na(ReceptionMultimodality_YN))
# Take out the lines "None", "NA" dans Context
table(datRecMod$Context, useNA="ifany")
datRecMod <- datRecMod[datRecMod$Context != "NA", ]
datRecMod <- subset(datRecMod, !is.na(Context))
# Take out the lines "NA" dans RecAttentionState_YN
table(datRecMod$RecAttentionState_YN, useNA="ifany")
datRecMod <- datRecMod[datRecMod$RecAttentionState_YN != "NA", ]
datRecMod <- subset(datRecMod, !is.na(RecAttentionState_YN))
# Take out the lines "None", "NA" dans ASO
table(datRecMod$ASO, useNA="ifany")
datRecMod <- datRecMod[datRecMod$ASO != "NA", ]; datRecMod <- datRecMod[datRecMod$ASO != "None", ]
datRecMod <- subset(datRecMod, !is.na(ASO))

datRecMod$ReceptionMultimodality_YN <- as.numeric(datRecMod$ReceptionMultimodality_YN)

# Omit Subjects that contribute less than 2 cases to each context
nb.signals.per.ind <- datRecMod %>%
  group_by(Subject, Context) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Context, values_from = Count, values_fill = list(Count = 0))
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$Social>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$`Non social`>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
datRecMod <- subset(datRecMod, Subject %in% nb.signals.per.ind$Subject) 

table(datRecMod$ReceptionMultimodality_YN,datRecMod$Group)
#      A    B   C2   C4    L
# 0  748  216 1569  442  752
# 1  660  100 1444  232  355
table(datRecMod$ReceptionMultimodality_YN,datRecMod$Context)
#   Non social Social
# 0       2673   1054
# 1       2138    653

# switch chr to factors
datRecMod$Subject <- as.factor(datRecMod$Subject)
datRecMod$Recipient <- as.factor(datRecMod$Recipient)
datRecMod$Sex <- as.factor(datRecMod$Sex)
datRecMod$Group <- as.factor(datRecMod$Group)
datRecMod$Setting <- as.factor(datRecMod$Setting)
datRecMod$Dyad <- as.factor(datRecMod$Dyad)
#datRecMod$Context <- ifelse(datRecMod$Context=="Social",0,1)
#datRecMod$Context <- as.numeric(datRecMod$Context)
datRecMod$Context <- as.factor(datRecMod$Context)
datRecMod$RecAttentionState_YN <- as.factor(datRecMod$RecAttentionState_YN)
datRecMod$ASO <- as.factor(datRecMod$ASO)


# Check collinearity 
vif(lm(ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN, data = datRecMod))
# scale(Age)                  Sex              Setting              Context RecAttentionState_YN 
#   1.185031             1.128011             1.112418             1.075957             1.058768 

# Random intercept model
ri_RM <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                 (1|Subject) + (1|Recipient) + (1|ASO), family = binomial, data = datRecMod, control = contr)   # random effects

length(residuals(ri_RM)) # 6517 ok 

# Fit "null models" without random intercepts & slope for individual identity
ri_RM.null <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                      (1|Recipient) + (1|ASO), family = binomial, data = datRecMod, control = contr)

summary(ri_RM)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -3.34140    0.30899 -10.814   <2e-16 ***
# scale(Age)             0.07675    0.05706   1.345   0.1786    
# SexM                  -0.07964    0.12595  -0.632   0.5272    
# SettingSanctuary       0.29266    0.13186   2.219   0.0265 *  
# ContextSocial          0.18913    0.09741   1.942   0.0522 .  
# RecAttentionState_YNT  2.81508    0.13219  21.296   <2e-16 ***


as.data.frame(anova(ri_RM.null, ri_RM, test="LTR")) # test the full (ri_RM) against the null (ri_RM.null) model using a likelihood ratio test
#         npar      AIC      BIC    logLik deviance    Chisq Df  Pr(>Chisq)
# ri_RM.null   8 7508.616 7562.873 -3746.308 7492.616       NA NA           NA
# ri_RM        9 7437.807 7498.847 -3709.904 7419.807 72.80818  1 1.428831e-17 --> p-value<0.05 so ri_RM is better than ri_RM.null 

# Calculate repeatability using rptR --- i.e. variance standardized individual variation in focal behavior
##install.packages("devtools");devtools::install_github("mastoffel/rptR", build_vignettes = TRUE);library(rptR)
datRecMod <- na.omit(datRecMod)
rep_ri <- rpt(ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                (1|Subject) + (1|Recipient) + (1|ASO), grname = c("Subject", "Recipient","ASO"), data = datRecMod, datatype = "Binary", nboot = 1000, 
              npermut = 0, adjusted = FALSE)
print(rep_ri)
#Repeatability estimation using the glmm method and logit link 

#Repeatability for Subject
#--------------------------------
#  Link-scale approximation:
#  R  = 0.024
#SE = 0.006
#CI = [0.012, 0.035]
#P  = 7.97e-18 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.015
#SE = 0.004
#CI = [0.007, 0.024]
#P  = 7.97e-18 [LRT]
#NA [Permutation]

#Repeatability for Recipient
#--------------------------------
#  Link-scale approximation:
#  R  = 0.014
#SE = 0.005
#CI = [0.004, 0.023]
#P  = 1.17e-12 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.008
#SE = 0.003
#CI = [0.003, 0.015]
#P  = 1.17e-12 [LRT]
#NA [Permutation]

#Repeatability for ASO
#--------------------------------
#  Link-scale approximation:
#  R  = 0.176
#SE = 0.051
#CI = [0.073, 0.27]
#P  = 7.59e-122 [LRT]
#NA [Permutation]

#Original-scale approximation:
#  R  = 0.105
#SE = 0.03
#CI = [0.051, 0.167]
#P  = 7.59e-122 [LRT]
#NA [Permutation]



# Plot individual differences ---
randomSimsRM <- REsim(ri_RM, n.sims = 1000)

# Add info on the individuals
randomSimsRM <- merge(randomSimsRM[randomSimsRM$groupFctr=="Subject",],
                      datRecMod[!duplicated(datRecMod$Subject),c("Subject","Group","Setting","Age","Sex","SexSetting")], # + Rank
                      by.x = "groupID",by.y="Subject")

# Add identifier to color individuals uniquely
randomSimsRM$SexSetting <- factor(randomSimsRM$SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary"))
randomSimsRM$COL <- ifelse(randomSimsRM$SexSetting=="M/Captive",col.MCaptive,ifelse(randomSimsRM$SexSetting=="F/Captive",col.FCaptive,ifelse(randomSimsRM$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(randomSimsRM$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
cols <- c("M/Captive" = col.MCaptive, "F/Captive" = col.FCaptive, "M/Sanctuary" = col.MSanctuary, "F/Sanctuary" = col.FSanctuary)

# Add population intercept and site specific differences for easier interpretation of realized sequence use
randomSimsRM[randomSimsRM$Setting == "Captive",]$mean <- randomSimsRM[randomSimsRM$Setting == "Captive",]$mean + fixef(ri_RM)["(Intercept)"]
randomSimsRM[randomSimsRM$Setting == "Sanctuary",]$mean <- randomSimsRM[randomSimsRM$Setting == "Sanctuary",]$mean + (fixef(ri_RM)["(Intercept)"] + fixef(ri_RM)["SettingSanctuary"])

# Sort data by value and group
table(randomSimsRM$SexSetting)
sort.order <- c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")
randomSimsRM <- arrange(randomSimsRM, mean)
randomSimsRM <- arrange(randomSimsRM, factor(SexSetting, levels = sort.order))

# Tell ggplot that you have an ordered factor already
randomSimsRM$groupID <- factor(randomSimsRM$groupID, levels = randomSimsRM$groupID) #for ordering: [order(randomSimsRM$mean)]

theme_angele_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 16,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 16,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour="black"),
                         axis.title.x = element_text(size = 20, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 20, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 16, vjust = 3.3, family="sans", margin = margin(t = 15)),
                         legend.title = element_text(size = 17, vjust = 2, family="sans", margin = margin(t = 11.5)),
                         legend.key = element_blank(),
                         legend.position = "left",
                         legend.spacing.x = unit(0.2, 'cm'),
                         title = element_text(size = 16, family="sans"),
                         strip.text = element_text(size = 15))
# Set y-axis limits and breaks
y_limits <- c(0, 0.25)
y_breaks <- seq(0, 1, by = 0.1)

BLUPrm <- ggplot() +
  geom_errorbar(data = randomSimsRM, aes(x = groupID, ymin = plogis(mean-sd), ymax = plogis(mean+sd), col=SexSetting)) +
  geom_point(data = randomSimsRM, aes(x = groupID, y = plogis(mean), col=SexSetting), size = 2) +
  # geom_vline(xintercept = c(4.5,12.5,17.5), linetype=2) + 
  ggtitle("(a)") +
  theme_angele_ss +
  scale_y_continuous("Multisensory acts", limits = y_limits, breaks = y_breaks) +
  xlab("Subject") +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  # annotate("text",x = 3, y = 0.4, label = "R = 0.1", size = 4) +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary")) +
  theme(plot.title = element_text(face = "bold"))

print(BLUPrm)

mylegend <- get_legend(BLUPm)
grid.arrange(arrangeGrob(BLUPm + theme(legend.position="none"),
                         BLUPrm + theme(legend.position="none")), mylegend, nrow = 2, heights = c(6,1))


#---------------------------------------------------------------------------------------------------------------------------------------------------
# BRN MULTICOMPONENT ACTS*Context Social
#---------------------------------------------------------------------------------------------------------------------------------------------

library(writexl);library(dplyr);library(tidyr);library(ggplot2)
library(lme4);library(arm);library(MuMIn);library(tidyverse)
library(broom);library(coda);library(grid)
library(gridExtra);library(brms); library(broom.mixed); library(merTools);library(rptR)
library(tidybayes);library(parallel);library(car);library(RColorBrewer)

col.MCaptive <- "#004bbb" #brewer.pal(n = 10, name = "RdBu")[1]
col.FCaptive <- "#02b2e8" #brewer.pal(n = 10, name = "RdBu")[5]
col.MSanctuary <- "#941501" #brewer.pal(n = 10, name = "RdBu")[6]
col.FSanctuary <- "#ed6651" #brewer.pal(n = 10, name = "RdBu")[7]
cols <- c("M/Captive" = col.MCaptive, "F/Captive" = col.FCaptive, "M/Sanctuary" = col.MSanctuary, "F/Sanctuary" = col.FSanctuary)

# --------------------------------------------------------------------------------------------------------
# (2) Read in all data & settings ------------------------------------------------------------------------

contr <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

# extract legend function
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# --------------------------------------------------------------------------------------------------------
# (3) Prepare data ---------------------------------------------------------------------------------------

Data$SexRec <- str_sub(Data$SexCombinaison, start = 2)
Data <- Data %>% unite("SexSetting", c(Sex,Setting), sep = "/", remove = FALSE)

# Take out the lines "no focal subject"
Data <- Data[Data$Subject != "No focal subject", ]  
# Take out duplicated lines
Datareduit <- Data[Data$Duplicated != "T", ]  
# Take out the Keepers-directed signals, None and NA in Recipient
Datareduit <- Datareduit[Datareduit$Recipient != "Keepers", ]; Datareduit <- Datareduit[Datareduit$Recipient != "NA", ]; Datareduit <- Datareduit[Datareduit$Recipient != "None", ]
Datareduit <- subset(Datareduit, !is.na(Recipient))

# Create a "Dyad" column ensuring that dyads are treated identically
Datareduit$Dyad <- apply(Datareduit[, c("Subject", "Recipient")], 1, function(row) {
  paste(sort(row), collapse = "/")
})

# Subset data for Multimodality (= Multicomponent acts)--- 
datMulti <- subset(Datareduit, select = c("ObservationID", "Subject", "Context", "SocialContext","Multimodality_YN","RecAttentionState_YN","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison","SexRec","SexSetting","Dyad"))
# Take out lines with "None", "NA" in Multimodality_YN
table(datMulti$Multimodality_YN, useNA="ifany")
datMulti <- datMulti[datMulti$Multimodality_YN != "NA", ]
datMulti <- subset(datMulti, !is.na(Multimodality_YN))
# Take out the lines "None", "NA" dans Context
table(datMulti$Context, useNA="ifany")
datMulti <- datMulti[datMulti$Context != "NA", ]
datMulti <- subset(datMulti, !is.na(Context))
# Take out the lines "NA" dans RecAttentionState_YN
table(datMulti$RecAttentionState_YN, useNA="ifany")
datMulti <- datMulti[datMulti$RecAttentionState_YN != "NA", ]
datMulti <- subset(datMulti, !is.na(RecAttentionState_YN))
# Take out the lines "None", "NA" dans ASO
table(datMulti$ASO, useNA="ifany")
datMulti <- datMulti[datMulti$ASO != "NA", ]; datMulti <- datMulti[datMulti$ASO != "None", ]
datMulti <- subset(datMulti, !is.na(ASO))

datMulti$Multimodality_YN <- as.numeric(datMulti$Multimodality_YN)

# Omit Subjects that contribute less than 2 cases to each context
nb.signals.per.ind <- datMulti %>%
  group_by(Subject, Context) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Context, values_from = Count, values_fill = list(Count = 0))
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$Social>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$`Non social`>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
datMulti <- subset(datMulti, Subject %in% nb.signals.per.ind$Subject) 

table(datMulti$Multimodality_YN,datMulti$Group)
#      A    B   C2   C4    L
# 0 1194  270 2305  627  971
# 1  209   46  709   44  118
table(datMulti$Multimodality_YN,datMulti$Context)
#   Non social Social
# 0       3822   1545
# 1        967    159

# switch chr to factors
datMulti$Subject <- as.factor(datMulti$Subject)
datMulti$Recipient <- as.factor(datMulti$Recipient)
datMulti$Sex <- as.factor(datMulti$Sex)
datMulti$Group <- as.factor(datMulti$Group)
datMulti$Setting <- as.factor(datMulti$Setting)
datMulti$Dyad <- as.factor(datMulti$Dyad)
#datMulti$Context <- ifelse(datMulti$Context=="Social",0,1)
#datMulti$Context <- as.numeric(datMulti$Context)
datMulti$Context <- as.factor(datMulti$Context)
datMulti$RecAttentionState_YN <- as.factor(datMulti$RecAttentionState_YN)
datMulti$ASO <- as.factor(datMulti$ASO)


# Check collinearity 
vif(lm(Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN, data = datMulti))
# scale(Age)                  Sex              Setting              Context RecAttentionState_YN 
#   1.183073             1.126997             1.110915             1.075583             1.058408 

# Random intercept model
ri_M <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                (1|Subject) + (1|Recipient) + (1|ASO), family = binomial, data = datMulti, control = contr)   # random effects

# Random slope model

ris_M <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                 (1|Subject) + (1|Recipient) + (1|ASO) + (0+Context|Subject), family = binomial, data = datMulti, control = contr)   # random effects

length(residuals(ri_M)) # 6493 ok 
length(residuals(ris_M)) # 6493

# Fit "null models" without random intercepts & slope for individual identity
ri_M.null <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                     (1|Recipient) + (1|ASO), family = binomial, data = datMulti, control = contr)

ris_M.null <- glmer(formula = Multimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                      (1|Recipient) + (1|ASO), family = binomial, data = datMulti, control = contr)

summary(ri_M)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -2.72050    0.34612  -7.860 3.84e-15 ***
# scale(Age)             0.06563    0.07299   0.899  0.36854    
# SexM                  -0.33731    0.16183  -2.084  0.03713 *  
# SettingSanctuary       0.49729    0.17608   2.824  0.00474 ** 
# ContextSocial          0.34557    0.12428   2.781  0.00543 ** 
# RecAttentionState_YNT  0.14939    0.13396   1.115  0.26476    

summary(ris_M)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -2.71066    0.34458  -7.867 3.65e-15 ***
# scale(Age)             0.07046    0.07349   0.959  0.33767    
# SexM                  -0.32730    0.16310  -2.007  0.04478 *  
# SettingSanctuary       0.49314    0.17668   2.791  0.00525 ** 
# ContextSocial          0.26504    0.15688   1.689  0.09114 .  
# RecAttentionState_YNT  0.14616    0.13430   1.088  0.27645    
  
as.data.frame(anova(ri_M, ris_M, test="LTR"))
#       npar      AIC      BIC    logLik deviance    Chisq Df   Pr(>Chisq)
# ri_M     9 5041.326 5102.332 -2511.663 5023.326       NA NA         NA
# ris_M   12 5046.031 5127.372 -2511.015 5022.031 1.295188  3  0.7302761

as.data.frame(anova(ri_M.null, ri_M, test="LTR")) # test the full (ri_RM) against the null (ri_RM.null) model using a likelihood ratio test
#         npar      AIC      BIC    logLik deviance    Chisq Df  Pr(>Chisq)
# ri_RM.null   8 5103.876 5158.104 -2543.938 5087.876      NA NA           NA
# ri_RM        9 5041.326 5102.332 -2511.663 5023.326 64.5505  1 9.408963e-16 --> p-value<0.05 so ri_RM is better than ri_RM.null 

as.data.frame(anova(ris_M.null, ris_M, test="LTR"))
#            npar      AIC      BIC    logLik deviance    Chisq Df   Pr(>Chisq)
# ris_M.null    8 5103.876 5158.104 -2543.938 5087.876      NA NA           NA
# ris_M        12 5046.031 5127.372 -2511.015 5022.031 65.84569  4 1.707193e-13


# Plot output of linear mixed-effects models (lmer) ---

# Extract ri coefficients 
RI_M <- coef(ri_M)$Subject 
RI_M$Subject <- rownames(RI_M) # put the row names as a column (Subject)
RI_M <- RI_M %>% relocate(Subject, .before = `(Intercept)`)
RI_M$`(Intercept)+ContextSocial` <- RI_M$`(Intercept)`+RI_M$ContextSocial
RI_M <- merge(RI_M, datMulti[!duplicated(datMulti$Subject),c("Subject","SexSetting")], by.x = "Subject",by.y="Subject")
RI_M$COL <- ifelse(RI_M$SexSetting=="M/Captive",col.MCaptive,ifelse(RI_M$SexSetting=="F/Captive",col.FCaptive,ifelse(RI_M$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(RI_M$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
RI_M_long <- RI_M %>%
  pivot_longer(cols = c(`(Intercept)`, `(Intercept)+ContextSocial`), 
               names_to = "Context", 
               values_to = "Value")

# Extract ris coefficients 
RIS_M <- coef(ris_M)$Subject 
RIS_M$Subject <- rownames(RIS_M) # put the row names as a column (Subject)
RIS_M <- RIS_M %>% relocate(Subject, .before = `(Intercept)`)
RIS_M$`(Intercept)+ContextSocial` <- RIS_M$`(Intercept)`+RIS_M$ContextSocial
RIS_M$`(Intercept)+ContextNon social` <- RIS_M$`(Intercept)`+RIS_M$`ContextNon social`
RIS_M <- merge(RIS_M, datMulti[!duplicated(datMulti$Subject),c("Subject","SexSetting")], by.x = "Subject",by.y="Subject")
RIS_M$COL <- ifelse(RIS_M$SexSetting=="M/Captive",col.MCaptive,ifelse(RIS_M$SexSetting=="F/Captive",col.FCaptive,ifelse(RIS_M$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(RIS_M$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
RIS_M_long <- RIS_M %>%
  pivot_longer(cols = c(`(Intercept)+ContextNon social`, `(Intercept)+ContextSocial`), 
               names_to = "Context", 
               values_to = "Value")

theme_angele_ss <- theme(panel.background = element_blank(),
                         plot.margin = margin(0, 5, 0, 5),
                         panel.border =element_rect(colour="black", fill=NA),
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 16,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 16,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour="black"),
                         axis.title.x = element_text(size = 20, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 20, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 16, vjust = 3.3, family="sans", margin = margin(t = 15)),
                         legend.title = element_text(size = 17, vjust = 2, family="sans", margin = margin(t = 11.5)),
                         legend.key = element_blank(),
                         legend.position = "left",
                         legend.spacing.x = unit(0.2, 'cm'),
                         title = element_text(size = 15, family="sans"),
                         strip.text = element_text(size = 15))
# Set y-axis limits and breaks
y_limits <- c(0, 0.25)
y_breaks <- seq(0, 1, by = 0.1)

plot_riM <- ggplot(RI_M_long, aes(x = Context, y = plogis(Value), group = Subject, color = factor(SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")))) +
  geom_line(linewidth = 0.8) +
  theme_angele_ss +
  scale_y_continuous("Multicomponent acts", limits = y_limits, breaks = y_breaks) +
  ggtitle("(c)") +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary"))+
  scale_x_discrete("Context", labels = c("Non Social","Social")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(plot.title = element_text(face = "bold"))

plot_risM <- ggplot(RIS_M_long, aes(x = Context, y = plogis(Value), group = Subject, color = factor(SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")))) +
  geom_line(linewidth = 0.8) +
  theme_angele_ss +
  scale_y_continuous("", limits = y_limits, breaks = y_breaks) +
  ggtitle("(d)") +
  scale_x_discrete("Context", labels = c("Non Social","Social")) +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary")) +
  theme(plot.title = element_text(face = "bold"))
BRN_MC <- grid.arrange(arrangeGrob(plot_riM + theme(legend.position="none"),
                                   plot_risM + theme(legend.position="none"), ncol=2))



#---------------------------------------------------------------------------------------------------------------------------------------------------
# BRN MULTISENSORY ACTS*Context Social
#---------------------------------------------------------------------------------------------------------------------------------------------

# Prepare data ---------------------------------------------------------------------------------------

Data$SexRec <- str_sub(Data$SexCombinaison, start = 2)
Data <- Data %>% unite("SexSetting", c(Sex,Setting), sep = "/", remove = FALSE)

# Take out the lines "no focal subject"
Data <- Data[Data$Subject != "No focal subject", ]  
# Take out duplicated lines
Datareduit <- Data[Data$Duplicated != "T", ]  
# Take out the Keepers-directed signals, None and NA in Recipient
Datareduit <- Datareduit[Datareduit$Recipient != "Keepers", ]; Datareduit <- Datareduit[Datareduit$Recipient != "NA", ]; Datareduit <- Datareduit[Datareduit$Recipient != "None", ]
Datareduit <- subset(Datareduit, !is.na(Recipient))

# Create a "Dyad" column ensuring that dyads are treated identically
Datareduit$Dyad <- apply(Datareduit[, c("Subject", "Recipient")], 1, function(row) {
  paste(sort(row), collapse = "/")
})

# Subset data for ReceptionMultimodality_YN (= Multisensory acts)--- 
datRecMod <- subset(Datareduit, select = c("ObservationID", "Subject", "Context", "SocialContext","ReceptionMultimodality_YN","RecAttentionState_YN","ASO", "Recipient", "Age", "Rank", "Sex","Group","Setting","KinRelationship","SexCombinaison","SexRec","SexSetting","Dyad"))
# Take out lines with "None", "NA" in ReceptionMultimodality_YN
table(datRecMod$ReceptionMultimodality_YN, useNA="ifany")
datRecMod <- datRecMod[datRecMod$ReceptionMultimodality_YN != "NA", ]
datRecMod <- subset(datRecMod, !is.na(ReceptionMultimodality_YN))
# Take out the lines "None", "NA" dans Context
table(datRecMod$Context, useNA="ifany")
datRecMod <- datRecMod[datRecMod$Context != "NA", ]
datRecMod <- subset(datRecMod, !is.na(Context))
# Take out the lines "NA" dans RecAttentionState_YN
table(datRecMod$RecAttentionState_YN, useNA="ifany")
datRecMod <- datRecMod[datRecMod$RecAttentionState_YN != "NA", ]
datRecMod <- subset(datRecMod, !is.na(RecAttentionState_YN))
# Take out the lines "None", "NA" dans ASO
table(datRecMod$ASO, useNA="ifany")
datRecMod <- datRecMod[datRecMod$ASO != "NA", ]
datRecMod <- subset(datRecMod, !is.na(ASO))

datRecMod$ReceptionMultimodality_YN <- as.numeric(datRecMod$ReceptionMultimodality_YN)

# Omit Subjects that contribute less than 2 cases to each context
nb.signals.per.ind <- datRecMod %>%
  group_by(Subject, Context) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Context, values_from = Count, values_fill = list(Count = 0))
nb.signals.per.ind$Subject <- as.character(nb.signals.per.ind$Subject)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$Social>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
nb.signals.per.ind <- subset(nb.signals.per.ind, nb.signals.per.ind$`Non social`>1) # Keeps only the values in "column" that appear more than 2 times (N >= 2)
datRecMod <- subset(datRecMod, Subject %in% nb.signals.per.ind$Subject) 

table(datRecMod$ReceptionMultimodality_YN,datRecMod$Group)
#      A    B   C2   C4    L
# 0  748  216 1569  442  752
# 1  660  100 1444  232  355
table(datRecMod$ReceptionMultimodality_YN,datRecMod$Context)
#   Non social Social
# 0       2673   1054
# 1       2138    653

# switch chr to factors
datRecMod$Subject <- as.factor(datRecMod$Subject)
datRecMod$Recipient <- as.factor(datRecMod$Recipient)
datRecMod$Sex <- as.factor(datRecMod$Sex)
datRecMod$Group <- as.factor(datRecMod$Group)
datRecMod$Setting <- as.factor(datRecMod$Setting)
datRecMod$Dyad <- as.factor(datRecMod$Dyad)
#datRecMod$Context <- ifelse(datRecMod$Context=="Social",0,1)
#datRecMod$Context <- as.numeric(datRecMod$Context)
datRecMod$Context <- as.factor(datRecMod$Context)
datRecMod$RecAttentionState_YN <- as.factor(datRecMod$RecAttentionState_YN)
datRecMod$ASO <- as.factor(datRecMod$ASO)


# Check collinearity 
vif(lm(ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN, data = datRecMod))
#  scale(Age)                  Sex              Setting              Context RecAttentionState_YN 
#    1.185031             1.128011             1.112418             1.075957             1.058768 

# Random intercept model
ri_RM <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                (1|Subject) + (1|Recipient) + (1|ASO), family = binomial, data = datRecMod, control = contr)   # random effects

# Random slope model

ris_RM <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                 (1|Subject) + (1|Recipient) + (1|ASO) + (0+Context|Subject), family = binomial, data = datRecMod, control = contr)   # random effects

length(residuals(ri_RM)) # 6517 ok 
length(residuals(ris_RM)) # 6517

# Fit "null models" without random intercepts & slope for individual identity
ri_RM.null <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                     (1|Recipient) + (1|ASO), family = binomial, data = datRecMod, control = contr)

ris_RM.null <- glmer(formula = ReceptionMultimodality_YN ~ scale(Age) + Sex + Setting + Context + RecAttentionState_YN +    # fixed effects
                      (1|Recipient) + (1|ASO), family = binomial, data = datRecMod, control = contr)

summary(ri_RM)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -3.34140    0.30899 -10.814   <2e-16 ***
# scale(Age)             0.07675    0.05706   1.345   0.1786    
# SexM                  -0.07964    0.12595  -0.632   0.5272    
# SettingSanctuary       0.29266    0.13186   2.219   0.0265 *  
# ContextSocial          0.18913    0.09741   1.942   0.0522 .  
# RecAttentionState_YNT  2.81508    0.13219  21.296   <2e-16 ***

summary(ris_RM)
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -3.33264    0.30873 -10.795  < 2e-16 ***
# scale(Age)             0.07983    0.05328   1.498  0.13409    
# SexM                  -0.17822    0.12187  -1.462  0.14363    
# SettingSanctuary       0.39104    0.12516   3.124  0.00178 ** 
# ContextSocial          0.25602    0.12921   1.981  0.04754 *  
# RecAttentionState_YNT  2.77662    0.13158  21.103  < 2e-16 ***

as.data.frame(anova(ri_RM, ris_RM, test="LTR"))
#        npar      AIC      BIC    logLik deviance    Chisq Df   Pr(>Chisq)
# ri_RM     9 7437.807 7498.847 -3709.904 7419.807       NA NA           NA
# ris_RM   12 7399.303 7480.689 -3687.651 7375.303 44.50477  3 1.178975e-09

as.data.frame(anova(ri_RM.null, ri_RM, test="LTR")) # test the full (ri_RM) against the null (ri_RM.null) model using a likelihood ratio test
#         npar      AIC      BIC    logLik deviance    Chisq Df  Pr(>Chisq)
# ri_RM.null   8 7508.616 7562.873 -3746.308 7492.616       NA NA           NA
# ri_RM        9 7437.807 7498.847 -3709.904 7419.807 72.80818  1 1.428831e-17 --> p-value<0.05 so ri_RM is better than ri_RM.null 

as.data.frame(anova(ris_RM.null, ris_RM, test="LTR"))
#             npar      AIC      BIC    logLik deviance    Chisq Df   Pr(>Chisq)
# ris_RM.null    8 7508.616 7562.873 -3746.308 7492.616       NA NA           NA
# ris_RM        12 7399.303 7480.689 -3687.651 7375.303 117.313  4 2.002044e-24


# Plot output of linear mixed-effects models (lmer) ---

# Extract ri coefficients 
RI_RM <- coef(ri_RM)$Subject 
RI_RM$Subject <- rownames(RI_RM) # put the row names as a column (Subject)
RI_RM <- RI_RM %>% relocate(Subject, .before = `(Intercept)`)
RI_RM$`(Intercept)+ContextSocial` <- RI_RM$`(Intercept)`+RI_RM$ContextSocial
RI_RM <- merge(RI_RM, datMulti[!duplicated(datMulti$Subject),c("Subject","SexSetting")], by.x = "Subject",by.y="Subject")
RI_RM$COL <- ifelse(RI_RM$SexSetting=="M/Captive",col.MCaptive,ifelse(RI_RM$SexSetting=="F/Captive",col.FCaptive,ifelse(RI_RM$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(RI_RM$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
RI_RM_long <- RI_RM %>%
  pivot_longer(cols = c(`(Intercept)`, `(Intercept)+ContextSocial`), 
               names_to = "Context", 
               values_to = "Value")

# Extract ris coefficients 
RIS_RM <- coef(ris_RM)$Subject 
RIS_RM$Subject <- rownames(RIS_RM) # put the row names as a column (Subject)
RIS_RM <- RIS_RM %>% relocate(Subject, .before = `(Intercept)`)
RIS_RM$`(Intercept)+ContextSocial` <- RIS_RM$`(Intercept)`+RIS_RM$ContextSocial
RIS_RM$`(Intercept)+ContextNon social` <- RIS_RM$`(Intercept)`+RIS_RM$`ContextNon social`
RIS_RM <- merge(RIS_RM, datMulti[!duplicated(datMulti$Subject),c("Subject","SexSetting")], by.x = "Subject",by.y="Subject")
RIS_RM$COL <- ifelse(RIS_RM$SexSetting=="M/Captive",col.MCaptive,ifelse(RIS_RM$SexSetting=="F/Captive",col.FCaptive,ifelse(RIS_RM$SexSetting=="M/Sanctuary",col.MSanctuary,ifelse(RIS_RM$SexSetting=="F/Sanctuary",col.FSanctuary,NA))))
RIS_RM_long <- RIS_RM %>%
  pivot_longer(cols = c(`(Intercept)+ContextNon social`, `(Intercept)+ContextSocial`), 
               names_to = "Context", 
               values_to = "Value")

theme_angele_ss <- theme(panel.background = element_blank(),
                         plot.margin = margin(0, 5, 0, 5),
                         panel.border =element_rect(colour="black", fill=NA),
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 16,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 16,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour="black"),
                         axis.title.x = element_text(size = 20, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 20, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 16, vjust = 3.3, family="sans", margin = margin(t = 15)),
                         legend.title = element_text(size = 17, vjust = 2, family="sans", margin = margin(t = 11.5)),
                         legend.key = element_blank(),
                         legend.position = "left",
                         legend.spacing.x = unit(0.2, 'cm'),
                         title = element_text(size = 15, family="sans"),
                         strip.text = element_text(size = 15))
# Set y-axis limits and breaks
y_limits <- c(0, 0.25)
y_breaks <- seq(0, 1, by = 0.1)

plot_riRM <- ggplot(RI_RM_long, aes(x = Context, y = plogis(Value), group = Subject, color = factor(SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")))) +
  geom_line(linewidth = 0.8) +
  theme_angele_ss +
  scale_y_continuous("Multisensory acts", limits = y_limits, breaks = y_breaks) +
  ggtitle("(a)") +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary"))+
  scale_x_discrete("Context", labels = c("Non Social","Social")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(plot.title = element_text(face = "bold"))

plot_risRM <- ggplot(RIS_RM_long, aes(x = Context, y = plogis(Value), group = Subject, color = factor(SexSetting, levels = c("M/Captive", "F/Captive", "M/Sanctuary", "F/Sanctuary")))) +
  geom_line(linewidth = 0.8) +
  theme_angele_ss +
  scale_y_continuous("", limits = y_limits, breaks = y_breaks) +
  ggtitle("(b)") +
  scale_x_discrete("Context", labels = c("Non Social","Social")) +
  scale_color_manual(values = cols, name="Sex/Setting Category: ", labels=c("Male/Zoo", "Female/Zoo", "Male/Sanctuary", "Female/Sanctuary")) +
  theme(plot.title = element_text(face = "bold"))
BRN_MS <- grid.arrange(arrangeGrob(plot_riRM + theme(legend.position="none"),
                                   plot_risRM + theme(legend.position="none"), ncol=2))


mylegend <- g_legend(plot_riM + theme(legend.position = "bottom") + 
                       guides(color = guide_legend(title.position = "left", nrow = 1)))
grid.arrange(BRN_MC, BRN_MS, mylegend, ncol = 1, heights = c(15,15,2))
