library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(ggpmisc)
library("factoextra")
library(mice)

df1 <- read_xlsx(path = "CELLS Used in Paper.xlsx")
df2 <- read_xlsx(path = "CellsForPaper_v2.xlsx")
df3 <- read_xlsx(path = "Data_Extraction_V9.xlsx")


#df <- df3[-c(1:2,419,526),]
df<- df3[-c(1:2),]
#df3_row <- df3[-c(1:2,419,526),]
df3_row <- df3[-c(1:2),]


#convert to df
df <- as.data.frame(sapply(df[,2:ncol(df)],as.numeric))
df <- cbind(df3_row[,1:2],df)

# Clean up names
colnames(df) <- make.names(colnames(df), unique=TRUE)

## Collapsing 1&1.5 into 1, 3&3.5 into 3, and everything else (2&2.5) into 2
# removing 0, 8 (?), and 9 (?)
df <- df %>%
  filter(Subtype...Broad != 0) %>%
  filter(Subtype...Broad != 8) %>%
  filter(Subtype...Broad != 9) %>%
  mutate(Subtype_Precise = as.factor(ifelse(Subtype...Broad < 2, 1,
                                  ifelse(Subtype...Broad >= 3, 3, 2))))

# df$Subtype_Precise <- as.factor(df$Subtype_Precise)

df <- df %>%
  mutate(DV_NLG = 1-(df$V.D.Norm..Position...Imprecise))

df_Prefilter <- df

df_Ephys_Clean <- df %>% 
  filter(is.na(Excluded_Physiology)) %>% 
  filter(Age > 16) %>% 
  filter(Cell.Name != "150720_09") # incredibly high RinSS phasic removed


### Removing from Excluded and Anatomy, and those found in Outlier analysis

df_Morph_Clean <- df %>% 
  filter(is.na(Excluded_Anatomy)) %>% 
  filter(Cell.Name != "170703_03") %>% 
  filter(Cell.Name != "160112_18")


