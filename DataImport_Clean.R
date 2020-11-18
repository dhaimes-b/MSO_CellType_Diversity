library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggfortify)

df1 <- read_xlsx(path = "CELLS Used in Paper.xlsx")
df2 <- read_xlsx(path = "CellsForPaper_v2.xlsx")
df3 <- read_xlsx(path = "Data_Extraction_V8.xlsx")



# From Brian -> CELLS Used in Paper.xlsx (ie: v1) is primarily ephys data
# v2 is a combination of electrophysiological data AND morphological data

colnames(df2)
#Going to try to clean out columns that are primarily NA, 
#and change to DF for simplicity
# df <- df3 %>% 
#   filter(!is.na(`Cell Name`))
df <- df2 %>% 
  filter(!is.na(Cell_Name))

#df <- df2[,colSums(is.na(df2))<nrow(df2)]
#Found some info on stackoverflow, the following is faster and less memory intensive

colnames(df) <- make.names(colnames(df), unique=TRUE)

df <- Filter(function(x)!all(is.na(x)), df)

colnames(df)
table(is.na(df))

## THESE ARE WHEN USING V2 SPREADSHEET
# test<- remove_empty(df2) #also brings us down to 818 variables
colnames(df)
#looks like 26:101;145:323 is bulk of the ephys data
df_Ephys <- df[,c(2,24:25,26:101,145:323)]
# "Demographic" data seems to lie 6:9; more 552:557
df_Demo <- df[,c(2,24:25,6:9,552:557)]
# Morphological data 119:144/324:548/558:794
df_Morph <- df[,c(2,24:25,119:144,324:548,558:794)]

# ## THESE FOR Data_Extraction_V8
# colnames(df)
# #looks like 26:101;145:323 is bulk of the ephys data
# df_Ephys <- df[,c(1,2,14,15,16:80)]
# # "Demographic" data seems to lie 6:9; more 552:557
# df_Demo <- df[,c(1:4,14,15)]
# # Morphological data 119:144/324:548/558:794
# df_Morph <- df[,c(1,2,14,15,81:482)]




# Dealing with the missing values issues
colnames(df_Ephys)[ apply(df_Ephys, 2, anyNA) ]
# Looks like every single variable of the 255 have an NA
apply(df_Ephys, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
# Threshold cutoff, only keep columns where there are fewer than 100 NAs
df_Ephys_NARem <- df_Ephys[ , colSums(is.na(df_Ephys)) <150]  
# ~25 Variables remaining to deal with
#double checking there's no totally empty observations
df_Ephys_NARem <- remove_empty(df_Ephys_NARem, which = "rows")
table(is.na(df_Ephys_NARem))
#Down to less than 10% Na's

#use with v2
df_Ephys_Cluster <- df_Ephys_NARem %>%
  select(Cell_Name, Subtype...25,SubtypeBroad, # ID columns
        Rin_Peak,Rin_SS,Vrest,Rheobase_Current,Absolute_Threshold,
         Threshold_Relative_Rest,Spike_Amp_Relative_Thresh,
         Spike_Amp_Relative_Rest,Max_Depol_Rate,Latency_To_Threshold,
         Latency_To_Peak,AHP_Absolute_Amp,Halfwidth)
table(is.na(df_Ephys_Cluster))

# df_Ephys_Cluster <- df_Ephys_NARem %>%
#   select(`Cell Name`, Subtype,`Subtype - Broad`, # ID columns
#         `Peak input Resistance`,`Steady State Input Resistance`,
#          `V Rest`,`Rheobase Current`,`Threshold Relative Rest`,
#          `Spike Amp. Relative Thresh.`,
#          `Spike Amp. Relative Rest`,`Max Depol. Rate`,`Latency To Spike Threshold`,
#          `Latency To spike Peak`,`AHP Absolute Amp.`,Halfwidth)
# table(is.na(df_Ephys_Cluster))
# df_Ephys_Cluster <- df_Ephys_Cluster[-c(1:2),-c(1:3)]
# 
# colnames(df_Ephys_Cluster) <- make.names(colnames(df_Ephys_Cluster), unique=TRUE)
# 



# Data Imputation

# Data Imputation Using mice (multivariate imputation by chained equations)
library(mice)
md.pattern(df_Ephys_Cluster)

impute_df_Ephys <- mice(df_Ephys_Cluster,m = 5,seed=101)
summary(impute_df_Ephys)

imp3_df_Ephys <- complete(impute_df_Ephys, 3)
table(is.na(imp3_df_Ephys))
which(is.na(imp3_df_Ephys))

#looks like we lose ~30 observations 
PCA_df_Ephys <- na.omit(imp3_df_Ephys)

# Let's start with a pca
MSO.pca <- prcomp(PCA_df_Ephys[,4:16], scale = TRUE)
summary(MSO.pca)

MSO.pca %>%
  as.data.frame() %>%
  ggplot(aes(x=PC1, PC2)) +
  geom_point()

autoplot(prcomp(PCA_df_Ephys[,4:16],scale=T),
         colour= PCA_df_Ephys$Subtype)+
  scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()

test<- PCA_df_Ephys[,4:16]
autoplot(prcomp(test,scale = T),
         colour= PCA_df_Ephys$Subtype)+
 # scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()

# K means
wss <- (nrow(PCA_df_Ephys[,4:16])-1)*sum(apply(PCA_df_Ephys[,4:16],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(PCA_df_Ephys[,4:16],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

input_Clust <- scale(PCA_df_Ephys[,4:16],center = T, scale = T)
kmeans(input_Clust, 3)
autoplot(kmeans(input_Clust, 3,
                nstart = 25,iter.max = 200), data = PCA_df_Ephys) +
  theme_minimal()

# Add morphological data
df_merge <- merge(df_Ephys, df_Morph)
apply(df_merge, 2, function(x) length(which(is.na(x))))
df_merge_Thresh <- df_merge[ , colSums(is.na(df_merge)) <200]  


#md.pattern(df_merge_Thresh)
# df_merge_Thresh %>% 
#   janitor::make_clean_names()

impute_df_merge <- mice(df_merge_Thresh,m = 5,seed=101)
summary(impute_df_merge)

imp3_df_merge <- complete(impute_df_merge, 3)
table(is.na(imp3_df_merge))
which(is.na(imp3_df_merge))

#looks like we lose ~30 observations 
PCA_df_Merge <- na.omit(imp3_df_merge)




