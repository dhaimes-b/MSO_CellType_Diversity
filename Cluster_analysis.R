df3 <- read_xlsx(path = "Data_Extraction_V8.xlsx")

df <- df3


colnames(df_Ephys)[ apply(df_Ephys, 2, anyNA) ]
# Looks like every single variable of the 255 have an NA
apply(df_Ephys, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
# Threshold cutoff, only keep columns where there are fewer than 100 NAs
df_Ephys_NARem <- df_Ephys[ , colSums(is.na(df_Ephys)) <150]  
# ~20 Variables remaining to deal with
#double checking there's no totally empty observations
df_Ephys_NARem <- remove_empty(df_Ephys_NARem, which = "rows")
table(is.na(df_Ephys_NARem))
#Down to less than 10% Na's


df_Ephys_Cluster <- df_Ephys_NARem %>%
  select(`Cell.Name`, Subtype,`Subtype...Broad`, # ID columns
         `Peak.input.Resistance`,`Steady.State.Input.Resistance`,
         `V.Rest`,`Rheobase.Current`,`Threshold.Relative.Rest`,
         `Spike.Amp..Relative.Thresh.`,
         `Spike.Amp..Relative.Rest`,`Max.Depol..Rate`,`Latency.To.Spike.Threshold`,
         `Latency.To.spike.Peak`,`AHP.Absolute.Amp.`,Halfwidth)
table(is.na(df_Ephys_Cluster))
df_Ephys_Cluster <- df_Ephys_Cluster[-c(1:2),]


df_Morph_Cluster <- df_Morph[-c(1:2),]
apply(df_Morph_Cluster, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
df_Morph_Cluster <- df_Morph_Cluster[ , colSums(is.na(df_Morph_Cluster)) <500]  

# df_Morph_Cluster <- df_Morph_Cluster %>% 
#   select()
df_morph_pca <- as.numeric()

autoplot(prcomp(na.omit(df_Morph_Cluster[,-c(1:4)]),scale=T),
         colour= PCA_df_Ephys$Subtype)+
  scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()














df_merge <- merge(df_Morph_Cluster, df_Ephys_Cluster,)

#threshold NA's
colnames(df_merge)[ apply(df_merge, 2, anyNA) ]
# Looks like every single variable of the has an NA
apply(df_merge, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
# Threshold cutoff, only keep columns where there are fewer than 100 NAs
df_merge_Thresh <- df_merge[ , colSums(is.na(df_merge)) <600]  



df_merge_filtered <- df_merge_Thresh %>% 
  filter(!is.na(Axon.Distance.from.Soma.center))

test <- na.omit(df_merge_filtered)

autoplot(prcomp(test,scale = T),
         colour= PCA_df_Ephys$Subtype)+
  # scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()
