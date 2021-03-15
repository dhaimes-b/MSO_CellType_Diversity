df3 <- read_xlsx(path = "Data_Extraction_V8.xlsx")

df <- df3
colnames(df) <- make.names(colnames(df), unique=TRUE)

# ## THESE FOR Data_Extraction_V8
colnames(df)
#looks like 26:101;145:323 is bulk of the ephys data
df_Ephys <- df[,c(1,2,14,15,16:80)]
# "Demographic" data seems to lie 6:9; more 552:557
df_Demo <- df[,c(1:4,14,15)]
# Morphological data 119:144/324:548/558:794
df_Morph <- df[,c(1,2,14,15,81:482)]

colnames(df_Ephys)[ apply(df_Ephys, 2, anyNA) ]
# Looks like every single variable of the 255 have an NA
apply(df_Ephys, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
# Threshold cutoff, only keep columns where there are fewer than 100 NAs
df_Ephys_NARem <- df_Ephys[ , colSums(is.na(df_Ephys)) <100]  
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

df_Ephys_Cluster <- as.data.frame(sapply(df_Ephys_Cluster[,2:ncol(df_Ephys_Cluster)],as.numeric))

df_Ephys_Cluster <- cbind(df_Ephys_NARem[-c(1:2),1],df_Ephys_Cluster)
#removing subtype data, because I don't want that imputed
impute_df_Ephys <- mice(df_Ephys_Cluster[,4:ncol(df_Ephys_Cluster)],
                        m=5, seed = 101)
imputed_ephys <- complete(impute_df_Ephys)
ephys_comb <- cbind(df_Ephys_Cluster[,1:3],imputed_ephys)

removed.ephys_pca <- na.omit(ephys_comb)
ephys_pca <- prcomp(removed.ephys_pca[,4:ncol(removed.ephys_pca)],
                    scale=T, center = T)  

fviz_pca_ind(ephys_pca,
             geom.ind= "point", pointshape = 21,
             pointsize = 4,stroke=2,
             fill.ind = as.factor(removed.ephys_pca$Subtype...Broad),
             col.ind="black",
             #palette = c("black","blue","red"),
             addEllipses = F)


df_Morph_Cluster <- df_Morph[-c(1:2),]
df_Morph_Cluster <- df_Morph_Cluster %>% 
  select(`Cell.Name`, Subtype,`Subtype...Broad`,
         starts_with("total"),number.of.dendritic.endings,
         number.of.dendritic.nodes,
         number.of.dendritic.trees,starts_with("soma"),) %>% 
  filter(!is.na(Soma.Area))


apply(df_Morph_Cluster, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
df_Morph_Cluster <- df_Morph_Cluster[ , colSums(is.na(df_Morph_Cluster)) <500]  

library(mice)
impute_df_Morph <- mice(df_Morph_Cluster,m = 5,seed=101)


df_Morph_Clean <- as.data.frame(sapply(df_Morph_Cluster[,4:ncol(df_Morph_Cluster)],as.numeric))
df_Morph_Clean <- cbind(df_Morph_Cluster[,1:3], df_Morph_Clean)

df_Morph_Clean <- na.omit(df_Morph_Clean)
autoplot(prcomp(df_Morph_Clean[,4:ncol(df_Morph_Clean)],scale=T,
          color = df_Morph_Clean$Subtype...Broad
                      ))

morph_pca <- prcomp(df_Morph_Clean[,4:ncol(df_Morph_Clean)],
                    scale=T, center = T)
library(factoextra)
fviz_pca_ind(morph_pca, geom.ind= "point", pointshape = 21,
             pointsize = 2,
             fill.ind = df_Morph_Clean$Subtype...Broad,
             col.ind="black",
             palette = c("yellow","black","blue","red"),
             addEllipses = T
)

##
df_Morph_Clean <- df_Morph_Clean %>% 
  filter(Subtype != 0) 

morph_pca <- prcomp(df_Morph_Clean[,4:ncol(df_Morph_Clean)],
scale=T, center = T)

fviz_pca_ind(morph_pca, geom.ind= "point", pointshape = 21,
             pointsize = 4,stroke=2,
             fill.ind = df_Morph_Clean$Subtype...Broad,
             col.ind="black",
             palette = c("black","blue","red"),
             addEllipses = F)








str(df_Morph_Clean$Cell.Name)
str(df_Ephys_Cluster$Cell.Name)

df_merge <- merge(df_Morph_Clean, df_Ephys_Cluster,
                  by.x = df_Morph_Clean$Cell.Name,
                  by.y = df_Ephys_Cluster$Cell.Name
                  )

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








