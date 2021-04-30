# Cluster Analysis rewrite

colnames(df_Ephys_Clean)
#looks like 26:101;145:323 is bulk of the ephys data
df_Ephys <- df_Ephys_Clean[,c(1,2,14,15,18:82, 485:486)]

# Morphological data 119:144/324:548/558:794
df_Morph <- df_Morph_Clean[,c(1,2,10:15,83:486)]

# Clean up ephys dataset, choose some variables to use in cluster analysis
df_Ephys_NARem <- df_Ephys[ , colSums(is.na(df_Ephys)) <150]  
df_Ephys_NARem <- remove_empty(df_Ephys_NARem, which = "rows")

df_Ephys_Cluster <- df_Ephys_NARem %>%
  select(`Cell.Name`,`Subtype_Precise`, # ID columns
         `Peak.input.Resistance`,`Steady.State.Input.Resistance`,
         `V.Rest`,`Rheobase.Current`,`Threshold.Relative.Rest`,
         `Spike.Amp..Relative.Thresh.`,
         `Spike.Amp..Relative.Rest`,`Max.Depol..Rate`,`Latency.To.Spike.Threshold`,
         `Latency.To.spike.Peak`,`AHP.Absolute.Amp.`,Halfwidth)

# library(mice) # impute data without demographic info
set.seed(13)
impute_df_Ephys <- mice(df_Ephys_Cluster[,3:ncol(df_Ephys_Cluster)],
                        m=5, seed = 101)
imputed_ephys <- complete(impute_df_Ephys)
# add demo info back on front
ephys_comb <- cbind(df_Ephys_Cluster[,1:2],imputed_ephys)
# remove more NAs
removed.ephys_pca <- na.omit(ephys_comb)
ephys_pca <- prcomp(removed.ephys_pca[,5:ncol(removed.ephys_pca)],
                    scale=T, center = T)  
fviz_pca_biplot(ephys_pca,
             geom.ind= "point", pointshape = 1,
             pointsize = 4,stroke=2,
             habillage = removed.ephys_pca$Subtype_Precise,
             palette = POTcolors,repel=T,
             addEllipses = T)+
  theme_pubr()+theme(legend.position = "none")+
  ggtitle("EPHYS: 474 cells, 12 variables; MICE imputation; 3-9-21")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/PCA_Ephys.eps", width = 6, height = 5, unit = "in")



## ******** Morphological Parameters ********** \\

df_Morph_Cluster <- df_Morph %>% 
  select(`Cell.Name`,`Subtype_Precise`,
         starts_with("total.dendrite"),number.of.dendritic.endings,
         number.of.dendritic.nodes,
         number.of.dendritic.trees,starts_with("soma"),) %>% 
  filter(!is.na(Soma.Area))
#mice imputation
impute_df_Morph <- mice(df_Morph_Cluster[,3:ncol(df_Morph_Cluster)],m = 5,seed=101)
imputed_morph <- complete(impute_df_Morph)
# add demo info back on front
morph_comb <- cbind(df_Morph_Cluster[,1:2],imputed_morph)

removed.morph_pca <- na.omit(morph_comb)
morph_pca <- prcomp(removed.morph_pca[,3:ncol(removed.morph_pca)],
                    scale=T, center = T)  

fviz_pca_biplot(morph_pca,
             geom.ind= "point", pointshape = 1,
             pointsize = 4,stroke=2,
             habillage = removed.morph_pca$Subtype_Precise,
             palette = POTcolors,
             addEllipses = T,repel = T)+
  theme_pubr()+theme(legend.position = "none")+
  ggtitle("Morphology: 223 cells, 19 variables; MICE imputation; 1-7-21")
dim(morph_comb)

## ****** Combined PCA ********* \\

allparams <- merge(morph_comb, ephys_comb, by="Cell.Name")

allparams_clean <- allparams %>% 
  select(-starts_with("Subtype")) %>% 
  mutate(Subtype_Precise = allparams[,4])

all_pca <- prcomp(allparams_clean[,2:(ncol(removed.morph_pca)-1)],
                    scale=T, center = T)  

fviz_pca_biplot(all_pca,
             geom.ind= "point", pointshape = 1,
             pointsize = 4,stroke=2,
             habillage = removed.morph_pca$Subtype_Precise,
             palette = POTcolors,
             addEllipses = T)+
  theme_pubr()+ggtitle("Everything: 229 cells, 31 variables; MICE imputation; 12-31-20")
dim(allparams_clean)

# All round 2

all_cluster <- merge(df_Ephys_Cluster, df_Morph_Cluster, by = "Cell.Name")
all_cluster_clean <- all_cluster %>% 
  select(-starts_with("Subtype")) %>% 
  mutate(Subtype_Precise = all_cluster[,2])
impute_df_all <- mice(all_cluster_clean[,2:(ncol(all_cluster_clean)-1)],m = 5,seed=101)
imputed_all <- complete(impute_df_all)

all_comb <- cbind(all_cluster_clean[,c(1,33)],imputed_all)

removed.all_pca <- na.omit(all_comb)
all_pca <- prcomp(removed.all_pca[,3:ncol(removed.all_pca)],
                    scale=T, center = T)  

fviz_pca_biplot(all_pca,
             geom.ind= "point", pointshape = 1,
             pointsize = 4,stroke=2,
             habillage = removed.all_pca$Subtype_Precise,
             palette = POTcolors,
           #  select.ind = list("contrib" = 5),
             select.var = list("contrib"= 25),
             addEllipses = T)+
  theme_pubr()+theme(legend.position = "none")+
  ggtitle("Everything: 200 cells, 31 variables; MICE imputation; 3-11-21")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/PCA_All.eps", width = 5, height = 7, unit = "in")

dim(removed.all_pca)

fviz_pca_var(all_pca, col.var = "contrib",
             gradient.cols = c("white","blue","red"),
             ggtheme=theme_minimal())




### OLD STUFF


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




