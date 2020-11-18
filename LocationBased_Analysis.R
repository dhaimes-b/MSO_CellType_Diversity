df3 <- read_xlsx(path = "Data_Extraction_V8.xlsx")

df <- df3[-c(1:2),]
colnames(df) <- make.names(colnames(df), unique=TRUE)

df <- df %>% 
  filter(!is.na(V.D.Norm..Position...Imprecise))

df <- df %>% 
  filter(!is.na(R.C.Norm..Position.Imprecise))

df <- df %>% 
  filter(is.na(Excluded_Physiology)) %>% 
  filter(is.na(Excluded_Anatomy)) 

df <- df[,colSums(is.na(df))<(nrow(df)/2)]


apply(df, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
df_All_Thresh <- df[ , colSums(is.na(df)) < (nrow(df)/2)]  


df_imp <- mice(df_All_Thresh,m = 5,seed=101)

df_All_Thresh[, 2:148] <- sapply(df_All_Thresh[, 2:148], as.numeric)

df_All_Thresh <- df_All_Thresh %>% 
  mutate()


df_All_Thresh %>% 
  ggplot(aes(x = VD.Norm..Position...Precise, 
             y= Absolute.Threshold,
             color = Subtype)) +
  geom_point()
  


