#outlier assessment
df3 <- read_xlsx(path = "Data_Extraction_V9.xlsx")
df <- df3[-c(1:2),]
colnames(df) <- make.names(colnames(df), unique=TRUE)
df3_row <- df3[-c(1:2),]

#convert to df
df <- as.data.frame(sapply(df[,2:ncol(df)],as.numeric))
df <- cbind(df3_row[,1:2],df)

df %>% 
  ggplot(aes(x=df$total.dendrite.Complexity))+
  geom_histogram()
df[which.max(df$total.dendrite.Complexity),]
outlier <- df[which.max(df$total.dendrite.Complexity),]
outlier$`Cell Name`
df %>% 
  ggplot(aes(x=df$number.of.dendritic.endings))+
  geom_histogram()
df[which.max(df$number.of.dendritic.endings),]
outlier <- df[which.max(df$number.of.dendritic.endings),]
outlier$`Cell Name`

df %>% 
  ggplot(aes(x=df$total.dendrite.mean.len))+
  geom_histogram()
outlier <- df[which.max(df$total.dendrite.mean.len),]
outlier$`Cell Name`


df_remove <- df[which.max(df$number.of.dendritic.endings),]

df_model <- df
df_model[, sapply(df_model, class) == 'factor']
vapply(df_model, function(x) length(unique(x)) > 1, logical(1L))
colnames(df_model)
df_model <- df_model[,-c(1:2,5:6,84:96,116:175,178:199,200:481)]

mod <- lm(df_model$total.dendrite.mean.len ~ 1, data=df_model)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

outlier <- df[247,]
outlier$`Cell Name`

df_phasic <- df %>% 
  filter(Subtype_Precise == 1)
outlier_phasic <- df_phasic[which.max(df_phasic$Steady.State.Input.Resistance),]
outlier_phasic$Cell.Name
