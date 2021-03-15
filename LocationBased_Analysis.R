df3 <- read_xlsx(path = "Data_Extraction_V8.xlsx")

df <- df3[-c(1:2,419,526),]
#df<- df[-c(419,526),]
df3_row <- df3[-c(1:2,419,526),]
colnames(df) <- make.names(colnames(df), unique=TRUE)


#filtering
# df <- df %>%
#   filter(!is.na(V.D.Norm..Position...Imprecise))
# df <- df %>%
#   filter(!is.na(R.C.Norm..Position.Imprecise))
# df <- df %>%
#   filter(is.na(Excluded_Physiology)) %>%
#   filter(is.na(Excluded_Anatomy))
# df <- df[,colSums(is.na(df))<(nrow(df)/2)]


#convert to df
df <- as.data.frame(sapply(df[,2:ncol(df)],as.numeric))
df <- cbind(df3_row[,1:2],df)

## Collapsing 1&1.5 into 1, 3&3.5 into 3, and everything else (2&2.5) into 2
# removing 0, 8 (?), and 9 (?)
df <- df %>%
  filter(Subtype...Broad != 0) %>%
  filter(Subtype...Broad != 8) %>%
  filter(Subtype...Broad != 9) %>%
  mutate(Subtype_Precise = ifelse(Subtype...Broad < 2, 1,
                                  ifelse(Subtype...Broad >= 3, 3, 2)))

df$Subtype_Precise <- as.factor(df$Subtype_Precise)

df <- df %>%
  mutate(DV_NLG = 1-(df$V.D.Norm..Position...Imprecise))

################
apply(df, 2, function(x) length(which(is.na(x)))) #find the total # of NA's by col
df_All_Thresh <- df[ , colSums(is.na(df)) < (nrow(df)/2)]  


df_imp <- mice(df_All_Thresh,m = 5,seed=101)

df_All_Thresh[, 2:148] <- sapply(df_All_Thresh[, 2:148], as.numeric)

df_All_Thresh <- df_All_Thresh %>% 
  mutate()


df_All_Thresh %>% 
  ggplot(aes(x = VD.Norm..Position...Precise, 
             y= ,
             color = Subtype)) +
  geom_point()
  

#################
# EXPLORATORY GRAPHING OF ALL DATA - PARTICULAR EMPHASIS ON SUBTYPES, LOCATION, AND MORPH

df %>% 
  ggplot(aes(x = VD.Norm..Position...Precise, 
             y=  Soma.Area,
             color = as.factor(Subtype...Broad))) +
  geom_point() +
  geom_smooth(method = "lm",se = F)

df %>% 
  ggplot(aes(x = VD.Norm..Position...Precise, 
             y= total.Medial.dendrite.Length)) +
  geom_point() +
  geom_smooth(method = "lm",se = F)
  facet_wrap(~Subtype...Broad)

  
df<- df %>% 
  mutate(VorD = ifelse(VD.Norm..Position...Precise > .5, "V", "D"))
  
df %>% 
      ggplot(aes(y = total.dendrite.Length, 
               x= VD.Norm..Position...Precise,
                 color = as.factor(Subtype...Broad))) +
    geom_point() +
    geom_smooth(method = "lm",se = F)
  df %>% 
    ggplot(aes(y = number.of.dendritic.nodes, 
               x= VD.Norm..Position...Precise,
               color = as.factor(Subtype...Broad))) +
    geom_point() +
    geom_smooth(method = "lm",se = F)
  df %>% 
    ggplot(aes(y = total.dendrite.Volume, 
               x= VD.Norm..Position...Precise,
               color = as.factor(Subtype...Broad))) +
    geom_point() +
    geom_smooth(method = "lm",se = F)
  

  
# this isn't that helpful since the whole paper would need to be revised to assign cells by FI/FFT
#   df %>% 
#     ggplot(aes(x= VD.Norm..Position...Precise,
#                y= total.dendrite.Length,
#                  color = FFT.Baseline.Voltage)) +
#     geom_point()+
#     scale_color_gradient(low = "black",high="red")+
#     theme_pubclean()
#   
  


my.formula <- y~x
df %>% 
  ggplot(aes(y = df$total, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Average Dendritic Length")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()

df %>% 
  ggplot(aes(y = number.of.dendritic.nodes, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Number of Dendritic Nodes")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()

df %>% 
  ggplot(aes(y = Soma.Area, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Soma Area")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()
  
df %>% 
  ggplot(aes(y = longest.dendrite, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Longest Dendrite")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()

df %>% 
  ggplot(aes(y = df$total.dendrite.mean.len, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Mean Length")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()


df %>% 
  ggplot(aes(y = df$total.dendrite.Complexity, 
             x= VD.Norm..Position...Precise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Dendritic Complexity")+xlab("Ventral Dorsal Normalized Position")+
  theme_pubclean()




df %>% 
  ggplot(aes(y = df$total.dendrite.mean.len, 
               x= df$R.C.Norm..Position.Imprecise,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  xlim(0,1.5)+
  ylab("Dendritic Complexity")+xlab("RC Normalized Position")+
  theme_pubclean()

df %>% 
  ggplot(aes(x= df$Age, 
             y= df$Subtype,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  ylab("Subtype")+xlab("Age")+
  theme_pubclean()


df %>% 
  ggplot(aes(x= Age, fill = Subtype_Precise))+
  geom_histogram(position = "identity",alpha=1, bins = 50)+
  scale_fill_manual(values = c("grey","blue","red"),
                    labels = c("Phasic","Oscillator","Tonic"))+
    ylab("Number of Cells")+
    xlab("Postnatal Day")+
    labs(fill = "Firing Types")+
  theme_minimal()
  #facet_wrap(~Subtype_Precise)


df %>% 
  ggplot(aes(x= Age, fill = Subtype_Precise))+
  geom_histogram(position = "identity",alpha=1, bins = 50)+
  scale_fill_manual(values = c("grey","blue","red"),
                    labels = c("Phasic","Oscillator","Tonic"))+
 # geom_density(aes(y=..count..), colour="black", adjust=4)+
 # stat_function(fun = dnorm, args = list(mean = mean(df$Subtype_Precise), sd = sd(df$Subtype_Precise)))+

  ylab("Number of Cells")+
  xlab("Postnatal Day")+
  labs(fill = "Firing Types")+
  theme_minimal()
# Look up how to graph 3D graphs in R!!!!!


binwidth = 55 # passed to geom_histogram and stat_function
#set.seed(1)
subtype.labs <- c("1" = "Phasic","2" = "Oscillator","3" = "Tonic")

df %>% 
  ggplot(aes(x= Age))+
  geom_histogram(position = "identity",alpha=1, bins = 55)+
  xlim(0,80)+
  facet_wrap(~Subtype_Precise,
             labeller = labeller(Subtype_Precise = subtype.labs))+

  ylab("Number of Cells")+
  xlab("Postnatal Day")+
  labs(fill = "Firing Types")+
  theme_minimal()

df %>% 
  ggplot(aes(x=Age, y = df$Spike.Amp..Relative.Rest, 
             color = df$Subtype_Precise)) +
  geom_point() + 
  scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()

df %>% 
  ggplot(aes(x = df$Threshold.Relative.Rest, y = df$Spike.Amp..Relative.Thresh.,
             color = df$Subtype_Precise))+
  geom_point()+ 
  scale_color_manual(values = c("black","blue","red"))




# *************************************** \\
# updates based on Nace notes 12/18/20
# removed row 526 (outlier) at beginning


df %>% 
  ggplot(aes(y = df$number.of.dendritic.nodes, 
             x= df$DV_NLG,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  xlim(0,1)+
  ylab("Number of Branchpoints")+xlab("D-V Normalized Position")+
  theme_pubclean()+
  ggtitle("Update 12-26-20: Branch Points")

df %>% 
  ggplot(aes(y = df$total.dendrite.Complexity, 
             x= DV_NLG,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  xlim(0,1)+
  ylab("Dendritic Complexity")+xlab("D-V Normalized Position")+
  theme_pubclean()+
  ggtitle("Update 12-26-20: Dend Complexity")


df %>% 
  ggplot(aes(y = df$total.dendrite.mean.len, 
             x= DV_NLG,
             color = as.factor(Subtype_Precise))) +
  geom_smooth(method = "lm",se = F)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point() +
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  xlim(0,1)+
  ylab("Mean Dendritic Length")+xlab("D-V Normalized Position")+
  theme_pubclean()+
  ggtitle("Update 12-26-20: Mean Dend Length")

df %>% 
  filter(Age != 0) %>% 
  ggplot(aes(x= Age,
             y= Spike.Amp..Relative.Thresh.,
             color = Subtype_Precise)) +
  geom_jitter()+
  scale_color_manual(name = "Subtype", values = c("black","blue","red"), labels = 
                       c("Phasic", "Oscillator","Tonic"))+
  theme_pubclean()+
  ylab("Spike Amplitude (Rel. Threshold [mV])")+
  xlab("Postnatal Age (Days)") +
  xlim(0,80)


#REDO INPUT