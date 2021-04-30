# Let's use this to build an "ALL" Subtype for graphing a 4th group
# in addition to all the split ones

#if pulling from outlier analyses:
df_subtype <- df_Ephys_Clean
#df_subtype <- df

df_recode <- df_subtype %>% 
  mutate(Subtype_Expand = 0)
df_recode_2 <- df_subtype %>% 
  mutate(Subtype_Expand = Subtype_Precise)

df_Subtype_All <- rbind(df_recode,df_recode_2)
df_Subtype_All$Subtype_Expand <- factor(df_Subtype_All$Subtype_Expand,
                                        levels=c(1,2,3,0))


# Test

df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, y=df_Subtype_All$Steady.State.Input.Resistance)) +
  geom_jitter(width = 0.4,height = 5)+
  ylim(0,310)

df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, y=df_Subtype_All$Steady.State.Input.Resistance)) +
  geom_boxplot()+
  ylim(0,310)



## Exploratory graphing to see if "All" group is working

library(ggbeeswarm)
set.seed(13)
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=df_Subtype_All$Steady.State.Input.Resistance,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  scale_color_manual(values = c("black","blue","red","grey"))+
  theme_pubclean()+
  scale_x_discrete(labels=c("Phasic","Oscillator","Tonic","All"))+
  scale_y_log10()+
  ylab("Steady State Input Resistance (log MOhm)")+
  xlab("Firing Types" )+
    theme(legend.position="none")
  
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=log(df_Subtype_All$Peak.input.Resistance),
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  scale_color_manual(values = c("black","blue","red","grey"))+
  theme_pubclean()+
  scale_x_discrete(labels=c("Phasic","Oscillator","Tonic","All"))+
  ylab("Peak Input Resistance (log MOhm)")+
  xlab("Firing Types" )+
  theme(legend.position="none")

