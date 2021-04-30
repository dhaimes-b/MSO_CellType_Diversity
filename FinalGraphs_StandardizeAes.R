# Final Figures

# For after cleaning + filtering -
# might also need to run some of Subtype_Expanded.R

# Set up Defaults
# Labels, colors, library, seed
library(ggbeeswarm)
set.seed(13)

POTcolors <- c("#000000","#3399CC","#CC0000") # "3399CC" - oscillator blue
POTlabels <- c("Phasic", "Oscillator","Tonic")# "CC0000" - tonic red
POTAcolors <- c("#000000","#3399CC","#CC0000","#AAAAAA")
POTAlabels <- c("Phasic", "Oscillator","Tonic","All")

update_geom_defaults("point", list(shape = 1, size = 2,stroke=1.5,unit = "in"))


# Figure 1 Graphs ********************** \\

# PANEL 1 - F
#On Log Scale
pairings <- c(c("Phasic", "Oscillator"),c("Phasic","Tonic"),c("Oscillator","Tonic"),
              c("Phasic", "All"), c("Oscillator","All"), c("Tonic","All"))
pairs <- c(c("Phasic", "Oscillator"),c("Phasic","Tonic"),c("Oscillator","Tonic"))

df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             #y=Steady.State.Input.Resistance,
             y=Peak.input.Resistance,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5,bandwidth = .3)+
  geom_boxplot(outlier.alpha = 0, fill = NA, coef = 0)+
  scale_color_manual(values = POTAcolors)+
  theme_pubr()+
  scale_x_discrete(labels=POTAlabels)+
  scale_y_continuous(trans = "log10",limits = c(4,1000))+
  annotation_logticks(sides = "l")+
  ylab("Peak Input Resistance (log MOhm)")+
  xlab("Firing Types" )+
  stat_compare_means(label = "p.signif", ref.group = ".all.")+
  theme(legend.position="none")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/PeakRin_FiringTypeAll.eps", width = 4.5, height = 2.55, unit = "in")
#Not on Log scale
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=Steady.State.Input.Resistance,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  scale_color_manual(values = POTAcolors)+
  theme_pubr()+
  scale_x_discrete(labels=POTAlabels)+
  ylab("Steady State Input Resistance")+
  xlab("Firing Types" )+
  theme(legend.position="none")

# 1 -G
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=Spike.Amp..Relative.Rest,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  geom_boxplot(outlier.alpha = 0, fill = NA, coef = 0)+
  scale_color_manual(values = POTAcolors)+
  theme_pubr()+
  scale_x_discrete(labels=POTAlabels)+
  stat_compare_means(label = "p.signif", ref.group = ".all.")+
  ylim(0,100)+
  ylab("Action Potential Amplitude (mV)")+
  xlab("Firing Types" )+
  theme(legend.position="none")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/APAmp.eps", width = 4.5, height = 2.55, unit = "in")

# 1 - H
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=Absolute.Threshold,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  geom_boxplot(outlier.alpha = 0, fill = NA, coef = 0)+
  scale_color_manual(values = POTAcolors)+
  theme_pubr()+
  scale_x_discrete(labels=POTAlabels)+
  stat_compare_means(label = "p.signif", ref.group = ".all.")+
  ylab("Absolute Threshold (mV)")+
  xlab("Firing Types" )+
  theme(legend.position="none")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/AbsThresh.eps", width = 4.5, height = 2.55, unit = "in")

# 1 - I
df_Subtype_All %>% 
  ggplot(aes(x=Subtype_Expand, 
             y=Resonance.Frequency,
             color = Subtype_Expand)) +
  geom_quasirandom(varwidth = T,dodge.width = .5)+
  geom_boxplot(outlier.alpha = 0, fill = NA, coef = 0)+
  scale_color_manual(values = POTAcolors)+
  theme_pubr()+
  scale_x_discrete(labels=POTAlabels)+
  stat_compare_means(label = "p.signif", ref.group = ".all.")+
  ylab("Resonance Frequency (Hz)")+
  xlab("Firing Types" )+
  theme(legend.position="none")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/Resonance.eps", width = 4.5, height = 2.55, unit = "in")


# Getting statistics for comparisons
compare_means(Steady.State.Input.Resistance ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test", comparisons=pairs)
compare_means(Steady.State.Input.Resistance ~ Subtype_Precise,
              data = df_Ephys_Clean, ref.group=".all.")
compare_means(Spike.Amp..Relative.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test", comparisons=pairs)
compare_means(Spike.Amp..Relative.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, comparison = pairs)
compare_means(Absolute.Threshold ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test", comparisons=pairs)
compare_means(Absolute.Threshold ~ Subtype_Precise,
              data = df_Ephys_Clean, comparison= pairs)
compare_means(Resonance.Frequency ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test", comparisons=pairs)
compare_means(Resonance.Frequency ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)

compare_means(Rheobase.Current ~ Subtype_Precise,
              data = df_Ephys_Clean, comparison= pairs)

# *************************************** \\
# updates based on Nace notes 12/31/20
# removed row 419&526 (outlier)

# Branchpoints
my.formula <- y~x
df_Morph_Clean %>% 
  ggplot(aes(y = number.of.dendritic.nodes, 
             x= DV_NLG,
             color = Subtype_Precise)) +
  geom_smooth(method = "lm",se = F, lwd=2)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")),
               label.x = .5,
               parse = TRUE,position = "dodge") +   
  geom_point() +
  scale_color_manual(name = "Firing Type", values = POTcolors, 
                     labels = POTlabels)+
  xlim(0,1)+ylim(0,30)+
  ylab("Number of Branchpoints")+xlab("D-V Normalized Position")+
  theme_pubr()+
  ggtitle("Update 3-9-21: Branch Points")
#ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/BranchPts.eps", width = 4.5, height = 2.55, unit = "in")

# Dendritic complexity - total.dendrite.Complexity
df_Morph_Clean %>% 
  ggplot(aes(y = total.dendrite.Complexity, 
             x= DV_NLG,
             color = Subtype_Precise)) +
  geom_smooth(method = "lm",se = F, lwd= 2)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")),
               label.x = .5,
               parse = TRUE,position = "dodge") +   
  geom_point() +
  scale_color_manual(name = "Firing Type", values = POTcolors, 
                     labels = POTlabels)+
  xlim(0,1)+ylim(0,30000)+
  ylab("Dendritic Complexity")+xlab("D-V Normalized Position")+
  theme_pubr()+
  ggtitle("Update 3-9-21: Dendritic Complexity")

# Mean dendritic length - total.dendrite.mean.len
df_Morph_Clean %>% 
  ggplot(aes(y = total.lateral.dendrite.mean.len, 
             x= DV_NLG,
             color = Subtype_Precise)) +
  geom_smooth(method = "lm",se = F, lwd = 2)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")),
               label.x = .5,
               parse = TRUE,position = "dodge") +   
  geom_point() +
  scale_color_manual(name = "Firing Type", values = POTcolors, 
                     labels = POTlabels)+
  xlim(0,1)+ylim(0,500)+
  ylab("Mean Dendritic Length")+xlab("D-V Normalized Position")+
  theme_pubr()+
  ggtitle("Update 3-9-21: Mean LATERAL Dendritic Length")
df_Morph_Clean %>% 
  ggplot(aes(y = total.Medial.dendrite.mean.len, 
             x= DV_NLG,
             color = Subtype_Precise)) +
  geom_smooth(method = "lm",se = F, lwd = 2)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")),
               label.x = .5,
               parse = TRUE,position = "dodge") +   
  geom_point() +
  scale_color_manual(name = "Firing Type", values = POTcolors, 
                     labels = POTlabels)+
  xlim(0,1)+ylim(0,500)+
  ylab("Mean Dendritic Length")+xlab("D-V Normalized Position")+
  theme_pubr()+
  ggtitle("Update 3-9-21: Mean MEDIAL Dendritic Length")


# Age Distr
df_Filter <- df_Ephys_Clean %>% 
  mutate(AgeBin = ifelse(Age > 28, 0, 1))
group_means <- df_Filter %>% 
  group_by(Subtype_Precise, AgeBin) %>% 
  summarise(mean = mean(Spike.Amp..Relative.Rest,na.rm = T),
            sdv = sd(Spike.Amp..Relative.Rest, na.rm=T))
group_means <- group_means[-c(3,6,9),]

df_Filter <- df_Filter %>% 
  filter(Age > 0)


df_Filter %>% 
  ggplot(aes(x= Age,
             y= Spike.Amp..Relative.Rest,
             color = Subtype_Precise,
             shape = factor(AgeBin))) +
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+ 
  # geom_boxplot(outlier.shape = NA)+
  scale_shape_manual(values = c(1,16))+
  stat_boxplot(geom= "errorbar", width=.6)+
  geom_boxplot(width=0.6, fill="lightgrey",outlier.shape = NA)+
  
  geom_point()+ 
  theme_pubr()+ylab("Spike Amplitude Relative Rest")+
  facet_wrap(~Subtype_Precise)
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/AgeDistr_SpikeAmplitude.eps", width = 5, height = 5, unit = "in")



df_Filter %>% 
  ggplot(aes(x= Age,
             y= Steady.State.Input.Resistance,
             color = Subtype_Precise,
             shape = factor(AgeBin))) +
  stat_boxplot(geom= "errorbar", width=.6, color = "grey")+
  geom_boxplot(width=0.6, fill="lightgrey",outlier.shape = NA)+
  geom_point()+
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+  
  scale_shape_manual(values = c(1,16))+
  theme_pubr()+ylab("Input Resistance (log)")+
  scale_y_continuous(trans="log10")+
  facet_wrap(~Subtype_Precise)
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/AgeDistr_InputResistance.eps", width = 5, height = 5, unit = "in")


df_Ephys_Clean %>% 
  filter(Subtype_Precise == 3) %>% 
  summarise(Age)

df_Filter %>% 
  group_by(Subtype_Precise) %>% 
  summarise(AgeBin) %>% 
  table()



# Morph PCA

fviz_pca_ind(morph_pca, geom.ind= "point", pointshape = 21,
             pointsize = 3,stroke=2,
             fill.ind = removed.morph_pca$Subtype_Precise,
             col.ind="black",
             palette = POTcolors,
             addEllipses = F)+theme_pubr()+theme(legend.position="none")

# Ephys PCA






df_Ephys_Clean %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             #y= ,
             fill = Subtype_Precise)) +
  geom_histogram(binwidth = 10, stat = ..percent..)+
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  theme_pubr()+
#  ylab("Spike Amplitude (Rel. Threshold [mV])")+
  xlab("Postnatal Age (Days)") +
  xlim(0,80)+
  theme(legend.position="none")

df_Ephys_Clean %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= Threshold.Relative.Rest,
             color = Subtype_Precise)) +
  geom_jitter()+
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  theme_pubr()+
 # ylab("Spike Amplitude (Rel. Threshold [mV])")+
  xlab("Postnatal Age (Days)") +
  xlim(0,80)+
  theme(legend.position="none")


df_Ephys_Clean %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= Absolute.Threshold,
             color = Subtype_Precise)) +
  geom_jitter()+
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  theme_pubr()+
  # ylab("Spike Amplitude (Rel. Threshold [mV])")+
  xlab("Postnatal Age (Days)") +
  xlim(0,80)+
  theme(legend.position="none")

df_Ephys_Clean %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= Threshold.Relative.Rest,
             color = Subtype_Precise)) +
  geom_jitter()+
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  theme_pubr()+
  # ylab("Spike Amplitude (Rel. Threshold [mV])")+
  xlab("Postnatal Age (Days)") +
  xlim(0,80)+
  theme(legend.position="none")


df_Ephys_Clean %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= DV_NLG,
             color = Subtype_Precise)) +
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  scale_y_continuous(trans = "reverse")+
  geom_jitter()+
  theme_pubr()+
  ylab("Tonotopic Position")+
  xlab("Postnatal Age (Days)") +
   xlim(0,80)+
  theme(legend.position="none")
ggsave("C:/Users/dhaim/Box/MSO Cell Type - Brian Data/Tonotopy_AgeDistr.eps", width = 4.5, height = 3, unit = "in")


p1 <- df_Ephys_Clean %>% 
  filter(Age < 28) %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= Spike.Amp..Relative.Rest)) +
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  geom_point()+
  theme_pubr()+
  facet_wrap(~Subtype_Precise + (Age > 28))


p2 <- df_Ephys_Clean %>% 
  filter(Age > 28) %>% 
  filter(Age > 0) %>% 
  ggplot(aes(x= Age,
             y= Spike.Amp..Relative.Rest,
             color = Subtype_Precise)) +
  scale_color_manual(name = "Subtype", values = POTcolors, 
                     labels = POTlabels)+
  geom_point()+
  theme_pubr()
grid.arrange(p1,p2)




  
  df_Ephys_Clean %>% 
    filter(Age > 0) %>% 
    ggplot(aes(x= Age,
               y= Spike.Amp..Relative.Thresh.,
               color = Subtype_Precise)) +
    geom_jitter()+
    scale_color_manual(name = "Subtype", values = POTcolors, 
                       labels = POTlabels)+
    theme_pubr()+
    ylab("Spike Amplitude (Rel. Threshold [mV])")+
    xlab("Postnatal Age (Days)") +
    xlim(0,80)+
    theme(legend.position="none")
  
  
  
# Figure 5 EPSP Stats
  
  compare_means(EPSP.halfwidth ~ Subtype_Precise,
                data = df_Ephys_Clean, method="kruskal.test")#, comparisons=pairs)
  compare_means(EPSP.halfwidth ~ Subtype_Precise,
                data = df_Ephys_Clean, comparisons = pairs)

  
  
  
  df_Ephys_Clean %>% 
    filter(Age > 0) %>% 
    ggplot(aes(x= FFT.Peak.Freq,
               y= log(FFT.Peak.power),
               color = Subtype_Precise)) +
    geom_jitter()+
    scale_color_manual(name = "Subtype", values = POTcolors, 
                       labels = POTlabels)+
    theme_pubr()+
    ylab("power")+
    xlab("freq") +
    theme(legend.position="none")
  