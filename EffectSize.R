library(esc)

# Exploration to figure what I'm trying to pull
  
  df_Ephys_Clean %>% 
  group_by(Subtype_Precise) %>% 
  summarise(mean(Resonance.Frequency,na.rm=T), 
            sd(Resonance.Frequency,na.rm=T),
            n= n())

variable <- Resonance.Frequency
SummaryStats <- df_Ephys_Clean %>% 
  filter(!is.na(Resonance.Frequency)) %>% 
  group_by(Subtype_Precise) %>% 
  summarise(n=n(),
            mean = mean(Resonance.Frequency),
            sd = sd(Resonance.Frequency))
            #sem = se(Resonance.Frequency))
            
SummaryStats
ResonanceEffectSize <- (100-30.7)/(77.2) # need pooled SD here
ResonanceEffectSize

esc_mean_sd(grp1m = SummaryStats[1,3], 
            grp1sd = SummaryStats[1,4],
            grp1n=SummaryStats[1,2],
            grp2m = SummaryStats[2,3],
            grp2sd = SummaryStats[2,4],
            grp2n=SummaryStats[2,2],
            es.type = "g")



se <- function (x) {sqrt(var(x)/length(x))}

RunEffectSize <- function(df, col_name, var_name) {
  col_name <- enquo(col_name)
  var_name <- enquo(var_name)
  SummaryStats <- df %>% 
    filter(!is.na(!!var_name)) %>% 
    group_by(!!col_name) %>% 
    summarise(n=n(),
              mean = mean(!!var_name),
              sd = sd(!!var_name),
              sem = se(!!var_name))
    test <- esc_mean_sd(grp1m = SummaryStats[1,3], 
              grp1sd = SummaryStats[1,4],
              grp1n=SummaryStats[1,2],
              grp2m = SummaryStats[2,3],
              grp2sd = SummaryStats[2,4],
              grp2n=SummaryStats[2,2],
              es.type = "g")
    test1.2 <- test$es
    test <- esc_mean_sd(grp1m = SummaryStats[1,3], 
                        grp1sd = SummaryStats[1,4],
                        grp1n=SummaryStats[1,2],
                        grp2m = SummaryStats[3,3],
                        grp2sd = SummaryStats[3,4],
                        grp2n=SummaryStats[3,2],
                        es.type = "g")
    test1.3 <- test$es
    test <- esc_mean_sd(grp1m = SummaryStats[2,3], 
                        grp1sd = SummaryStats[2,4],
                        grp1n=SummaryStats[2,2],
                        grp2m = SummaryStats[3,3],
                        grp2sd = SummaryStats[3,4],
                        grp2n=SummaryStats[3,2],
                        es.type = "g")
    test2.3 <- test$es
    
    output <- as.data.frame(c(SummaryStats,test1.2,test1.3,test2.3))
    #summary(output)
    return(output)
}

compare_means(Peak.input.Resistance ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Peak.input.Resistance)
compare_means(Threshold.Relative.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test")

compare_means(Time.Constant...Neg..step ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
compare_means(Absolute.Threshold ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
compare_means(Rheobase.Current ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
compare_means(V.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
compare_means(Spike.Amp..Relative.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, comparisons = pairs)
compare_means(Threshold.Relative.Rest ~ Subtype_Precise,
              data = df_Ephys_Clean, method="kruskal.test", comparisons=pairs)


RunEffectSize(df_Ephys_Clean, Subtype_Precise, Resonance.Frequency)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Spike.Amp..Relative.Rest)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Absolute.Threshold)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Rheobase.Current)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Peak.input.Resistance)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, Time.Constant...Neg..step)
RunEffectSize(df_Ephys_Clean, Subtype_Precise, V.Rest)

RunEffectSize(df_Ephys_Clean, Subtype_Precise, Threshold.Relative.Rest)


## ADD IN SOME CODE TO CALCULATE DEGREES OF FREEDOM (df= N -k), 
#N = total sample size, k = number of samples (3)
# clean up output, then run this baby!
df_Ephys_Clean %>% 
  ggplot(aes(x=Peak.input.Resistance,
             y=EPSP.halfwidth,
              color = Subtype_Precise))+
  geom_point()+ scale_color_manual(values = POTcolors)+
  ylim(0,3.5)
  #geom_smooth(method ="lm", show.legend = T)

summary(lm(formula = EPSP.halfwidth ~ Peak.input.Resistance, data = df_Ephys_Clean))
summary(lm(formula = EPSP.halfwidth ~ Steady.State.Input.Resistance, data = df_Ephys_Clean))
