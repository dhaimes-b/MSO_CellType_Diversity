# Pull example cell lists for Nace

filtered <- df %>% 
  filter(Age < 45)

filtered %>% 
  ggplot(aes(x=Age, y = Spike.Amp..Relative.Rest, 
             color = Subtype_Precise)) +
  geom_point() + 
  scale_color_manual(values = c("black","blue","red"))+
  theme_minimal()

ordered <- filtered[order(filtered$Age, decreasing = T),]
pick <- ordered %>% 
  select(`Cell Name`,Age, Subtype_Precise)

return <- pick[1:20,]

write.csv(return, file = "Example_Cells.csv")
