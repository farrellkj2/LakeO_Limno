## Basic 'heat map' style plots of Lake O Limno data ##
#### Load packages & import data ####
# install.packages('pacman') 
pacman::p_load(RColorBrewer, tidyverse, zoo) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=18),
                 panel.background = element_rect(fill = NA, color = "black"),
                 strip.background = element_rect(fill = NA, color = 'black'),
                 legend.title=element_text(size=14), 
                 legend.text=element_text(size=14))

phys <- read_csv('./data/LakeO_data_Physical.csv') %>% 
  pivot_longer(names_to = 'Var', values_to = 'Value', Temp:pH) %>% 
  group_by(Var, Year, Depth) %>% 
  summarize(Value = round(mean(Value),2)) %>% ungroup() %>% 
  arrange(Var, Year, Depth) %>% 
  mutate(Filled = ifelse((Depth >0), round(na.approx(Value, maxgap = 1, rule=2),2), NA),
         Filled = ifelse(is.na(Filled), Value, Filled)) %>% 
  select(-Value) %>% 
  pivot_wider(names_from = Var, values_from = Filled)

chem <- read_csv('./data/LakeO_data_Chemical.csv') %>%
  pivot_longer(names_to = 'Var', values_to = 'Value', Alkalinity:Chla) %>% 
  group_by(Var, Year, Depth) %>% 
  summarize(Value = round(mean(Value),2)) %>% 
  pivot_wider(names_from = Var, values_from = Value) %>% ungroup()

LakeO <- left_join(phys, chem) %>% 
  filter(Depth <7.5) %>% 
  mutate(DIN = NO3 + NH4,
         Zone = ifelse(Depth >=5, "Hypolimnion", 
                       ifelse(Depth <=2, "Epilimnion", "Meta")))

long <- LakeO %>% select(-Zone) %>% 
  pivot_longer(names_to = 'Var', values_to = 'Value', DO:DIN) 

#### Heatmaps ####
## Water temp ##
Temp <- long %>% filter(Var == "Temp") %>% 
  mutate(Value = ifelse(Value > 31.9, NA, Value))

ggplot(Temp, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
  ggtitle(expression(Water~temperature ~ (degree*C)))+
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

## DO ##
DO <- long %>% filter(Var == "DO")

ggplot(DO, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"BuPu"))) +
  #scale_fill_viridis(option='magma') +
  ggtitle(expression(Dissolved~oxygen~ (mg ~ L^{-1})))+
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

## Chla ##
chla <- long %>% filter(Var == "Chla") %>% na.omit()

ggplot(chla, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=brewer.pal(7,"YlGn")) +
  ggtitle(expression(Chlorophyll-a ~ (mu*g ~ L^{-1}))) +
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

## SRP ##
SRP <- long %>% filter(Var == "SRP") %>% na.omit()

ggplot(SRP, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=brewer.pal(7,"YlOrRd")) +
  ggtitle(expression(Soluble~reactive~phosphorus ~ (mu*g ~ L^{-1}))) +
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

## Nitrogen species ##
DIN <- long %>% filter(Var == "DIN") %>% na.omit()

ggplot(DIN, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=brewer.pal(7,"YlGnBu")) +
  ggtitle(expression(Dissolved~inorganic~nitrogen ~ (mu*g ~ L^{-1}))) +
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

NH4 <- long %>% filter(Var == "NH4") %>% na.omit()

ggplot(NH4, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=brewer.pal(7,"YlGnBu")) +
  ggtitle(expression(Ammonium ~ (mu*g ~ L^{-1}))) +
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))

NO3 <- long %>% filter(Var == "NO3") %>% na.omit()

ggplot(NO3, aes(x = Year, y = Depth, fill = Value)) + mytheme +
  geom_tile()+
  scale_x_discrete() +
  scale_y_continuous(trans = 'reverse', breaks=seq(0,7,0.5)) +
  scale_fill_gradientn(colours=brewer.pal(7,"YlGnBu")) +
  ggtitle(expression(Nitrate ~ (mu*g ~ L^{-1}))) +
  theme(axis.text.x = element_text(angle = -90, vjust=0.5, hjust=0))
