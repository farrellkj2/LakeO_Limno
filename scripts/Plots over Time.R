## Plot Lake O. trends over time ##
#### Load packages & import data ####
# install.packages('pacman') 
pacman::p_load(tidyverse) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=18),
                 panel.background = element_rect(fill = NA, color = "black"),
                 strip.background = element_rect(fill = NA, color = 'black'),
                 legend.title=element_text(size=14), 
                 legend.text=element_text(size=14),
                 legend.background = element_rect(fill = NA))

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

#### Temperatures in Epi vs. Hypo ####
ggplot(subset(LakeO, Zone != "Meta"), aes(x = Year, y = Temp, col= Zone)) +
  geom_point(size = 2) + mytheme + 
  stat_summary(aes(group=Zone), fun="median", geom= "line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  scale_x_continuous(breaks=seq(2008,2019,1))+
  scale_y_continuous(breaks=seq(10,35,5), limits=c(10,35)) +   
  labs(y = expression(Water~temperature ~ (degree*C))) +
  theme(legend.position= c(0,1), legend.justification = c(-.1,1),
        axis.text.x = element_text(angle = -45, vjust=0.5, hjust=.3))

#### DO in Epi vs. Hypo ####
ggplot(subset(LakeO, Zone != "Meta"), aes(x = Year, y = DO, col= Zone)) + mytheme + 
  scale_y_continuous(breaks=seq(0,16,2), limits=c(0,16)) +   
  geom_point(size = 2) +
  stat_summary(aes(group=Zone), fun="median", geom= "line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  scale_x_continuous(breaks=seq(2008,2019,1))+
  labs(y = expression(Dissolved~oxygen~(mg~L^-1))) +
  theme(legend.position= c(0,1), legend.justification = c(-.1,1))

#### SRP (2009 data likely bad) ####
ggplot(LakeO, aes(x = Year, y = SRP)) + mytheme + 
  #scale_y_continuous(breaks=seq(0,50,10)) +   # Includes 2009
  geom_point(size = 2, col='dodgerblue') +
  stat_summary(fun="median", geom="line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
    scale_x_continuous(breaks=seq(2008,2019,1))+
  labs(y = expression(Soluble~reactive~phosphorus ~ (mu*g ~ L^{-1})))

# Exclude 2009 data, 2015 outlier point
ggplot(subset(LakeO, Year !='2009'), aes(x = Year, y = SRP)) + mytheme + 
  scale_y_continuous(breaks=seq(0,25,5), limits=c(0,25)) +
  geom_point(size = 2, col='dodgerblue') +
  stat_summary(fun="median", geom="line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  #geom_smooth(method='lm',se=F, col='black', lty=2, lwd=.5) + # Linear fit over time
  scale_x_continuous(breaks=seq(2008,2019,1))+
  labs(y = expression(Soluble~reactive~phosphorus ~ (mu*g ~ L^{-1})))

nitrogen <- LakeO %>% select(Depth, Year, NH4, NO3, DIN) %>%
  group_by(Year, Depth) %>%
  gather(Metric, Value, NH4:DIN, na.rm=T) %>% filter(!is.na(Value))

ggplot(nitrogen, aes(x = Year, y = Value, col = Metric)) + mytheme + 
  geom_point(size = 2) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  stat_summary(fun="median", geom="line") +
  facet_grid(Metric~., scales= 'free_y') +
  scale_x_continuous(breaks=seq(2008,2019,1))+
  labs(y = expression(Dissolved~inorganic~nitrogen ~ (mu*g ~ L^{-1}))) + 
  theme(legend.position= 'none')

#### Chlorophyll-a ####
ggplot(LakeO, aes(x = Year, y = Chla)) + mytheme +
  geom_point(size = 2, col = 'darkgreen') +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  stat_summary(fun="median", geom="line") +
  scale_x_continuous(breaks=seq(2008,2019,1)) +
  scale_y_continuous(breaks=seq(0,300,50)) +   
  labs(y = expression(Chlorophyll~a ~ (mu*g ~ L^{-1})))

# Depth of chl-a maximum
chla_peak <- LakeO %>% select(Depth, Year, Chla) %>%
  filter(!is.na(Chla)) %>% 
  group_by(Year) %>%  arrange(Chla) %>% 
  summarize(maxChlaDepth = Depth[which.max(Chla)])

ggplot(chla_peak, aes(x = Year, y = maxChlaDepth)) + mytheme +
  geom_point(size = 2, col = 'darkgreen') +
  geom_line() +
  scale_x_continuous(breaks=seq(2008,2019,1)) +
  scale_y_reverse(limits=c(8,0)) +   
  labs(y = "Depth of chlorophyll maximum (m)")

#### Specific Conductance ####
ggplot(subset(LakeO, Zone != "Meta"), aes(x = Year, y = SpCond, col= Zone)) + mytheme + 
  geom_point(size = 2) +
  stat_summary(aes(group=Zone), fun="median", geom= "line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  scale_x_continuous(breaks=seq(2008,2019,1))+
  labs(y = expression(Specific~conductance~(mu*S~cm^-1))) +
  theme(legend.position= c(0,1), legend.justification = c(-.1,1)) +
  geom_rect(data=subset(LakeO, Zone != "Meta"), 
            mapping=aes(xmin=2016.1, xmax=2018.92, ymin=-Inf, ymax=250), 
                        fill='white',color=NA, alpha=0.8)

#### pH ####
ggplot(subset(LakeO, Zone != "Meta"), aes(x = Year, y = pH, col= Zone)) + mytheme + 
  scale_y_continuous("pH", limits=c(6,12)) +
  geom_point(size = 2) +
  stat_summary(aes(group=Zone), fun="median", geom= "line", lwd=1) +
  stat_summary(fun="median", geom="point",pch=21,  size=3.5, fill='black') +
  scale_x_continuous(breaks=seq(2008,2019,1))+
  theme(legend.position= c(0,1), legend.justification = c(-.1,1))
