# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")

##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów ####

library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(ggrepel) #do pie chart - nie beda nachodzic na siebie teksty 
#install.packages("ggpubr")
library(ggpubr)
library(readxl)
library(scales)
#library(reshape2)
#library(gridExtra)
#library(grid)
 
#library(ggmap) #do zbierania wspó³rzednych
#library(sp)  #do zbierania wspó³rzednych
library(grid) #do polaczenia wykresow w jeden
#install.packages("gridExtra")
library(gridExtra) #do polaczenia wykresow w jeden
#install.packages("extrafont")
library(extrafont) #do czcionek
#font_import()
loadfonts()
#################################################################\
#potrzebne funkcje do wykresów

#funkcja do ³adnego formaowania liczb
space <- function(x, ...) { 
  format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}

?percent
percent(0.1)
percent(0.001)

##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\Przynale¿nosæ Wroc³awia")

#kraj
Country  <- read.csv2("czyj.csv") %>%
  mutate(Year    = Od,
         Country = Kraj) %>%
  select(Year, Country) %>%
  arrange(Year) 

#Populacja
Pop      <- read.csv2("Ludnosc.csv") %>%
  mutate(Year = as.numeric(Rok),
         Pop = as.numeric(Ludnosc)) %>%
  select(Year, Pop) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise(Pop = mean(Pop)) %>%
  mutate(Year_lead = lead(Year),
         Pop_lead  = lead(Pop),
         Year_diff = Year_lead - Year,
         Pop_diff  = Pop_lead  - Pop,
         Pop_add   = Pop_diff / Year_diff) %>%
    select(Year, Pop, Pop_add) 

#powierzchnia
Land <- read.csv2("Powierzchnia.csv")  %>%
  mutate(Year = as.numeric(Rok),
         Land = as.numeric(Powierzchnia)) %>%
  select(Year, Land) %>% 
  filter(!is.na(Year)) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise(Land = mean(Land)) %>%
  mutate(Year_lead = lead(Year),
         Land_lead = lead(Land),
         Year_diff = Year_lead - Year,
         Land_diff = Land_lead  - Land,
         Land_add  = Land_diff / Year_diff) %>%
  select(Year, Land, Land_add) 

##########################################################################
#### stworzenie tabeli danych ####

Data_long <- data.frame(Year = 850:2018) %>%
  merge(Pop, all.x = T)  %>%
  merge(Land, all.x = T) %>%
  merge(Country, all.x = T) %>%
  mutate(temp = 1,
         Country = as.character(Country),
         Country = factor(Country, levels = c("Polska", "Czechy", "Œl¹sk", "Wêgry", "Niemcy", "Francja")))
  

for(i in 1:nrow(Data_long)){
  print(i)
  if(is.na(Data_long[i, "Pop"])) {
    Data_long[i, "Pop"]     <- Data_long[i - 1, "Pop_add"] + Data_long[i - 1, "Pop"] 
    Data_long[i, "Pop_add"] <- Data_long[i - 1, "Pop_add"] 
  }
  if(is.na(Data_long[i, "Land"])) {
    Data_long[i, "Land"]     <- Data_long[i - 1, "Land_add"] + Data_long[i - 1, "Land"] 
    Data_long[i, "Land_add"] <- Data_long[i - 1, "Land_add"] 
  }
  if(is.na(Data_long[i, "Country"])) {
    Data_long[i, "Country"]     <- Data_long[i - 1, "Country"] 
  }
}

Data_long <- Data_long %>%
  filter(Year>=1000)


Data_pie <- Data_long %>%
  group_by(Country) %>%
  summarise(Years = n(),
            Pop_Years = sum(Pop),
            Land_Years = sum(Land)) %>%
  mutate(pos_Years = cumsum(Years)- Years/2,
         pos_Pop   = cumsum(Pop_Years)- Pop_Years/2,
         pos_Land  = cumsum(Land_Years)- Land_Years/2) %>%
  ungroup() %>%
  mutate(Years_share = Years / sum(Years),
         Pop_share   = Pop_Years / sum(Pop_Years),
         Land_share  = Land_Years / sum(Land_Years))


##########################################################################
#### Wykresy ####


Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 12, hjust = 0, color = "#22211d"),
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.title   = element_blank(),
                axis.title   = element_blank(),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x  = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                #axis.ticks.x = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                text = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
)  

#### po latach ####
w1 <- 
  ggplot(Data_long, aes(x = Year, col = Country, fill = Country)) +
  geom_bar() +
  coord_cartesian(xlim = c(1000, 2018)) + 
  expand_limits(x = 0, y = 0) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette  = "Spectral")  + 
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme + 
  theme(legend.position="none")


w2 <- 
  ggplot(Data_pie, aes(x = factor(1), y = Years, fill = Country))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette  = "Spectral")  + 
  geom_label_repel(aes(label = paste0(percent(Years / sum(Years)))),
                   position = position_stack(vjust = 0.5),
                   alpha = 0.8, 
                   label.padding=.1,
                   seed = 1234, 
                   family  = "Ubuntu", size = 3.5, color = "#22211d")  +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme +
  theme(axis.text.x=element_blank())+ 
  theme(legend.position="none")


#### po ludnosci ####
w3 <- 
  ggplot(Data_long, aes(x = Year, y = Pop, col = Country, fill = Country)) +
  geom_bar(stat="identity") +
  coord_cartesian(xlim = c(1000, 2018)) + 
  expand_limits(x = 0, y = 0) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette  = "Spectral")  + 
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme + 
  theme(legend.position="none")


w4 <- 
  ggplot(Data_pie, aes(x = factor(1), y = Pop_Years, fill = Country))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette  = "Spectral")  + 
  geom_label_repel(aes(label = paste0(percent(Pop_Years / sum(Pop_Years)))),
                   position = position_stack(vjust = 0.5),
                   alpha = 0.8, 
                   label.padding=.1,
                   seed = 1234, 
                   family  = "Ubuntu", size = 3.5, color = "#22211d")  + 
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme +
  theme(axis.text.x=element_blank())+ 
  theme(legend.position="none")




#### po powierzchni ####
w5 <- 
  ggplot(Data_long, aes(x = Year, y = Land, col = Country, fill = Country)) +
  geom_bar(stat="identity") +
  coord_cartesian(xlim = c(1000, 2018)) + 
  expand_limits(x = 0, y = 0) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette  = "Spectral")  + 
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme


w6 <- 
  ggplot(Data_pie, aes(x = factor(1), y = Land_Years, fill = Country))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette  = "Spectral")  + 
  geom_label_repel(aes(label = paste0(percent(Land_Years / sum(Land_Years)))),
                   position = position_stack(vjust = 0.5),
                   alpha = 0.8, 
                   label.padding=.1,
                   seed = 1234, 
                   family  = "Ubuntu", size = 3.5, color = "#22211d") + 
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "" ) +
  Theme +
  theme(axis.text.x=element_blank())+ 
  theme(legend.position="none")



p1 <- ggarrange(w1, w2, ncol=2, widths = c(0.7, 0.3)) 
p2 <- ggarrange(w3, w4, ncol=2, widths = c(0.7, 0.3)) 
p3 <- ggarrange(w5, w6, ncol=2, widths = c(0.7, 0.3),
                common.legend = TRUE, legend = "bottom") 

p1 <- annotate_figure(p1, top = text_grob("Przynale¿noœæ Wroc³awia wa¿ona latami panowania", 
                                          family  = "Ubuntu", size = 13, color = "#22211d"))
p2 <- annotate_figure(p2, top = text_grob("Przynale¿noœæ Wroc³awia wa¿ona populacj¹", 
                                          family  = "Ubuntu", size = 13, color = "#22211d"))
p3 <- annotate_figure(p3, top = text_grob("Przynale¿noœæ Wroc³awia wa¿ona powierzchni¹", 
                                          family  = "Ubuntu", size = 13, color = "#22211d"))

p <- ggarrange(p1, p2, p3, nrow=3, common.legend = TRUE, legend="bottom", heights = c(3, 3, 3.75)) 
p <- annotate_figure(p, top = text_grob("Jaki jest Wroc³aw?", 
                                        family  = "Ubuntu", size = 20, color = "#22211d"),
                     bottom = text_grob("Autor: WroData", 
                                        family  = "Ubuntu", size = 12, color = "#22211d",
                                        hjust = 1, x = 1))
p <- p +
  bgcolor("#f5f5f2")+
  border("#f5f5f2")

png(filename = paste0("wykres\\Wykres ", Sys.Date(), ".png"), width = 6, height = 8, units = 'in', res = 500)
  p
dev.off()

