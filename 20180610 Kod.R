# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")

##########################################################################
#### upewnienie si� �e nie ma �adnych pakiet�w za�adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od��czeni wszytkich pakiet�w - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo�anie funkcji 


##########################################################################
#### za�adowanie pakiet�w ####

library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(scales)
#library(reshape2)
#library(gridExtra)
#library(grid)

#library(ggmap) #do zbierania wsp�rzednych
#library(sp)  #do zbierania wsp�rzednych
library(grid) #do polaczenia wykresow w jeden
#install.packages("gridExtra")
library(gridExtra) #do polaczenia wykresow w jeden
#install.packages("extrafont")
library(extrafont) #do czcionek
#font_import()
loadfonts()
#################################################################\
#potrzebne funkcje do wykres�w

#funkcja do �adnego formaowania liczb
space <- function(x, ...) { 
  format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}
# Multiple plot function
# �r�d�o: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\Przynale�nos� Wroc�awia")

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
         Country = factor(Country, levels = c("Polska", "Czechy", "�l�sk", "W�gry", "Niemcy", "Francja")))
  

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
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.title   = element_blank(),
                axis.title   = element_blank(),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x  = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                #axis.ticks.x = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                text = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
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
  labs(title = "blabla",
       subtitle = "bla",
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
  geom_text(aes(label = paste0(percent(Years / sum(Years)))),
                position = position_stack(vjust = 0.5)) +
  labs(title = "blabla",
       subtitle = "bla",
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
  labs(title = "blabla",
       subtitle = "bla",
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
  geom_text(aes(label = paste0(percent(Pop_Years / sum(Pop_Years)))),
            position = position_stack(vjust = 0.5)) +
  labs(title = "blabla",
       subtitle = "bla",
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
  labs(title = "blabla",
       subtitle = "bla",
       x = "",
       y = "",
       caption = "" ) +
  Theme


w6 <- 
  ggplot(Data_pie, aes(x = factor(1), y = Land_Years, fill = Country))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette  = "Spectral")  + 
  geom_text(aes(label = paste0(percent(Land_Years / sum(Land_Years)))),
            position = position_stack(vjust = 0.5)) +
  labs(title = "blabla",
       subtitle = "bla",
       x = "",
       y = "",
       caption = "" ) +
  Theme +
  theme(axis.text.x=element_blank())+ 
  theme(legend.position="none")




grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}



#grid_arrange_shared_legend(w1, w2, w3, w4, w5, w6)

plot_list <- list(w1, w2, w3, w4, w5, w6)
layout <- matrix(c(1, 1, 2, 3, 3, 4, 5, 5, 6), nrow = 3, byrow = TRUE)
multiplot(plotlist = plot_list, layout = layout) 