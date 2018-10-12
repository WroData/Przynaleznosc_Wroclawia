DF <- data.frame(ID = 1:10, Pop = (1:10)^2, gr = c("A", rep("B", 8), "A"))
DF_Pie <- DF %>%
  group_by(gr) %>%
  summarise(Years = n(),
            Pop_Years = sum(Pop))

A <-  ggplot(DF, aes(x = ID, col = gr, fill = gr)) +
  geom_bar()+ 
  theme(legend.position="none")

B <- ggplot(DF_Pie, aes(x = factor(1), y = Years, fill = gr))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+ 
  theme(legend.position="none")

C <- ggplot(DF, aes(x = ID, y = Pop, col = gr, fill = gr)) +
  geom_bar(stat="identity") +
  theme(legend.position="bottom")

D <- ggplot(DF_Pie, aes(x = factor(1), y = Pop_Years, fill = gr))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  theme(legend.position="none")

grid_arrange_shared_legend(A, B, C, D)

plot_list <- list(A, B, C, D)
layout <- matrix(c(1, 1, 2, 3, 3, 4), nrow = 2, byrow = TRUE)
multiplot(plotlist = plot_list, layout = layout) 

grid.arrange(A, B, C, D, top = "Common main title",
             layout_matrix = layout)
