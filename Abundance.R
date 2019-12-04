Abundance <- read.table("Abundance.txt",header = TRUE,sep = "\t")

library(ggplot2)

cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#D55E00")
Palette <- c("#000000", "#000000", "#000000", "#000000", "#000000", "#CC79A7", "#D55E00")
Abundance$Taxon <- factor(Abundance$Taxon,levels = c("0 d","3 d","6 d","9 d","12 d","15 d","18 d","21 d","24 d"))

p <- ggplot(Abundance,aes(Taxon,Fusobacterium,fill = Group)) + 
  geom_bar(stat = "identity",width = 0.8) +
  annotate(geom = "text",x = 4,y = 0.4,label = "Diarrhea",size=10,hjust=0.5,vjust=-0.5,fontface = "bold") +
  scale_colour_manual(values=Palette)+
  scale_fill_manual(values=cbbPalette,guide = FALSE)+
  ylab("The relative abundance of Fusobacterium")+
  theme_bw()+
  theme(axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour='black', size=20,face = "bold",vjust = 3),
        axis.text.y=element_text(colour='black',size=18,face = "bold"),
        axis.text.x=element_text(colour = "black",size = 20,face = "bold"),
        plot.margin = margin(t = 5,r = 5,b = 5, l = 20, unit = "pt"),
        text = element_text(colour = "black",size = 23,face = "bold.italic")) +
  theme(plot.title = element_text(size=28,colour = "black",face = "bold",hjust = 0.5))

tiff(filename="Abundance.tif",res=600,height=3600,width=5400,type="windows",compression = "lzw")
p
dev.off()
