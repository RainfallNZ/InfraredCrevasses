#R Script to create publication-ready camera setup plot for paper.
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be single column (86 mm) wide and about 100 mm high.

#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("imager","magick","terra","tidyr","dplyr","ggplot2","ggtext","tidyterra","gridExtra","grid","gtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
CameraViewImageFile <- file.path(DataDirectory,"CameraAndViewBenSchmacher_Edited.jpg")
ExampleIRImageFile  <- file.path(DataDirectory,"Image49.tif")

#Load the two images, resizing the camera view photo to 86 mm wide at 300 dpi
CameraView <- imager::load.image(CameraViewImageFile) %>% imager::imresize(scale = 86/25.4 * 300/imager::width(.))
IRExample  <- terra::rast(ExampleIRImageFile)

#Create a plot of CameraView
CameraViewDataFrame <- as.data.frame(CameraView,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
ViewPlot <- ggplot(CameraViewDataFrame,aes(x,y))+
  geom_raster(aes(fill=rgb.val))+
  #labs(tag="a") +
  scale_fill_identity()+
  scale_y_reverse()+
  theme_classic() +
  coord_fixed(ratio = 1)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        plot.tag = element_text(vjust = -8,hjust=-20), #manual position of tag to align with the second plot
        panel.border = element_blank())

GCPs <- data.frame("name"   = c("a","b"),
                   "column" = c(53,76),
                   "row"    = c(111,148))

#Create a plot of the IR image
IRPlot <- ggplot() +
  geom_spatraster(data=IRExample) +
  geom_point(aes(alpha=""),x=53,y=500,size=3,shape=4,color="black")+
  geom_point(x=c(53,76,53,188,160,139,172,206),y=285-c(111,148,90,112,155,86,83,103), aes(alpha=rep("",8)),size=3,shape=4,color="black",show.legend=FALSE) +
  theme_classic() +
  coord_fixed(ratio = 1)+
  scale_alpha_manual(values = 1) +
  labs(alpha="Control Points")+
  #guides(alpha = "none")+
  guides(alpha = guide_legend(direction = "vertical",label.text = element_blank()))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(vjust = 0.8,hjust=0.5),
        legend.position = "bottom",
        legend.justification.bottom = "left",
        legend.margin = margin(0,0,0,-0.1,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +

  #scale_fill_gradientn(colours = c("#5153a6","#c7a53f","#cc7833","#e8481c","#ff0004"),
  scale_fill_gradientn(colours = c("#30123b","#2fb2f4","#a7fc3a","#fc8524","#7e0502"),
                       values=c(0,0.25,0.5,0.75,1),
                       #values=c(0,0.5,1),
                       #labels = c("warmer","colder"),
                       labels = c(-18,-15,-12,-9,-6),
                       #breaks = c(-6,-18),
                       breaks = c(-18,-15,-12,-9,-6),
                       na.value=NA,
                       limits=c(-18,-6),
                       name="Raw temperature",
                       guide=guide_colorbar(title.position = "left",ticks = FALSE),
                       oob=scales::squish)
  

IRPlot

#They need to be combined, but I want the relative position of the IR plot to roughly
#match the equivalent top right of the camera view photo, so I want the right hand side
#to be aligned. This is done using gtables and manually setting the size of columns to
#make it look about right

#create a 2x3 empty gtable. Set the row and column sizes explicitly to get alignment correct
#This was a real hack!!
#And the positions on the PDF don't quite match those on RStudio viewer 
tg <- gtable(widths = unit(c(12,74),c("mm")),heights = unit(c(59,56,2),c("mm")))

#Create plot grobs
TopPlot <- ggplotGrob(ViewPlot)
BottomPlot <- ggplotGrob(IRPlot)

#Create plot label grobs
TopPlotLabel    <- textGrob(label="a",gp=gpar(fontsize=9,col="white"),vjust = -9, hjust = -1)
BottomPlotLabel <- textGrob(label="b",gp=gpar(fontsize=9),vjust = -9, hjust = -1)

#Add each grob to it's allocated place.
CombinedPlot <- gtable_add_grob(tg,TopPlot, t=1,l=1,r=2,z=Inf,clip="on") %>%
  gtable_add_grob(TopPlotLabel,t=1,l=1,z=Inf,clip="off") %>%
  gtable_add_grob(BottomPlot, t=2,b=3,l=2,z=Inf,clip="on") %>%
  gtable_add_grob(BottomPlotLabel, t=2,l=1,z=Inf,clip="off")

#For a preliminary check of alignment use:
#But note the PDF alignment is a bit different!
grid.newpage()
grid.draw(CombinedPlot)

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"CameraView.pdf"),CombinedPlot,width = 86,units="mm",height = 125, dpi=300, device = "pdf")

#ggsave(file.path(outputDirectory,"CameraView.tif"),CombinedPlot,width = 86, height = 125,units="mm", dpi=300, device = "tiff")
