#R Script to create publication-ready infrared image orthorectification example plot for paper.
#Show RGB ortho image above an example thermal ortho image. Use the same colour palette as for the Camera plot.
#Include the crevasse-classification and the viewshed plots
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be single column (86 mm) wide and about 100 mm high.

#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("imager","magick","terra","terrainr","tidyr","dplyr","ggplot2","ggtext",
                      "ggspatial","tidyterra","gridExtra","grid","gtable","rasterVis","cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

source("UsefulFunctions.R")

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
UCDirectory       <- file.path(ProjectDirectory,"CopiesFromUC") 
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
GISDirectory      <- file.path(DataDirectory,"GIS")
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
RGBOrthoImageFile <- file.path(GISDirectory,"TG_20202_orthoNZTM_1mres.tif")
ExampleIROrthoImageFile  <- file.path(GISDirectory,"OrthoImages","OrthoImage_49_20200225_03_20_clipped.tif")
#ExampleIROrthoImageFile  <- file.path(GISDirectory,"OrthoImages","OrthoImage_119_20200225_15_00_clipped.tif")
CrevasseClassFile <- file.path(GISDirectory,"CrevasseClassified.tif")
AreaOfInterestFile<- file.path(GISDirectory,"IRCameraAOI.shp")
ViewshedFile      <- file.path(GISDirectory,"Viewshed.tif")
DustClassFile     <- file.path(GISDirectory,"DustPolygon.shp") 

#Load an example image
IRExample          <- terra::rast(ExampleIROrthoImageFile)

#Load the area of interest, ensure projection is the same as the image
AreaOfInterest     <- terra::vect(AreaOfInterestFile) %>% terra::project(IRExample)

#Mask the image to the area of interest
IRExample          <- terra::mask(IRExample, AreaOfInterest)

#Load other data, ensuring projection, resolution and extent matches the example image
RGBImage           <- terra::rast(RGBOrthoImageFile) %>% terra::project(IRExample) %>%  terra::resample(IRExample) %>% terra::mask(IRExample)
CrevasseClassification <- terra::rast(CrevasseClassFile)%>% terra::project(IRExample, method="near") %>%  terra::resample(IRExample, method="near") %>% terra::mask(IRExample)
Viewshed           <- terra::rast(ViewshedFile)%>% terra::project(IRExample,method="near") %>%  terra::resample(IRExample,method="near") %>% terra::mask(IRExample)
DustClass          <- terra::vect(DustClassFile)%>% terra::project(IRExample) %>% terra::crop(AreaOfInterest)

#Create a low resolution crevasse classification
LoResCrevasseClass <- CrevasseClassifierLowRes(HiResCrevasseClassification = CrevasseClassification,
                                       AreaOfInterest = AreaOfInterest,
                                       Viewshed = Viewshed,
                                       LowResRaster = IRExample) 

WeatherStaionSitesFile <- file.path(GISDirectory,"GEO-XH.shp")
CameraLocationFile <- file.path(GISDirectory,"IRCamera.shp")

Camera <- terra::vect(CameraLocationFile)
WeatherStations <- terra::vect(WeatherStaionSitesFile) %>% tidyterra::filter(Comment %in% c("aws lower","aws top"))

#Create a plot of the IR image
IRPlot <- ggplot() +
  geom_spatraster(data=IRExample) +
  geom_spatvector(data = Camera, shape = 19,size=4) +
#  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(vjust = 0.8),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  guides(fill = guide_colourbar(position="bottom"))+
  scale_fill_gradientn(colours = c("#30123b","#2fb2f4","#a7fc3a","#fc8524","#7e0502"),
                       values=c(0,0.25,0.5,0.75,1),
                       #values=c(0,0.5,1),
                       labels = c("warmer","colder"),
                       breaks = c(-6,-18),
                       na.value=NA,
                       limits=c(-18,-6),
                       name="Temperature",
                       guide=guide_colorbar(title.position = "left",ticks = FALSE),
                       oob=scales::squish)+
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

IRPlot

OrthoImagePlot <-  ggplot() +
  terrainr::geom_spatial_rgb(data=RGBImage,
                             mapping = aes(x = x,
                                           y = y,
                                           r = red,
                                           g = green,
                                           b = blue)) +
  coord_sf(crs = 2193) +
  geom_spatvector(data = Camera, shape = 19,size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

OrthoImagePlot

values(CrevasseClassification) <- as.factor(values(CrevasseClassification))
CrevasseClassHiResPlot <- ggplot() +
  geom_spatvector(data = AreaOfInterest,fill=NA,colour="black") +
  geom_spatraster(data=CrevasseClassification) +
  scale_fill_manual(breaks="1",values = c("black",NA),na.translate = FALSE, name = NULL,labels=c("crevasses"),guide=FALSE)+
  geom_spatvector(data = Camera, shape = 19,size=4) +
  #  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  labs(caption="Crevasse classification, high res.") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        plot.caption = element_text(hjust=0.5, size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

CrevasseClassHiResPlot

values(LoResCrevasseClass) <- as.factor(values(LoResCrevasseClass))
CrevasseClassLoResPlot <- ggplot() +
  #geom_spatvector(data = AreaOfInterest,fill=NA,colour="black") +
  geom_spatraster(data=LoResCrevasseClass) +
  scale_fill_manual(breaks=c("1","2"),values = c("black","grey",NA),na.translate = FALSE, name = NULL,labels=c("Crevassed","Not crevassed"))+
  geom_spatvector(data = Camera, shape = 19,size=4) +
  labs(caption="Crevassed/Not Crevassed Classes") +
  guides(fill = guide_legend(position="inside"))+
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        plot.caption = element_text(hjust=0.5, size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position.inside = c(0.2,0.3),
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

CrevasseClassLoResPlot

values(Viewshed) <- as.factor(values(Viewshed))
ViewshedPlot <- ggplot() +
  geom_spatvector(data = AreaOfInterest,fill=NA,colour="black") +
  geom_spatraster(data=Viewshed) +
  scale_fill_manual(breaks="0",values = c("black",NA),na.translate = FALSE, name = NULL,labels=c("Hidden from camera view"), guide=FALSE)+
  geom_spatvector(data = Camera, shape = 19,size=4) +
  #  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  labs(caption="Areas hidden from  camera view") +
  theme_classic() +
  theme(axis.line = element_blank(),
        plot.caption = element_text(hjust=0.5, size=9),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

ViewshedPlot

DustClassPlot <- ggplot() +
  geom_spatvector(data = AreaOfInterest,fill=NA,colour="black") +
  geom_spatvector(data = DustClass,colour=NA,fill="tan",show.legend = "polygon") +
  #scale_fill_manual(breaks=c("1","2"),values = c("black","blue"),na.translate = FALSE, name = NULL,labels=c("Dust","No dust"))+
  geom_spatvector(data = Camera, shape = 19,size=4) +
  labs(caption="Dust area classification") +
  guides(fill = guide_legend(position="inside"))+
  theme_classic() +
  theme(axis.line = element_blank(),
        plot.caption = element_text(hjust=0.5, size=9),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position.inside = c(0.2,0.2),
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

DustClassPlot 


aligned <- cowplot::align_plots(IRPlot,OrthoImagePlot,CrevasseClassHiResPlot,CrevasseClassLoResPlot,ViewshedPlot,DustClassPlot, align = "vh", axis="blrt")

FullPlot = cowplot::plot_grid(aligned[[1]], aligned[[2]], aligned[[3]], aligned[[6]],aligned[[4]],aligned[[5]],ncol=2, nrow=3,
                              labels =c("a","b","c","d","e","f"), label_fontface="plain",label_size = 9)

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"OrthoExample.pdf"),FullPlot,width = 178,units="mm",height = 205, dpi=300, device = "pdf")

#ggsave(file.path(outputDirectory,"OrthoExample.tif"),FullPlot,width = 86, height = 205,units="mm", dpi=300, device = "tiff")
