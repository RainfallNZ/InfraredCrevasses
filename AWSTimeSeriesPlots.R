#R Script to create publication-ready surface time-series plot panel for a paper.
#Intended to show air temperature, shortwave, longwave in and out, and wind speed
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be double column (86 mm) wide and about 205 mm high.
#The colorscale is based on that used in Purdie et al. (2022) Figure 8.
#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("lubridate","tidyr","ggplot2","cowplot","ggtext","stringr","plotly","xts","egg","tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Function to import Campbell Data logger data. From https://www.campbellsci.com/blog/tool-to-import-data-to-r
#Modified to guess the data delimiter
importCSdata <- function(filename,RetOpt="data"){
  
  if ("data.table" %in% rownames(installed.packages()) == FALSE) { install.packages("data.table")};library(data.table)
  if(RetOpt=="info"){
    # bring in entire header of CSI TOA5 data file for metadata
    stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
    return(stn.info)
  } else {
    # second line of header contains variable names
    #header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
    header <- unname(unlist(fread(file=filename,skip=1,header=FALSE, na.strings=c("NAN"),sep="auto",nrows=1)))
    # bring in data
    #stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
    stn.data <- fread(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep="auto",data.table=FALSE)
    names(stn.data) <- header
    # add column of R-formatted date/timestamps
    #stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
    return(stn.data)}
}

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")

#Weather Station Data Files
LoggerFiles <- list.files(file.path(ProjectDirectory,"CopiesFromUC","TAS_2020","2020_FieldData","cr300 data"), pattern = ".*_ten_min_latest\\.dat$", full.names = TRUE)
names(LoggerFiles) <- file_path_sans_ext(basename(LoggerFiles))

#Specify the start and finish times of interest. Set to match the first period of the available Infra red data
StartDateTime <- as.POSIXct("2020-02-24 19:30", tz= "Etc/GMT+12")
EndDateTime   <- as.POSIXct("2020-02-25 21:50", tz= "Etc/GMT+12")

#Load the data and restrict to period of interest when the Infra-red camera was operating
LoggerData <- lapply(LoggerFiles, function(LoggerFile) {
  #Import the data
  AWSData <- importCSdata(LoggerFile)
  
  #Convert times to POSIXct format, but force timezone to be NZST (the logger assumes UTC!)
  LoggerDateTimes <- as.POSIXct(AWSData$TIMESTAMP) %>% lubridate::force_tz(tzone="Etc/GMT+12")
  #Convert data to an xts object
  AWSDataxts <- AWSData %>% select(-TIMESTAMP) %>% xts(order.by = LoggerDateTimes) %>% window(start = StartDateTime, end = EndDateTime )
})

#Convert logger data into a long format data frame to enable plotting. Do just for one
#weather station to start with
PlotData <- LoggerData[[2]] %>% ggplot2::fortify()
{
#Prepare a base set of axis
  AWSPlotBase <- PlotData %>%
  ggplot() +
    theme_bw() + 
    theme(text=element_text(size=9),
          axis.text = element_text(size=9),
          #axis.text.x=element_blank(),
          legend.text = element_text(size=9),
          legend.key.height = unit(0.3, 'cm'),
          legend.title = element_blank(),
          axis.title.y.left = element_markdown(),
          axis.title.y.right = element_markdown(),
          axis.title.x = element_blank(),
          legend.justification = c(0,1),
          panel.background = element_rect(colour=NA, fill = "transparent"), 
          plot.background = element_rect(colour=NA, fill = "transparent"),
          plot.title = element_text(hjust = 0.1,vjust = -10,size=9))
  
#Now prepare the plot of the AWS data series of interest in a single column
AWSPlotUpperTemperature <- AWSPlotBase +
  geom_line(aes(x = Index, y = Air_temp_upper_Avg)) +
  ylab("<sup>o</sup>C") +
  ggtitle("Air temperature") +
  theme(plot.title = element_text(hjust = 0.1,vjust = -1.5,size=9),
        axis.text.x=element_blank())

AWSPlotIncomingShortwaveRadiation <- AWSPlotBase +
  geom_line(aes(x = Index, y = incommingSW_Avg)) +
  ylab("W m<sup>-2</sup>") +
  ggtitle("Incoming shortwave\nradiation") +
  scale_y_continuous(position = "left") +
  theme(plot.title = element_text(hjust = 0.1,vjust = -13,size=9),
        axis.text.x=element_blank())

AWSPlotLongwaveRadiation <- AWSPlotBase +
  geom_line(aes(x = Index, y = outgoingLW_Avg,colour="In")) +
  geom_line(aes(x = Index,y=incomingLW_Avg,colour = "Out"))+
  scale_colour_manual(labels=c("Out","In"),
                      values = c("#c7a53f","#5153a6"))+
  ylab("W m<sup>-2</sup>") +
  ggtitle("Longwave radiation") +
  theme(legend.position = c(0.04,0.6),
        legend.justification = c(0,1),
        plot.title = element_text(hjust = 0.1,vjust = -2.82,size=9),
        axis.text.x=element_blank()) +
  guides(color=guide_legend(nrow=1))
        
AWSPlotWindSpeed <- AWSPlotBase +
  geom_line(aes(x = Index, y = Wind_speed_lower_Avg)) +
  ylab("m s<sup>-1</sup>") +
  ggtitle("Wind speed") +
  scale_y_continuous(position = "left")+
  scale_x_datetime(date_labels = '%H:%M\n%d %b') +
  theme(plot.title = element_text(hjust = 0.35,vjust = -1.5,size=9),
        axis.text = element_text(size=9))
  
Multiplot <- cowplot::align_plots(AWSPlotUpperTemperature,
                                  AWSPlotIncomingShortwaveRadiation,
                                  AWSPlotLongwaveRadiation,
                                  AWSPlotWindSpeed,align = "hv")


OutputPlot <- ggdraw() + cowplot::draw_plot(Multiplot[[4]],0,0,1,0.35)+
  cowplot::draw_plot(Multiplot[[3]],0,0.2,1,0.35) +
  cowplot::draw_plot(Multiplot[[2]],0,0.4,1,0.35) +
  cowplot::draw_plot(Multiplot[[1]],0,0.6,1,0.35)


#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"WeatherStationPlotLower.pdf"),OutputPlot,width = 86, height = 125,units="mm", dpi=300, device = "pdf")
ggsave(file.path(outputDirectory,"WeatherStationPlotLower.tif"),OutputPlot,width = 86, height = 125,units="mm", dpi=300, device = "tiff")
}










