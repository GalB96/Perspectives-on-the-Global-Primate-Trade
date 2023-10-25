#load libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)

####Figure 1 line graph
#import data
xdata<-read.csv("PrimateTrade_Figure1.csv", header = T, sep = ",", stringsAsFactors = T)

#plots the data
plot_1 <- ggplot(xdata, aes(x = Year, y = Count, color = Genus)) +
  geom_line() +
  labs(
       x = "Year",
       y = "Count (log-transformed)") +
  scale_color_discrete(name = "Genus") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1), labels = as.character(seq(2010, 2020, by = 1)))+
  theme(
    axis.text = element_text(size = 15),      # Adjust axis text size
    axis.title = element_text(size = 20),     # Adjust axis label size
    legend.text = element_text(size = 15),    # Adjust legend text size
    legend.title = element_text(size = 20)    # Adjust legend title size
  )
 



######2023-05
setwd("/Users/gb64/Desktop/R_temp")
xdata<-read.csv("PrimateTrade_Figure3_4.csv", header = T, sep = ",", stringsAsFactors = T)


####Figure 3
#recorganise the data into a dataframe for the purpose of export
gl_purpose<-aggregate(Exporter.reported.quantity~Purpose+Year+Export.region, data=xdata, FUN=sum)
colnames(gl_purpose)<-c("Purpose", "Year", "Region", "Count")
#remove unused levels
gl_purpose<-gl_purpose[gl_purpose$Purpose!="",]
gl_purpose<-gl_purpose[gl_purpose$Region!="Europe",]
gl_purpose<-gl_purpose[gl_purpose$Region!="Unk",]
gl_purpose<-gl_purpose[gl_purpose$Region!="Other",]
gl_purpose<-gl_purpose[gl_purpose$Region!="Other",]



#keep only categories that occur more than 1% of exports in all regions
#these include B (Bred in captivity), M (Medical), S (Scientific), T (Commercial), Z (Zoo)
gl_purpose<-gl_purpose[gl_purpose$Purpose%in%c("B", "M", "S", "T", "Z"),]
gl_purpose<-droplevels(gl_purpose)
#must detach package before running next code
detach(package:plyr)
library(dplyr)

#get percent goal used per gesture
Fig3 <-as.data.frame (gl_purpose %>%
                        group_by(Year, Region ) %>%
                        mutate(Percent = (Count / sum(Count))*100))

Fig3<-Fig3[Fig3$Percent>=1,]#remove cases that were less than 1%
Fig3<-droplevels(Fig3)
Fig3<-na.omit(Fig3)

#plot
library(plyr)
library(ggplot2)


ce=ddply(Fig3, "Year", transform, percent_weight=Percent)

bp=ggplot(ce, aes(x=Region, y=percent_weight, fill=Purpose))+
  geom_bar(stat="identity",color = "black", size=0.2)+
  scale_fill_brewer(palette = "Blues")+facet_grid(~Year)

Fige3<-bp+theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5, size = 12))+
  ylab("Percentage of Exports")+
  xlab("Purpose by Year")+
  theme(axis.title = element_text(size = 15),strip.text=element_text(face="bold", size=rel(1)), strip.background=element_rect(fill="lightblue", colour="black", size=0.5))



####Figure 4
#create dataframe for the source of export information
gl_source<-aggregate(Exporter.reported.quantity~Source+Year+Export.region, data=xdata, FUN=sum)
colnames(gl_source)<-c("Source", "Year", "Region", "Count")
#remove unused levels
gl_source<-gl_source[gl_source$Source!="",]
gl_source<-gl_source[gl_source$Region!="Europe",]
gl_source<-gl_source[gl_source$Region!="Unk",]
gl_source<-gl_source[gl_source$Region!="Other",]
gl_source<-gl_source[gl_source$Source%in%c("C", "F", "W"),]

head(gl_source)
colnames(gl_source)<-c("Source", "Year", "Region", "Count")
#must detach package before running next code
detach(package:plyr)

#get percent goal used per gesture
Fig4 <-as.data.frame (gl_source %>%
                        group_by(Year, Region ) %>%
                        mutate(Percent = (Count / sum(Count))*100))


Fig4<-Fig4[Fig4$Percent>=1,]
Fig4<-droplevels(Fig4)
Fig4<-na.omit(Fig4)
levels(Fig4$Source)
#plot
library(plyr)
library(ggplot2)

ce=ddply(Fig4, "Year", transform, percent_weight=Percent)

bp=ggplot(ce, aes(x=Region, y=percent_weight, fill=Source))+
  geom_bar(stat="identity",color = "black", size=0.2)+
  scale_fill_brewer(palette = "Blues")+facet_grid(~Year)

bp+theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5, size = 12))+
  ylab("Percentage of Exports")+
  xlab("Source by Year")+
  theme(axis.title = element_text(size = 15),strip.text=element_text(face="bold", size=rel(1)), strip.background=element_rect(fill="lightblue", colour="black", size=0.5))