#load packages
library(dplyr)

#import data
xdata<-read.csv("CITES_Raw_2010-2022.csv",header = T,sep = ",",stringsAsFactors = T)

#subset to live trade only
xdata<-xdata[xdata$Term=="live",]
xdata<-droplevels(xdata)

#add region (combine countries into three regions: Asia, Africa, American tropics)
xdata$Export_Region <- ifelse(xdata$Exporter %in% c("ZA","CI","GA","MG","SL","GN","CG","ML","SN","UG","NE","TZ","NA","MZ","ZW","CD","SD","ZM","ET","TD","CM","MU","BJ","KE","BW","CF","GH","TG","MA"),
                              "Africa",
                              ifelse(xdata$Exporter %in% c("CR","MX","GT","PE","PA","BR","GY","EC","SR","KN","BB","CO","CU","AR","SV","TT","JM"),
                                     "America",
                                     ifelse(xdata$Exporter %in% c("TH","MY","SY","KR","ID","IL","CY","JP","CN","UZ","AM","GE","IN","TW","PH","KH","VN","MM","KG","IR","JO","BD","BH","KZ","AE","LB"),
                                            "Asia",
                                            NA)))

#create datasets that report the proportion of exports under each source or purpose codes
#source codes from 2010-2022
TOTAL <- sum(xdata$Exporter.reported.quantity, na.rm = TRUE)
source_prop<-as.data.frame(xdata%>%
                             group_by(Source)%>%
                             mutate(P_source=(sum(Exporter.reported.quantity,na.rm = TRUE)/TOTAL)*100)%>%
                             select(Source, P_source)%>%
                             distinct())


#purpose codes from 2010-2022
TOTAL <- sum(xdata$Exporter.reported.quantity, na.rm = TRUE)
purpose_prop<-as.data.frame(xdata%>%
                              group_by(Purpose)%>%
                              mutate(P_purpose=(sum(Exporter.reported.quantity,na.rm = TRUE)/TOTAL)*100, scientific = FALSE)%>%
                              select(Purpose, P_purpose)%>%
                              distinct())
#descriptive
#number of exporting CITES parties
nlevels(xdata$Exporter)

#number of species
nlevels(xdata$Taxon)

#number of exports total
sum(xdata$Exporter.reported.quantity,na.rm = TRUE)

#number of macaques
sum(xdata$Exporter.reported.quantity[xdata$Genus=="Macaca"],na.rm = TRUE)

#count total exports for each species
genus_tot<-as.data.frame(xdata%>%
                           group_by(Genus)%>%
                           mutate(Genus_tot=sum(Exporter.reported.quantity,na.rm = TRUE))%>%
                           select(Genus,Genus_tot)%>%
                           distinct()%>%
                           arrange(Genus_tot))

#see 10 most traded species
tail(genus_tot,10)
#extract list top 10 most traded genus
top10_genus <- (genus_tot %>%
                  mutate(Genus = factor(Genus)) %>%
                  group_by(Genus) %>%
                  summarise(Genus_tot = sum(Genus_tot)) %>%
                  slice_max(order_by = Genus_tot, n = 10) %>%
                  pull(Genus))

#how many more macaca were traded compared to callithrix
569200/17536


#create dataset for figure 1: only 10 most exported
#count number of individuals from each genus trader per year
exp_genus<-as.data.frame(xdata%>%
                           group_by(Year, Genus)%>%
                           mutate(Count_Export=sum(Exporter.reported.quantity,na.rm = TRUE))%>%
                           select(Year,Genus,Count_Export))

#extract data for top 10 most exported genera
exp_genus<-exp_genus[exp_genus$Genus%in%top10_genus,]

exp_genus<-exp_genus[!duplicated(exp_genus),]#remove duplicated
exp_genus$logCount<-log(exp_genus$Count_Export+1)#log transform (add 1 to avoid 0s)

#create simple final dataset to use in the figure
fig1<-exp_genus%>%select(Year,Genus,logCount)
colnames(fig1)<-c("Year","Genus","Count")#rename columns for clarity

#export
write.csv(fig1, "PrimateTrade_Figure1_2010-2022.csv")



#create dataset for Figure 2: comparing reports of imports and exports
fig2<-as.data.frame(xdata%>%
                      group_by(Year)%>%
                      mutate(Year_export=sum(Exporter.reported.quantity,na.rm = TRUE))%>%
                      mutate(Year_import=sum(Importer.reported.quantity,na.rm = TRUE))%>%
                      select(Year,Year_import,Year_export)%>%
                      distinct())

colnames(fig2)<-c("Year","Importer", "Exporter")#simplify column names
#export dataset
write.csv(fig2, "PrimateTrade_Figure2_2010-2022.csv")

#create datasets for Figures 3 and 4: comparing the proportion of exports from different regions under different source or purpose codes
fig3_4<-as.data.frame(xdata%>%select(Year,Genus,Export_Region,Exporter.reported.quantity,Purpose,Source))#select relevant columns
fig3_4<-fig3_4[fig3_4$Export_Region%in%c("Africa","America","Asia"),]#ensure only regions of interest are selects
fig3_4<-droplevels(fig3_4)#remove unwanted levels
colnames(fig3_4)<-c("Year","Genus","Export.region","Exporter.reported.quantity","Purpose","Source")#rename for clarity
#export dataset
write.csv(fig3_4, "PrimateTrade_Figure3-4_2010-2022.csv")


#create datasets for Figure 5: top 10 exported genera in each region
#count number of exports per genera and per region
Region_Count<-as.data.frame(xdata%>%
                              group_by(Genus,Export_Region)%>%
                              mutate(Reg_count=sum(Exporter.reported.quantity,na.rm = TRUE))%>%
                              select(Genus, Export_Region,Reg_count)%>%
                              distinct())

#create Africa specific dataset
Africa<-Region_Count[Region_Count$Export_Region=="Africa",]
Africa<-na.omit(Africa)
Africa<-Africa[order(Africa$Reg_count),]
#export
write.csv(Africa,"Africa_top10_Fig5.csv")

#create American tropics specific dataset
America<-Region_Count[Region_Count$Export_Region=="America",]
America<-na.omit(America)
America<-America[order(America$Reg_count),]
#export
write.csv(America,"America_top10_Fig5.csv")

#create Asia specific dataset
Asia<-Region_Count[Region_Count$Export_Region=="Asia",]
Asia<-na.omit(Asia)
Asia<-Asia[order(Asia$Reg_count),]
#export
write.csv(Asia,"Asia_top10_Fig5.csv")


#######Figures plots######
#load libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

####Figure 1 line graph
# Define a color palette with visually distinct colors for 10 genera
distinct_palette <- brewer.pal(10, "Paired")

#plots figure 1
plot_1 <- ggplot(fig1, aes(x = Year, y = Count, color = Genus)) +
  geom_line(size=1) +
  labs(
    x = "Year",
    y = "Count") +
  scale_color_manual(values = distinct_palette, name="Genus")  +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), labels = as.character(seq(2010, 2022, by = 1)))+
  scale_y_continuous(trans = "log10", 
                     labels = function(x) format(exp(x), scientific = FALSE),breaks = log(c(0,1,10,100, 1000, 10000, 100000)))+
  theme(text=element_text(size=50),
        axis.text = element_text(size = 20),      # Adjust axis text sizz
        axis.title = element_text(size = 30),     # Adjust axis label size
        legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 30)    # Adjust legend title size
  )+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black")
  )+theme(legend.position="right")

plot_1+theme(text=element_text(size=20),
             axis.text = element_text(size = 15),      # Adjust axis text size
             axis.title = element_text(size = 20),     # Adjust axis label size
             legend.text = element_text(size = 15),    # Adjust legend text size
             legend.title = element_text(size = 20),
             axis.text.y = element_text(color = "black"),
             axis.text.x = element_text(color = "black"),
             axis.line = element_line(color = "black"),
             panel.border = element_blank(),
)+annotate("rect", xmin = 2021.5, xmax = 2022.5, ymin = 0, ymax = Inf,
           alpha = .1,fill = "red")

####Figure 2
# reorganise data into useful format
fig2 <- as.data.frame(fig2 %>%
                        pivot_longer(cols = c(Importer, Exporter), names_to = "Type", values_to = "Value"))

#plot figure 2
plot2 <- ggplot(fig2, aes(x = as.numeric(Year), y = Value, fill=Type)) +
  geom_bar(stat = "identity", position = position_dodge(),colour = "dodgerblue4") + 
  scale_fill_manual(values = c("dodgerblue4", "lightblue")) +# Change fill color to light grey
  labs(title = "",
       x = "Year",
       y = "Number of Individuals Reported") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 1), labels = as.character(seq(2010, 2022, by = 1))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

plot2+theme(text=element_text(size=20),
            axis.text = element_text(size = 15),      # Adjust axis text size
            axis.title = element_text(size = 20),     # Adjust axis label size
            legend.text = element_text(size = 15),    # Adjust legend text size
            legend.title = element_text(size = 20),
            panel.grid.major = element_blank(),       # Remove major grid lines
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), # Adjust legend title size
)+annotate("rect", xmin = 2021.5, xmax = 2022.5, ymin = 0, ymax = Inf,
           alpha = .1,fill = "red")

####Figure 3
#create dataframe for the source of export information
gl_source<-aggregate(Exporter.reported.quantity~Source+Year+Export.region, data=fig3_4, FUN=sum)
colnames(gl_source)<-c("Source", "Year", "Region", "Count")
#remove unused levels
gl_source<-gl_source[gl_source$Source!="",]
gl_source<-gl_source[gl_source$Region!="Europe",]
gl_source<-gl_source[gl_source$Region!="Unk",]
gl_source<-gl_source[gl_source$Region!="Other",]
gl_source<-gl_source[gl_source$Source%in%c("C", "F", "W"),]

colnames(gl_source)<-c("Source", "Year", "Region", "Count")#change column names for clarity
#must detach package before running next code
detach(package:plyr)

#get percent goal used per gesture
Fig3 <-as.data.frame (gl_source %>%
                        group_by(Year, Region ) %>%
                        mutate(Percent = (Count / sum(Count))*100))

#only keep cases where more than 1% were exported
Fig3<-Fig3[Fig3$Percent>=1,]
Fig3<-droplevels(Fig3)
Fig3<-na.omit(Fig3)

#plot
library(plyr)

#set names for sources
source_names <- c("Captive-bred", "Born in captivity", "Wild")

ce=ddply(Fig3, "Year", transform, percent_weight=Percent)

bp=ggplot(ce, aes(x=Region, y=percent_weight, fill=Source))+
  geom_bar(stat="identity",color = "black", size=0.2)+
  ylab("Percentage of Exports")+
  xlab("Source by Year")+
  scale_fill_brewer(palette = "Blues", labels=source_names)+facet_grid(~Year)

Fige3 <- bp +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15,color="black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold", size = rel(0.75)),
        strip.background = element_rect(fill = "lightblue", colour = "black", size = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),
        legend.position = "right",
        text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        plot.title = element_text(color = "black")
  )

#add highlight
Figee3 <- Fige3 +
  geom_rect(data = subset(ce, Year == "2022"), 
            aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf), 
            fill = "red", alpha = 0.025)

#####Figure 4
#recorganise the data into a dataframe for the purpose of export
gl_purpose<-aggregate(Exporter.reported.quantity~Purpose+Year+Export.region, data=fig3_4, FUN=sum)
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
gl_purpose$Purpose<-ifelse(gl_purpose$Purpose=="B", "Breeding in captivity",
                           ifelse(gl_purpose$Purpose=="M", "Medical",
                                  ifelse(gl_purpose$Purpose=="S","Scientific",
                                         ifelse(gl_purpose$Purpose=="T","Commercial",
                                                ifelse(gl_purpose$Purpose=="Z", "Zoo",NA)))))
#must detach package before running next code
detach(package:plyr)
library(dplyr)

#get percent goal used per gesture
Fig4 <-as.data.frame (gl_purpose %>%
                        group_by(Year, Region ) %>%
                        mutate(Percent = (Count / sum(Count))*100))

Fig4<-Fig4[Fig4$Percent>=1,]#remove cases that were less than 1%
Fig4<-droplevels(Fig4)
Fig4<-na.omit(Fig4)

#plot
library(plyr)

# set the names for the legend
purpose_names <- c("Breeding in captivity", "Commercial", "Medical", "Scientific", "Zoo")

ce <- ddply(Fig4, "Year", transform, percent_weight = Percent)

bp <- ggplot(ce, aes(x = Region, y = percent_weight, fill = Purpose)) +
  geom_bar(stat = "identity", color = "black", size = 0.2) +
  scale_fill_brewer(palette = "Blues", labels = purpose_names) +
  ylab("Percentage of Exports")+
  xlab("Purpose by Year")+
  facet_grid(~Year)

Fige4 <- bp +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15,color="black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold", size = rel(0.75)),
        strip.background = element_rect(fill = "lightblue", colour = "black", size = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "right",
        text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        plot.title = element_text(color = "black")
  )

#add highlight for 2022
Figee4 <- Fige4 +
  geom_rect(data = subset(ce, Year == "2022"), 
            aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf), 
            fill = "red", alpha = 0.025)
