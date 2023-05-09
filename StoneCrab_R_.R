## Trying to find:

#How CPUE changes per site per month
#Is this data normally distributed (Normalization)?
#Relationship amongst catch rate and site
#Relationship between sex and location
#Relationship between proportion of claws found and percentage of legal claws found per location

#setting my working directory
setwd("~/Desktop/")

#Data
sctotal<-read.csv("FWC_SC_Trap_Totals.csv")
scbm<-read.csv("2021_2022 SC FWC.csv")

#Librarys
library(ggplot2)
library(tidyverse)
library(dplyr)
library(vegan)
library(ggdist)
library(tidyverse)
library(ISwR)
library(lme4)
library(AICcmodavg)
library(colorspace)
library(MetBrewer)
library(lmer)
library(car)

sumc$Date = as.Date(sumc$Date, format = "%m/%d%/Y") #setting the data for date to a different format so that the date loads correctly and in order

#Task 1- Line Graph of CPUE for each site for the year, show PER LOCATION, the change in CPUE for the season by month
#Add color to graphs to show chnage in location and fix the fonts/make pretty, subsets for the data and knowing what goes where by location
Keys<-c("Key West", "Harbor Key", "Marathon")
Keysdata<- sctotal %>% filter(Location=="Key West"|Location=='Harbor Keys Bank'|Location=='Marathon')

South<-c("Pavilion Key", "Marco", "Sanibel Island")
Southdata<- sctotal %>% filter(Location=="Marco"|Location=='Pavilion Key'|Location=='Sanibel')

Middle<-c("Englewood", "Johns Pass")
Middledata<- sctotal %>% filter(Location=="Englewood"|Location=='Johns Pass')

North<-c("Homosassa", "Cedar Key", "Steinhatchee")
Northdata<- sctotal %>% filter(Location=="Homosassa"|Location=='Cedar Key'|Location=='Steinhatchee')

ggplot(sctotal, aes(x=Date, y=CPUE_lbs.Trap, group= Location)) + #This is the wild original graph
  geom_line(aes(linetype= Location)) + 
  geom_point() +
  xlab("Date/Month") +
  ylab("CPUE Per Trap Site") +
  ggtitle("CPUE Change over Stone crab season")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=20))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=15))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=15))

Keysdata$Date <- fct_relevel(Keysdata$Date,"21-Oct","21-Nov","21-Dec", #This is subsetting the data again to make it work date wise and in order
                             "22-Jan","22-Feb","22-Mar","22-Apr")
Southdata$Date <- fct_relevel(Southdata$Date,"21-Oct","21-Nov","21-Dec",
                             "22-Jan","22-Feb","22-Mar","22-Apr")
Middledata$Date <- fct_relevel(Middledata$Date,"21-Oct","21-Nov","21-Dec",
                             "22-Jan","22-Feb","22-Mar","22-Apr")
Northdata$Date <- fct_relevel(Northdata$Date,"21-Oct","21-Nov","21-Dec",
                             "22-Jan","22-Feb","22-Mar","22-Apr")
sctotal$Date <- fct_relevel(sctotal$Date,"October-21","November-21","December-21",
                            "January-22","February-22","March-22","April-22")


ggplot(Keysdata, aes(x=Date, y=CPUE_lbs.Trap, group= Location)) + #This is the Keys subset graph
  geom_line(aes(linetype= Location, color=Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("CPUE (Catch Per Unit Effort)") +
  ggtitle("CPUE Change Over Stone Crab Season for Key's Locations")+
  ylim(0,1.5)+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=11.75))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggplot(Southdata, aes(x=Date, y=CPUE_lbs.Trap, group= Location)) + #This is the South subset graph
  geom_line(aes(linetype= Location, color=Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("CPUE (Catch Per Unit Effort)") +
  ggtitle("CPUE Change Over Stone Crab Season for Southern Locations")+
  ylim(0,1.5)+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=11.75))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggplot(Middledata, aes(x=Date, y=CPUE_lbs.Trap, group= Location)) + #This is the Middle subset graph
  geom_line(aes(linetype= Location, color=Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("CPUE (Catch Per Unit Effort)") +
  ggtitle("CPUE Change Over Stone Crab Season for Middle Locations")+
  ylim(0,1.5)+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=12.5))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggplot(Northdata, aes(x=Date, y=CPUE_lbs.Trap, group= Location)) + #This is the North subset graph
  geom_line(aes(linetype= Location, color=Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("CPUE (Catch Per Unit Effort)") +
  ggtitle("CPUE Change Over Stone Crab Season for Northern Locations")+
  ylim(0,1.5)+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=11.75))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

sctotal$Date <- fct_relevel(sctotal$Date,"21-Oct","21-Nov","21-Dec",
                             "22-Jan","22-Feb","22-Mar","22-Apr")

ggplot(sctotal, aes(x=Date, y=CPUE_lbs.Trap)) + #This is the Box plot of the CPUE to 
  geom_boxplot() + 
  xlab("Collection Date") +
  ylab("CPUE (Catch Per Unit Effort)") +
  ggtitle("CPUE Change Over the 2021-2022 Stone Crab Season in Florida")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=13.5))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=12))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=12))


#Task 2- Proportion of legal claws to total claws and proportion of total claws per crab per site
#want to show this as a stacked bar graph of the locations

ggplot(sctotal, aes(x=Date, y=Prop_Total_Claws, group= Location)) + #This is the original graph for total claws per time with Location
  geom_line(aes(color= Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("Proportion of Total Stone Crab Claws") +
  ggtitle("Proportion of Total Claws Found Per Crab Per Location")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=12.75))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size


ggplot(sctotal, aes(x=Date, y=Prop_Legal_Claws, group= Location)) + #This is the graph for total claws per time with Location
  geom_line(aes(color= Location)) + 
  geom_point() +
  xlab("Collection Date") +
  ylab("Proportion of Legal Stone Crab Claws") +
  ggtitle("Proportion of Legal Claws Foudnd Per Total Claws Collected")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=11.75))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=11))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=11))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size


#Task 3- Shows total amount of claws per area and total amount of legal claws
sctotal2 <- sctotal[-5,] #Creating another subset to add all the totals together to get the total amount of each location together into one sum
sctotal2 <- sctotal2 %>% group_by(Location) %>% summarize(totalClaws=sum(Number_Legal_Claws),
                                                          maxtotalClaws=sum(Number_Claws)) #sets the new data frame up to add all of the data together to create one total sum

ggplot(sctotal2, aes(x=Location, y=totalClaws))+
  geom_col(aes(color=Location, fill=Location))+
  geom_text(aes(label=totalClaws),position = position_dodge(width = 0.9),vjust=-0.25)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  xlab("Location") +
  ylab("Number of Legal Claws Found") +
  ggtitle("Number of Legal Claws Found per Location for the 2021-2022 Season")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=10.5))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=10))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=10))+
  theme(legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

ggplot(sctotal2, aes(x=Location, y=maxtotalClaws))+
  geom_col(aes(color=Location, fill=Location))+
  geom_text(aes(label=maxtotalClaws),position = position_dodge(width = 0.9),vjust=-0.25)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  xlab("Location") +
  ylab("Number of Total Claws Found") +
  ggtitle("Number of Claws Found per Location for the 2021-2022 Season")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=11.3))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=10))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=10))+
  theme(legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.title = element_text(size=11), #change legend title font size
        legend.text = element_text(size=10)) #change legend text font size

#Task 4- Sex related to keeper claws, which sex had a higher rate of legal claws?

ggplot(sctotal, aes(x=Location, y=Male)) + #This is the Box plot of Male crab density per location
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylim(0,250)+
  xlab("Location") +
  ylab("Amount of Male Crabs") +
  ggtitle("Total Male Crab Density per Location")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=17))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=12))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=12))


ggplot(sctotal, aes(x=Location, y=Female)) + #This is the Box plot of Female crab density per location
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ylim(0,250)+
  xlab("Location") +
  ylab("Amount of Female Crabs") +
  ggtitle("Total Female Crab Density per Location")+
  theme(plot.title = element_text(face="bold.italic", colour="black", size=17))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=12))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=12))

#Task 5- Kruskal Wallis test Use a Kruskal Wallis test for not normal data (shapiro test is significant (P value is below 0.05))

## Normality/ Kruskal for non normal data and Anova for normalized data
shapiro.test(sctotal$Male) #This data is not normalized (P value is significant p is lower then 0.05)
resultLT = leveneTest(Male~Location, data = sctotal) 
resultLT 
kruskal.test(Male ~ Location, data = sctotal) #Found that location is significant to the number of male crabs found

shapiro.test(sctotal$Female) #This data is not normalized (P value is significant p is lower then 0.05)
resultLT1 = leveneTest(Male~Location, data = sctotal) 
resultLT1 
kruskal.test(Female ~ Location, data = sctotal) #Found that location is significant to the number of female crabs

kruskal.test(Male ~ Female, data = sctotal) #Found that location is significant to the number of female crabs
kruskal.test(Male ~ Number_Legal_Claws, data = sctotal)
kruskal.test(Female ~ Number_Legal_Claws, data = sctotal)

shapiro.test(sctotal$CPUE_lbs.Trap) #This data is not normalized (P value is significant p is lower then 0.05)
resultLT = leveneTest(CPUE_lbs.Trap~Location, data = sctotal) 
resultLT 
kruskal.test(CPUE_lbs.Trap ~ Location, data = sctotal) #Found that location is not significant to the CPUE

shapiro.test(sctotal$CPUE_lbs.Trap) #This data is not normalized (P value is significant p is lower then 0.05)
resultLT = leveneTest(CPUE_lbs.Trap~Date, data = sctotal) 
resultLT 
kruskal.test(CPUE_lbs.Trap ~ Date, data = sctotal) #Found that Date is significant to the CPUE

shapiro.test(sctotal$Number_Claws)#This data is not normalized (P value is significant p is lower then 0.05)
resultLT2 = leveneTest(Number_Claws~Location, data = sctotal) 
resultLT2 
kruskal.test(Number_Claws ~ Location, data = sctotal) #Found that location is significant to the number of claws found
kruskal.test(Number_Claws ~ Date, data = sctotal)

shapiro.test(sctotal$Number_Legal_Claws) #This data is not normalized (P value is significant p is lower then 0.05)
kruskal.test(Number_Legal_Claws ~ Location, data = sctotal) #Found that location is significant to the number of Legal keepr claws
kruskal.test(Number_Legal_Claws ~ Date, data = sctotal)

shapiro.test(sctotal$CPUE_lbs.Trap) #This data is not normalized (P value is significant p is lower then 0.05)
kruskal.test(CPUE_lbs.Trap ~ Location, data = sctotal) #Found that location is NOT SIGNIFICANT to the CPUE

kruskal.test(Number_Legal_Claws ~ Number_Claws, data = sctotal) #Legal claws to total claws was not significant

shapiro.test(sctotal$Prop_Legal_Claws) #This data is not normalized (P value is significant P is lower then 0.05)
kruskal.test(Prop_Legal_Claws ~ Location, data = sctotal) #Found that location is significant to the the proportion of legal claws
kruskal.test(Prop_Legal_Claws ~ Date, data = sctotal) #Found that the date is significant to the proportion of legal claws

shapiro.test(sctotal$Prop_Total_Claws) #This data is not normalized (P value is significant p is lower then 0.05)
kruskal.test(Prop_Total_Claws ~ Location, data = sctotal) #Found that location is significant to the the proportion of total claws
kruskal.test(Prop_Total_Claws ~ Date, data = sctotal) #Found that the date is NOT significant to the proportion of total claws

kruskal.test(Prop_Total_Claws ~ Prop_Legal_Claws, data = sctotal) #Found that the date is NOT significant to the proportion of total claws