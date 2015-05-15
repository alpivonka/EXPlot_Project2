library(curl)
require(ggplot2)
library(grid)
library(gridExtra)

download_data<-function(){
  # download and unzip the dataset
  if(file.exists('data/NEI_data.zip') ==FALSE){
    print("Pulling down data")
    download.file("http://d396qusza40orc.cloudfront.net/exdata/data/NEI_data.zip", destfile="data/NEI_data.zip", method="auto")
    unzip("data/NEI_data.zip",exdir = "data/temp")
  }
  
}

load_data<-function(plotNum){
  download_data()
  if(file.exists('data/mergedNEISCC.rds')==TRUE){
    print("loading from storage")
    theData<- readRDS("data/mergedNEISCC.rds")
  }else{
    NEI <- readRDS("data/temp/summarySCC_PM25.rds")
    #print(names(NEI))
    SCC <- readRDS("data/temp/Source_Classification_Code.rds")
    #print(names(SCC))
    theData<-merge(NEI,SCC,by="SCC")
    saveRDS(theData,'data/mergedNEISCC.rds')
  }
  #SCC <- readRDS("data/temp/Source_Classification_Code.rds")
  #print(names(SCC))
  #View(unique(SCC$SCC.Level.Four))
  #stop()
  
  if(plotNum ==1){
    datax<-theData[,c("year","Emissions")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
  }
  if(plotNum ==2){
    datax<-theData[theData$fips == "24510",c("year","Emissions")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
  }
  if(plotNum ==3){
    datax<-theData[theData$fips == "24510",c("year","Emissions","type")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
    print(head(theData))
  }
  if(plotNum ==4){
    datax<-theData[(grepl("combustion",theData$SCC.Level.One,ignore.case=TRUE) & 
                      grepl("coal",theData$SCC.Level.Three,ignore.case=TRUE)),c("year","Emissions")]
    datax[,2]<-as.numeric(datax[,2])
    #print(head(datax,10))
    theData<-datax
  }
  
  if(plotNum ==5){
    datax<-theData[theData$type == "OnRoad",c("year","Emissions","type")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
    print(head(theData))
  }
  
  
  print("done with loading data")
  #theData
}


plot1<-function(){
  datax<-load_data(1)
  png(filename = "plot1.png", 
      width = 480, height = 480, 
      units = "px", bg = "transparent")
  
  options(scipen=999)
  #View(datax)
  sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  colnames(sumByYear)<-c("year","Emissions")
  #print(sumByYear)
  plot(sumByYear$year,sumByYear$Emissions,type = "b",main="Decrease of PM2.5 in the United States",sub="Al Pivonka",
       xlab="Year", ylab="Total Emissions Per Year",col="red")
  dev.off()
}
plot2<-function(){
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(2)
  #png file setup
  
  png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "transparent")
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  #Get only the data we want and summed
  sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  #Give it good column names
  colnames(sumByYear)<-c("year","Emissions")
  #print(sumByYear)
  #Create the plot
  plot(sumByYear$year,sumByYear$Emissions,type = "b",main="Decrease of PM2.5 in Baltimore City, Maryland",sub="Al Pivonka",
       xlab="Year", ylab="Total Emissions Per Year",col="red")
  dev.off()
}

plot31<-function(){
  # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
  # which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
  # plot answer this question.
  # type: The type of source (point, non-point, on-road, or non-road)
  
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(3)
  
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  print(as.factor(unique(datax$type)))
  #Get only the data we want and summed
  #sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  #Give it good column names
  #print(datax)
  colnames(datax)<-c("Year","Emissions","type")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "transparent")
  gAllData<-ggplot(datax,aes(Year, Emissions))+ylab("Emissions")+geom_point(color="firebrick")+facet_wrap(~type,scales = "free_y")+stat_smooth(method = "lm", se = FALSE)+ggtitle("PM25")
  #g+geom_point(color="firebrick")+geom_boxplot()+facet_wrap(~type,scales = "free")+geom_quantile()+ggtitle("Baltimore City,Maryland\nAl Pivonka")+geom_violin(alpha=0.5, color="green")
  
  #ggsave(file="plot3.png")#,width = 5,height=3)
  #dev.off()
  gAllData
  
}
plot32<-function(){
  # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
  # which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
  # plot answer this question.
  # type: The type of source (point, non-point, on-road, or non-road)
  
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(3)
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #print(head(datax))
  #Get only the data we want and summed
  datax<-aggregate(datax,by=list(datax$year,datax$type),FUN=mean)
  #Give it good column names
  #print(datax)
  #stop()
  colnames(datax)<-c("Year","Type","year2","Emissions","type2")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "transparent")
  gMeanData<-ggplot(datax,aes(Year, Emissions))+ylab("Emissions")+geom_point(color="firebrick")+geom_line()+facet_wrap(~Type,scales = "free_y")+ggtitle("Mean of PM25")
  
  #g+geom_point(color="firebrick")+geom_boxplot()+facet_wrap(~type,scales = "free")+geom_quantile()+ggtitle("Baltimore City,Maryland\nAl Pivonka")+geom_violin(alpha=0.5, color="green")
  
  #ggsave(file="plot3_2.png")#,width = 5,height=3)
  #dev.off()
  gMeanData
  
}
plot3<-function(){
  p1<-plot31()
  p2<-plot32()
  #grid.arrange(p1,p2, ncol=2,main = "Baltimore City,Maryland\nAl Pivonka\nPlease notice the difference in the scales.")
  xx<-arrangeGrob(p1,p2, ncol=2,main = "Baltimore City,Maryland\nAl Pivonka\nPlease notice the difference in the scales.")
  ggsave(file="plot3.png",xx,width = 10,height=5)#,width = 5,height=3)
}


plot4<-function(){
  # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
  # which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
  # plot answer this question.
  # type: The type of source (point, non-point, on-road, or non-road)
  
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(4)
  
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  #print(as.factor(unique(datax$Type)))
  #Get only the data we want and summed
  #sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  dataxMean<-aggregate(datax,by=list(datax$year),FUN=mean)
  #Give it good column names
  #print(head(datax))
  #stop()
  #colnames(datax)<-c("Year","Emissions")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "transparent")
  gAllData<-ggplot(datax,aes(year, Emissions))+ylab("PM2.5 Emissions")+geom_point(color="firebrick")+ggtitle("PM2.5 Emissions")+stat_smooth(method = "lm", se = FALSE,colour="darkblue")
  
  
  gMeanData<-ggplot(dataxMean,aes(year, Emissions))+ylab("PM2.5 Emissions")+geom_line(color="blue")+geom_point()+ggtitle("Mean of PM2.5 Emissions")#+stat_smooth(method = "lm", se = FALSE)
  #gMeanData
  
  #gAllData<-ggplot(datax,aes(Year, Emissions))+ylab("Emissions")+geom_point(color="firebrick")+facet_wrap(~type,scales = "free_y")+stat_smooth(method = "lm", se = FALSE)+ggtitle("PM25")
  #g+geom_point(color="firebrick")+geom_boxplot()+facet_wrap(~type,scales = "free")+geom_quantile()+ggtitle("Baltimore City,Maryland\nAl Pivonka")+geom_violin(alpha=0.5, color="green")
  xx<-arrangeGrob(gAllData,gMeanData, ncol=2,main = "Coal Combustion-related Sources \nAl Pivonka\nPlease notice the difference in the scales.")
  ggsave(file="plot3.png",xx,width = 10,height=5)
  
  #dev.off()
  #gAllData
  
}

plot5<-function(){
  # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
  # which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
  # plot answer this question.
  # type: The type of source (point, non-point, on-road, or non-road)
  
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(4)
  
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  #print(as.factor(unique(datax$Type)))
  #Get only the data we want and summed
  #sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  dataxMean<-aggregate(datax,by=list(datax$year),FUN=mean)
  #Give it good column names
  #print(head(datax))
  #stop()
  #colnames(datax)<-c("Year","Emissions")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "transparent")
  gAllData<-ggplot(datax,aes(year, Emissions))+ylab("PM2.5 Emissions")+geom_point(color="firebrick")+ggtitle("PM2.5 Emissions")+stat_smooth(method = "lm", se = FALSE,colour="darkblue")
  
  
  gMeanData<-ggplot(dataxMean,aes(year, Emissions))+ylab("PM2.5 Emissions")+geom_line(color="blue")+geom_point()+ggtitle("Mean of PM2.5 Emissions")#+stat_smooth(method = "lm", se = FALSE)
  #gMeanData
  
  #gAllData<-ggplot(datax,aes(Year, Emissions))+ylab("Emissions")+geom_point(color="firebrick")+facet_wrap(~type,scales = "free_y")+stat_smooth(method = "lm", se = FALSE)+ggtitle("PM25")
  #g+geom_point(color="firebrick")+geom_boxplot()+facet_wrap(~type,scales = "free")+geom_quantile()+ggtitle("Baltimore City,Maryland\nAl Pivonka")+geom_violin(alpha=0.5, color="green")
  xx<-arrangeGrob(gAllData,gMeanData, ncol=2,main = "Coal Combustion-related Sources \nAl Pivonka\nPlease notice the difference in the scales.")
  ggsave(file="plot3.png",xx,width = 10,height=5)
  
  #dev.off()
  #gAllData
  
}
