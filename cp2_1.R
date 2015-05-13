library(curl)
require(ggplot2)
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
  #print(head(SCC))
  
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
  }
  if(plotNum ==32){
    datax<-theData[c("year","Emissions","type")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
  }
  print("done with loading data")
  theData
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

plot3<-function(){
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
  colnames(datax)<-c("year","Emissions","type")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3_1.png", width = 480, height = 480, units = "px", bg = "transparent")
  g<-ggplot(datax,aes(year, Emissions))
  g+geom_point(color="firebrick")+geom_line()+facet_wrap(~type,scales = "free")
  
  ggsave(file="plot3_1.png")#,width = 5,height=3)
  #dev.off()
 
  
}
plot3_2<-function(){
  # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
  # which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a 
  # plot answer this question.
  # type: The type of source (point, non-point, on-road, or non-road)
  
  #Source the data setup file
  #source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(32)
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  print(as.factor(unique(datax$type)))
  #Get only the data we want and summed
  #sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  #Give it good column names
  #print(datax)
  colnames(datax)<-c("year","Emissions","type")
  #print(sumByYear)
  #Create the plot
  #png(filename = "plot3_1.png", width = 480, height = 480, units = "px", bg = "transparent")
  g<-ggplot(datax,aes(year, Emissions))
  g+geom_point(color="firebrick")+geom_line()+facet_wrap(~type,scales = "free")
  
  ggsave(file="plot3_2.png")#,width = 5,height=3)
  #dev.off()
  
  
}
  