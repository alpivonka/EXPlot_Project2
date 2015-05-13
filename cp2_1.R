library(curl)
download_data<-function(){
  # download and unzip the dataset
  if(file.exists('data/NEI_data.zip') ==FALSE){
    print("Pulling down data")
    download.file("https://d396qusza40orc.cloudfront.net/exdata/data/NEI_data.zip", destfile="data/NEI_data.zip", method="libcurl")
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
 
  
  if(plotNum ==1){
    datax<-theData[,c("year","Emissions")]
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
  }
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