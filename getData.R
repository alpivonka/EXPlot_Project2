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
  if(plotNum ==2){
    datax<-theData[theData$fips=="24510",c("year","Emissions","fips")]
    #print(datax)
    datax[,2]<-as.numeric(datax[,2])
    theData<-datax
  }
  theData
}