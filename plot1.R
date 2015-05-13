plot1<-function(){
  #Source the data setup file
  source("getData.R")
  #Load the data and for Plot #1
  datax<-load_data(1)
  #png file setup
  png(filename = "plot1.png", 
      width = 480, height = 480, 
      units = "px", bg = "transparent")
  #Tell it not to use scientifice numerics
  options(scipen=999)
  #View(datax)
  #Get only the data we want and summed
  sumByYear<-aggregate(datax$Emissions,by=list(datax$year),FUN=sum)
  #Give it good column names
  colnames(sumByYear)<-c("year","Emissions")
  #print(sumByYear)
  #Create the plot
  plot(sumByYear$year,sumByYear$Emissions,type = "b",main="Decrease of PM2.5 in the United States",sub="Al Pivonka",
       xlab="Year", ylab="Total Emissions Per Year",col="red")
  dev.off()
}