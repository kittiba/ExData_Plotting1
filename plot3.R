plot3 <-function(file){
        #This function plot3 takes "household_power_consumption.txt" as data 
        #input and creates a png file with a line plot of date/time of
        #couple of dates that we are interested in vs energy sub metering 1,2,3
        
        #file argument should be the path of the data file
        data<-read.table(file,
                        sep=";",header=TRUE,na.string=c("NA","?"))
        
        #formatting the character 'Date' to a Date column and storing separately
        data$Dateformatted<-as.Date(data$Date,"%d/%m/%Y")
        
        #formatting the couple of dates that we are pulling the data for to a date
        dates<-c(as.Date('2007-02-01',"%Y-%m-%d"),as.Date('2007-02-02',"%Y-%m-%d"))
        
        #subsetting the data by the couple of dates we are interested in
        data1<-subset(data, Dateformatted %in% dates)
        
        #formatting the date 'Dateformatted' to a Date and time column
        data1$Dateformatted<-strptime(paste(data1$Date,data1$Time),"%d/%m/%Y %H:%M:%S")
        
        #changing the sub metering 1 values to numeric
        data1$Sub_metering_1 = as.numeric(as.character(data1$Sub_metering_1))
        
        #changing the sub metering 2 values to numeric
        data1$Sub_metering_2 = as.numeric(as.character(data1$Sub_metering_2))
        
        #changing the sub metering 3 values to numeric
        data1$Sub_metering_3 = as.numeric(as.character(data1$Sub_metering_3))
        
        
        #setting the graph margins so that labels are visible
        par(mar=c(6,6,2,2))
        
        #Creating a new PNG file with the right dimensions
        png(file="plot3.png",width = 480, height = 480, units = "px")
        
        #Plotting with no lines initially
        plot(data1$Dateformatted, data1$Sub_metering_1,
             type="n",xlab="", ylab="Energy sub metering")
        
        #Plotting the lines for all metering
        lines(data1$Dateformatted, data1$Sub_metering_1,  type="l",col="black")
        lines(data1$Dateformatted, data1$Sub_metering_2,  type="l",col="red")
        lines(data1$Dateformatted, data1$Sub_metering_3,  type="l",col="blue")
        
        #Defining the legend
        legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               col=c("black","red","blue"),lwd=1, cex=1)
        
        #closing the file
        dev.off()
       

}