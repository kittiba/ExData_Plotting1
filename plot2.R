plot2 <-function(file){
        #This function plot2 takes "household_power_consumption.txt" as data 
        #input and creates a png file with a line plot of date/time of
        #couple of dates that we are interested in vs Global Active Power
        
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
        
        
        #changing the Global active power values to numeric
        data1$Global_active_power = as.numeric(as.character(data1$Global_active_power))
        
        #setting the graph margins so that labels are visible
        par(mar=c(6,5,2,2))
        
        
        #Plotting a line plot to the screen
        with(data1, plot(data1$Dateformatted, data1$Global_active_power, 
                         type="l",
                         xlab="",
                         ylab="Global Active power (kilowatts)",
                         col="black"))
        
        #Copying the histrogram from screen to png file with correct dimensions
        dev.copy(png, file="plot2.png", width = 480, height = 480, units = "px")
        dev.off()
       

}