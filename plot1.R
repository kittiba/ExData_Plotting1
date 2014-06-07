plot1 <-function(file){
        #file argument should be the path of the data file
        data<-read.table(file,
                        sep=";",header=TRUE,na.string=c("NA","?"))
        
        #formatting the character 'Date' to a Date column and storing separately
        data$Dateformatted<-as.Date(data$Date,"%d/%m/%Y")
        
        #formatting the couple of dates that we are pulling the data for to date
        dates<-c(as.Date('2007-02-01',"%Y-%m-%d"),as.Date('2007-02-02',"%Y-%m-%d"))
        
        #subsetting the data by the couple of dates we are interested in
        data1<-subset(data, Dateformatted %in% dates)
        
        #setting the graph margins so that labels are visible
        par(mar=c(6,5,2,2))
        
        #Plotting the histogram to the screen
        with(data1, hist(data1$Global_active_power, 
                         xlab="Global Active Power(kilowatts)",
                         ylab="Frequency",
                         main="Global Active Power",
                         col="red"))
        
        #Copying the histrogram from screen to png file with correct dimensions
        dev.copy(png, file="plot1.png", width = 480, height = 480, units = "px")
        dev.off()
       

}