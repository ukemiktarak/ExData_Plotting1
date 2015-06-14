library(data.table)

## reads data and returns a data.table
readData <- function(file){
    stDate <- strptime("01/02/2007", "%d/%m/%Y")
    endDate <- strptime("02/02/2007", "%d/%m/%Y")
    naCharacter <- "?"
    
    dt <- read.table(file, sep=";", na.strings = naCharacter, header=TRUE, stringsAsFactors=FALSE)
    
    ## take only the values for 01/02/2007 and 02/02/2007
    dt <-  dt[strptime(dt[, "Date" ], "%d/%m/%Y") >=  stDate & strptime(dt[,"Date" ], "%d/%m/%Y") <= endDate, ]
    
    dt
}

plot4 <- function(){
    ## file <- "household_power_consumption.txt"
    file <- "2dayData.txt"
    
    plotFileName <- "plot4.png"
    wd <- 480  ## plot file width
    ht <- 480  ## plot file height
    
    ## read data and filter
    dt <- readData(file)
    
    ## draw the plot
    ## set the frame for 4 plots (2x2)
    
    ## Margins are specified as 4-long vectors of integers. Each number tells how many lines of text to leave at
    ## each side. The numbers are assigned clockwise starting at the bottom. The default for the inner margin is
    ## c(5.1, 4.1, 4.1, 2.1) so you can see we reduced each of these so we'll have room for some outer text.
    par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,1,0))
    ## draw plot 1, 1
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Global_active_power, type = "l",  ylab="Global Active Power", xlab="")
    
    ## draw plot 1, 2
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Voltage, type = "l",  col="black", ylab="Voltage", xlab="dateTime")
    
    ## draw plot 2, 1
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Sub_metering_1, type = "l",  col="black", ylab="Energy sub metering", xlab="",
         ylim=range( c(dt$Sub_metering_1, dt$Sub_metering_2, dt$Sub_metering_3) ) )
    par(new=T)
    
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Sub_metering_2, type = "l",  col="red", ylab ="", xlab="",
         ylim=range( c(dt$Sub_metering_1, dt$Sub_metering_2, dt$Sub_metering_3) )
    )
    par(new=T)
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Sub_metering_3, type = "l",  col="blue", ylab ="", xlab="",
         ylim=range( c(dt$Sub_metering_1, dt$Sub_metering_2, dt$Sub_metering_2) )
    )
    
    ## bty box type none
    legend("topright", lty=c(1,1,1), col=c("black", "red", "blue"), 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ),
           bty="n")
    
    ## draw plot(2, 2)
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Global_reactive_power, type = "l",  col="black",
         ylab="Global_reactive_power", xlab="dateTime")
    
    ## copy the output to a png file
    dev.copy(png, file=plotFileName, width=wd, height=ht)
    dev.off()
}
