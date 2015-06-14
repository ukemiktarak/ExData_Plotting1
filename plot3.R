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

plot3 <- function(){
    ## file <- "household_power_consumption.txt"
    file <- "2dayData.txt"
    
    plotFileName <- "plot3.png"
    wd <- 480  ## plot file width
    ht <- 480  ## plot file height
    
    ## read data and filter
    dt <- readData(file)
    
    ## draw the plot
    
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
    
    legend("topright", lty=c(1,1,1), col=c("black", "red", "blue"), 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ))
    
    ## copy the output to a png file
    dev.copy(png, file=plotFileName, width=wd, height=ht)
    dev.off()
}
