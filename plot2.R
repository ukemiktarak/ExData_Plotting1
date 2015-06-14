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

plot2 <- function(){
    file <- "household_power_consumption.txt"
    ## file <- "2dayData.txt"
    
    plotFileName <- "plot2.png"
    wd <- 480  ## plot file width
    ht <- 480  ## plot file height
    
    ## read data and filter
    dt <- readData(file)
    
    ## draw the plot
    plot(as.POSIXct(paste(dt$Date, dt$Time), format="%d/%m/%Y %H:%M:%S"), 
         dt$Global_active_power, type = "l",  ylab="Global Active Power (kilowatts)", xlab="")
    
    ## copy the output to a png file
    dev.copy(png, file=plotFileName, width=wd, height=ht)
    dev.off()
}
