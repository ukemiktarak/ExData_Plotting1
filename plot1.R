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

plot1 <- function(){
    ## file <- "household_power_consumption.txt"
    file <- "household_power_consumption.txt"
    plotFileName <- "plot1.png"
    wd <- 480  ## plot file width
    ht <- 480  ## plot file height
    
    ## read data and filter
    dt <- readData(file)
    
    ## draw the plot
    hist(as.numeric(dt$Global_active_power), main="Global Active Power", 
         xlab = "Global Active Power (kilowatts)", col="red")
    
    ## copy the output to a png file
    dev.copy(png, file=plotFileName, width=wd, height=ht)
    dev.off()
}
