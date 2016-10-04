library(datasets)

# import data into memory                  
readData <- function(filePath){
    # create a class to convert date field while reading
    setClass("myDate")
    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
    
    # read the data from the file
library(datasets)
    epc <- read.table(filePath, 
                      sep=";", 
                      colClasses = c("myDate", "character", "character", "character", "character", "character", "character", "character", "character"), 
                      stringsAsFactors = FALSE, 
                      na.strings="?",
                      header = TRUE)
    
    # filter the frame to fetch only records for 2007-02-01 and 2007-02-02
    epc <- epc[epc$Date >= as.Date("2007-02-01", "%Y-%m-%d") & epc$Date < as.Date("2007-02-03", "%Y-%m-%d"), ]
    epc
}

# convert numeric fields from character to numeric
convertToNumeric <- function(epc){
    # 
    epc$Time <- as.POSIXct(paste(epc$Date, epc$Time), format="%Y-%m-%d %H:%M:%S")
    epc$Global_active_power <- as.numeric(epc$Global_active_power)
    epc$Global_reactive_power <- as.numeric(epc$Global_reactive_power)
    epc$Voltage <- as.numeric(epc$Voltage)
    epc$Global_intensity <- as.numeric(epc$Global_intensity)
    epc$Sub_metering_1 <- as.numeric(epc$Sub_metering_1)
    epc$Sub_metering_2 <- as.numeric(epc$Sub_metering_2)
    epc$Sub_metering_3 <- as.numeric(epc$Sub_metering_3)
    epc
}
    
plot4 <- function(){
    # set file path
    fPath = "household_power_consumption.txt"
    
    # read the file
    epc <- readData(fPath)
    
    # convert all numeric fields from char to numeric
    epc <- convertToNumeric(epc)
    
    # generate plot4
    png(file = "plot4.png", width=480, height = 480)
    par(mfrow=c(2,2))
    plot(epc$Time, epc$Global_active_power, type="l", xlab="", ylab="Global Active Power")
    plot(epc$Time, epc$Voltage, type="l", xlab="datetime", ylab="Voltage")
    plot(epc$Time, epc$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    points(epc$Time, epc$Sub_metering_2, type="l", col="red")
    points(epc$Time, epc$Sub_metering_3, type="l", col="blue")
    legend("topright", lty=c(1,1,1), pch=c(NA, NA, NA), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(epc$Time, epc$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
    dev.off()
}

