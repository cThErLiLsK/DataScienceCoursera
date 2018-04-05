library(datasets)

# Load data from disk
path <- '../HouseholdPowerConsumption/'
file <- read.csv(paste0(path, 'household_power_consumption.txt'), sep = ';', stringsAsFactors=FALSE)
downloadDate <- date()

# Format date and time & create new column with date and time combined
house <- file
house$DateTime <- paste(house$Date, house$Time)
house$DateTime <- strptime(house$DateTime, '%d/%m/%Y %H:%M:%S')
house$Date <- strptime(house$Date, '%d/%m/%Y')

# Select data for the required two dates
house <- house[(house$Date == '2007-02-01' | house$Date == '2007-02-02'),]

# Convert variable into numeric variable
house$Global_active_power <- as.numeric(house$Global_active_power)
house$Sub_metering_1 <- as.numeric(house$Sub_metering_1)
house$Sub_metering_2 <- as.numeric(house$Sub_metering_2)
house$Sub_metering_3 <- as.numeric(house$Sub_metering_3)
house$Voltage <- as.numeric(house$Voltage)
house$Global_reactive_power <- as.numeric(house$Global_reactive_power)

# Change display of weekdays to English
curr_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","C")

# Set dev to png
png(file = 'plot4.png',  width = 480, height = 480)

# Set raster to 2x2
par(mfcol = c(2, 2), mar = c(4, 4, 2, 1))

# Create line chart

## 1
plot(house$DateTime, house$Global_active_power, type = 'n', xlab = '', ylab = 'Global Active Power (kilowatts)')
lines(house$DateTime, house$Global_active_power)

## 2
plot(house$DateTime, house$Sub_metering_1, type = 'n', xlab = '', ylab = 'Energy sub metering')
lines(house$DateTime, house$Sub_metering_1, col = 'black')
lines(house$DateTime, house$Sub_metering_2, col = 'red')
lines(house$DateTime, house$Sub_metering_3, col = 'blue')
legend('topright', lty = rep(1, 3), col = c('black', 'red', 'blue'), 
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))

## 3
plot(house$DateTime, house$Voltage, type = 'n', xlab = 'datetime', ylab = 'Voltage')
lines(house$DateTime, house$Voltage)

## 4
plot(house$DateTime, house$Global_reactive_power, type = 'n', xlab = 'datetime', ylab = 'Global_reactive_power')
lines(house$DateTime, house$Global_reactive_power)


# Save chart as png file
dev.off()

# Change display of weekdays back to initial language
Sys.setlocale("LC_TIME",curr_locale)