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

# Change display of weekdays to English
curr_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","C")

# Create line chart
plot.new()
plot(house$DateTime, house$Global_active_power, type = 'n', xlab = '', ylab = 'Global Active Power (kilowatts)')
lines(house$DateTime, house$Global_active_power)

# Save chart as png file
dev.copy(png, file = 'plot2.png',  width = 480, height = 480)
dev.off()

# Change display of weekdays back to initial language
Sys.setlocale("LC_TIME",curr_locale)