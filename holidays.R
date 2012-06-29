fbd <- read.csv("full birthdate data.csv")
fbd$date <- as.Date(paste(fbd$year, fbd$month, fbd$day, sep="-"))
#filter invalid dates
fbd <- fbd[!is.na(fbd$date),]

#flag days of the week
fbd$dow <- as.numeric(strftime(fbd$date, "%w"))

#load holiday data
holidays <- read.csv("holidays47/holidays.csv")
holidays$date <- as.Date(holidays$date)
holidays <- holidays[!duplicated(holidays$date, fromLast=TRUE), 1:2]

merge(fbd, holidays, all.x=TRUE)

fbd$majorholiday <- fbd$holiday %in% c("New Year’s Day", "Ash Wednesday", "Valentine’s Day", "Palm Sunday", "St. Patrick’s day", "Good Friday", "Easter Sunday", "Easter Monday", "April Fools’ Day", "Cinco de Mayo", "Mother’s Day", "Memorial Day", "Father’s Day", "Independence Day", "American Labor Day", "Halloween", "Veterans Day", "American Thanksgiving", "Black Friday", "Christmas Eve", "Christmas in the USA", "New Year’s Eve")