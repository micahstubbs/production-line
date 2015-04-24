# set your working directory, if different from your R home directory
# setwd("C:\\Users\\you\\workspace\\production-vis")
filename <- "production.csv"

production_wide <- read.csv(filename, header=TRUE, sep=",")
library('reshape2')
production <- melt(production_wide, id.vars = c("UWI", "VOLUME_DATE", "ACTIVITY_TYPE"))

# rename some columns
names(production)[names(production)=="UWI"] <- "uniqueWellIdentifier"
names(production)[names(production)=="VOLUME_DATE"] <- "volumeDate"
names(production)[names(production)=="ACTIVITY_TYPE"] <- "activityType"
names(production)[names(production)=="variable"] <- "key"

# format the dates
production$date <- as.Date(production$volumeDate, "%m/%d/%Y")
production$date <- format(production$date, "%m/%d/%Y")
production <- subset(production, select=-c(volumeDate))

# give the keys nice names
levels(production$key) <- c(levels(production$key), "Oil")
levels(production$key) <- c(levels(production$key), "Gas")
levels(production$key) <- c(levels(production$key), "Water")
production$key[production$key=="OIL_VOLUME"] <- "Oil"
production$key[production$key == "GAS_VOLUME"] <- "Gas"
production$key[production$key == "WATER_VOLUME"] <- "Water"

# re-order the columns
production <- production[c("key", "value", "date", "uniqueWellIdentifier", "activityType")]

###


###

# find a list of wells with non-zero gas, oil, and water production
library('dplyr')
gas_wells <- filter(production, key == "Gas", value > 0)
oil_wells <- filter(production, key == "Oil", value > 0)
water_wells <- filter(production, key == "Water", value > 0)

gas_wells <- gas_wells[ , c("uniqueWellIdentifier")]
oil_wells <- oil_wells[ , c("uniqueWellIdentifier")]
water_wells <- water_wells[ , c("uniqueWellIdentifier")]

all_products_wells <- Reduce(intersect, list(gas_wells, oil_wells, water_wells))
head(all_products_wells)

###


# make a dataframe with the data from the first well in the list of the wells that produced non-zero quantities of oil, gas, and water
single_well <- filter(production, uniqueWellIdentifier == all_products_wells[4])

# verbose approach. to do - make this concise with a function like
# interpolate_values(dataframe, by=(columnName), interval=("day"))

# insert records for missing days

library('zoo')
zoo.to.data.frame <- function(x, index.name="Date") {
  stopifnot(is.zoo(x))
  xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
  setNames(data.frame(index(x), x, row.names=NULL), c(index.name,xn))
}

#oil
df1_oil <- subset(single_well, key == "Oil")
df1_oil <- subset(sw_subset, select=c(date, value))

df1_oil$date<-as.POSIXct(df1_oil$date,format="%m/%d/%Y")
df2_oil.zoo<-zoo(df1_oil[,-1],df1_oil[,1]) #set date to Index
df3_oil <- merge(df1.zoo,zoo(,seq(start(df1.zoo),end(df1.zoo),by="day")), all=TRUE)

zoo.to.data.frame <- function(x, index.name="Date") {
  stopifnot(is.zoo(x))
  xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
  setNames(data.frame(index(x), x, row.names=NULL), c(index.name,xn))
}

# use linear interpolation to infer values for missing days
zoo_oil <- na.approx(zoo_oil)

df_oil <- zoo.to.data.frame(zoo_oil, index.name="date")
names(df_oil)[names(df_oil)=="zoo_oil"] <- "value"

character_vector <- vector(mode="character", length=nrow(df_oil))
numeric_vector <- vector(mode="numeric", length=nrow(df_oil))

df_oil$key <- character_vector
df_oil$key <- levels(df_oil$key) <- c(levels(df_oil$key), "Gas")
df_oil$key <- levels(df_oil$key) <- c(levels(df_oil$key), "Water")
df_oil$key <- levels(df_oil$key) <- c(levels(df_oil$key), "Oil")
df_oil$key[df_oil$key==""] <- "Oil"

df_oil$activityType <- character_vector
df_oil$activityType <- levels(df_oil$activityType) <- c(levels(df_oil$activityType), "PRODUCTION")
df_oil$activityType[df_oil$activityType==""] <- "PRODUCTION"

df_oil$uniqueWellIdentifier <- numeric_vector
df_oil$uniqueWellIdentifier[df_oil$uniqueWellIdentifier==0] <- single_well$uniqueWellIdentifier[1]

# re-order the columns
df_oil <- df_oil[c("key", "value", "date", "uniqueWellIdentifier", "activityType")]

###
#####
###

#gas
sw_subset <- subset(single_well, key == "Gas")
sw_subset <- subset(sw_subset, select=c(date, value))

# use linear interpolation to infer values for missing days
zoo_gas <- na.approx(zoo_gas)

df_gas <- zoo.to.data.frame(zoo_gas, index.name="date")
names(df_gas)[names(df_gas)=="zoo_gas"] <- "value"

character_vector <- vector(mode="character", length=nrow(df_gas))
numeric_vector <- vector(mode="numeric", length=nrow(df_gas))

df_gas$key <- character_vector
df_gas$key <- levels(df_gas$key) <- c(levels(df_gas$key), "Oil")
df_gas$key <- levels(df_gas$key) <- c(levels(df_gas$key), "Water")
df_gas$key <- levels(df_gas$key) <- c(levels(df_gas$key), "Gas")
df_gas$key[df_gas$key==""] <- "Gas"

df_gas$activityType <- character_vector
df_gas$activityType <- levels(df_gas$activityType) <- c(levels(df_gas$activityType), "PRODUCTION")
df_gas$activityType[df_gas$activityType==""] <- "PRODUCTION"

df_gas$uniqueWellIdentifier <- numeric_vector
df_gas$uniqueWellIdentifier[df_gas$uniqueWellIdentifier==0] <- single_well$uniqueWellIdentifier[1]

# re-order the columns
df_gas <- df_gas[c("key", "value", "date", "uniqueWellIdentifier", "activityType")]

###
#####
###

#water
sw_subset <- subset(single_well, key == "Water")
sw_subset <- subset(sw_subset, select=c(date, value))

df <- sw_subset
df$date<-as.POSIXct(df$date,format="%m/%d/%Y")
df1.zoo<-zoo(df[,-1],df[,1]) #set date to Index
zoo_water <- merge(df1.zoo,zoo(,seq(start(df1.zoo),end(df1.zoo),by="day")), all=TRUE)

# use linear interpolation to infer values for missing days
zoo_water <- na.approx(zoo_water)

df_water <- zoo.to.data.frame(zoo_water, index.name="date")

character_vector <- vector(mode="character", length=nrow(df_water))
numeric_vector <- vector(mode="numeric", length=nrow(df_water))

df_water$key <- character_vector
df_water$key <- levels(df_water$key) <- c(levels(df_water$key), "Oil")
df_water$key <- levels(df_water$key) <- c(levels(df_water$key), "Gas")
df_water$key <- levels(df_water$key) <- c(levels(df_water$key), "Water")
df_water$key[df_water$key==""] <- "Water"

df_water$activityType <- character_vector
df_water$activityType <- levels(df_water$activityType) <- c(levels(df_water$activityType), "PRODUCTION")
df_water$activityType[df_water$activityType==""] <- "PRODUCTION"

df_water$uniqueWellIdentifier <- numeric_vector
df_water$uniqueWellIdentifier[df_water$uniqueWellIdentifier==0] <- single_well$uniqueWellIdentifier[1]

# re-order the columns
df_water <- df_water[c("key", "value", "date", "uniqueWellIdentifier", "activityType")]

###
#####
###

library('dplyr')
single_well_subset_all_days <- rbind(df_oil, df_gas, df_water)

# round the values
single_well_subset_all_days$value <- round(single_well_subset_all_days$value, digits=0)

# format the dates
single_well_subset_all_days$date <- format(single_well_subset_all_days$date, "%m/%d/%Y")

#write the results out to a .csv file for visualization
output_file <- "single-well.csv"
write.csv(single_well_subset_all_days, file = output_file,row.names=FALSE, na="",quote=c(3))

