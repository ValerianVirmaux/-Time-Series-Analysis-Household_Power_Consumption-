start_time <- Sys.time() # take 17 minutes to run

# Install libraries
pacman::p_load(RMySQL, dplyr, lubridate, ggplot2, tseries, padr, zoo)

#### LOAD DATA ####

# Connect to the Server
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# Loop among tables to grab the data together
j <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010") 
HHPC <- c()
for (i in 1:length(j)) {
  X <- dbGetQuery(con, paste("SELECT * FROM ", j[i]))
  HHPC <- rbind(HHPC,X)
}

#### FEATURE TIME ####

# Creat global DateTime feature
HHPC$DateTime <- paste(HHPC$Date, HHPC$Time)
HHPC$DateTime <- ymd_hms(HHPC$DateTime)

# Add time feature
HHPC$year <- year(HHPC$Date)
HHPC$month <- month(HHPC$Date)
HHPC$minute <- minute(HHPC$Date)
HHPC$day <- day(HHPC$Date)
HHPC$week <- week(HHPC$Date)
HHPC$hour <- hour(HHPC$Date)
HHPC$quarter <- quarter(HHPC$Date)

#### MISSING VALUES ####

# Replace missing values by NA
HHPC <- pad(x = HHPC, break_above = 3)

paste("There is ", sum(is.na(HHPC$Sub_metering_1)), "missing values :", 
      round(sum(is.na(HHPC$Sub_metering_1)) / nrow(HHPC) * 100, 2), 
      "% of the data")

# Plot NA's
ggplot(HHPC, aes(DateTime, Global_active_power)) +
  geom_smooth(color = "black", se = F) +
  geom_vline(aes(xintercept = DateTime),
             data = HHPC %>% filter(is.na(Global_active_power)),
             colour="red",
             size = 0.05) +
  theme_classic() +
  labs(title = "Non available data from our time serie") +
  xlab("Time line") +
  ylab("Submeter 2")

# Store NA's by index, then merge them to list by groupe
naindex <- which(is.na(HHPC$Sub_metering_2)==TRUE) 
Breaks <- c(0, which(diff(naindex) != 1), length(naindex)) 
listNA <- sapply(seq(length(Breaks) - 1), function(i) naindex[(Breaks[i] + 1):Breaks[i+1]]) 

# Store index in three vectores
vector1 <- c() # Less than One hour
vector2 <- c() # From One Hour to One Day
vector3 <- c() # More than One Day

# Slit NA'a in three categories
for (i in 1:length(listNA)){
  # Less than one hour of NA's
  if (length(listNA[[i]]) <= 60){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector1 <- c(vector1,listNA[[i]][j])
    }
    # From onw hour to toone day
  } else if (length(listNA[[i]]) > 60 & length(listNA[[i]]) <= 1440){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector2 <- c(vector2,listNA[[i]][j])
    }
    # More than onw hour
  } else {
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector3 <- c(vector3,listNA[[i]][j])
    }
  }
}

# Plot NA's proportions
v1 <- data.frame(length(vector1), "Short_cut")
v2 <- data.frame(length(vector2), "Mid_cut")
v3 <- data.frame(length(vector3), "Long_cut")

colnames(v1) <- c("Value", "Name")
colnames(v2) <- c("Value", "Name")
colnames(v3) <- c("Value", "Name")

df <- rbind(v1, v2, v3)
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF")

ggplot(df, aes(x = "", y = Value, fill = Name)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = df$Value, label = paste(round((Value / sum(df$Value)) * 100), "%")), 
            color = "white", position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = mycols) +
  theme_void() +
  labs(title = "Proportion of NA's") 

# Replace vector1 (< 3 minutes missing) by the most recent non-NA
for (i in 4:10){
  for(v in vector1){
    HHPC[v,i] <- na.locf(unname(unlist(HHPC[,i])))[v]  
  }
}

paste("We now have ", sum(is.na(HHPC$Sub_metering_1)), "missing values :", 
      round(sum(is.na(HHPC$Sub_metering_1)) / nrow(HHPC) * 100, 2), 
      "% of the data")

# Replace vector2 by (from 3 minutes to 1 hour) 
# an average between 7 days ago, 14 days ago, 7 day ahead and 14 days ahead

for (i in 4:10){
  for(v in vector2){
    HHPC[v,i] <- (HHPC[v+10080,i] + 
                    HHPC[v+20160,i] + 
                    HHPC[v-10080,i] + 
                    HHPC[v-20160,i])/4
  }
}

paste("We now have ", sum(is.na(HHPC$Sub_metering_1)), "missing values :", 
      round(sum(is.na(HHPC$Sub_metering_1)) / nrow(HHPC) * 100, 2), 
      "% of the data")

# Replace vector 3 by the mean betweem one week ago and one week ahead

for (i in 4:10){
  for(v in vector3){
    HHPC[v,i] <- mean(HHPC[v+10080,i], HHPC[v-10080,i])
  }
}

paste("We finaly have", sum(is.na(HHPC$Sub_metering_1)), "missing values.")

#### OUTLIERS ####

# See outlier (august 2008)
ggplot(HHPC, aes(HHPC$DateTime, HHPC$Global_active_power)) +
  geom_line() +
  geom_vline(xintercept = c(as.POSIXct("2008-08-05 01:00:00"), as.POSIXct("2008-08-31 20:59:00")),
             colour="red",
             size = 0.5) +
  theme_classic() +
  labs(title = "Outlier in the time serie") 

# Focus on August 2008  
august_2008 <- HHPC %>% filter(year == 2008 & c(month == 08 | month == 07 | month == 09))
ggplot(august_2008, aes(august_2008$DateTime, august_2008$Global_active_power)) +
  geom_line() +
  geom_vline(xintercept = c(as.POSIXct("2008-08-05 01:00:00"), as.POSIXct("2008-08-31 20:59:00")),
             colour="red",
             size = 1) +
  theme_classic() +
  labs(title = "August 2008 outlier in the time serie") 

# Subset index of outliers
Hollidays <- which(HHPC$Date >= ymd_hms("2008-08-05 01:00:00") & HHPC$Date <= ymd_hms("2008-08-31 20:59:00"))

# Store August 2007 & 2009 
august_07_08 <- HHPC %>% filter(c(year == 2007 | year == 2009) & month == 8 & c(day > 5 & day < 31))

# Replace August 2008 by the mean of the above
HHPC$Sub_metering_1[Hollidays] <- mean(august_07_08$Sub_metering_1)
HHPC$Sub_metering_2[Hollidays] <- mean(august_07_08$Sub_metering_2)
HHPC$Sub_metering_3[Hollidays] <- mean(august_07_08$Sub_metering_3)
HHPC$Global_active_power[Hollidays] <- mean(august_07_08$Global_active_power)

#### DAYLIGHT SAVING #####
Summer_2007 <- which(HHPC$DateTime >= ymd_hms("2007-03-25 01:00:00") & HHPC$DateTime <= ymd_hms("2007-10-28 1:59:00"))
Summer_2008 <- which(HHPC$DateTime >= ymd_hms("2008-03-30 01:00:00") & HHPC$DateTime <= ymd_hms("2008-10-26 1:59:00"))
Summer_2009 <- which(HHPC$DateTime >= ymd_hms("2009-03-29 01:00:00") & HHPC$DateTime <= ymd_hms("2009-10-25 1:59:00"))
Summer_2010 <- which(HHPC$DateTime >= ymd_hms("2010-03-28 01:00:00") & HHPC$DateTime <= ymd_hms("2010-10-31 1:59:00"))

onehour <- hours(1)

HHPC$DateTime[Summer_2007] <- HHPC$DateTime[Summer_2007] + onehour
HHPC$DateTime[Summer_2008] <- HHPC$DateTime[Summer_2008] + onehour
HHPC$DateTime[Summer_2009] <- HHPC$DateTime[Summer_2009] + onehour
HHPC$DateTime[Summer_2010] <- HHPC$DateTime[Summer_2010] + onehour

#### SCALE THE VALUES ####

# Scale conversion in whats
HHPC$kitchen_kwh <- HHPC$Sub_metering_1/1000
HHPC$laundry_kwh <- HHPC$Sub_metering_2/1000
HHPC$waterheat_aircond_kwh <- HHPC$Sub_metering_3/1000
HHPC$Global_active_power_kwh <- HHPC$Global_active_power/60

# Remove columns and add remain
HHPC$Other_kwh <- HHPC$Global_active_power_kwh - HHPC$kitchen_kwh - HHPC$laundry_kwh - HHPC$waterheat_aircond_kwh
HHPC <- HHPC[,-which(names(HHPC) %in% c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))]  # delete old columns (sub 1, 2, 3)


#### CLEAN DATA FRAME ####

# New time variables
HHPC$Year <- floor_date(HHPC$DateTime, unit = "year")
HHPC$MonthYear <- floor_date(HHPC$DateTime, unit = "month")
HHPC$WeekYear <- floor_date(HHPC$DateTime, "week")

# Remove 2006 (incomplet year)
HHPC <- HHPC %>% filter(HHPC$year != 2006)

#### GRANULARITY KWH ####

granularity <- list()
group <- as.list(c("Year","MonthYear","WeekYear","Date"))

for(i in group) {
  granularity[[i]] <- HHPC %>% group_by_at(i) %>% 
    dplyr::summarise(Global_reactive_power = sum(Global_reactive_power),
              Global_active_power_kwh = sum(Global_active_power_kwh),
              kitchen_kwh = sum(kitchen_kwh), 
              laundry_kwh = sum(laundry_kwh),
              waterheat_aircond_kwh = sum(waterheat_aircond_kwh),
              Other_kwh = sum(Other_kwh),
              Voltage = mean(Voltage),
              Global_intensity = mean(Global_intensity))
}

nrow(granularity$Year) # 4
nrow(granularity$MonthYear) # 47
nrow(granularity$WeekYear) # 204
nrow(granularity$Date)# 1426


granularity$MonthYear$Month <- lubridate::month(granularity$MonthYear$MonthYear, label =  FALSE, abbr = FALSE, locale = "English")
granularity$Date$week <- lubridate::week(granularity$Date$Date)
granularity$MonthYear <- granularity$MonthYear %>% mutate(id = seq.int(nrow(granularity$MonthYear)))

#### GRANULARITY PRICE ####

# Creat new dataframe
HHPC_PRICE <- HHPC

# Add price feature
HHPC_PRICE <- HHPC_PRICE %>% dplyr::mutate(
  Price = dplyr::if_else(hour >= 7 & hour <= 21, 0.158, 
                         if_else(hour == 6 & minute >= 30 | 
                                   hour == 22 & minute <= 30, 0.158, 0.123)))

# Convert watt in euros
HHPC_PRICE$Global_active_power <- HHPC_PRICE$Global_active_power * HHPC_PRICE$Price
HHPC_PRICE$kitchen_kwh <- HHPC_PRICE$kitchen_kwh * HHPC_PRICE$Price
HHPC_PRICE$laundry_kwh <- HHPC_PRICE$laundry_kwh * HHPC_PRICE$Price
HHPC_PRICE$waterheat_aircond_kwh <- HHPC_PRICE$waterheat_aircond_kwh * HHPC_PRICE$Price
HHPC_PRICE$Other_kwh <- HHPC_PRICE$Other_kwh * HHPC_PRICE$Price

# Get the granularity
granularity_P <- list()
for(i in group) {
  granularity_P[[i]] <- HHPC_PRICE %>% group_by_at(i) %>% 
    summarise(Global_reactive_power = sum(Global_reactive_power),
              Global_active_power_kwh = sum(Global_active_power_kwh),
              kitchen_kwh = sum(kitchen_kwh), 
              laundry_kwh = sum(laundry_kwh),
              waterheat_aircond_kwh = sum(waterheat_aircond_kwh),
              Other_kwh = sum(Other_kwh),
              Voltage = mean(Voltage),
              Global_intensity = mean(Global_intensity))
}

nrow(granularity_P$Year) # 4
nrow(granularity_P$MonthYear) # 47
nrow(granularity_P$WeekYear) # 204
nrow(granularity_P$Date)# 1426


granularity_P$MonthYear$Month <- lubridate::month(granularity_P$MonthYear$MonthYear, label =  FALSE, abbr = FALSE, locale = "English")
granularity_P$Date$week <- lubridate::week(granularity_P$Date$Date)
granularity_P$MonthYear <- granularity_P$MonthYear %>% mutate(id = seq.int(nrow(granularity_P$MonthYear)))

# Clean environment

end_time <- Sys.time()

paste("The script run in", as.numeric(round(difftime(end_time,start_time,units="mins"), 2)), "minutes")

rm(list = setdiff(ls(), c("HHPC", "granularity", "granularity_P")))
