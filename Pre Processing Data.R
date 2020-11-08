datafix<-read.csv("D:/DSC/energydata_complete.csv",header=T,sep=";")
head(datafix)

str(datafix)
View(datafix)

datafix$date.Appliances.lights.T1.RH_1.T2.RH_2.T3.RH_3.T4.RH_4.T5.RH_5.T6.RH_6.T7.RH_7.T8.RH_8.T9.RH_9.T_out.Press_mm_hg.RH_out.Windspeed.Visibility.Tdewpoint.rv1.rv2 <- as.numeric(datafix$date.Appliances.lights.T1.RH_1.T2.RH_2.T3.RH_3.T4.RH_4.T5.RH_5.T6.RH_6.T7.RH_7.T8.RH_8.T9.RH_9.T_out.Press_mm_hg.RH_out.Windspeed.Visibility.Tdewpoint.rv1.rv2)

str(datafix)

datafix <- na.omit(datafix)

n <-dim(datafix)[1]
n

p <-dim(datafix)[2]
p
xbar <-matrix(ncol=p, nrow =1)
varx<-matrix(ncol=p,nrow =1)
z <-matrix(ncol=p,nrow=n)
sum(is.na(datafix))

datafix$date  <- strptime (as.character ( datafix $ date ), format = " % Y-% m-% d% H:% M:% S " )
datafix $ date  <- as.POSIXct ( datafix $ date , tz  =  " UTC " )
class ( datafix $ date )
str ( datafix )

names( datafix )


weekend_weekday  <-  function ( x ) {
  val  <- weekdays ( x )
  if ( val  ==  " Saturday "  |  val  ==  " Sunday " ) {
    val2  =  " Weekend "
  }
  else {
    val2 =  "Weekdays "
  }
  return ( val2 )
}
datafix $ WeekStatus  <- unlist (lapply ( datafix $ date , weekend_weekday ))
datafix $ Day_of_week  <- weekdays ( datafix $ date )
unique ( datafix $ WeekStatus )
unique ( datafix $ Day_of_week )
class ( datafix $ Day_of_week )
datafix $ Day_of_week  <- as.factor ( datafix $ Day_of_week )
datafix $ WeekStatus  <- as.factor ( datafix $ WeekStatus )
str ( datafix )
dim ( datafix )
names ( datafix )

# HEAT MAP visualization
# Visualisasi penggunaan Energi per minggu dengan peta panas
library ( lubridate )
datafix $ my  <- floor_date ( datafix $ date , " month " )
datafix $ mhr  <- floor_date ( datafix $ date , " hour " )

library ( plyr )
datafix_Total_per_hour  <-   ddply ( datafix , " mhr " ,summarise ,
                                         Appliances = sum ( Appliances ))
datafix_Total_per_hour
datafix_Total_per_hour $ Day_week  <- wday ( datafix_Total_per_hour $ mhr , label = TRUE )
head ( datafix_Total_per_hour )
class ( datafix_Total_per_hour )
summary ( datafix_Total_per_hour )
datafix_Total_per_hour_na_removed  <- na.omit ( datafix_Total_per_hour )

dim (datafix_Total_per_hour )
names ( datafix_Total_per_hour )
dim ( datafix_Total_per_hour_na_removed )
names ( datafix_Total_per_hour_na_removed )
summary (datafix_Total_per_hour_na_removed )



