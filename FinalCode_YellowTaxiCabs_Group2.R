#Clearing workspace
rm(list=ls()) 

#setting work directory
getwd()
setwd('/Users/jitkalra8/Desktop/Columbia/semester2/Framework2/Project')

#reading data
DataWithTimeStamp_YellowTaxi = read.csv("DataWithTimeStamp_YellowTaxi.csv")
#processing some outliars identified
data_test = subset(DataWithTimeStamp_YellowTaxi, trip_distance >0)
data_final = subset(data_test, trip_distance < 173.500)
#writing file for submission
write.csv(data_final, 'data_final.csv',row.names = F)

#loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(radiant)
library(data.table)
library(gridExtra)
library(dendextend)
library(cluster)
library(caret)
library(lubridate)
library(rgdal)
library(RColorBrewer)
library(h2o)
library(reshape2)

#check if any NA
table(is.na(data_final))

#randomly select 10000 data from cleaned data set for culstering
#only select a few variables since too many similiar varibale will cause error in calculating the distance
a = sample(dim(data_final)[1], size=10000)
b <- data_final[a,]
clusterdata <- b %>% select(payment_type,passenger_count,VendorID,trip_distance,fare_amount,tip_amount, RatecodeID)

###exploration of data
summary(data_final)
dim(data_final)
str(data_final)

#plot of passegner count distribution using the whole data set
plot1 <- data_final %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  ggtitle("passenger count") +
  scale_x_discrete(limits=c("1","2","3","4","5","6"))
plot1

#plot of vendor ID  ID=2 have way more people to take
plot2 <- data_final %>%
  ggplot(aes(VendorID, fill = VendorID)) +
  geom_bar() +
  ggtitle("Vendor ID count")+
  scale_x_discrete(limits=c("1","2"))
plot2  

#Trip count by Days grouped by vendorID
plot3 <- data_final %>%
  mutate(wday = wday(tpep_pickup_datetime)) %>%
  group_by(wday, VendorID) %>%
  count() %>%
  ggplot(aes(wday, n, colour = VendorID)) +
  geom_point(size = 5) +
  labs(x = "Day of the week", y = "Total number of pickups")+
  ggtitle("Week day trip count")
plot3

#relationship between trip distance and tip amount we can suggest that the tip option started at 20% which will lead people to choose not to tip
#plot 5 using histrcial gram
dx <- data_final[data_final$payment_type==1,c('fare_amount','tip_amount')]
dx$perc <- dx$tip_amount / dx$fare_amount
hist(dx$perc[dx$perc < 1])


#plot 6 grouped by pick hour
plot6 <- data_final %>%
  mutate(pickup_hour = as.POSIXlt(tpep_pickup_datetime)$hour) %>%
  group_by(pickup_hour) %>%
  count() %>%
  ggplot(aes(pickup_hour, n, colour = pickup_hour)) +
  geom_bar(stat='identity') +
  labs(x = "Pickup Hour of the day", y = "Total number of pickups")+
  ggtitle("Week day trip count")
plot6

#plot7 dropoff hour
plot7 <- data_final %>%
  mutate(dropoff_hour = as.POSIXlt(tpep_dropoff_datetime)$hour) %>%
  group_by(dropoff_hour) %>%
  count() %>%
  ggplot(aes(dropoff_hour, n, colour = dropoff_hour)) +
  geom_bar(stat='identity') +
  labs(x = "Dropoff Hour of the day", y = "Total number of Dropoffs")+
  ggtitle("Week day trip count")
plot7

#plot 9 trip distance
plot9= data_final %>% 
  ggplot(aes(x=trip_distance)) + 
  geom_histogram(bins=50000, fill="blue")+
  theme_bw()+theme(axis.title = element_text(size=12),axis.text = element_text(size=8))+
  ylab("numbers of trips")+
  coord_cartesian(x=c(0,25))
ggsave("myplot.png", plot = plot9)


#calcuat dist 
data_cluster = scale(clusterdata)
d = dist <- dist(data_cluster,method = 'euclidean')
clust = hclust(dist,method = "ward.D2")

#check goodness of fit
cor(cophenetic(clust),d)
plot(cut(as.dendrogram(clust),h=5)$upper)
#cluster plot 
plot(clust)
rect.hclust(tree=clust,k = 6)

#select clustering  clustering = 6 is the best bet
h_segments = cutree(tree = clust,k=6) 
table(h_segments)


####### Cost Analysis###
#Total revenue from current data 
Total_revenue = sum(data_final$fare_amount,data_final$extra,data_final$mta_tax,data_final$tip_amount,data_final$tolls_amount,
                    data_final$improvement_surcharge, data_final$congestion_surcharge)
Total_revenue
# Revenue bifurcation
Revenue_bifucration <- data.frame( Type_of_Charge = c("fare_amount","extra","mta_tax","tip_amount","tolls_amount",
                                                      "improvement_surcharge","congestion_surcharge"),
                                   Total_Amount = c(sum(x = data_final$fare_amount, na.rm = TRUE),
                                                    sum(x = data_final$extra,na.rm = TRUE),
                                                    sum(x = data_final$mta_tax, na.rm = TRUE),
                                                    sum(x= data_final$tip_amount,na.rm = TRUE),
                                                    sum(x = data_final$tolls_amount,na.rm = TRUE),
                                                    sum(x = data_final$improvement_surcharge,na.rm = TRUE),
                                                    sum(x = data_final$congestion_surcharge,na.rm = TRUE)))
                                   
                                 
Revenue_bifucration
#Sorting for graphical representation
Revenue_bifucration$Type_of_Charge <- factor(Revenue_bifucration$Type_of_Charge, 
                                             levels = Revenue_bifucration$Type_of_Charge
                                             [order(-Revenue_bifucration$Total_Amount)])

#Graph1 for revenue bifurcation
Graph1 <- ggplot(Revenue_bifucration, aes(x = Type_of_Charge, y = Total_Amount, fill=Total_Amount)) +
  theme_bw() + geom_bar(stat = "identity")
Graph1

#revenue location wise 
data_final %>% group_by(PULocationID) %>% summarize(Location_Revenue = sum(x = fare_amount, na.rm = TRUE))

#Filtering routes to check price and trip distance on an average & plotting graph
Avgs_By_RateCodeId <- data_final %>% group_by(RatecodeID) %>% summarize(average_price = mean(x = fare_amount, na.rm = TRUE),
                                                                        average_distance = mean(x = trip_distance,na.rm = TRUE)) 
Avgs_By_RateCodeId$Price_To_Distance_Ratio = Avgs_By_RateCodeId$average_price/Avgs_By_RateCodeId$average_distance

Avgs_By_RateCodeId_melted <- melt(Avgs_By_RateCodeId, id.vars="RatecodeID")

Graph2 <- ggplot(Avgs_By_RateCodeId_melted, aes(x = RatecodeID, y = value, colour = variable, group = variable)) +
  geom_point() + geom_line()
Graph2

#Ratecode ID description below just for better understanding
# #1= Standard rate 
# 2=JFK 
# 3=Newark
# 4=Nassau or Westchester 
# 5=Negotiated fare 
# 6=Group ride

#the total revenue
Count_of_taxis= 13857
Total_revenue_monthly = (Total_revenue)
Total_revenue_monthly
Total_revenue_monthly_per_taxi=Total_revenue_monthly/Count_of_taxis
Total_revenue_monthly_per_taxi

#the total tolls
Total_TollsTaxes_Monthly =  (sum(data_final$tolls_amount,data_final$mta_tax,
                                 data_final$improvement_surcharge, data_final$congestion_surcharge))
Total_TollsTaxes_Monthly
Total_TollsTaxes_Monthly_per_taxi = Total_TollsTaxes_Monthly/Count_of_taxis
Total_TollsTaxes_Monthly_per_taxi

#Tip amount
Total_Tips_Monthly = sum(data_final$tip_amount)
Total_Tips_Monthly
Average_tip = sum(data_final$tip_amount)/Count_of_taxis
Average_tip

# Count of taxis in yellow cabs :13,857 
#Source: https://ny.curbed.com/2017/1/17/14296892/yellow-taxi-nyc-uber-lyft-via-numbers
Count_of_taxis= 13857 
# Monthly revenue of single taxi
Revenue_Monthly = sum(data_final$fare_amount,data_final$extra)
Revenue_Monthly
MonthlyRevenue_singleCab = Revenue_Monthly/Count_of_taxis
MonthlyRevenue_singleCab

# operational cost
#average for the price of gas ($2.5 per gallon)
#Source: https://archive.nytimes.com/www.nytimes.com/packages/html/business/20060510_LEONHARDT/cost_per_mile.html?scp=115&sq=gas&st=Search
Fuel_cost = 2.5


# monthly miles for taxi
Total_miles = sum(data_final$trip_distance)
Total_miles
#Occupancy factor is considered with the fact that everytime a taxi doesnot have rider & they may have to commute more to pick up rides
# so no amount is paid for that trip
occupancy_factor = 1.5
Effective_total_miles= Total_miles*occupancy_factor
MonthlyMiles_perTaxi = Effective_total_miles/Count_of_taxis
MonthlyMiles_perTaxi

#Milegae (miles per gallon = 15) 
#Source: https://www.greencarreports.com/news/1047703_cities-want-high-mileage-hybrid-taxis-judge-says-its-illegal#:~:text=The%20city's%20Taxi%20%26%20Limousine%20Commission,gallon%20on%20the%20city%20cycle.
Mileage = 15

Fuel_cost_monthly = (MonthlyMiles_perTaxi*Fuel_cost)/Mileage
Fuel_cost_monthly  #dollars per month##

# Insurance cost
# Source: https://lite987.com/cost-of-owning-a-car-in-new-york-state/#:~:text=According%20to%20figures%20published%20by,gas%2C%20and%20%24761%20for%20maintenance.
Insurance_Cost = 1050/12
Insurance_Cost

#maintenance cost
#Source: http://www.tlc-mag.com/archive/pre_2013_site/tlpa_jan10.html
# above article states 3100 annualy for 2 cabs in 2016, so considered 2000 as an approx estimate yearly

maintenance_cost = 2000/12
maintenance_cost
#License cost

#Source: https://www.cityandstateny.com/articles/personality/interviews-profiles/what-are-taxi-medallions-worth.html
#License_cost = 110500/(12*30)
License_cost = 3000 #- 4000 / month 
# Depreciation cost

#Source: https://gothamist.com/news/thinking-about-getting-car-nyc-think-again
Avg_price_taxi = 28000 #(taken average of new & used car price)
Years_of_usage = 3
Depreciation_cost_per_month = (Avg_price_taxi/(Years_of_usage*12))
Depreciation_cost_per_month
# Operational Cost per month ( Not considered licence cost as of now, need to check with professor how to accomodate) 

Operational_cost = Insurance_Cost+maintenance_cost+Fuel_cost_monthly+License_cost+Depreciation_cost_per_month
Operational_cost

#Decductible
#Source: https://www.irs.gov/newsroom/irs-issues-standard-mileage-rates-for-2020
Deductible = MonthlyMiles_perTaxi*0.575
Deductible_Savings= Deductible*0.25
#Income of one taxi per month excluding tips 
Income_per_taxi = (MonthlyRevenue_singleCab) - Operational_cost+Deductible_Savings
Income_per_taxi
# Taxi Driver Salary##
#source: https://www.salary.com/research/salary/benchmark/taxi-driver-salary/new-york-ny
# Need to discuss if we can consider this approach
Low_salary = 36132
High_salary = 53090
Medium_Salary = (Low_salary+ High_salary)/2
Medium_Salary 

#Spatial Analysis
# Loading shape file
#install.packages("rgdal")
library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/taxi_zones") , 
  layer="taxi_zones",
  verbose=FALSE)

# Plotting to just verify prooer loading of shape file
par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=2 )

#Data in shape file
#Spacial_data
head(my_spdf@data)

#Grouping Pick up lo cation ids 
pickup_by_loc <- data_final %>%
  group_by(PULocationID) %>%
  count()
pickup_by_loc

Spatial_data <- merge( my_spdf, pickup_by_loc, by.x = "LocationID", by.y = "PULocationID", duplicateGeoms = TRUE)
Spatial_data

Spatial_data@data
plot (Spatial_data, border="#aaaaaa")
# selecting  colors from the palette
pal <- brewer.pal(7, "OrRd") 
class(pal)

my.palette <- brewer.pal(n = 7, name = "OrRd")

spplot(Spatial_data, "n", col.regions = my.palette, cuts = 6, main = "NYC Taxi Stats", 
       sub = "Pick Up Location Density", col = "transparent" ) 



