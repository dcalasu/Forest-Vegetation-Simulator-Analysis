
install.packages(c("DBI","RSQLite","dplyr"))
library(DBI)
library(RSQLite)
library(dplyr)
library(geosphere)
library(ggplot2)

#Collecting the data from the server
db_file <- "/Users/danic/Downloads/US-MMSF/SQLite_FIADB_IN.db"
portaldb <- dbConnect(RSQLite::SQLite(), db_file)
#connect to my datbase,your working directory and database should be the same

dbListTables(portaldb)
dbListFields(portaldb,"FVS_STANDINIT_PLOT")# open the table I want to work with 
dbListFields(portaldb,"FVS_TREEINIT_COND")
StandInit <- tbl(portaldb,"FVS_STANDINIT_PLOT")# connects to the table I want to work with 
TreeInit <-tbl(portaldb,"FVS_TREEINIT_COND")
StandInit_df <- collect(StandInit)#bring the data from the server to R, now T can work with is as a data frame
TreeInit_df<- collect(TreeInit)#bring the data from the server to R, now T can work with is as a data frame

####select and extract stands by characteristics
###By age
StandInit_df_age <- subset(StandInit_df,StandInit_df$AGE>=60)
indiana_byStandID <- subset(StandInit_df,StandInit_df$STAND_ID== "0018198603000105500055")
StandInit_df_age2 <- subset(StandInit_df,StandInit_df$AGE<100)
#indiana_ageplot2 <- subset(indiana_ageplot,indiana_ageplot$AGE>100)

###By Inventory year
sort(unique(StandInit_df$INV_YEAR))
StandInit_df_date <- subset(StandInit_df,StandInit_df$INV_YEAR == "1998")
sort(unique(StandInit_df_date$LATITUDE))### to know unique coordinates 
###By closest national forest ## check for each specific variant/region
sort(unique(StandInit_df_date$LOCATION))
#StandInit_df_dis <- subset(StandInit_df_date,StandInit_df_date$LOCATION == "922")## if one site close 
StandInit_df_dis <- subset(StandInit_df_date, LOCATION == "912"| LOCATION == "80215" | LOCATION == "908"| LOCATION == "80213"|LOCATION == "80211")
                          # | LOCATION == "81109" | LOCATION == "81110" | LOCATION == "81111")
StandInit_df_coor <-StandInit_df_dis[,11:12]
flux_lat <- 39.3232
flux_long <- -86.4131
### one approach to distance
#distance <- distHaversine(c(StandInit_df_coor$LONGITUDE[1], StandInit_df_coor$LATITUDE[1]), c(flux_long, flux_lat))/1000
#distance <- sqrt((StandInit_df_coor$LATITUDE - flux_lat )^2 + (StandInit_df_coor$LONGITUDE- flux_long)^2)
# Check if any values in df1$STAND_ID exist in df2$STAND_ID
#shared_values <- indiana_dfplot$STAND_ID %in% indiana_age_date_1$STAND_ID
###second approach to distance
calculate_distance <- function(row) {
  distance <- distHaversine(c(row["LONGITUDE"], row["LATITUDE"]), c(flux_long, flux_lat))/1000
  return(distance)
}

# Calculate distances for all rows
distances <- apply(StandInit_df_coor, 1, calculate_distance)
select_index <- (which(distances < 20))# & distances < 40))
select_stand <- StandInit_df_dis[select_index,]

sort(select_stand$STAND_ID)
write.csv(select_stand, file = "StandsID_US-MMSF_40km.csv") ### change name and directory based on input file



####If I need to extract info by trees 
### Check if any shared values exist
if (any(shared_values)) {
  print("There are shared values in the 'STAND_ID' column.")
  shared_rows <- indiana_dfplot[which(shared_values), ]
  
} else {
  print("There are no shared values in the 'STAND_ID' column.")
}

#### for disconnect from database
dbDisconnect(portaldb)



shared_columns <- indiana_dfplot$STAND_ID %>%
  filter(shared_values)

####ADD 1 At the end of each 
#new_values <- paste0(indiana_age_date$STAND_ID, "1")
indiana_age_date_1 <- indiana_age_date %>%
  mutate(STAND_ID = paste0(STAND_ID, "1"))



# Display the new values
print(new_values)


##################################################################
#######Create a new dataframe from the shared values in column ”Stand_ID” in “indiana_age_date” and “indiana_dfplot” by appending
#The issue here is the columns vary by type in each data frame and are uncompatible when trying to join, so I fixed it

#append combines two datasets and fills in non-shared areas with blanks or NAs
#this works but changes everything to character; solution found here: https://stackoverflow.com/questions/68187461/cant-combine-x-character-and-x-double
sapply(indiana_age_date, class)
sapply(indiana_dfplot, class)

#I want to see which col types differ between indiana_age_date and indiana_dfplot 
install.packages("janitor")
library(janitor)
compare_df_cols(indiana_age_date, indiana_dfplot, return = "mismatch") #this compares column types for each dataset and returns differences

typeof(indiana_age_date$PV_REF_CODE) #checking columns individually
typeof(indiana_dfplot$PV_REF_CODE)  #checking columns individually

indiana_age_date$PV_REF_CODE <- as.character(indiana_age_date$PV_REF_CODE) #turning PV_REF_CODE into character type

compare_df_cols(indiana_age_date, indiana_dfplot, return = "mismatch") #this compares column types for each dataset and returns differences
#nothing comes back now - they are now compatible

test <- indiana_age_date  #new variable for indiana_age_date in case I break it
test1 <- indiana_dfplot #new variable for indiana_dfplot in case I break it

#Appending happens here:
test2 <- bind_rows(test1, test) #appends the dataframes
View(test2) #Make sure everything looks okay
sapply(test2, class) #Make sure everything looks okay

#verifying it works 
mean(test2$PLOT_ID, na.rm=T)
typeof(test2$PLOT_ID)
#It worked!
##################################################################





#"FVS_TREEINIT_COND" #Loop to extract values from plot dataframe
new<- data.frame()

for (i in (1:length(indiana_dfplot[2,])))
  for (j in (1:length(indiana_age_date[2,]))){
    if(indiana_dfplot[i,2]==indiana_age_date[j,2]) print(indiana_dfplot[]);
  }
#append(new,indiana_dfplot[j,2])}
if(indiana_dfplot[i,2]==indiana_age_date[j,2]){
  new <- indiana_dfplot[i,2, append="TRUE"] 
}


#download data as an excel file
install.packages("writexl")
library("writexl")
write_xlsx(indiana_age_date, "out.xlsx")
write_xlsx(shared_rows, "out_plots.xlsx")

write_xlsx(indiana_age_date,"C:/Users/danic/Downloads\\stand_age.xlsx"

position <- which(indiana_dfplot$STAND_ID == "00252018070601001000661")
           
           # Print the position
print(position)
