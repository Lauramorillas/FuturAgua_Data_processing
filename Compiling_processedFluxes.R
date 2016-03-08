#####
# Script design Notes:

#This script is written to compiled all processed fluxes from EddyPro in a unique dataset for each site 
# It can be used for both sites

#The output files to be compiled are  "eddypro_Site_Eddypro_projectname_full_output_date_exp.csv"

#The script saves the data compiled by the name "Site_fluxes_compiled"in the folder:/Documents/UBC/futuragua_research/Data management/EDDYPRO_OUTPUTS/

######
#Coding notes:

#read.csv(paste(path,"/",raw.files[i],sep=""),skip=3,header=FALSE) #skipt=3 means skip the first 3 rows of the file 
#raw.data[ order(raw.data$Timestamp, decreasing = FALSE ),]   To order the dataset by Timestamp columns
#library(openair) openair package to deal with missing rows
#sprintf("%02s", cont.DS$min_local[i]): To generate 2 digits minutes, so 0 minutes gets converted to 00 : 



#CODE:
#############################################################################################
####
# 1. Reading all the files
###
# clear memory
rm(list=ls())

        #############################################################################################
        #############################################################################################
        #Choose a Site:
        SITE<-"C"   #Here you will need to specify the site V for el Viejo, and C for La Costenya
        #############################################################################################
        #############################################################################################


if (SITE=="V"){        
  #For el Viejo          
  # set path
  path <- "~/Documents/UBC/Futuragua_research/Data management/EDDYPRO_OUTPUTS/Viejo_Express_DynaMeta/FULL_OUTPUT_FILES"
  random_dataset<-read.csv( "~/Documents/UBC/futuragua_research/Data management/EDDYPRO_OUTPUTS/Viejo_Express_DynaMeta/FULL_OUTPUT_FILES/eddypro_Viejo_Express_DynaMet_9_full_output_2015-10-19T172526_exp.csv",skip=1,sep=",",header=TRUE,dec=".") #from I Mac
  
}else{
  
  #For Costena         
  # set path
  path <- "~/Documents/UBC/Futuragua_research/Data management/EDDYPRO_OUTPUTS/Costena_Express_DynaMeta/FULL_OUTPUT_FILES"
  random_dataset<-read.csv( "~/Documents/UBC/futuragua_research/Data management/EDDYPRO_OUTPUTS/Costena_Express_DynaMeta/FULL_OUTPUT_FILES/eddypro_Costena_Expess_DynaMet_10_full_output_2015-11-03T102820_exp.csv",skip=1,sep=",",header=TRUE,dec=".") #from I Mac
  
}


raw.files <- as.list(dir(path))
raw.data <- data.frame()

for(i in 1:length(raw.files)) {
  temp <- read.csv(paste(path,"/",raw.files[i],sep=""),skip=3,header=FALSE) #skipt=3 means skip the first 3 rows of the file
  raw.data <- rbind(raw.data,temp)
}


names(raw.data) <- names(random_dataset)




####
# 2. Creating a Timestamp with date and time variable to order the data by date-time
###
raw.data$Timestamp<-as.factor(paste(raw.data$date, raw.data$time, sep = " "))
raw.data$Timestamp<-as.POSIXct(strptime(raw.data$Timestamp,format="%Y-%m-%d %H:%M",tz="UTC")) 

data.ordered<-raw.data[ order(raw.data$Timestamp, decreasing = FALSE ),]



####
#3. GAPFILLING ROWS BY GENERATING A NEW FILE  (Adapted from my "Gapfilling incomplete Time series" script)
#####

ts<-data.ordered

#to estimate necessary parameters to generate the new empty dataframe with complete rows 
begining<-as.numeric(ts$Timestamp[1])                           #to find the number that is represented by the first data of our time series
as.POSIXct(begining,origin="1970-01-01 00:00:00",tz="GMT") #to confirm that the begining is the right one
ts$Timestamp[1]

Ystart<-as.integer(as.character(ts[1,ncol(ts)], "%Y"))
Yend<-as.integer(as.character(ts[nrow(ts),ncol(ts)], "%Y"))
Dstart<-as.POSIXlt(ts[1,ncol(ts)])$yday+1
Dend<-as.POSIXlt(ts[nrow(ts),ncol(ts)])$yday+1


Ndays<-365-Dstart+Dend+(365*(Yend-Ystart-1)) #to find out the number of days included in our time serie 
                                            #THIS WILL WORK WHILE THE INTERMEDIATE YEARS BETWEEN THE LAST 
                                            #AND THE INITIAL ONE ARE REGULAR YEARS (NOT LEAP YEARS)

#To generate a serie of all minutes in a day:
Tsteps<-begining+seq(from=0,to=((Ndays+1)*(60*60*24)),by=(30*60)) # half an hours data from the begining date to Ndays +1 (that way I assure all measured data are included in the new file)
DATE<-as.POSIXct(Tsteps,origin="1970-01-01 00:00:00",tz="GMT")

#Confirming length of the new Time string 
#(THE NEW FILE SHOULD END SAME TIME THE TIME SERIES START AND ONE DAY AFTER THE MEASUREMENTS TIME SERIE)
DATE[1]
DATE[length(DATE)]
ts$Timestamp[1]
ts$Timestamp[length(ts$Timestamp)]

#GENERATING A NEW DATA FRAME WITH CONTINUOUS TIME STEPS and data from THE ORIGINAL ONE
cont.DS<-as.data.frame(DATE)
cont.DS[,c("DATE",names(ts)[1:(length(names(ts)))-1])]<-NA   #we will just copy the data from the variable "Batt_volt_Min" in ts (variable 3)
cont.DS$DATE<-DATE

#FILLING THE NEW DATAFRAME WITH DATA FROM THE ORIGINAL DATA FRAME 
for(i in 2:ncol(cont.DS)){  
  cont.DS[,i]<-ts[pmatch(cont.DS$DATE,ts$Timestamp),i-1]  #pmatch look for the observation rows when time columns of both (old and new) dataframes match
} 



####
# 4. Adding local time
###

date_loca<-format(cont.DS$DATE,tz="America/Costa_Rica",usetz=T)  #as.POSIXlt(format(cont.DS$DATE[i],tz="America/Costa_Rica",usetz=T),tz="America/Costa_Rica")
date_local<-as.POSIXlt(date_loca,tz="America/Costa_Rica")

for (i in 1:nrow(cont.DS)){
cont.DS$Year_local[i]<-as.integer(as.character(date_local[i],"%Y"))
cont.DS$DOY_local[i]<-as.POSIXlt(date_local[i])$yday+1
cont.DS$month_local[i]<-as.numeric(format(date_local[i],"%m"))
cont.DS$hour_local[i]<-as.integer(as.character(date_local[i],"%H"))
cont.DS$min_local[i]<-sprintf("%02s",as.integer(as.character(date_local[i],"%M")))  #sprintf function converts 0 in 00 to be pasted with hour to generate local time
cont.DS$time_local[i]<-paste(cont.DS$hour_local[i],cont.DS$min_local[i],sep=":")

}


####
# 5. Replacing -9999 by NA
###
cont.DS[cont.DS == -9999] <- NA


####
# 5. Saving the data
###
if (SITE=="V"){        
  #For el Viejo
  write.csv(cont.DS,paste('~/Documents/UBC/futuragua_research/Data management/EDDYPRO_OUTPUTS/Viejo_fluxes_compiled','.csv',sep=''),row.names=FALSE)   
  
}else{  
  
  #For Costena
  write.csv(cont.DS,paste('~/Documents/UBC/futuragua_research/Data management/EDDYPRO_OUTPUTS/Costena_fluxes_compiled','.csv',sep=''),row.names=FALSE)   
  
}

