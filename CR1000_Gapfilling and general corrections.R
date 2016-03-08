
##################################################
###################################################
#
#     GENERATING A 30 MIN CONTINUOUS DATASET
#      WITH SOME SYSTEMATIC ERRORS FIXED

#(Inserting rows of NA when measurements were lost)
# Sistematic errors fixed:
#    1. Rn and PAR calibration coefficients are fixed before August 2015 (sensor specific calibration coefficients applied)
#    2. PRI SIGN FIXED BEFORE August 2015 (PRI attending to equation 3 from SRS manual, Gamon et al 1990, 1992)
####################################################
###################################################
#PENDING TASKS to add:
     
        #1.Correction of SHF_Hukse applying calibration before April 2015, when CR1000 program fixed this issue
        #2.Improve calibration factor applied to TEM sensors based on calibration factor of SHF_Hukse
        



##################################
#INPUT FILE FOR THIS SCRIPT:"CR1000_site_comp_filt_forR"
#The input file should:
          #1. have NA as missing or filtered values 
          #2. have only one headers row, 
          #3. no  comments column


    #Exporting dataset:
    #############################################################################################
    #############################################################################################
    #Choose a Site:
    SITE<-"C"   #Here you will need to specify the site V for el Viejo, and C for La Costenya
    #############################################################################################
    #############################################################################################

    if (SITE=="V"){        
      #For el Viejo          
        #ts<-read.csv( 'C:/Users/lmorillas/Documents/CR1000/El Viejo/CR1000_Viejo_comp_filt_forR.csv',sep=",",header=T) #from PC
        ts<-read.csv( "~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_filt_forR.csv",sep=",",header=TRUE,dec=".") #from I Mac

    }else{
  
      #For Costena         
        #ts<-read.csv( 'C:/Users/lmorillas/Documents/CR1000/Costena/CR1000_Costena_comp_filt_for R.csv',sep=",",header=T)
        ts<-read.csv( "~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/Costena/CR1000_Costena_comp_filt_forR.csv",sep=",",header=TRUE,dec=".") #from I Mac
        }

######################################################
#GAPFILLING ROWS BY GENERATING A NEW FILE
######################################################

#Adding a new row with Timestamp in posix R format
      #ts$date<-as.POSIXct(strptime(ts$TIMESTAMP,format="%m/%d/%Y %H:%M",tz="UTC"))
ts$date<-as.POSIXct(strptime(ts$TIMESTAMP,format="%y-%m-%d %H:%M",tz="UTC")) #%y identifies the year from the last two digits i.e 14=2014, %Y needs the foru year digits if they are in the date 

#to estimate necessary parameters to generate the new empty dataframe with complete rows 
begining<-as.numeric(ts$date[1])                           #to find the number that is represented by the first data of our time series
       as.POSIXct(begining,origin="1970-01-01 00:00:00",tz="GMT") #to confirm that the begining is the right one
       ts$date[1]
       
Ystart<-ts[1,match('year',names(ts))]
Yend<-ts[nrow(ts),match('year',names(ts))]
Dstart<-ts[1,match('jday',names(ts))]
Dend<-ts[nrow(ts),match('jday',names(ts))]
lsyd<-max(ts[ts$year==Ystart,match('jday',names(ts))]) #lsyd=last start year day

Ndays<-(lsyd-Dstart+Dend)+(365*(Yend-Ystart-1)) #to find out the number of days included in our time serie (only works if years in between first and last year are regular years)


#To generate a serie of all minutes in a day:
Tsteps<-begining+seq(from=0,to=((Ndays+1)*(60*60*24)),by=(30*60)) # half an hours data from the begining date to Ndays +1 (that way I assure all measured data are included in the new file)
DATE<-as.POSIXct(Tsteps,origin="1970-01-01 00:00:00",tz="GMT")

#Confirming length of the new Time string 
#(THE NEW FILE SHOULD END SAME TIME THE TIME SERIES START AND ONE DAY AFTER THE MEASUREMENTS TIME SERIE)
      #Tstring[1]
      #Tstring[length(Tstring)]
      #ts$date[1]
      #ts$date[length(ts$date)]
      
#GENERATING A NEW DATA FRAME WITH CONTINUOUS TIME STEPS and data from THE ORIGINAL ONE
cont.DS<-as.data.frame(DATE)
cont.DS[,c("DATE",names(ts)[match( "batt_volt_Min",names(ts)):(length(names(ts))-1)])]<-NA   #we will just copy the data from the variable "Batt_volt_Min" in ts (variable 3)
cont.DS$DATE<-DATE

#FILLING THE NEW DATAFRAME WITH DATA FROM THE ORIGINAL DATA FRAME 
for(i in 2:ncol(cont.DS)){  
cont.DS[,i]<-ts[pmatch(cont.DS$DATE,ts$date),i+1]           #pmatch look for the observation rows when time columns of both (old and new) dataframes match
                          } 


#FILLING THE TIME COLUMNS IF THE DATA WERE NOT RECORDED:
for(i in 1:nrow(cont.DS)){ 
  
  cont.DS[i,4]<-ifelse(is.na(cont.DS[i,4]), as.integer(as.character(cont.DS[i,1], "%Y")),cont.DS[i,4]) #To gapfill the Year (column 4)
  cont.DS[i,5]<-ifelse(is.na(cont.DS[i,5]), as.POSIXlt(cont.DS[i,1])$yday+1 ,cont.DS[i,5])             #To gapfill the jday (column 5)
  cont.DS[i,6]<-ifelse(is.na(cont.DS[i,6]), as.integer(as.character(cont.DS[i,1],"%H")) ,cont.DS[i,6]) #To gapfill the hour (column 6)
  cont.DS[i,7]<-ifelse(is.na(cont.DS[i,7]), as.integer(as.character(cont.DS[i,1],"%M")) ,cont.DS[i,7]) #To gapfill the minutes (column 7)
} 


###############################
#APPLYING SYSTEMATIC CORRECTIONS:
################################
DATA<-cont.DS
DATA$obs<-c(1:nrow(cont.DS))

if (SITE=="V"){     #For el Viejo      
  
  #Adding imn gapfilled rain:
  rain_imn_gapfilled<-read.csv('~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_FiltGAPfil_WITH rain gapfilled.csv',sep=",",header=TRUE,dec=".") 
  rain_imn_gapfilled<-rain_imn_gapfilled[ ,match(c("DATE","imn_rain","imn_rain_distributed"),names(rain_imn_gapfilled))]
  DATA$imn_rain<-rep(NA,nrow(DATA))
  DATA$imn_rain_distributed<-rep(NA,nrow(DATA))
  
  DATA[1:nrow(rain_imn_gapfilled),match('imn_rain',names(DATA))]<- rain_imn_gapfilled$imn_rain
  DATA[1:nrow(rain_imn_gapfilled),match('imn_rain_distributed',names(DATA))]<- rain_imn_gapfilled$imn_rain_distributed
  
  #Detailing Dates of changes (in form of a vector including the year,juliandoy, hour and minutes of the change) 
  DateRad<-c(2015,216,18,0) #DateRad IS THE DATE WHEN WE UPLOAD A NEW PROGRAM WITH NEW CALIBRATION FACTORS FOR Rn  
                              #doy 218 is August 4,  
  DateRn_fix<-c(2015,304,18,0) #doy 304 is August 31 THIS WILL NEED TO BE UPDATED TO THE DATE SILJA UPLOAD THE PROGRAM WITH THE RIGHT Rn multiplier
  DatePRI<-c(2015,216,18,0)  #doy 216 is August 4
  
  Rn_Mult<-71.94    #MULTIPLIER FOR CAL COEF =13.9 WM'S/MV
  Rn_Mult_wrong<-72.46  # This is the multimiter I included in the new program we uploaded in August 6 at la Costenya but it is wrong (it should be 72.46) 
  #WE DON'T CHANGE THE par multiplier (253.3) because we don't have information about the right calibration of this sensor at this site
  
  #Assigning the dates of change to the dataset
  DATERAD<-DATA[DATA$year==DateRad[1] & DATA$jday==DateRad[2] & DATA$hour==DateRad[3] &DATA$minutes==DateRad[4] ,match("DATE",names(DATA))]
  DATERn_fix<- DATA[DATA$year==DateRn_fix[1] & DATA$jday==DateRn_fix[2] & DATA$hour==DateRn_fix[3] &DATA$minutes==DateRn_fix[4] ,match("DATE",names(DATA))]
  DATEPRI<-DATA[DATA$year==DatePRI[1] & DATA$jday==DatePRI[2] & DATA$hour==DatePRI[3] &DATA$minutes==DatePRI[4] ,match("DATE",names(DATA))]
  
  DATERAD_obs<-DATA[DATA$DATE==DATERAD,match('obs',names(DATA))]
  DATERn_fix_obs<-ifelse(DateRn_fix[1]==DATA[nrow(DATA),4] && DateRn_fix[2]>DATA[nrow(DATA),5], DATA[nrow(DATA),match("obs",names(DATA))], DATA[DATA$DATE== DATERn_fix,match('obs',names(DATA))])
  DATEPRI_obs<-DATA[DATA$DATE==DATEPRI,match('obs',names(DATA))]
  
  #Applying the corrections
  for (i in 1:DATERAD_obs) {
    
    DATA[i,match('Rn',names(DATA))] <- Rn_Mult*( cont.DS[i,match('Rn',names(cont.DS))]/100)
   
  }
  
  for (i in DATERAD_obs:DATERn_fix_obs){
    DATA[i,match('Rn',names(DATA))]<- Rn_Mult*(cont.DS[i,match('Rn',names(cont.DS))]/Rn_Mult_wrong)
  }
  
  for (i in 1:DATEPRI_obs) {
    DATA[i,match('PRI',names(DATA))]<- ifelse(is.na(cont.DS[i,match('PRI',names(cont.DS))]),NA,-cont.DS[i,match('PRI',names(cont.DS))])
  }
  

 
}else{               #For Costena 
  
  
  #Detailing Dates of changes (in form of a vector including the year,juliandoy, hour and minutes of the change) 
  DateRad<-c(2015,218,18,0) #DateRad IS THE DATE WHEN WE UPLOAD A NEW PROGRAM WITH NEW CALIBRATION FACTORS FOR Rn and PAR sensors 
                            #doy 218 is August 6,  
  DateRn_fix<-c(2015,304,18,0) #doy 304 is August 31 THIS WILL NEED TO BE UPDATED TO THE DATE SILJA UPLOAD THE PROGRAM WITH THE RIGHT Rn multiplier
  DatePRI<-c(2015,213,18,0)  #doy 218 is August 1
  
  Rn_Mult<-72.46      #MULTIPLIER FOR CAL COEF =13.8 WM'S/MV
  Rn_Mult_wrong<-74.25  # This is the multimiter I included in the new program we uploaded in August 6 at la Costenya but it is wrong (it should be 72.46) 
  PAR_Mult<-233.3
  PAR_Mult_wrong<-253.3
  PAR_Offset<-2.7
  
  #Assigning the dates of change to the dataset
  DATERAD<-DATA[DATA$year==DateRad[1] & DATA$jday==DateRad[2] & DATA$hour==DateRad[3] &DATA$minutes==DateRad[4] ,match("DATE",names(DATA))]
  DATERn_fix<- DATA[DATA$year==DateRn_fix[1] & DATA$jday==DateRn_fix[2] & DATA$hour==DateRn_fix[3] &DATA$minutes==DateRn_fix[4] ,match("DATE",names(DATA))]
  DATEPRI<-DATA[DATA$year==DatePRI[1] & DATA$jday==DatePRI[2] & DATA$hour==DatePRI[3] &DATA$minutes==DatePRI[4] ,match("DATE",names(DATA))]
  
  DATERAD_obs<-DATA[DATA$DATE==DATERAD,match('obs',names(DATA))]
  DATERn_fix_obs<-ifelse(DateRn_fix[1]==DATA[nrow(DATA),4] && DateRn_fix[2]>DATA[nrow(DATA),5], DATA[nrow(DATA),match("obs",names(DATA))], DATA[DATA$DATE== DATERn_fix,match('obs',names(DATA))])
  DATEPRI_obs<-DATA[DATA$DATE==DATEPRI,match('obs',names(DATA))]
  
  #Applying the corrections
  for (i in 1:DATERAD_obs) {
  
    DATA[i,match('Rn',names(DATA))] <- Rn_Mult*( cont.DS[i,match('Rn',names(cont.DS))]/100)
    DATA[i,match('PAR',names(DATA))]<-PAR_Mult*( (cont.DS[i,match('PAR',names(cont.DS))]-PAR_Offset)/PAR_Mult_wrong)+PAR_Offset
  }
  
  for (i in DATERAD_obs:DATERn_fix_obs){
    DATA[i,match('Rn',names(DATA))]<- Rn_Mult*(cont.DS[i,match('Rn',names(cont.DS))]/Rn_Mult_wrong)
  }
  
  for (i in 1:DATEPRI_obs) {
    DATA[i,match('PRI',names(DATA))]<- ifelse(is.na(cont.DS[i,match('PRI',names(cont.DS))]),NA,-cont.DS[i,match('PRI',names(cont.DS))])
  }


}




##################################################################
#SAVING THE NEW FILE WITHOUT MISSING ROWS AND CORRECTIONS APPLIED:
##################################################################

if (SITE=="V"){        
  #For el Viejo
  write.csv(DATA,paste('~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_FiltGAPfil','.csv',sep=''),row.names=FALSE)      

  }else{  
  
#For Costena
  #write.csv(cont.DS,paste('C:/Users/lmorillas/Documents/CR1000/Costena/CR1000_Costena_comp_FiltGAPfil','.csv',sep=''),row.names=FALSE) 
  write.csv(DATA,paste('~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/Costena/CR1000_Costena_comp_FiltGAPfil','.csv',sep=''),row.names=FALSE)    
}












###########################################################################
###########################################################################
#
# Generating daily datasets   
#
#############################################################################
############################################################################

#For el Viejo
DATA<-cont.DS
names(DATA)<- c("DATE","batt_volt_Min","PTemp","year","jday","hours","minutes","Hukse_shf_UP_8cm","Hukse_shf_mV_Avg","Hukse_shf_cal",
                "EC_UP_1_8cm","EC_IR_1_8cm","EC_UP_2_8cm","EC_IR_2_8cm","GS3Temp_UP_1_8cm","GS3Temp_IR_1_8cm","GS3Temp_UP_2_8cm",
                "GS3Temp_IR_2_8cm","VWC_UP_1_8cm","VWC_IR_1_8cm","VWC_UP_2_8cm","VWC_IR_2_8cm","WP_UP_1_8cm","WP_IR_1_8cm","WP_UP_2_8cm",
                "WP_IR_2_8cm","MPSTemp_UP_1_8cm",   "MPSTemp_IR_1_8cm","MPSTemp_UP_2_8cm","MPSTemp_IR_2_8cm","TEM_UP_1_5cm","TEM_UP_1_10cm",
                "TEM_IR_1_5cm","TEM_IR_1_10cm","TEM_UP_2_5cm","TEM_UP_2_10cm","TEM_7miss","TEM_IR_2_5cm","TEM_IR_2_10cm","Rn","PAR",
                "Wd","Ws","Tair","RH","Pair","Rain","Rduration","Rintensity","Rpeak","Vaisala_supply","PRI_Avg","Down570nm","Up570nm",
                "Down531nm","Up531nm","Ind1","Ind2", "EC_IR_1_2.5cm","EC_IR_2_2.5cm","EC_UP_1_2.5cm","EC_UP_2_2.5cm","GS3Temp_IR_1_2.5cm",
                "GS3Temp_IR_2_2.5cm","GS3Temp_UP_1_2.5cm","GS3Temp_UP_2_2.5cm","VWC_IR_1_2.5cm","VWC_IR_2_2.5cm","VWC_UP_1_2.5cm","VWC_UP_2_2.5cm",
                "Can_dist_avg","Can_dist_std","Can_ht","Ta_can")

DATA_24h<-aggregate(DATA, by=list(DATA$jday,DATA$year),FUN=mean, na.rm=TRUE)
DATA_24h$Rain<-aggregate(DATA$Rain, by=list(DATA$jday,DATA$year),FUN=sum,na.rm=TRUE)$x            #na.rm=TRUE means that NA's will be ignored. 
DATA_24h$Rain_complete<-aggregate(DATA$Rain, by=list(DATA$jday,DATA$year),FUN=sum,na.rm=FALSE)$x  #na.rm=F will generate a NA FOR EVERYDAY INCLUDING AT LEAST ONE MISING 30 MIN RAIN DATA



write.csv(DATA_24h[,3:ncol(DATA_24h)],paste('~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_24h','.csv',sep=''),row.names=FALSE)   


###########################################################################
###########################################################################
#
# Generating hourly datasets   
#
#############################################################################
############################################################################

#For el Viejo
DATA<-cont.DS
names(DATA)<- c("DATE","batt_volt_Min","PTemp","year","jday","hours","minutes","Hukse_shf_UP_8cm","Hukse_shf_mV_Avg","Hukse_shf_cal",
                "EC_UP_1_8cm","EC_IR_1_8cm","EC_UP_2_8cm","EC_IR_2_8cm","GS3Temp_UP_1_8cm","GS3Temp_IR_1_8cm","GS3Temp_UP_2_8cm",
                "GS3Temp_IR_2_8cm","VWC_UP_1_8cm","VWC_IR_1_8cm","VWC_UP_2_8cm","VWC_IR_2_8cm","WP_UP_1_8cm","WP_IR_1_8cm","WP_UP_2_8cm",
                "WP_IR_2_8cm","MPSTemp_UP_1_8cm",   "MPSTemp_IR_1_8cm","MPSTemp_UP_2_8cm","MPSTemp_IR_2_8cm","TEM_UP_1_5cm","TEM_UP_1_10cm",
                "TEM_IR_1_5cm","TEM_IR_1_10cm","TEM_UP_2_5cm","TEM_UP_2_10cm","TEM_7miss","TEM_IR_2_5cm","TEM_IR_2_10cm","Rn","PAR",
                "Wd","Ws","Tair","RH","Pair","Rain","Rduration","Rintensity","Rpeak","Vaisala_supply","PRI_Avg","Down570nm","Up570nm",
                "Down531nm","Up531nm","Ind1","Ind2", "EC_IR_1_2.5cm","EC_IR_2_2.5cm","EC_UP_1_2.5cm","EC_UP_2_2.5cm","GS3Temp_IR_1_2.5cm",
                "GS3Temp_IR_2_2.5cm","GS3Temp_UP_1_2.5cm","GS3Temp_UP_2_2.5cm","VWC_IR_1_2.5cm","VWC_IR_2_2.5cm","VWC_UP_1_2.5cm","VWC_UP_2_2.5cm",
                "Can_dist_avg","Can_dist_std","Can_ht","Ta_can")

DATA_h<-aggregate(DATA, by=list(DATA$hour,DATA$jday,DATA$year),FUN=mean, na.rm=TRUE)
DATA_h$Rain<-aggregate(DATA$Rain, by=list(DATA$hour,DATA$jday,DATA$year),FUN=sum,na.rm=TRUE)$x            #na.rm=TRUE means that NA's will be ignored. 
DATA_h$Rain_complete<-aggregate(DATA$Rain, by=list(DATA$hour,DATA$jday,DATA$year),FUN=sum,na.rm=FALSE)$x  #na.rm=F will generate a NA FOR EVERYDAY INCLUDING AT LEAST ONE MISING 30 MIN RAIN DATA



write.csv(DATA_h[,4:ncol(DATA_h)],paste('~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_hourly','.csv',sep=''),row.names=FALSE)   





###########################################################################
###########################################################################
#
# Generating diagnostics plots of 30min data    (UNDER CONSTRUCTION YET)
#
#############################################################################
############################################################################

DATA<-cont.DS
names(DATA)<- c("DATE","batt_volt_Min","PTemp","year","jday","hours","minutes","Hukse_shf_UP_8cm","Hukse_shf_mV_Avg","Hukse_shf_cal",
"EC_UP_1_8cm","EC_IR_1_8cm","EC_UP_2_8cm","EC_IR_2_8cm","GS3Temp_UP_1_8cm","GS3Temp_IR_1_8cm","GS3Temp_UP_2_8cm","GS3Temp_IR_2_8cm","VWC_UP_1_8cm","VWC_IR_1_8cm","VWC_UP_2_8cm","VWC_IR_2_8cm","WP_UP_1_8cm","WP_IR_1_8cm","WP_UP_2_8cm","WP_IR_2_8cm","MPSTemp_UP_1_8cm",   "MPSTemp_IR_1_8cm","MPSTemp_UP_2_8cm","MPSTemp_IR_2_8cm","TEM_UP_1_5cm","TEM_UP_1_10cm","TEM_IR_1_5cm","TEM_IR_1_10cm","TEM_UP_2_5cm",       "TEM_UP_2_10cm","TEM_Avg.7.","TEM_IR_2_5cm","TEM_IR_2_10cm","Rn","PAR","Wd","Ws","Tair","RH","Pair","Rain_mm","Rduration","Rintensity",   
"Rpeak","Vaisala_supply","PRI_Avg","DownNIR","UpNIR","DownRed","UpRed","Ind1","Ind2")

DATA$obs<-c(1:nrow(DATA))


 dev.off()
 dev.off()

                        
pdf(file = "C:/Users/lmorillas/Documents/CR1000/El Viejo/Diagnostics plots/PlotTEST.pdf")

Y<-2015
LO<-max(DATA[DATA$year==Y,match("obs",names(DATA))],na.rm=T)  #LAST OBSERVATION OF THE YEAR
FO<-min(DATA[DATA$year==Y,match("obs",names(DATA))],na.rm=T)  #FIRST OBSERVATION OF THE YEAR
STEPS<-ceiling((LO-FO)/4)

plot(DATA[DATA$year==Y,match("obs",names(DATA))],DATA[DATA$year==Y,match("VWC_UP_1_8cm",names(DATA))],type="l",col="darkgreen",xlab="",ylab="VWC",ylim=c(0.2,0.6),main="Y")

Oaxis<-c(FO,FO+STEPS,FO+2*STEPS,FO+3*STEPS,LO)
Daxis<-DATA[Oaxis,match("jday",names(DATA))]
axis(1,at=Oaxis,Daxis,line=3)

mtext("Observation",1,line=1,at=FO)
mtext("DOY",1,line=3,at=FO)
lines(DATA[DATA$year==Y,match("obs",names(DATA))],DATA[DATA$year==Y,match("VWC_UP_2_8cm",names(DATA))],col="green")
lines(DATA[DATA$year==Y,match("obs",names(DATA))],DATA[DATA$year==Y,match("VWC_IR_1_8cm",names(DATA))],type="l",col="red")
lines(DATA[DATA$year==Y,match("obs",names(DATA))],DATA[DATA$year==Y,match("VWC_IR_2_8cm",names(DATA))],type="l",col="brown")
par(new=TRUE)
plot(DATA[DATA$year==Y,match("obs",names(DATA))],DATA[DATA$year==Y,match("Rain_mm",names(DATA))],type="h",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,100))
axis(4,c(0,10,20,30,40,50,60,70,80,90,100),col="blue")

legend("topleft",col=c("darkgreen","green","red","brown"),lty=1,legend=c("Under Plant 1 8cm","Under Plant 2 8cm","Inter row 1 8cm","Inter row 1 8cm"))

dev.off()
dev.off()



#example from filtering:
par(mfrow=c(3,1),mar=c(2, 4, 2, 0.5),oma=c(2,0,2,4))

for( i in 2:ncol(rawData)) {
        
plot(timestampDF[,5],rawData[,i],type="l",lwd=2,ylim=c(0,0.5),ylab="SWC",xlab="DOY",main=names(rawData)[i])
lines(timestampDF[,5],OUTPUT[,i],type="l",col="red",lwd=2)
par(new=TRUE)                                                                           #This is to create a secondary axis in the same plot
plot(timestampDF[,5],fluxAll[,ncol(fluxAll)],type="h",col="blue",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,max(fluxAll[,ncol(fluxAll)],na.rm=T)))        #This is to include precipitation
Yincrease<-(max(fluxAll[,ncol(fluxAll)],na.rm=T))/4  
axis( side=4)
mtext("Precip",side=4,line=3,cex=0.8)                                                                           #4 means that the rain axis is going to be writen in the right hand of the plot
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("raw SWC","Filtered SWC","Precipitation"))

     }
     
dev.off()

