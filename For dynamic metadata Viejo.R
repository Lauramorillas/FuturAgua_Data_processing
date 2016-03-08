#####
# Script design Notes:

#This script is written to estimate variable daily canopy height and generate a dynamic metadata file
# including all variables changing in the metadata of both sites.

#New datasets for canopy height will start in the date cr1000 datasets started, using the file "CR1000_site_comp_filt_forR"

######
#Coding notes:

#write.table(dynamic2,"dynamic",row.names=FALSE,sep = ",",quote=F) #Adding 'quote=F', you are able to avoid that characters in the output file get saved quoted(entre commillas). 
#This save my life! becasue otherwise the dynamic metadata file I was generating could not be read by EddyPro


#function to convert julian day on entire month (round number,from 1 to 12)
MfromD<- function (x){ 
  MEND<-c(31,59,90,120,151,181,212,243,273,304,334,366)
  ifelse(x>=0 & x<=MEND[1],1,ifelse(x>MEND[1] & x<=MEND[2],2,ifelse(x>MEND[2] & x<=MEND[3],3,ifelse(x>MEND[3] & x<=MEND[4],4,
                              ifelse(x>MEND[4] & x<=MEND[5],5,ifelse(x>MEND[5] & x<=MEND[6],6,ifelse(x>MEND[6] & x<=MEND[7],7,ifelse(x>MEND[7] & x<=MEND[8],8,
                              ifelse(x>MEND[8] & x<=MEND[9],9,ifelse(x>MEND[9] & x<=MEND[10],10,ifelse(x>MEND[10] & x<=MEND[11],11,ifelse(x>MEND[11] & x<=MEND[12],12,0)))))))))))) 
}




#CODE:
#############################################################################################
#############################################################################################
#Exporting dataset:
#############################################################################################
#############################################################################################
    
  
  ts<-read.csv( '~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_filt_forR.csv',sep=",",header=TRUE,dec=".") #from I Mac
  

#Adding a new row with Timestamp in posix R format
#ts$date<-as.POSIXct(strptime(ts$TIMESTAMP,format="%m/%d/%Y %H:%M",tz="UTC"))
ts$date<-as.POSIXct(strptime(ts$TIMESTAMP,format="%y-%m-%d %H:%M",tz="UTC")) #%y identifies the year from the last two digits i.e 14=2014, %Y needs the four year digits if they are in the date 

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


#To generate a serie of daily data from the first day of measurements:
Tsteps<-begining+seq(from=0,to=((Ndays+1)*(60*60*24)),by=(24*60*60)) # days in seconds from the begining date to Ndays +1 (that way I assure all measured data are included in the new file)
DATE<-as.POSIXct(Tsteps,origin="1970-01-01 00:00:00",tz="GMT")


#GENERATING A NEW DATA FRAME WITH CONTINUOUS daily TIME STEPS  
newdata<-as.data.frame(DATE)
newdata$obs<-c(1:nrow(newdata))
newdata$year<-as.integer(as.character(newdata$DATE, "%Y"))
newdata$month<-as.integer(as.character(newdata$DATE, "%m"))
newdata$day<-as.integer(as.character(newdata$DATE, "%d"))
newdata$jday<-as.POSIXlt(newdata$DATE)$yday+1

#ADDING A "JULIAN MONTH" TO APPLIED THE CANOPY HEIGHT MODEL (BASED ON MONTH):
newdata$jmonth<-rep(NA,nrow(newdata))                 #jmonth="julian month",month fraction depending on the day of the month

longm<-c(1,3,5,7,8,10,12)   #longm=long month =months with 31 days

for (i in 1:nrow(newdata)){
  
  if (newdata[i,match('month',names(newdata))] %in% longm){
    newdata$jmonth[i]<-newdata$month[i]+ ((newdata$day[i]-1)*1/31)
  } else {
    newdata$jmonth[i]<-newdata$month[i]+ ((newdata$day[i]-1)*1/30)
  }
}


############################
############################
#Let's generate a jmonth or day value starting to count from seeding date:
############################
############################

      
  HARVEST_13<-as.POSIXct("2013-12-08 17:00:00",tz = "GMT")
  HARVEST_14<-newdata[newdata$year==2014 & newdata$month==12 & newdata$day==8,match("DATE",names(newdata))] #2014-12-08 17:00:00 GMT harves date in 2014
  HARVEST_15<-newdata[newdata$year==2015 & newdata$month==12 & newdata$day==9,match("DATE",names(newdata))] #This will need to be updated with more information
  

#Estimating days and months after harvesting by site: 

  
  counter<-rep(NA,nrow(newdata))        #this will be the counter of days after last harvesting at el Viejo
  
  harv_days<-c(newdata[newdata$DATE==HARVEST_14,match('obs',names(newdata))],newdata[newdata$DATE==HARVEST_15,match('obs',names(newdata))])
  
  #before harvesting 2014 (first harvesting included in measurements series):
  HARVEST14<-harv_days[1]
  
  for (i in newdata$obs[1]:(HARVEST14-1)){
    daysAH_2013<-365-(as.POSIXlt(HARVEST_13)$yday)    #daysAH2013= days after harvest in 2013 (not including harvesting 2013 day)
    daysAH_premes<-daysAH_2013+newdata$jday[1]        #daysAH_prem= days passed between previous harvesting (2013) and start of measurements
    TdaysAH<-daysAH_premes+length(newdata$jday[1]:(as.POSIXlt(HARVEST_14)$yday))-1  #TdaysAH= total days passed after harvest in 2013 and harvest 2014
    
    counter[i]<-seq(from=daysAH_premes,to=TdaysAH,by=1)[i]           
  }   
  
  #Between harvesting 2014 and harvesting 2015:
  
  if (is.na(harv_days[2])){
    
    for (i in (harv_days[1]):newdata$obs[nrow(newdata)]){
      
      counter[i]<-seq(from=0,to=length((harv_days[1]):(newdata$obs[nrow(newdata)]-1)),by=1)[i-(harv_days[1]-1)]           ##seq(from=1,to=length((harv_days[1]+1):(harv_days[2]-1))/31,by=(1/31))
    } 
  } else {  
    
    HARVEST15<-harv_days[2]   
    for (i in (HARVEST14):(HARVEST15-1)){
      counter[i]<-seq(from=0,to=length((HARVEST14):(HARVEST15-1)),by=1)[i-(HARVEST14-1)]           
    } 
  }
  
  
  #After harvesting 2015:
  
  if (is.na(harv_days[2])){
    newdata$days_after_harvest<-counter
    
  } else {  
    HARVEST15<-harv_days[2] 
    for (i in (HARVEST15):newdata$obs[nrow(newdata)]){
      counter[i]<-seq(from=0,to=length((HARVEST15):(newdata$obs[nrow(newdata)]-1)),by=1)[i-(HARVEST15-1)]           ##seq(from=1,to=length((harv_days[1]+1):(harv_days[2]-1))/31,by=(1/31))
    } 
  } 
  
   #NOTE: After harvesting 2016 we will need to add other loop or step here





#ADDING A "Harvesting JULIAN MONTH" TO APPLY THE CANOPY HEIGHT MODEL (BASED ON MONTH):
newdata$hday<-counter
newdata$hmonth_round<-MfromD(newdata$hday)                 #jmonth="julian month",month fraction depending on the day of the month
newdata$hmonth<-rep(NA,nrow(newdata))

#function to convert julian day on decimal month
MEND<-c(31,59,90,120,151,181,212,243,273,304,334,366)

for (i in 1:nrow(newdata)){
  
  if (newdata[i,match('hmonth_round',names(newdata))] %in% longm){
    
    newdata$hmonth[i]<-ifelse(newdata$hmonth_round[i]==1,newdata$hmonth_round[i]+ ((newdata$hday[i]-1)*1/31),newdata$hmonth_round[i]+ (((newdata$hday[i]-MEND[newdata$hmonth_round[i]-1])-1)*1/31))
  } else {
    newdata$hmonth[i]<-ifelse(newdata$hmonth_round[i]==1,newdata$hmonth_round[i]+ ((newdata$hday[i]-1)*1/30) ,newdata$hmonth_round[i]+ (((newdata$hday[i]-MEND[newdata$hmonth_round[i]-1])-1)*1/30))
  }
}




########################################################
########################################################
#ESTIMATING CANOPY HEIGHT FROM HMONTH 
#   Applying equations from Don Fermin' data or our measurements at la Costena
#(quadratic equations y= ax2+bx+c, 
#where y=tasa alargamiento de los tallos , x=decimal months from harvest)
########################################################
########################################################
  
  #Finding my own polynomial model from CP72-2086 variety (Figure 3)

  x<-c(0:12)
  y<-c(0,13,14,99,179,241,269,279,332,326,399,442,442)
  model<-lm(y ~ poly(x, 2, raw=TRUE))   #You want to perform a linear regression, but you want to force the intercept to be zero:Add "+ 0" to the righthand side of your regression formula
  summary(model)
  mod_a<-summary(model)$coefficients[3] #-1.101898
  mod_b<-summary(model)$coefficients[2] #53.6733
  mod_c<-summary(model)$coefficients[1] #-33.4835
  y_mod2<-mod_a*x^2 + mod_b*x + mod_c
  sc_mod_laura<-function (x) {mod_a*x^2 + mod_b*x + mod_c}

#Finding my own polynomial model from CP72-2086 variety (Figure 3) From harvest doy
#x_days<-c(0,MEND)
#y<-c(0,13,14,99,179,241,269,279,332,326,399,442,442)
#model_days<-lm(y ~ poly(x_days, 2, raw=TRUE))   #You want to perform a linear regression, but you want to force the intercept to be zero:Add "+ 0" to the righthand side of your regression formula
#summary(model_days)
#MOD_a<-summary(model_days)$coefficients[3] #-1.101898
#MOD_b<-summary(model_days)$coefficients[2] #53.6733
#MOD_c<-summary(model_days)$coefficients[1] #-33.4835
#sc_mod<-function (x) {MOD_a*x^2 + MOD_b*x + MOD_c}

  
  #Using the polinomial equation provided by Don Fermin in Figure 3
  #El Viejo
  a<- -1.974675
  b<- 65.97739
  c<- - 67.89141
  y_mod<-a*x^2 + b*x + c 
  sc_mod<-function (x) {a*x^2 + b*x + c}
  
  
  #Ploting two model equation vs measurements:
  par(mfrow=c(2,1))
  plot(x,y,xlab='month from harvest',ylab='canopy height (cm)',main="Don Fermin's data for Canopy height model")
  points(x,y_mod,type='l',col='blue')
  points(x,y_mod2,type='l',col='red')
  legend("topleft",col=c("black","blue","red"),lty=1,legend=c("measurements","Don Fermin's model","Laura's model"))
  
  #Applying models to estimate canopy height for our measurements period: 
 
       #newdata$can_height_cor<-ifelse(sc_mod(newdata$hday)/100<0,0,sc_mod(newdata$hday)/100)  #canopy height in meters using my equation from his data and day after harvesting
  
  newdata$can_height<-ifelse(sc_mod(newdata$hmonth)/100<0,0,sc_mod(newdata$hmonth)/100)  #canopy height in meters using Don Fermi's equation
  newdata$can_height_2<-ifelse(sc_mod_laura(newdata$hmonth)/100<0,0,sc_mod_laura(newdata$hmonth)/100) #canopy height in meters using my equation from his data


 #Ploting modeled canopy height evolution:

  plot(newdata$obs,newdata$can_height_2,ylim=c(0,6),ylab='canopy height (m)',xlab='Day of year',main='VIEJO CANOPY HEIGHT EVOLUTION',xaxt='n',col='red' )
  ticks<-seq(from=1,to=nrow(newdata), by=round((nrow(newdata)/30)))
  for (i in ticks){ 
  axis(1,at=i, label= newdata$jday[i])
  }
  points(newdata$obs,newdata$can_height,col="blue")
  abline(h=4,lty=2, col='grey')
  
  #To compare with our measurements and add our measurements in the database
  obs_doy<-c(112,179,216,261,310,347)
  obs_ch<-c(2.2,3.2,3.45,3.9,3.9,0)

  newdata$meas_ch<-rep(NA,nrow(newdata))
  for (i in 1:length(obs_doy)) {
    newdata[newdata$year==2015 & newdata$jday==obs_doy[i],match('meas_ch',names(newdata))]<- obs_ch[i]
  }

  points(newdata$obs,newdata$meas_ch,pch=5)
  legend("topleft",col=c("black","blue","red"),lty=1,legend=c("measurements","Don Fermin's model","Laura's model"))

write.csv(newdata,paste('~/Documents/UBC/futuragua_research/Data management/For_eddypro/Evolution_can_heigth_ElViejo','.csv',sep=''),row.names=FALSE)      
  


########################################################
########################################################
#Generating a dynamic metadata file with the metadata that has historically varied at each site 
########################################################
########################################################      
 
  
  #Add here as many changes 
  CH1<-c("2014-06-06","18:00", 6.3,-0.23 ,-0.19,-0.30) #A vector including (date,time,irga_northward_separation,irga_eastward_separation,irga_vertical_separation)for change 1 (distances and displazements in m)
  CH2<-c("2015-04-22","19:00",6,-0.35,0.02,0)
  
  dynamic<-as.data.frame(as.Date(newdata$DATE,format = "%Y/%m/%d"))
  names(dynamic)<-c('date')      
  dynamic$time<-format("06:00", format = "%H:%m", tz = "GMT", usetz = FALSE)  #we forze the metadate file to change canopy height every midnight which in our case in UTC is 06:00 
  dynamic$master_sonic_height<-rep(NA,nrow(newdata))      
  dynamic$canopy_height<-round(ifelse(newdata$can_height<0,0,newdata$can_height),digits=3)  #HERE WE DECIDE WHAT MODELED CANOPY HEIGHT TO USE, IN THIS CASE can_height_2 
  dynamic$co2_irga_northward_separation<-rep(NA,nrow(newdata))           
  dynamic$co2_irga_eastward_separation<-rep(NA,nrow(newdata))         
  dynamic$co2_irga_vertical_separation<-rep(NA,nrow(newdata)) 
  dynamic$h2o_irga_northward_separation<-rep(NA,nrow(newdata))           
  dynamic$h2o_irga_eastward_separation<-rep(NA,nrow(newdata))         
  dynamic$h2o_irga_vertical_separation<-rep(NA,nrow(newdata)) 
  
  
  change2<-as.data.frame(as.Date(as.POSIXct(format(CH2[1], format = "%Y/%m/%d", tz = "GMT", usetz = FALSE)),format = "%Y/%m/%d"))
  names(change2)<-c('date') 
  change2$time<-format(CH2[2], format = "%H:%m", tz = "GMT", usetz = FALSE) 
  change2$master_sonic_height<-as.numeric(CH2[3])    
  change2$canopy_height<-dynamic[dynamic$date == CH2[1],match('canopy_height',names(dynamic))]   
  change2$co2_irga_northward_separation<-as.numeric(CH2[4])           
  change2$co2_irga_eastward_separation<- as.numeric(CH2[5])        
  change2$co2_irga_vertical_separation<- as.numeric(CH2[6])
  change2$h2o_irga_northward_separation<-as.numeric(CH2[4])           
  change2$h2o_irga_eastward_separation<- as.numeric(CH2[5])        
  change2$h2o_irga_vertical_separation<- as.numeric(CH2[6])   
  
  
  #Because dynamic dataframe starts before CH1 date at El Viejo, we applied to first row of dynamic data the values of the variables from CH1:   
  dynamic[1,match('master_sonic_height',names(dynamic))]<-as.numeric(CH1[3])
  dynamic[1,grep('_irga_northward_separation',names(dynamic))]<-as.numeric(CH1[4])
  dynamic[1,grep('_irga_eastward_separation',names(dynamic))]<-as.numeric(CH1[5])
  dynamic[1,grep('_irga_vertical_separation',names(dynamic))]<-as.numeric(CH1[6])
  
  
  #NOW WE NEED TO REPLACE THE NA FOR THE REAL VALUES OF THE IRGA PARAMETERS IN THE DYNAMIC METADATA:    
  
  #We need here as many loops for filling metadtata as changes in metadata variables have been, 
  #so a new loop will need to be included in case a new change in irga variables happen
  
  for (i in dynamic$date[1]:change2$date[1]){
    
    
    dynamic[dynamic$date == i,match('master_sonic_height',names(dynamic))]<-dynamic[1,match('master_sonic_height',names(dynamic))]
    dynamic[dynamic$date == i,grep('_irga_northward_separation',names(dynamic))]<-dynamic[1,match('co2_irga_northward_separation',names(dynamic))]
    dynamic[dynamic$date == i,grep('_irga_eastward_separation',names(dynamic))]<-dynamic[1,match('co2_irga_eastward_separation',names(dynamic))]
    dynamic[dynamic$date == i,grep('_irga_vertical_separation',names(dynamic))]<-dynamic[1,match('co2_irga_vertical_separation',names(dynamic))]
  }
  
  
  for (i in change2$date[1]:dynamic$date[nrow(dynamic)]){
    
    dynamic[dynamic$date == i,match('master_sonic_height',names(dynamic))]<-change2$master_sonic_height
    dynamic[dynamic$date == i,grep('_irga_northward_separation',names(dynamic))]<-change2$co2_irga_northward_separation
    dynamic[dynamic$date == i,grep('_irga_eastward_separation',names(dynamic))]<-change2$co2_irga_eastward_separation
    dynamic[dynamic$date == i,grep('_irga_vertical_separation',names(dynamic))]<-change2$co2_irga_vertical_separation
  }
  
  
  obs_ch2<-length(dynamic$date[1]:change2$date[1]) 
  
  dynamic2<-rbind(dynamic[1:(obs_ch2-1),],change2[1,],dynamic[(obs_ch2+1):nrow(dynamic),])
  
  write.table(dynamic2,paste('~/Documents/UBC/futuragua_research/Data management/For_eddypro/Dynamic_metadata_ElViejo','.txt',sep=''),row.names=FALSE,sep = ",",quote=FALSE)      
  



