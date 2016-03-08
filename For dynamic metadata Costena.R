#####
# Script design Notes:

#This script is written to estimate variable daily canopy height and generate a dynamic metadata file
#including all variables changing in the metadata for La Costena site

#New datasets for canopy height will start in the date cr1000 datasets started, using the file "CR1000_site_comp_filt_forR"



##################################
#INPUT FILE FOR THIS SCRIPT:  "CR1000_Costena_comp_FiltGAPfil.csv"



######
#Coding notes:

#write.table(dynamic2,"dynamic",row.names=FALSE,sep = ",",quote=F) #Adding 'quote=F', you are able to avoid that characters in the output file get saved quoted(entre commillas). 
#This save my life! becasue otherwise the dynamic metadata file I was generating could not be read by EddyPro


#CODE:
#############################################################################################
#############################################################################################

#Exporting dataset:
#############################################################################################
#############################################################################################
   
  ts<-read.csv( '~/Documents/UBC/futuragua_research/Data management/CR1000_processing_results/Costena/CR1000_Costena_comp_filt_forR.csv',sep=",",header=T)



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

#Defining harvesting dates :      
  SEED_R_14<-newdata[newdata$year==2014 & newdata$month==06 & newdata$day==28,match("DATE",names(newdata))] #2014-06-28 17:00:00 GMT rice sow date in 2014
  HARVEST_R_14<-newdata[newdata$year==2014 & newdata$month==11 & newdata$day==8,match("DATE",names(newdata))] #I decided from pictures that november 8 is the day when the rice has been harvested in the whole farm 
  
  SEED_M_15<-newdata[newdata$year==2015 & newdata$month==01 & newdata$day==19,match("DATE",names(newdata))] #I decide a date closer to the seeding in lotes 18 and 15 the most included in the footprint
  HARVEST_M_15<-newdata[newdata$year==2015 & newdata$month==03 & newdata$day==21,match("DATE",names(newdata))] #I decide a date closer to the seeding in lotes 18 and 15 the most included in the footprint
  
  SEED_R_15<-newdata[newdata$year==2015 & newdata$month==07 & newdata$day==14,match("DATE",names(newdata))] 
  HARVEST_R_15<-newdata[newdata$year==2015 & newdata$month==11 & newdata$day==8,match("DATE",names(newdata))] #According to Silja's observations
  
  SEED_M_16<-newdata[newdata$year==2016 & newdata$month==01 & newdata$day==21,match("DATE",names(newdata))] #This is the date lote 18 (the first one) gets planted.
  
days_rice_0.5_14<-13
  days_rice_0.5_15<-36  # At least we now that day 29 after harvesting date canopy height was still 50 cm, I assume that they didn't burn until 4 days later,Dec 14

#Estimating days and months after seeding rice or melon: 
          
          counter_r<-rep(NA,nrow(newdata))        #this will be the counter of days after rice seeding  at la Costenya
          counter_m<-rep(NA,nrow(newdata))        #this will be the counter of days after melon seeding at la Costenya
          counter_ar<-rep(NA,nrow(newdata))        #this will be the counter of days after rice harvesting (after rice) at la Costenya
          
          seed_days<-c(newdata[newdata$DATE==SEED_R_14,match('obs',names(newdata))],newdata[newdata$DATE==HARVEST_R_14,match('obs',names(newdata))]
                       ,newdata[newdata$DATE==SEED_M_15,match('obs',names(newdata))],newdata[newdata$DATE==HARVEST_M_15,match('obs',names(newdata))],
                       newdata[newdata$DATE==SEED_R_15,match('obs',names(newdata))],newdata[newdata$DATE==HARVEST_R_15,match('obs',names(newdata))],
                       newdata[newdata$DATE==SEED_M_16,match('obs',names(newdata))])#,newdata[newdata$DATE==HARVEST_R_15,match('obs',names(newdata))])

                #seed_days is a vector including the observation where (SEED_R_14,HARVEST_R_14,SEED_M_15,HARVEST_M_15,SEED_R_15), 
                      #I need to add to this vector the new seeding or harvest dates  when new things happen in the field
          
          #Between rice seeding 2014 and rice harvesting 2014:
          A<-seed_days[1]
          B<-seed_days[2]
          
          for (i in A:B){ 
            counter_r[i]<-seq(from=0,to=B-A,by=1)[i-(A-1)]          
          } 
          
          counter_r

        #After rice half half harvesting (0.5m) and total rice harvest (0 m) 2014:
          C<-seed_days[2]+1
          D<-seed_days[2]+days_rice_0.5_14

         for (i in C:D){ 
         counter_ar[i]<-1  #seq(from=0,to=D-C,by=1)[i-(C-1)]          
          }


          #After melon seeding 2015:
          
          E<-seed_days[3]
          F<-seed_days[4]
          
          for (i in E:F){ 
            counter_m[i]<-seq(from=0,to=F-E,by=1)[i-(E-1)]          
          } 
          
          counter_m
          
          
          #After rice seeding 2015:
          
          G<-seed_days[5]
          H<-seed_days[6]
          end<-nrow(newdata)
          
          if( is.na(H)){
            
            for (i in G:end){ 
              counter_r[i]<-seq(from=0,to=end-G,by=1)[i-(G-1)]          
            }
          }else{
            
            for (i in G:H){ 
              counter_r[i]<-seq(from=0,to=H-G,by=1)[i-(G-1)]          
            }
           }
          
          counter_r

          #After rice half half harvesting (0.5m) and total rice harvest (0 m) 2015:
          I<-seed_days[6]+1
          J<-seed_days[6]+days_rice_0.5_15

          for (i in I:J){ 
           counter_ar[i]<-1  #seq(from=0,to=D-C,by=1)[i-(C-1)]          
          }
              counter_ar
          
          #After melon seeding 2016:

            K<-seed_days[7]
            L<-seed_days[8]

          if( is.na(L)){
  
               for (i in K:end){ 
               counter_m[i]<-seq(from=0,to=end-K,by=1)[i-(K-1)]          
                        }
              }else{
  
              for (i in K:L){ 
               counter_m[i]<-seq(from=0,to=L-K,by=1)[i-(L-1)]          
                } 
                }
          counter_m


          #We will need to add a new after rice seeding 2016 loop here similar to the previous one to complete counter_r when the time come  
          
          
newdata$days_rice_grow<-counter_r
newdata$days_rice_0.5<-counter_ar
newdata$days_melon_grow<-counter_m
 
counter_r
counter_ar
counter_m
          
########################################################
########################################################
#ESTIMATING CANOPY HEIGHT FROM HMONTH 
#   Applying equations from our measurements at la Costena
#(quadratic equations y= ax2+bx+c, 
#where y=CANOPY HEIGHT , x=DAYS AFTER PLANT SEED
########################################################
########################################################
  
  #Finding my own polynomial model for rice variety Palmar 18 (from our canopy height measurements)
  x<-c(0,18,30,47,88,102,109,113)             #X is days after seeding
  y<-c(0,0.19,0.28,0.55,0.85,1.01,1.01,1.01)     #Y is CANOPY HEIGHT
  rice_model<-lm(y ~ poly(x, 2, raw=TRUE))   #You want to perform a linear regression, but you want to force the intercept to be zero:Add "+ 0" to the righthand side of your regression formula
  summary(model)
  rice_a<-summary(rice_model)$coefficients[3] #-1.101898
  rice_b<-summary(rice_model)$coefficients[2] #53.6733
  rice_c<-summary(rice_model)$coefficients[1] #-33.4835
  rice_mod<-function (x) {rice_a*x^2 + rice_b*x + rice_c}
  
  #Ploting rice growing model equation vs measurements:
  #pdf(file = "~/Documents/UBC/futuragua_research/Data management/Costena_canopy height.pdf")
  par(mfrow=c(3,1))
  plot(x,y,xlab='day after seeding',ylab='canopy height (cm)',main="RICE 2015")
  points(x,rice_mod(x),type='l',col='blue')
  legend("topleft",col=c("black","blue"),lty=1,legend=c("measurements","Laura's model"))
  
  #Finding my own polynomial model for melon variety (WE WILL NEED TO UPDATE THIS WITH OUR MEASUREMENTS, for the moment this is ramdom numbers)
  x<-c(0,15,20,25,30,35,40,45,50,55,60)
  y<-c(0,0.05,0.09,0.15,0.23,0.26,0.29,0.30,0.30,0.30,0.30)
  model<-lm(y ~ poly(x, 2, raw=TRUE))   #You want to perform a linear regression, but you want to force the intercept to be zero:Add "+ 0" to the righthand side of your regression formula
  summary(model)
  mel_a<-summary(model)$coefficients[3] #-1.101898
  mel_b<-summary(model)$coefficients[2] #53.6733
  mel_c<-summary(model)$coefficients[1] #-33.4835
  melon_mod<-function(x) { mel_a*x^2 +  mel_b*x +  mel_c}
  
  #Ploting melon growing model equation vs measurements:
  
  plot(x,y,xlab='day after seeding',ylab='canopy height (cm)',ylim=c(0,0.40),main='MELON (fake number)')
  points(x,melon_mod(x),type='l',col='blue')
  legend("topleft",col=c("black","blue"),lty=1,legend=c("measurements","Laura's model"))
  
  #Applying models to estimate canopy height for our measurements period: 
  
  rice_height_1<-ifelse(rice_mod(newdata$days_rice_grow)<0,0,rice_mod(newdata$days_rice_grow))  #canopy height in meters using our equation during rice growing period

  for (i in 1:nrow(newdata)){
  newdata$rice_height[i]<-ifelse(is.na(newdata$days_rice_0.5[i]),rice_height_1[i],0.5)   #canopy height=0.5 m after initial rice harvesting for 22 days
  }

  newdata$melon_height<-ifelse(melon_mod(newdata$days_melon_grow)<0,0,melon_mod(newdata$days_melon_grow))  #canopy height in meters using our equation
  
  for (i in newdata$obs){
    if (is.na(newdata$rice_height[i]) && is.na(newdata$melon_height[i])){
      newdata$can_height[i]<-0
    }else{
      
      newdata$can_height[i]<-rowSums(newdata[i,grep('_height',names(newdata))],dims=1,na.rm=TRUE)   #canopy height of rice or melon plant in meters using our equations 
    }
  }
  
  plot(newdata$obs,newdata$can_height,ylab='canopy height (m)',xlab='Day of year',main='COSTENA CANOPY HEIGHT EVOLUTION',xaxt='n')
  ticks<-seq(from=1,to=nrow(newdata), by=round((nrow(newdata)/30)))
  for (i in ticks){ 
  axis(1,at=i, label= newdata$jday[i])
  }

  write.csv(newdata,paste('~/Documents/UBC/futuragua_research/Data management/For_eddypro/Dynamic_can_heigth_Costena','.csv',sep=''),row.names=FALSE)      
  

########################################################
########################################################
#Generating a dynamic metadata file with the metadata that has historically varied at each site 
########################################################
########################################################      

  
  #Add here as many changes as happen:
  CH1<-c("2014-06-06","18:00", 3.26,-0.14 ,-0.02,-0.20) #A vector including (date,time,master_sonic_height,irga_northward_separation,irga_eastward_separation,irga_vertical_separation)for change 1 (distances and displazements in m)
  CH2<-c("2015-04-15","14:00",3.26,-0.18,0.32,0)
  
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
  
  write.table(dynamic2,paste('~/Documents/UBC/futuragua_research/Data management/For_eddypro/Dynamic_metadata_LaCostena','.txt',sep=''),row.names=FALSE,sep = ",",quote=FALSE) 
      







