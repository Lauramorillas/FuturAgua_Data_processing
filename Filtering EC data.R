

########################################
# Script design Notes:

#This script is written to proceed to filter EC data from processed compiled datasets 
# It can be used for both sites


####################################
#INPUT FILE FOR THIS SCRIPT:  "Site_prefiltered"

#OUTPUT FILES: "SITE_FILTERED"        in the folder:Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA
#              "Site_filtering_summary"
#              "Site_flags_summary"
#              "Site_Fluxes_QAQC"
#           

###################################################
#PENDING TASKS to add:

          #Check Energy balance
          #DETERMINE A SITE SPECIFIC U* THRESHOLD FOR FILTERING
          

#########################################################
#Coding notes:
#
#########################################################


# clear memory
rm(list=ls())

############################################################
# GENERAL PERIODS TO REMOVE BECAUSE OF MAINTENANCE WORK OR KNOWN INSTRUMENTAL FAILURE
############################################################

#Add here as many changes as happen:
#A vector describing the periods to remove including (date_UTC_beginning,date_UTC_end of the period to remove)
#Date is in form: year-month-day HH:MM

#Costena:
P1_C<-c("2015-08-01 13:00:00","2015-08-07 18:00:00" ) #Period after Licor's chemicals change prior to calibration, and then temporarily wrong calibration applied.Time is in UTC


#Viejo:
P1_V<-c("2015-03-25 00:00:00","2015-04-23 00:00:00" ) #Period with huge bee nest confirmed growing in the lICOR
P2_V<-c("2015-08-04 15:00:00","2015-08-05 21:00:00" ) #Period after chemicals change an d before calibration

###################################


##################################################################
# Filtering FLUXES
# This generates Site_QAQC.csv FILES
##################################################################

          ###############
          # FILTER 1:
          ###############
          ## What it is filtered here??

          ##    1.rain moments (LE and H20_flux,co2_flux)
          ##    2.wind coming from out of the area of interest (all fluxes except Tau)
          ##    3.Instrumental failure/calibration period (LE and H20_flux,co2_flux)
          ##    4.H, LE and Co2 reliability threshold (H, LE and H20_flux,co2_flux) 
          ##    5.Too high nightime (Rn<20) LE (LE and H20_flux) 
          ##    6.Negative daytime (Rn>50) LE  (LE and H20_flux) 
          ##    7. fluxes when u*< u* threshold (NOT YET)  (all fluxes except Tau)
          ##    8.qc_flux==2 (quality check from Eddypro) (all fluxes)

##FLAGS applied FOR co2_flux FILTERING:
          #Notes: 
          #       Co2_flux needs more work yet to filter by standard deviation and by reasonable limints
          #       Include filtering from licor diagnostic parameters
####################################
#Flaging periods to remove  
#flag=1:flux needs to be removed ,flag=0, flux might be good


############################
#Importing prefiltered data:
SITE<-"C"

if (SITE=="V"){        
  #For el Viejo  
  input<-read.csv( '~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_prefiltered.csv',sep=",",header=TRUE,dec=".")
}else{
  #For Costena
  input<-read.csv( '~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_prefiltered.csv',sep=",",header=TRUE,dec=".")
}


##########################
##Creating flags by site:

input$DATE<-as.POSIXct(input$DATE,origin="1970-01-01 00:00:00",tz="UTC")

Data_QAQC<-input[,1:match('qc_h2o_flux',names(input))]

#When there is rain:

  if (SITE=="V"){        
  #For el Viejo 

#General flags (for several fluxes filtering):
    #When there is rain:
    Data_QAQC$rain_flag<-ifelse(input$Rain>0 & !is.na(input$Rain),1,ifelse(input$imn_rain_distributed>0 & !is.na(input$imn_rain_distributed),1,0))
    #Instrumental failure/calibration periods:
    Data_QAQC$inst_flag<-ifelse(input$DATE>=P1_V[1] & input$DATE<=P1_V[2],1,ifelse(input$DATE>=P2_V[1] & input$DATE<=P2_V[2],1,0))
    #Wind direction coming out of area of interest:
      wind_min<-0
      wind_max<-360
    Data_QAQC$wind_flag<-ifelse(input$wind_dir<wind_min |input$wind_dir>wind_max,1,0)

 #LE specific flags:
    #Too high nightime LE: 
    Data_QAQC$night_LE_flag<-ifelse( input$Rn<20 & input$LE>=50, 1,0)
    #Negative daytime LE: 
    Data_QAQC$day_LE_flag<-ifelse( input$Rn>50 & input$LE<0, 1,0)
    #Out of reasonable boundaries LE:
      LE_MAX<-900
      LE_MIN<--200
    Data_QAQC$lim_LE_flag<-ifelse( input$LE<LE_MIN | input$LE>LE_MAX, 1,0)
 #H specific flags:
    #Out of reasonable boundaries H:
        H_MAX<-800
        H_MIN<--120
    Data_QAQC$lim_H_flag<-ifelse( input$H<H_MIN | input$H>H_MAX, 1,0)
 #Co2 specific flags:
     #Out of reasonable boundaries CO2 FLUX: I AM BEING CONSERVATIVE WITH THIS THRESHOLDS , I THINK THE MAX VALUE SHOULD BO LOWER
        CO2_MAX<-100
        CO2_MIN<--120
    Data_QAQC$lim_co2_flag<-ifelse( input$co2_flux<CO2_MIN | input$co2_flux>CO2_MAX, 1,0)
 
  #u_star to low for turbulence:
        u_star_max<-0
    Data_QAQC$u_star_flag<-ifelse(input$u.<u_star_max ,1,0)
    
  }else{
    
  #For Costena 
    
#General flags (for several fluxes filtering):    
    #When there is rain:
    Data_QAQC$rain_flag<-ifelse(input$Rain>0,1,0) 
    #Instrumental failure/calibration periods:
    Data_QAQC$inst_flag<-ifelse(input$DATE>=P1_C[1] & input$DATE<=P1_C[2],1,0) 
    #Wind direction coming out of area of interest:
    Data_QAQC$wind_flag<-0 #For the moment I am not going to filter by wind at this site
    
#LE specific flags:
    #Too high nightime LE: 
    Data_QAQC$night_LE_flag<-ifelse( input$Rn<20 & input$LE>=50, 1,0)
    #Negative daytime LE: 
    Data_QAQC$day_LE_flag<-ifelse( input$Rn>50 & input$LE<0, 1,0)
    #Out of reasonable boundaries LE:
       LE_MAX<-900
       LE_MIN<--200
    Data_QAQC$lim_LE_flag<-ifelse( input$LE<LE_MIN | input$LE>LE_MAX, 1,0)
#H specific flags:
    #Out of reasonable boundaries H:
          H_MAX<-800
          H_MIN<--100
    Data_QAQC$lim_H_flag<-ifelse( input$H<H_MIN | input$H>H_MAX, 1,0)
#Co2 specific flags:
    #Out of reasonable boundaries CO2 FLUX: I AM BEING CONSERVATIVE WITH THIS THRESHOLDS , I THINK THE MAX VALUE SHOULD BO LOWER
        CO2_MAX<-100
        CO2_MIN<--120
    Data_QAQC$lim_co2_flag<-ifelse( input$co2_flux<CO2_MIN | input$co2_flux>CO2_MAX, 1,0)

#u_star to low for turbulence:
        u_star_max<-0
    Data_QAQC$u_star_flag<-ifelse(input$u.< u_star_max ,1,0)
  }


#######################
#Applying the filters:

##To replace all the NA in the flag columns by 0 (so no filter):
FILTER<-Data_QAQC
FILTER[,grep('_flag',names(FILTER))][is.na(FILTER[,grep('_flag',names(FILTER))])]<-0


#Filtering 
FILTER$Tau_filtered<-ifelse( FILTER$qc_Tau!=2,FILTER$Tau ,-9999) 

FILTER$H_filtered<-ifelse( FILTER$qc_H!=2 & FILTER$wind_flag==0 & 
                             FILTER$lim_H_flag==0 & FILTER$u_star_flag==0,FILTER$H ,-9999) 

FILTER$LE_filtered<-ifelse( FILTER$qc_LE!=2 & FILTER$rain_flag==0 & FILTER$inst_flag==0 & FILTER$wind_flag==0 & 
                              FILTER$lim_LE_flag==0 & FILTER$night_LE_flag==0 & FILTER$day_LE_flag==0 & FILTER$u_star_flag==0,FILTER$LE ,-9999) 

FILTER$h2o_flux_filtered<-ifelse( FILTER$qc_h2o_flux!=2 & FILTER$rain_flag==0 & FILTER$inst_flag==0 & FILTER$wind_flag==0 & 
                                    FILTER$lim_LE_flag==0 & FILTER$night_LE_flag==0 & FILTER$day_LE_flag==0 & FILTER$u_star_flag==0,FILTER$h2o_flux,-9999) 

FILTER$co2_flux_filtered<-ifelse( FILTER$qc_co2_flux!=2 & FILTER$rain_flag==0 & FILTER$inst_flag==0 & FILTER$wind_flag==0 & Data_QAQC$lim_co2_flag==0 & 
                                    FILTER$u_star_flag==0,FILTER$co2_flux,-9999) 


#Counting the number of observations filtered by this filters:
N_Tau_FILT1<-length(FILTER$Tau_filtered[!is.na(FILTER$Tau_filtered) & FILTER$Tau_filtered==-9999])
N_H_FILT1<-length(FILTER$H_filtered[!is.na(FILTER$H_filtered) & FILTER$H_filtered==-9999])
N_LE_FILT1<-length(FILTER$LE_filtered[!is.na(FILTER$LE_filtered) & FILTER$LE_filtered==-9999])
N_H2O_FILT1<-length(FILTER$h2o_flux_filtered[!is.na(FILTER$h2o_flux_filtered) & FILTER$h2o_flux_filtered==-9999])
N_CO2_FILT1<-length(FILTER$co2_flux_filtered[!is.na(FILTER$co2_flux_filtered) & FILTER$co2_flux_filtered==-9999])

N_tot_filt<-c('N_filtered',length(FILTER$date[!is.na(FILTER$date)]),N_Tau_FILT1,N_H_FILT1,N_LE_FILT1,N_H2O_FILT1,N_CO2_FILT1)

N_Tau_QC<-as.vector(table(Data_QAQC$qc_Tau))[3]
N_H_QC<-as.vector(table(Data_QAQC$qc_H))[3]
N_LE_QC<-as.vector(table(Data_QAQC$qc_LE))[3]
N_H2O_Q<-as.vector(table(Data_QAQC$qc_h2o_flux))[3]
N_CO2_QC<-as.vector(table(Data_QAQC$qc_co2_flux))[3]
N_qc<-c("qc_flux_N",NA,N_Tau_QC,N_H_QC,N_LE_QC,N_H2O_Q,N_CO2_QC)

N_flags<-as.vector(colSums (Data_QAQC[,grep('flag',names(Data_QAQC))], na.rm = T))

Flags_name<-names(Data_QAQC[,grep('flag',names(Data_QAQC))])        
Filtered_name<-c('VARIABLE','Total Obs',"Tau","H","LE","co2_flux","h2o_flux")

N_flags<-as.data.frame(cbind(Flags_name,N_flags))
names(N_flags)<-c('Flag_name','N')

Summary_filt<-as.data.frame(rbind(N_qc,N_tot_filt))
names(Summary_filt)<-Filtered_name

#Saving a summary of filter:
if (SITE=="V"){        
  #For el Viejo          
  
  write.csv(Summary_filt,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_filtering_summary','.csv',sep=''),row.names=FALSE)   
  write.csv(N_flags,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_flags_summary','.csv',sep=''),row.names=FALSE)   
  
}else{
  
  #For Costena         
  write.csv(Summary_filt,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_filtering_summary','.csv',sep=''),row.names=FALSE)   
  write.csv(N_flags,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_flags_summary','.csv',sep=''),row.names=FALSE)   
}



#Adding Filtered fluxes to Data_QAQC DATASET:
Data_QAQC$Tau_filtered<-ifelse(FILTER$Tau_filtered==-9999,NA,FILTER$Tau_filtered)
Data_QAQC$H_filtered<-ifelse(FILTER$H_filtered==-9999,NA,FILTER$H_filtered)
Data_QAQC$LE_filtered<-ifelse(FILTER$LE_filtered==-9999,NA,FILTER$LE_filtered)
Data_QAQC$co2_flux_filtered<-ifelse(FILTER$co2_flux_filtered==-9999,NA,FILTER$co2_flux_filtered)
Data_QAQC$h2o_flux_filtered<-ifelse(FILTER$h2o_flux_filtered==-9999,NA,FILTER$h2o_flux_filtered)

#Adding Filtered fluxes to Site_Filtered_files and choosing variables to include in this final dataset:
final<-cbind( input[,1:match('qc_h2o_flux',names(input))],Data_QAQC[,match('Tau_filtered',names(Data_QAQC)):match('h2o_flux_filtered',names(Data_QAQC))],
              input[,match('co2_molar_density',names(input)):match('T.',names(input))],input[,match('co2_signal_strength_7500_mean',names(input)):ncol(input)])


#Saving a QA/QC file:
if (SITE=="V"){        
  #For el Viejo          
  
  write.csv(Data_QAQC,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_Fluxes_QAQC','.csv',sep=''),row.names=FALSE)   
  write.csv(final,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/VIEJO_FILTERED','.csv',sep=''),row.names=FALSE)   
  
  
}else{
  
  #For Costena         
  write.csv(Data_QAQC,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_Fluxes_QAQC','.csv',sep=''),row.names=FALSE)   
  write.csv(final,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/COSTENA_FILTERED','.csv',sep=''),row.names=FALSE)
}


#PLOTING FILTERING:

par(mfrow=c(5,1))

plot(Data_QAQC$Tau,type='l',ylab="Tau", main='Filter1')
points(Data_QAQC$Tau_filtered,col='red',type='l')

plot(Data_QAQC$H,type='l',ylab="H", main='Filter1')
points(Data_QAQC$H_filtered,col='red',type='l')

plot(Data_QAQC$LE,type='l',ylab="LE")
points(Data_QAQC$LE_filtered,col='red',type='l')

plot(Data_QAQC$h2o_flux,type='l',ylab="H2Oflux")
points(Data_QAQC$h2o_flux_filtered,col='red',type='l')

plot(Data_QAQC$co2_flux,type='l',ylab="CO2 flux")
points(Data_QAQC$co2_flux_filtered,col='red',type='l')



###########################
##Estimating U* threshold:

Data_QAQC$u_star<-input$u.

#PLOT TO ESTIMATE A U* THRESHOLD VALUE FOR FILTERING 
YEAR<-2014

#subset<-Data_QAQC[ Data_QAQC$year==YEAR,] #Here it is necessary to chose one period for the analysis
subset<-final[final$Rn<20,]      #To analize only nightime data

max_u_star<-round(max(subset$u.,na.rm=T),digits=2)
min_u_star<-round(min(subset$u.,na.rm=T),digits=2)
max_u_star
min_u_star
resolution<-0.05  #You can change the resolution changing this parameter
num_steps<-round(max_u_star/resolution,digits=0)
steps<-seq(from=0, to=max_u_star+resolution,by=resolution) 

#subset$u_star_class<-cut(subset$u_star, steps, include.lowest =T)
subset$u_star_class<-cut(subset$u., steps, include.lowest =T)
subset$u_star_bin<-ifelse( is.na(subset$u_star_class),NA,match(subset$u_star_class,levels(subset$u_star_class)))

#Estimating the mean and SD of all fluxes in the selected u* bins or classes
mean.LE <- tapply(subset$LE, subset$u_star_class, mean,na.rm = TRUE)
mean.H <- tapply(subset$H, subset$u_star_class, mean,na.rm = TRUE)
mean.CO2 <- tapply(subset$co2_flux, subset$u_star_class, mean,na.rm = TRUE)
mean.H2O <- tapply(subset$h2o_flux, subset$u_star_class, mean,na.rm = TRUE)

sd.LE <- tapply(subset$LE, subset$u_star_class, sd,na.rm = TRUE)
sd.H <- tapply(subset$H, subset$u_star_class, sd,na.rm = TRUE)
sd.CO2 <- tapply(subset$co2_flux, subset$u_star_class, sd,na.rm = TRUE)
sd.H2O <- tapply(subset$h2o_flux, subset$u_star_class, sd,na.rm = TRUE)

mean.LE_filt <- tapply(subset$LE_filtered, subset$u_star_class, mean,na.rm = TRUE)
mean.H_filt <- tapply(subset$H_filtered, subset$u_star_class, mean,na.rm = TRUE)
mean.CO2_filt <- tapply(subset$co2_flux_filtered, subset$u_star_class, mean,na.rm = TRUE)
mean.H2O_filt <- tapply(subset$h2o_flux_filtered, subset$u_star_class, mean,na.rm = TRUE)


sd.LE_filt <- tapply(subset$LE_filtered, subset$u_star_class, sd,na.rm = TRUE)
sd.H_filt <- tapply(subset$H_filtered, subset$u_star_class, sd,na.rm = TRUE)
sd.CO2_filt <- tapply(subset$co2_flux_filtered, subset$u_star_class, sd,na.rm = TRUE)
sd.H2O_filt <- tapply(subset$h2o_flux_filtered, subset$u_star_class, sd,na.rm = TRUE)

####################
#PLOT U STAR BINNED
####################

par(mfrow=c(1,3))
#LE
flux<-'LE'
var<-match(flux,names(subset))

plot(subset$u_star_bin,subset[,var],xlab="U* magnitude class",cex=0.5,pch=4,col='grey',ylab=flux,ylim=c(LE_MIN,LE_MAX),main=flux)
x<-1:length(levels(subset$u_star_class))
points(mean.LE, type = "b",pch=20)
arrows(x, mean.LE-sd.LE, x, mean.LE+sd.LE, length=0.05, angle=90, code=3,lwd=2)
points(mean.LE_filt, type = "b",col='red',)
arrows(x, mean.LE_filt-sd.LE_filt, x, mean.LE_filt+sd.LE_filt, length=0.05, angle=90, code=3,col='red',lwd=2)
legend('topleft',c("mean and sd by u* category from raw data","mean and sd by u* category after filtering the other criteria","measured fluxes by u* category"),text.col=c('black','red','grey'),bty='n')
#To see what is the bin of interest:
sel_class<-5
levels(subset$u_star_class)[sel_class]

#CO2
flux<-'co2_flux'
var<-match(flux,names(subset))

plot(subset$u_star_bin,subset[,var],xlab="U* magnitude class",cex=0.5,pch=4,col='grey',ylab=flux,ylim=c(CO2_MIN,CO2_MAX),main=flux)
x<-1:length(levels(subset$u_star_class))
points(mean.CO2, type = "b",pch=20)
arrows(x, mean.CO2-sd.CO2, x, mean.CO2+sd.CO2, length=0.05, angle=90, code=3,lwd=2)
points(mean.CO2_filt, type = "b",col='red',)
arrows(x, mean.CO2_filt-sd.CO2_filt, x, mean.CO2_filt+sd.CO2_filt, length=0.05, angle=90, code=3,col='red',lwd=2)

#To see what is the bin of interest:
sel_class<-5
levels(subset$u_star_class)[sel_class]

#H20
flux<-'h2o_flux'
var<-match(flux,names(subset))
H2O_MIN<--5
H2O_MAX<-20
plot(subset$u_star_bin,subset[,var],xlab="U* magnitude class",cex=0.5,pch=4,col='grey',ylab=flux,ylim=c(H2O_MIN,H2O_MAX),main=flux)
x<-1:length(levels(subset$u_star_class))
points(mean.H2O, type = "b",pch=20)
arrows(x, mean.H2O-sd.H2O, x, mean.H2O+sd.H2O, length=0.05, angle=90, code=3,lwd=2)
points(mean.H2O_filt, type = "b",col='red',)
arrows(x, mean.H2O_filt-sd.H2O_filt, x, mean.H2O_filt+sd.H2O_filt, length=0.05, angle=90, code=3,col='red',lwd=2)

#To see what is the bin of interest:
sel_class<-5
levels(subset$u_star_class)[sel_class]

  #H
  #flux<-'H'
  #var<-match(flux,names(subset))

  #plot(subset$u_star_bin,subset[,var],xlab="U* magnitude class",cex=0.5,pch=4,col='grey',ylab=flux,ylim=c(H_MIN,H_MAX),main=flux)
  #x<-1:length(levels(subset$u_star_class))
  #points(mean.H, type = "b",pch=20)
  #arrows(x, mean.H-sd.H, x, mean.H+sd.H, length=0.05, angle=90, code=3,lwd=2)
  #points(mean.H_filt, type = "b",col='red',)
  #arrows(x, mean.H_filt-sdH_filt, x, mean.H_filt+sd.H_filt, length=0.05, angle=90, code=3,col='red',lwd=2)

#To see what is the bin of interest:
sel_class<-5
levels(subset$u_star_class)[sel_class]









##################################################################
# Preliminary analisys prefiltered data (pLOTS)
##################################################################

if (SITE=="V"){        
  #For el Viejo  
  input<-read.csv( '~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_prefiltered.csv',sep=",",header=TRUE,dec=".")
}else{
  #For Costena
  input<-read.csv( '~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_prefiltered.csv',sep=",",header=TRUE,dec=".")
}

CO2_F_LIM<-c(-70,70)
H20_F_LIM<-c(-10,40)
LE_LIM<-c(-300,900)
H_LIM<-c(-300,600)
RAIN_LIM<-c(0,5)


#Ploting fluxes:
#BY YEAR
YEAR<-2014

par(mfrow=c(3,1))
plot(input[ input$year==YEAR,match("DATE",names(input))],input[input$year==YEAR,match("co2_flux",names(input))],
     ylab='CO2_flux (umol m-2 s-1)',xlab='DATE',main=YEAR,type='l',ylim=c(-70,70))

plot(input[ input$year==YEAR,match("DATE",names(input))],input[input$year==YEAR,match("h2o_flux",names(input))],
     ylab='H20 flux (mmol s-1 m-2)',xlab='DATE',type='l',ylim=c(-10,40)) 


plot(input[input$year==YEAR,match("DATE",names(input))],input[input$year==YEAR,match("LE",names(input))],
     ylab='LE (W m-2)',xlab='DATE',type='l',ylim=LE_LIM) 


#By month of data:
MONTH<-c(9,12)
obs1<-min(input[ input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match('obs',names(input))])
obs2<-max(input[ input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match('obs',names(input))])

par(mfrow=c(3,1))
plot(input[ input$obs>=obs1 & input$obs<=obs2,match("jday",names(input))],
     input[  input$obs>=obs1 & input$obs<=obs2,match("co2_flux",names(input))],
     ylab='CO2_flux (umol m-2 s-1)',xlab='DATE',type='l',ylim=CO2_F_LIM)#,main=MONTH
    


plot(input[ input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match("jday",names(input))],
     input[ input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match("H",names(input))],
     ylab='H (W m-2)',xlab='DATE',type='l',ylim=H_LIM) 


plot(input[  input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match("jday",names(input))],
     input[ input$year==YEAR & input$month_local>=MONTH[1] & input$month_local<=MONTH[2],match("ET",names(input))],
     ylab='LE (W m-2)',xlab='DATE',type='l',ylim=LE_LIM) 


#Ploting LE VS U*
par(mfrow=c(3,1))
plot(input$u.,input$LE,xlab="u*",ylab='LE', main="u* preliminary analysis")
plot(input$u.,input$LE,xlab="u*",ylab='LE',ylim=c(-300,900))
plot(input[input$qc_LE!=2,match("LE",names(input))],input[input$qc_LE!=2,match("u.",names(input))],ylab="u*",xlab='LE') #
#plot(input[input$u.<0.2,match("u.",names(input))],input[input$u.<0.2,match("LE",names(input))],xlab="u*",ylab='LE')
#plot(input[input$u.<0.2 &input$qc_LE!=2,match("u.",names(input))],input[input$u.<0.2 & input$qc_LE!=2,match("LE",names(input))],xlab="u*",ylab='LE')
dev.off()
#NOTE:I don't see a decrease in LE quality depending on u*, no more dispersion when U*<0.20

#CHECKING DIAGNOSTIC VARIABLES
max(input$chopper_LI.7500,na.rm=T)
max(input$detector_LI.7500,na.rm=T)
max(input$pll_LI.7500,na.rm=T)
max(input$sync_LI.7500,na.rm=T)

input[input$pll_LI.7500>100 & !is.na(input$chopper_LI.7500),1:15]


#NOTE:They all look fine for El Viejo, at la Costenya there are a few moments when the diagnostic variables are >100, when Licor chemicals were changed and Tsonic units were changed

#CHECKING spikes VARIABLES
plot(input$h2o_spikes,input$LE,xlab="u_spikes",ylab='LE')
#Percentages of spikes registered:
100*max(input$u_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data
100*max(input$v_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data
100*max(input$w_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data
100*max(input$ts_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data
100*max(input$co2_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data
100*max(input$h2o_spikes,na.rm=T)/36000 #Max percentage of spikes included in the data

##Ploting LE VS CO2 STRENGTH SIGNAL*

plot(input$co2_signal_strength_7500_mean,input$LE,xlab="co2_signal_strength",ylab='LE')
plot(input[input$co2_signal_strength_7500_mean<50  ,match("hour_local",names(input))],input[input$co2_signal_strength_7500_mean<50 ,match("LE",names(input))],
     xlab="Hour of day (local time)",ylim=c(-700,700),ylab='LE',main='LE values when SIGNAL STRENGTH is <50 by time of day')
abline(h=0)
#NOTE:SIGNAL STRENGTH <40% SEEMS TO RESULT IN UNUSUAL NEGATIVE VALUES OF le

##Ploting nightime LE

#plot(input[input$hour_local>19 & input$hour_local<5,match("LE",names(input))],ylab='LE',ylim=5000)  #it doesn't work and i DONT KNOW WHY
par(mfrow=c(2,2))
plot(input[input$hour_local>19 ,match("LE",names(input))],ylab='LE',xlab='Observation',ylim=c(-300,600), main='From 7pm to midnight (local time)')
plot(input[input$hour_local<5 ,match("LE",names(input))],ylab='LE',xlab='Observation',ylim=c(-300,600), main='From midnight to 5am (local time)')
plot(input[input$hour_local>19 ,match("u.",names(input))],input[input$hour_local>19 ,match("LE",names(input))],ylab='LE',xlab="U*") #adding u*to nightime analysis
abline(v=0.25)
plot(input[input$hour_local<5 ,match("u.",names(input))],input[input$hour_local<5 ,match("LE",names(input))],ylab='LE',xlab="U*") #adding u*to nightime analysis
abline(v=0.25)
dev.off()

#REMOVING LE_qc=2
par(mfrow=c(2,1))
plot(input[input$hour_local>19 & input$qc_LE!=2 ,match("u.",names(input))],input[input$hour_local>19 & input$qc_LE!=2,match("LE",names(input))],ylab='LE',xlab="U*", main='Removing QC=2 From 7pm to midnight (local time)') #adding u*to nightime analysis
plot(input[input$hour_local<5 & input$qc_LE!=2,match("u.",names(input))],input[input$hour_local<5 & input$qc_LE!=2,match("LE",names(input))],ylab='LE',xlab="U*", main='Removing QC=2 From 7pm to midnight (local time)') #adding u*to nightime analysis
dev.off()

#Ploting time frequency of QC=2 for main fluxes:
par(mfrow=c(2,2))
plot(input[input$qc_LE==2 ,match("hour_local",names(input))],input[input$qc_LE==2 ,match("LE",names(input))],ylab='LE',xlab='time of day (local time)',main='QC_LE=2 conditions')
plot(input[input$qc_Tau==2 ,match("hour_local",names(input))],input[input$qc_Tau==2 ,match("Tau",names(input))],ylab='Tau',xlab='time of day (local time)',main='QC_Tau=2 conditions')
plot(input[input$qc_H==2 ,match("hour_local",names(input))],input[input$qc_H==2 ,match("H",names(input))],ylab='H',xlab='time of day (local time)',main='QC_H=2 conditions')
plot(input[input$qc_co2_flux==2 ,match("hour_local",names(input))],input[input$qc_co2_flux==2 ,match("co2_flux",names(input))],ylab='co2_flux',xlab='time of day (local time)',main='QC_co2_flux=2 conditions')
dev.off()

#Ploting le values for those conditions when u*<0.2 by time:
plot(input[input$u.<0.2 ,match("hour_local",names(input))],input[input$u.<0.2 ,match("LE",names(input))],ylab='LE for U*<0.2',xlab='time of day (local time)',main='U*<0.2 conditions')
dev.off()

#Checking signal strength time tendency

plot(input$co2_signal_strength_7500_mean,ylab='signal strength (%)',xlab='DAY OF YEAR',xaxt='n')
ticks<-seq(from=1,to=nrow(input), by=round((nrow(input)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(input$DOY[i],digits=0),las=2)
}


#Cheking period of potential bee nest in Licor

last_visit<-as.POSIXct(strptime("2014-12-04 00:00:00",format="%Y-%m-%d %H:%M",tz="UTC"))
fixed<-as.POSIXct(strptime(P1_V[2],format="%Y-%m-%d %H:%M",tz="UTC"))
set<-input
set$DATE<-as.POSIXct(set$DATE,origin="1970-01-01 00:00:00",tz="UTC")

subset<-set[set$DATE>last_visit & set$DATE<fixed,]

par(mfrow=c(2,2))
plot(subset$co2_signal_strength_7500_mean,ylab='signal strength (%)',xlab='DAY OF YEAR',xaxt='n')
ticks<-seq(from=1,to=nrow(subset), by=round((nrow(subset)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(subset$DOY[i],digits=0),las=2)
}


plot(subset$co2_flux,ylab='co2_flux',xlab='DAY OF YEAR',xaxt='n',type='l',ylim=CO2_F_LIM)
ticks<-seq(from=1,to=nrow(subset), by=round((nrow(subset)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(subset$DOY[i],digits=0),las=2)
}

plot(subset$Rain,ylab='rain',xlab='DAY OF YEAR',xaxt='n',type='h',col='blue',ylim=RAIN_LIM)
lines(subset$imn_rain_distributed,ylab='rain',xlab='DAY OF YEAR',xaxt='n',type='h',col='darkgreen')
legend('topleft',c('imn rain','Vaisala rain'),text.col=c('darkgreen','blue'))
  ticks<-seq(from=1,to=nrow(subset), by=round((nrow(subset)/30)))
  for (i in ticks){ 
    axis(1,at=i, label= round(subset$DOY[i],digits=0),las=2)
  }
  

plot(subset$h2o_flux,ylab='H2O_flux',xlab='DAY OF YEAR',xaxt='n',type='l',ylim=H20_F_LIM)
ticks<-seq(from=1,to=nrow(subset), by=round((nrow(subset)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(subset$DOY[i],digits=0),las=2)
}


plot(subset$h2o_spikes,ylab='h2o_spikes',xlab='DAY OF YEAR',xaxt='n',type='l')
ticks<-seq(from=1,to=nrow(subset), by=round((nrow(subset)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(subset$DOY[i],digits=0),las=2)
}



plot(input$h2o_spikes,ylab='h2o_spikes',xlab='DAY OF YEAR',xaxt='n',type='l')


#Rain Signal strength effect:

par(mfrow=c(2,1))

plot(input$DATE,input$co2_signal_strength_7500_mean,ylab='signal strength (%)',xlab='DATE',main=ifelse(SITE=='V','El Viejo Site','Costena site'))


plot(input$Rain,ylab='rain',xlab='DAY OF YEAR',xaxt='n',type='h',col='blue',ylim=c(0,45))
lines(input$imn_rain_distributed,ylab='rain',xlab='DAY OF YEAR',xaxt='n',type='h',col='darkgreen')
legend('topleft',c('imn rain','Vaisala rain'),text.col=c('darkgreen','blue'))
ticks<-seq(from=1,to=nrow(input), by=round((nrow(input)/30)))
for (i in ticks){ 
  axis(1,at=i, label= round(input$DOY[i],digits=0),las=2)
}



par(mfrow=c(3,1))
plot(input[input$Rain>0 & is.na(input$Rain)==F  ,match("obs",names(input))],input[input$Rain>0 & is.na(input$Rain)==F  ,match("co2_signal_strength_7500_mean",names(input))],
     ylab='Signal strength',xlab='obs',main='Signal strength when it rains',ylim=c(0,175))

plot(input[input$Rain>0 & is.na(input$Rain)==F ,match("obs",names(input))],input[input$Rain>0 & is.na(input$Rain)==F ,match("Rain",names(input))],ylab='Rain',xlab='Obs',type='h',col='blue',ylim=c(0,10))
     

plot(input[input$Rain>0 & is.na(input$Rain)==F,match("Rain",names(input))],input[input$Rain>0 & is.na(input$Rain)==F ,match("co2_signal_strength_7500_mean",names(input))],ylab='Signal strength',
     xlab='rain',main='Signal strength when it rains',ylim=c(0,175))
dev.off()
#It seems that signal strength is only affected when rain is enough >2mm or very frecuent

par(mfrow=c(3,1))
plot(input[input$Rain>2 & is.na(input$Rain)==F  ,match("obs",names(input))],input[input$Rain>2 & is.na(input$Rain)==F  ,match("co2_signal_strength_7500_mean",names(input))],
     ylab='Signal strength',xlab='obs',main='Signal strength when it Rain>2mm',ylim=c(0,175))
plot(input[input$Rain>2 & is.na(input$Rain)==F ,match("obs",names(input))],input[input$Rain>2 & is.na(input$Rain)==F ,match("Rain",names(input))],ylab='Rain',xlab='Obs',type='h',col='blue',ylim=c(0,10))
plot(input[input$Rain>2 & is.na(input$Rain)==F,match("Rain",names(input))],input[input$Rain>2 & is.na(input$Rain)==F ,match("co2_signal_strength_7500_mean",names(input))],ylab='Signal strength',
     xlab='rain',main='Signal strength when Rain>2mm',ylim=c(0,175))
dev.off()


#Choosing some  signal strength thresholds for filtering
plot(input[input$Rain==0 & is.na(input$Rain)==F  ,match("obs",names(input))],input[input$Rain==0 & is.na(input$Rain)==F  ,match("co2_signal_strength_7500_mean",names(input))],
     ylab='Signal strength',xlab='obs',main='Signal strength when Rain=0',ylim=c(0,175))
abline(h=135)
abline(h=90)

#sIGNAL STRENGTH CAN BE REDUCED WHEN there is not rain at all. Should we eliminate all fluxes (co2 and h20)
#when licor is so dirty??? I think so actually....










##################################################################
##Chunk of code from Mark to deal with wind direction filtering:
#########################################################

#The 81000 is oriented with 
# u-axis aligned east-west; +u values = wind from the east 
# v-axis aligned north-south; +v values = wind from the north
# Wind from below (updraft) = +w. 

CRdata<-input

CRdata$month<-as.numeric(format(CRdata$DATE,"%m"))

#prep weir sonic
#CRdata$weir.windSpeed <- sqrt(CRdata$u_rot^2+CRdata$v_rot^2)
#CRdata$weir.windDir <- atan2(CRdata$u_rot,CRdata$v_rot)/(pi/180)
#CRdata$weir.windDir <- ifelse(CRdata$weir.windDir<0,360+CRdata$weir.windDir,CRdata$weir.windDir)
#CRdata$weir.windSpeed <- ifelse(is.na(CRdata$weir.windDir),NA,CRdata$weir.windSpeed)
#weir.wind <- subset(CRdata,!is.na(CRdata$weir.windSpeed))

#prep tower sonic
CRdata$tower.windSpeed <- sqrt(CRdata$u_rot^2+CRdata$v_rot^2)
#CRdata$tower.windDir <- atan2(CRdata$u_rot,CRdata$v_rot)/(pi/180) #it only provides 90 or NA AND i DON'T KNOW WHY
CRdata$tower.windDir <- ifelse(CRdata$tower.windDir<0,360+CRdata$tower.windDir,CRdata$tower.windDir)
CRdata$tower.windSpeed <- ifelse(is.na(CRdata$tower.windDir),NA,CRdata$tower.windSpeed)
tower.wind <- subset(CRdata,!is.na(CRdata$tower.windSpeed))

#Clean wind speed and direction from 40m tower
#V:/2012/CR/Climate/Clean/wind_direction_weighted_40m
#V:/2012/CR/Climate/Clean/wind_speed_40m

#use package oce
library(oce)
#weir.rose <- as.windrose(weir.wind$U.weir,weir.wind$V.weir,dtheta=15)
#plot(weir.rose)

par(mfrow=c(1,2))
tower.rose <- as.windrose(tower.wind$u_rot,tower.wind$v_rot,dtheta=15)
plot(tower.rose,main="rotated")
mtext(text='Rotated wind components',side=3)


tower.rose_unrotated <- as.windrose(tower.wind$u_unrot,tower.wind$v_unrot,dtheta=15)
plot(tower.rose_unrotated)
mtext(text='Unrotated wind components',side=3)


#windDir_mean <- aggregate(weir.wind$weir.windDir, list(weir.wind$mon.daypd), mean, na.rm = TRUE) # mean wind direction by month
#windDir_sd <- aggregate(weir.wind$weir.windDir, list(weir.wind$mon.daypd), sd, na.rm = TRUE) # mean wind direction by month

Vaisala.windDir_mean <- aggregate(tower.wind$WD, list(tower.wind$month,tower.wind$year), mean, na.rm = TRUE) # mean wind direction by month
Vaisala.windDir_sd <- aggregate(tower.wind$WD, list(tower.wind$month,tower.wind$year), sd, na.rm = TRUE) # mean wind direction by month


tower.windDir_mean <- aggregate(tower.wind$wind_dir, list(tower.wind$month,tower.wind$year), mean, na.rm = TRUE) # mean wind direction by month
tower.windDir_sd <- aggregate(tower.wind$wind_dir, list(tower.wind$month,tower.wind$year), sd, na.rm = TRUE) # mean wind direction by month

WD<-as.data.frame(cbind(tower.windDir_mean,(tower.windDir_sd)$x,(Vaisala.windDir_mean)$x,(Vaisala.windDir_sd)$x ) )
names(WD)<-c('month','year','tower_mean_WD_rotated','tower_sd_WD_rotated','Vaisala_mean_WD_rotated','Vaisala_sd_WD_rotated')

### Nov 1, 2012 -- just noticed that openair has some nice routines for wind roses, that includes [pg 80-81 of manual]
##    a percentile concentrations by season and day/nighttime - this could be revised to month and day/nighttime
##    percentileRose(mydata, type = c("season", "daylight"), pollutant = "o3", col = "brewer1")



