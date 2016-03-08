
########################################
# Script design Notes:

#This script is written to proceed to melt CR1000 AND EC data TO GENERATE A PREFILTERED DATASET
# It can be used for both sites


####################################
#INPUT FILE FOR THIS SCRIPT:  "Site_fluxes_compiled.csv" and "CR1000_Viejo_comp_FiltGAPfil.csv"

#OUTPUT FILES: "Site_prefiltered" in the folder:Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA


###################################################
#PENDING TASKS to add:

#ESTIMATE SOIL HEAT FLUX (G)
#DETERMINE A SITE SPECIFIC U* THRESHOLD FOR FILTERING


#########################################################
#Coding notes:
#
#########################################################


# clear memory
rm(list=ls())


#############################################################################################
#CODE:

####
# 1. Reading all the files
###


#############################################################################################
#############################################################################################
#Choose a Site:
SITE<-"C"   #Here you will need to specify the site V for el Viejo, and C for La Costenya
#############################################################################################
#############################################################################################


if (SITE=="V"){        
  #For el Viejo          
  
  Fluxes<-read.csv( "~/Documents/UBC/Futuragua_research/Data management/EDDYPRO_OUTPUTS/Viejo_fluxes_compiled.csv",sep=",",header=TRUE,dec=".") #from I Mac
  CR1000<-read.csv('~/Documents/UBC/Futuragua_research/Data management/CR1000_processing_results/El Viejo/CR1000_Viejo_comp_FiltGAPfil.csv',sep=",",header=TRUE,dec=".")
  
}else{
  
  #For Costena         
  
  Fluxes<-read.csv( '~/Documents/UBC/Futuragua_research/Data management/EDDYPRO_OUTPUTS/Costena_fluxes_compiled.csv',sep=",",header=TRUE,dec=".") #from I Mac
  CR1000<-read.csv('~/Documents/UBC/Futuragua_research/Data management/CR1000_processing_results/Costena/CR1000_costena_comp_FiltGAPfil.csv',sep=",",header=TRUE,dec=".")
}

##################################################################
#2. MELTING CR1000 AND PROCESSED FLUXES FILES
# This generates Site_prefiltered.csv files
##################################################################

#Looking for the time matching between CR1000 and Processed fluxes (start_date to end_date):
Fluxes$DATE<-as.POSIXct(Fluxes$DATE,origin="1970-01-01 00:00:00",tz="UTC")
CR1000$DATE<-as.POSIXct(CR1000$DATE,origin="1970-01-01 00:00:00",tz="UTC")
Fluxes$obs<-c(1:nrow(Fluxes))

start_date<-Fluxes$DATE[1]

if (Fluxes$DATE[nrow(Fluxes)]>CR1000[nrow(CR1000),1]) { end_date<-CR1000[nrow(CR1000),1] 
}else{
  end_date<-Fluxes$DATE[nrow(Fluxes)]
}

start_date  #start of matching period
end_date    #end of matching period

F_start<-Fluxes[Fluxes$DATE==start_date,match('obs',names(Fluxes))] 
F_end<-Fluxes[Fluxes$DATE==end_date,match('obs',names(Fluxes))]

C_start<-CR1000[CR1000$DATE==start_date,match('obs',names(CR1000))]
C_end<-CR1000[CR1000$DATE==end_date,match('obs',names(CR1000))]

C_end-C_start==F_end-F_start



input_cont<-cbind(Fluxes[F_start:F_end,1:(ncol(Fluxes)-1)],CR1000[C_start:C_end,2:ncol(CR1000)])

#TO ORDER THE INPUT FOR HAVING ALL THE TIME COLUMNS FIRST:
#The if function by site is important becauset the names of variables in CR1000 are not the same for both sites

if (SITE=="V"){  
  
  #For el Viejo  
  input<-cbind(Fluxes[F_start:F_end,match('DATE',names(Fluxes)):match('daytime',names(Fluxes))],CR1000[C_start:C_end,match('year',names(CR1000)):match('minutes',names(CR1000))],
               Fluxes[F_start:F_end,match('Year_local',names(Fluxes)):match('time_local',names(Fluxes))],Fluxes[F_start:F_end,match('file_records',names(Fluxes)):match('co2_signal_strength_7500_mean',names(Fluxes))],
               CR1000[C_start:C_end,match('Hukse_UP_8cm',names(CR1000)):match("Vsupply_cano_Avg",names(CR1000))], CR1000[C_start:C_end,match('imn_rain_distributed',names(CR1000)):match("imn_rain",names(CR1000))])
}else{
  
  #For Costena         
  
  input<-cbind(Fluxes[F_start:F_end,match('DATE',names(Fluxes)):match('daytime',names(Fluxes))],CR1000[C_start:C_end,match('year',names(CR1000)):match('minutes',names(CR1000))],
               Fluxes[F_start:F_end,match('Year_local',names(Fluxes)):match('time_local',names(Fluxes))],Fluxes[F_start:F_end,match('file_records',names(Fluxes)):match('co2_signal_strength_7500_mean',names(Fluxes))],
               CR1000[C_start:C_end,match('Hukse_0_2.cm',names(CR1000)):(ncol(CR1000)-1)])
  
}

head(input)
input$obs<-c(1:nrow(input))


if (SITE=="V"){        
  #For el Viejo          
  
  write.csv(input,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Viejo_prefiltered','.csv',sep=''),row.names=FALSE)   
  
}else{
  
  #For Costena         
  write.csv(input,paste('~/Documents/UBC/Futuragua_research/Data management/FINAL_PROCESSED_DATA/Costena_prefiltered','.csv',sep=''),row.names=FALSE)   
}




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


