##from __future__ import division
##from math import exp, sin, cos, sqrt, acos
##from math import pi

get_VPD <- function(T_min, T_max){
    #"""calculate VPD from min and max temperature
    #"""
    VPDx = 6.1078 * exp(17.269 * T_max / (237.3 + T_max))
    VPDn = 6.1078 * exp(17.269 * T_min / (237.3 + T_min))
    res = (VPDx - VPDn) / 2
    return(res)
}

get_day_length <- function(lat, month){
    #"""get length of day time at a specified latitude on a given month
    #"""
    list_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    day_in_year = sum(list_days[1:month]) - 15
    s_lat= sin(pi * lat / 180)
    c_lat = cos(pi * lat / 180)
    sin_dec = 0.4 * sin(0.0172 * (day_in_year - 80))
    cosH0 = -sin_dec * s_lat / (c_lat * sqrt(1 - (sin_dec) ^ 2))
    if(cosH0 > 1){
        res = 0
    } else if(cosH0 < -1){
        res = 1
    } else {
        res = acos(cosH0) / pi
    }
    return(86400 * res)
}

get_days_in_month <- function(month){
    #"""get number of days on a given month
    #"""
    list_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return(list_days[month])
}

get_stand_age <- function(lat, InitialYear, InitialMonth, YearPlanted, MonthPlanted, EndAge){

    # This procedure gets the starting month and intial stand age

    # Determine starting month for each year
    if(InitialMonth == 0){
        if(lat > 0){
            InitialMonth = 0
        } else {
            InitialMonth = 6
        }
    }
    if(MonthPlanted == 0){
        if(lat > 0){
            MonthPlanted = 0
        } else {
            MonthPlanted = 6
        }
    }

    # Assign initial stand age
    if(InitialYear < YearPlanted){
        InitialYear = YearPlanted + InitialYear
    }
    stand_age = (InitialYear + InitialMonth / 12.0) - (YearPlanted + MonthPlanted / 12.0)

    # get and check StartAge
    StartAge = as.integer(stand_age)
    if(StartAge < 0){
        str_message = "Invalid age limits. The starting age %s must be greater than 0!''' % StartAge"
        stop(str_message)
    } else if(StartAge > EndAge){
        str_message = "Invalid age limits. The starting age %s is greater than the ending age %s.''' % (StartAge, EndAge)"
        stop(str_message)
    }

    stand_age_list <- list(stand_age = stand_age, StartAge = StartAge, InitialYear = InitialYear, InitialMonth = InitialMonth, MonthPlanted = MonthPlanted)
    return(stand_age_list)
}

#if __name__ == '__main__':
#    pass

## FS599ECO script to calculate required meteorological variables for 3-PG from Tmax and Tmin mean monthly measurements
# from Waring Excel file: Frost days/month = Tmin * (-2.00) + 11.6; if <0, then 0 days of frost
# Note: if Tmin is <-9, then set Frost days to 30

frost_days <- function(Tmin){
  if (Tmin < -9) { 
    frost_days = 30
  } 
  if (Tmin >= 7) {
    frost_days = 0
  }
  else  {
    frost_days = Tmin * (-2.00) + 11.6
    frost_days = as.integer(frost_days)
  }
  return(frost_days)
}
# par(mfrow = c(2, 1))
# Tmin <- c(-5:15)
# frost_days = Tmin * (-2.00) + 11.6
# plot(Tmin,frost_days, type="l")

# now calculate mean monthly (daytime?) VPD from Tmax and Tmin
# VPD = (Sat Vap Pres at Tmax - Sat Vap Pres at Tmin)*0.62
##vpd <- function(Tmax, Tmin){
##  esat_max = 6.112*exp(17.67*Tmax/(Tmax + 243.5)) #[mbar or hPa]
##  esat_min = 6.112*exp(17.67*Tmin/(Tmin + 243.5)) #[mbar or hPa]
##  vpd = 0.62*(esat_max - esat_min) #[mbar or hPa]
##  vpd <- round(vpd,2)
##  return(vpd)
##}
# Tmin <- c(-5:15)
# Tmax <- c(5:25)
# DTR <- Tmax - Tmin
# esat_max = 6.112*exp(17.67*Tmax/(Tmax + 243.5)) #[mbar or hPa]
# esat_min = 6.112*exp(17.67*Tmin/(Tmin + 243.5)) #[mbar or hPa]
# vpd = 0.62*(esat_max - esat_min) 
# plot(Tmax, vpd, type="l")


## FS599ECO script to calculate radiation inputs at the canopy top 
# 3-PG requires mean monthly solar irradiance in MJ/m2/day
# formulas from this reference
# [Coops, N.C., R.H. Waring, and J.B. Moncreiff. 2000. Estimating mean monthly incident solar radiation 
# on horizontal and inclined slopes from mean monthly temperature extremes. Int. J. Biometerology 44:204-211.
# Formula for Clear Sky Transmissivity = (Elev *0.0001+1.0143)*0.65
# New Formula for B = 0.031+0.201EXP(-0.185 Temp.Diff)
radiation <- function(doy,tmax,tmin,latitude,elevation,aspect,slope) {
  #dtr <- Tmax - Tmin #; print(dtr) #[C] mean diurnal temperature range for each month (Tmax-Tmin)
  
  #convert latitude to radians for functions and abbreviate variable names
  #other universal constants
  rad2deg <- 180/pi # convert from radians to degrees 
  deg2rad <- pi/180 # convert from degrees to radians
  solar_constant = 1367 #[W/m2] solar flux at TOA
  
  # set site-specific values
  DOY = doy
  print(paste("Day of Year = ", DOY,""))
  print(paste("Latitude (degrees) = ", latitude, ""))
  elev = elevation #[m] elevation above sea level
  print(paste("Elevation (m) = ", elev, ""))
  aspect = aspect #[degrees from north]
  aspect = aspect*deg2rad
  slope = slope #[degrees from horizontal] slope 
  slope = slope*deg2rad
  lat = latitude*deg2rad
  dtr = tmax - tmin #[C] diurnal temperature range or Tmax - Tmin
  print(paste("The mean monthly diurnal temperature range (Tmax-Tmin) is", dtr,""))
  # calculate declination [radians]
  declination = 0.4102*sin(pi*(DOY-80)/180); declination_deg = rad2deg*declination #; print(declination_deg)
  # now calculate daylength in hours for latitudes between -66.5 and 66.5 at different times of year
  daylength <- (24/pi)*acos((-1)*tan(lat)*tan(declination))
  day_sec <- daylength*60*60 #[sec] Number of seconds for day length 
  print(paste("Daylength (hrs) at latitude", latitude, "and DOY", DOY, "is", round(daylength,2)))
  print(paste("Daylength (sec) at latitude", latitude, "and DOY", DOY, "is", round(day_sec,2)))
  # tau is the atmospheric transmittance parameter - typical values range from 0.6 to 0.7
  tau_max = (elev*0.0001 + 1.0143)*0.65  #Maximum clear sky transmittance (from Waring Excel with modification from Coops et al)
  #print(paste("Maximum clear sky transmittance (unitless) at", elev, "(m) equals", round(tau_max,3), ""))
  # B is a coefficient in the transmittance formula from (see Coops et al. 2000 Int. J. Biometerology)
  B = 0.031 + 0.201*exp(-0.185*dtr) 
  tau = tau_max*(1 - exp(-(B*dtr^1.47)))  #clear sky transmittance (1.47 from David King via Dick Waring correction)
  print(paste("Mean daily clear sky atmospheric transmissivity (unitless) with a DTR of", dtr, "(C) equals", round(tau,3), ""))
  # zenith angle correction 
  zac  = (((sin(lat)*cos(0*pi/180))*(-1*cos(aspect)*sin(slope))-sin(0*pi/180)*(sin(aspect)*
        sin(slope))+((cos(lat)*cos(0*pi/180))*cos(slope)))*cos(declination)) + 
        (((cos(lat)*(cos(aspect)*sin(slope))+sin(lat)*cos(slope)))*sin(declination))
  #print(paste("Zenith angle correction =",round(zac,3),""))
  air_mass = ifelse(test = zac > 0, yes = (1/(zac + 0.0000001)), no = 0) #optical air mass #; print(air_mass) 
  soldir <- ifelse(test = air_mass > 0, yes = (solar_constant*tau^air_mass)*zac, no = 0) #[W/m2] instantaneous direct solar irradiance
  print(paste("Instantaneous average direct solar irradiance (W/m2) =", round(soldir,2)))
  soldir_day = (((day_sec*2)/pi)*soldir)/1000000 #[MJ/m2 day]  Integrated I per day averaged across the month
  print(paste("Integrated daytime average solar direct irradiance (MJ/m2/day) =", round(soldir_day,2)))
  EFT  = soldir*zac #; print(EFT)  #EFT
  HRAD = EFT*tau^air_mass #; print(HRAD) #HRAD
  GLOBF = sqrt(HRAD*EFT) #; print(GLOBF) #GLOBF
  soldif = (1 - (GLOBF/EFT)) * GLOBF #[W/m2] Average instantaneous diffuse irradiance
  print(paste("Instantaneous average diffuse solar irradiance (W/m2) =", round(soldif,2)))
  DIFLD = (soldif*day_sec)/1000000  #[MJ/m2/day] soldif  (day in Mj)
  soldif_day = (((day_sec*2)/pi)*soldif)/1000000 #[MJ/m2/day] integrated diffuse
  print(paste("Integrated daytime average solar diffuse irradiance (MJ/m2/day)  =", round(soldif_day,3)))
  soltot_day = round(soldif_day + soldir_day,3)   #Total Integrated Daily Radiation (MJ m2 ) day for each month
  print(paste("Integrated total (direct+diffuse) daily average irradiance for the month =", soltot_day))
  return(soltot_day)
}

