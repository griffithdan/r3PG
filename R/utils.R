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
