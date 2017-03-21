#"""
#Water Balance Module
#Included for the version160926
#by YK, on 9/26/2016
#"""
#from __future__ import division
#from constants import Qa, Qb
#from utils import get_days_in_month

calc_transpiration_PM <- function(Q, VPD, h, gBL, gC){
    #"""
    #Input:
    #    Q, Double
    #    VPD, Double
    #    h, Double
    #    gBL, Double
    #    gC, Double
    #Output:
    #    canopy_transpiration, Double
    #Description:
    #    use Penman-Monteith equation for computing canopy transpiration
    #    in calcuation, the result is kg / (m^2 day),
    #    which is conmverted to mm/day in the output
    #"""
    # The following are constants in the PM formula (Landsberg & Gower, 1997)
    e20 = 2.2                   # rate of change of saturated VP with T at 20C
    rhoAir = 1.2                # density of air, kg/m3
    lambda_ = 2460000           # latent heat of vapourisation of H2O (J/kg)
    VPDconv = 0.000622          # convert VPD to saturation deficit = 18/29/1000

    netRad = Qa + Qb * (Q * (10 ^ 6) / h)                # Q in MJ/m2/day --> W/m2
    defTerm = rhoAir * lambda_ * (VPDconv * VPD) * gBL
    div = (1 + e20 + gBL / gC)
    Etransp = (e20 * netRad + defTerm) / div           # in J/m2/s
    canopy_transpiration = Etransp / lambda_ * h         # converted to kg/m2/day

    return(canopy_transpiration)
}

calc_interception <- function(rain, LAI, LAImaxIntcptn, MaxIntcptn){
    if(LAImaxIntcptn <= 0){
        Intcptn = MaxIntcptn
    } else {
        Intcptn = MaxIntcptn * min(1, LAI / LAImaxIntcptn)
    }
    intercepted_water =  Intcptn * rain
    return(intercepted_water)
}

calc_soil_water_balance <- function(ASW, rain, loss_water, irrig, MinASW, MaxASW){
    ASW = ASW + rain + (100 * irrig / 12) - loss_water # Irrig is Ml/ha/year

    monthlyIrrig = 0
    if(ASW < MinASW){
        if(MinASW > 0){ #make up deficit with irrigation
            monthlyIrrig = MinASW - ASW
        }
      ASW = MinASW
    } else if(ASW > MaxASW){
        ASW = MaxASW
    }
    soil_water_balance_list <- list(ASW = ASW, monthlyIrrig = monthlyIrrig)
    return(soil_water_balance_list)
}

water_balance <- function(solar_rad, VPD, day_length, LAI, rain, irrig, month, ASW, CanCond, LAIShrub, config){

    config_water = config$WaterBalance
    config_canopy = config$CanopyProduction
    config_shrub = config$ShrubEffect

    BLcond = as.numeric(config_water$BLcond)

    LAImaxIntcptn = as.numeric(config_water$LAImaxIntcptn)
    MaxIntcptn = as.numeric(config_water$MaxIntcptn)

    MinASW = as.numeric(config_canopy$MinASW)
    MaxASW = as.numeric(config_canopy$MaxASW)

    TrShrub = as.numeric(config_shrub$TrShrub)

    transp = max(0, calc_transpiration_PM(solar_rad, VPD, day_length, BLcond, CanCond)) #kg/m2/day
    #canopy transpiration in mol/m2/sec for Peclet effect calculations - make sure does not go to 0 to avoid divide by zero errors
    canopy_transpiration_sec = max(0.01, transp*(1.e3/(18.*86400.)))

    transpall = get_days_in_month(month) * transp * (LAIShrub * TrShrub + LAI) / LAI # total transpiration
    transp = get_days_in_month(month) * transp # tree only transpiration, in kg/m2/month
    transpshrub = max(0, transpall - transp) # shrub only transpiration

    intercepted_water = calc_interception(rain, LAI, LAImaxIntcptn, MaxIntcptn)

    loss_water = transp + intercepted_water

    soil_water_balance_list = calc_soil_water_balance(ASW, rain, loss_water, irrig, MinASW, MaxASW)
      ASW = soil_water_balance_list$ASW
      monthlyIrrig = soil_water_balance_list$monthlyIrrig

    water_balance_list <- list(transpall = transpall, transp = transp, transpshrub = transpshrub, loss_water = loss_water, ASW = ASW, monthlyIrrig = monthlyIrrig, canopy_transpiration_sec = canopy_transpiration_sec)
    return(water_balance_list)
}
