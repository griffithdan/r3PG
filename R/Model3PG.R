## """
## Included for the version160926
## by YK, on 9/26/2016
## """
## from __future__ import division

## import re
## from math import pi

## import numpy as np

## from framework import Model, BookKepper
## from utils import get_stand_age, get_day_length

## from CanopyProduction import canopy_production
## from BiomassPartition import biomass_partition
## from WaterBalance import water_balance
## from StemMortality import stem_mortality, calc_factors_age

##mapper <- list("stand_age" = "stand_age",
##               "lai" = "LAI",
##               "mai" = "MAI",
##               "basarea" = "BasArea",
##               "height" = "Height",
##               "d13ctissue" = "D13CTissue",
##               "modifier_physiology" = "modifier_physiology",
##               "npp" = "NPP",
##               "asw" = "ASW",
##               "transp" = "transp",
##               "loss_water" = "loss_water",
##               "standvol" = "StandVol",
##               "stemno" = "StemNo",
##               "par" = "PAR",
##               "intercippm" = "InterCiPPM",
##               "wf" = "WF",
##               "ws" = "WS",
##               "wr" = "WR",
##               "avstemmass" = "AvStemMass",
##               "delwf" = "delWF",
##               "delwr" = "delWR",
##               "delws" = "delWS",
##               "d18oleaf" = "d18Oleaf",
##               "d18ocell" = "d18Ocell",
##               "d18ocell_peclet" = "d18Ocell_peclet",
##               "avdbh" = "avDBH")

#class Model3PG(Model):
#    def __init__(self, fpath_setting):
#        super(Model3PG, self).__init__(fpath_setting)
#        self.initialize()

##initialize <- function(self){ # DMG need work
##        fpath_input = self.config.IO.input
##        fpath_output = self.config.IO.output
##
##        self.data = np.loadtxt(fpath_input, skiprows=1)
##        self.keeper = BookKepper(fpath_output)
##        self.keeper.initialize(self.config.Output)
##}

##teardown <- function(self){ # DMG need work
##        self.data = None
##        self.keeper.shutdown()
##}

instance3PG <- function(config, climate = NULL, output = NULL, grass = FALSE){
  
  
  if(!is.list(config)){
    config <- load_config(config)
  }


  if(is.null(climate)){
    climate <- read.table(config$IO$input, sep = "\t", header = TRUE) #
  } else if(!is.data.frame(climate)){
    climate <- read.table(climate, sep = "\t", header = TRUE) #
  }

    config_time = config$TimeRange
    config_site = config$SiteCharacteristics
    config_initial = config$InitialState
    config_stem = config$StemMortality

    lat = as.numeric(config_site$lat)
    EndYear = as.integer(config_time$EndYear) # there was a space before ENDYEAR in the python script... check that this was not purposely "turned off"????
    InitialYear = as.integer(config_time$InitialYear)
    InitialMonth = as.integer(config_time$InitialMonth)
    YearPlanted = as.integer(config_time$YearPlanted)
    MonthPlanted = as.integer(config_time$MonthPlanted)
    EndAge = as.integer(config_time$EndAge)

      nYears = EndYear - InitialYear + 1

    # Assign initial state of stand
      stand_age_list <- get_stand_age(lat, InitialYear, InitialMonth, YearPlanted, MonthPlanted, EndAge)
        stand_age <- stand_age_list$stand_age
        StartAge <- stand_age_list$StartAge
        InitialYear <- stand_age_list$InitialYear
        InitialMonth <- stand_age_list$InitialMonth
        MonthPlanted <- stand_age_list$MonthPlanted

        # do annual calculation
        metMonth = InitialMonth
        for(year in seq(from = StartAge, to = EndAge, by = 1)){
            print(paste('year ', year, sep = ""))

            # do monthly calculations
            month = InitialMonth #+ 1 # DMG added because R starts with 1 index, instead of 0
            for(month_counter in 1:12){
                if(year == 0 & month == InitialMonth){#+ 1){ # DMG added because R starts with 1 index, instead of 0
                    WS = as.numeric(config_initial$InitialWS)
                    WF = as.numeric(config_initial$InitialWF)
                    WR = as.numeric(config_initial$InitialWR)
                    StemNo = as.numeric(config_initial$InitialStocking)
                    ASW = as.numeric(config_initial$InitialASW)
                    TotalLitter = 0
                    # thinEventNo = 1
                    # defoltnEventNo = 1
                    irrig = 0 # TODO

                    SLA0 = as.numeric(config_stem$SLA0)
                    SLA1 = as.numeric(config_stem$SLA1)
                    tSLA = as.numeric(config_stem$tSLA)
                    fracBB0 = as.numeric(config_stem$fracBB0)
                    fracBB1 = as.numeric(config_stem$fracBB1)
                    tBB = as.numeric(config_stem$tBB)
                    StemConst = as.numeric(config_stem$StemConst)
                    StemPower = as.numeric(config_stem$StemPower)
                    Density = as.numeric(config_stem$Density)

                    factors_age_list <- calc_factors_age(stand_age, SLA0, SLA1, tSLA, fracBB0, fracBB1, tBB)
                      SLA <- factors_age_list$SLA
                      fracBB <- factors_age_list$fracBB

                    AvStemMass = WS * 1000 / StemNo                 # kg/tree
                    avDBH = (AvStemMass / StemConst) ^ (1 / StemPower)
                    BasArea = (((avDBH / 200) ^ 2) * pi) * StemNo
                    LAI = WF * SLA * 0.1
                    StandVol = WS * (1 - fracBB) / Density

                    if(stand_age > 0){
                        MAI = StandVol / stand_age
                    } else {
                        MAI = 0
                    }

                    Height = D13CTissue = NPP = InterCiPPM = delWF = delWR = delWS = 0
                    d18Oleaf = d18Ocell = d18Ocell_peclet = 0
                    transp = loss_water = canopy_transpiration_sec = 0
                    modifiers = rep(0, 7)
                    delStemNo = 0
                    modifier_physiology = 0
                    PAR = 0

                    out_var_names <- names(config$Output[config$Output == 1])
                    output_vars <- matrix(data = sapply(X = out_var_names, FUN = dynGet), nrow = 1, ncol = length(out_var_names), dimnames = list(NULL,out_var_names))


                } else {
                    #print(paste('month: ', month + 1, " and metMonth:", metMonth + 1, sep = ""))
                    config_site = config$SiteCharacteristics

                    lat = as.numeric(config_site$lat)
                    elev = as.numeric(config_site$elev)
                    # assign meteorological data at this month
                    if(month >= 12){month = 1}
                    # if metMonth > 12 * mYears:
                    #     metMonth = 1

                    # DMG index fix, try adding +1 to everthing

                    # T_max = self.data[metMonth, 0] #CJS note: does not need Tmax met. data: VPD and SRAD are already in inputs
                    # T_min = self.data[metMonth, 1] #CJS note: does not need Tmax met. data: VPD and SRAD are already in inputs
                    T_av = climate[metMonth + 1, 3]
                    # VPD = get_VPD(T_min, T_max) #CJS note: does not need VPD met. data: VPD data are already in inputs
                    VPD = climate[metMonth + 1, 4]
                    rain = climate[metMonth + 1, 5]
                    solar_rad = climate[metMonth + 1, 6]
                    # rain_days = int(self.data[metMonth, 7])
                    day_length = get_day_length(lat, month + 1)
                    frost_days = as.integer(climate[metMonth + 1, 8])
                    CaMonthly = climate[metMonth + 1, 9]
                    D13Catm = climate[metMonth + 1, 10]
                    d18Osrc = climate[metMonth + 1, 11]

                    CounterforShrub = NA

                    # Canopy Production Module
                      canopy_production_list <- canopy_production(T_av, CaMonthly, VPD, ASW, frost_days, stand_age, LAI, solar_rad, month + 1, CounterforShrub, config, grass)
                        PAR <- canopy_production_list$PAR
                        APAR <- canopy_production_list$APAR
                        APARu <- canopy_production_list$APARu
                        GPPmolc <- canopy_production_list$GPPmolc
                        GPPdm <- canopy_production_list$GPPdm
                        NPP <- canopy_production_list$NPP
                        modifiers <- canopy_production_list$modifiers
                        LAIShrub <- canopy_production_list$LAIShrub
                        CounterforShrub <- canopy_production_list$CounterforShrub
                        canopy_conductance <- canopy_production_list$canopy_conductance

          					# Water Balance Module
                      water_balance_list <- water_balance(solar_rad, VPD, day_length, LAI, rain, irrig, month + 1, ASW, canopy_conductance, LAIShrub, config)
                        transpall <- water_balance_list$transpall
                        transp <- water_balance_list$transp
                        transpshrub <- water_balance_list$transpshrub
                        loss_water <- water_balance_list$loss_water
                        ASW <- water_balance_list$ASW
                        monthlyIrrig <- water_balance_list$monthlyIrrig
                        canopy_transpiration_sec <- water_balance_list$canopy_transpiration_sec

                    # Biomass Partion Module
                      modifier_physiology = tail(modifiers, 1)
                      biomass_partition_list = biomass_partition(T_av, LAI, elev, CaMonthly, D13Catm, WF, WR, WS, TotalLitter, NPP, GPPmolc, stand_age, month + 1, avDBH, modifier_physiology, VPD, d18Osrc, canopy_conductance, canopy_transpiration_sec, config)
                        WF <- biomass_partition_list$WF
                        WR <- biomass_partition_list$WR
                        WS <- biomass_partition_list$WS
                        TotalW <- biomass_partition_list$TotalW
                        TotalLitter <- biomass_partition_list$TotalLitter
                        D13CTissue <- biomass_partition_list$D13CTissue
                        InterCiPPM <- biomass_partition_list$InterCiPPM
                        delWF <- biomass_partition_list$delWF
                        delWR <- biomass_partition_list$delWR
                        delWS <- biomass_partition_list$delWS
                        d18Oleaf <- biomass_partition_list$d18Oleaf
                        d18Ocell <- biomass_partition_list$d18Ocell
                        d18Ocell_peclet <- biomass_partition_list$d18Ocell_peclet

                    # Stem Mortality Module
                      stem_mortality_list <- stem_mortality(WF, WR, WS, StemNo, delStemNo, stand_age, config)
                        stand_age <- stem_mortality_list$stand_age
                        LAI <- stem_mortality_list$LAI
                        MAI <- stem_mortality_list$MAI
                        avDBH <- stem_mortality_list$avDBH
                        BasArea <- stem_mortality_list$BasArea
                        Height <- stem_mortality_list$Height
                        StemNo <- stem_mortality_list$StemNo
                        delStemNo <- stem_mortality_list$delStemNo
                        StandVol <- stem_mortality_list$StandVol
                        WF <- stem_mortality_list$WF
                        WR <- stem_mortality_list$WR
                        WS <- stem_mortality_list$WS
                        AvStemMass <- stem_mortality_list$AvStemMass

                        #print(paste("LAI: ", get("LAI"), sep = ""))
                        out_var_names <- names(config$Output[config$Output == 1])
                        output_vars <- rbind(output_vars, sapply(X = out_var_names, FUN = dynGet))


            # break
                }

              metMonth = metMonth + 1
              month = month + 1
            }
        }

        if(is.null(output)){
          write.table(x = output_vars, file = config$IO$output, sep = "\t", row.names = FALSE, col.names = TRUE)
        } else {
          write.table(x = output_vars, file = output, sep = "\t", row.names = FALSE, col.names = TRUE)
        }

    return(output_vars)
}

##if __name__ == '__main__':
##    fpath_test = r'../test/Test_config.cfg'
##model = Model3PG(fpath_test)
##    print model.data
##
##    model.run()
