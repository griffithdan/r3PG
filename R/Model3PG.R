#' Primary internal model code
#'
#' Accepts climate data, species characteristics, and site configuration 
#'   information and returns model outputs
#'
#' @param config Either the path to a 3PG configuration file OR a list object
#'   with the appropriate structure (see examples/read function).
#' @param climate Either the path to a 3PG climate file OR a data.frame object
#'   with the appropriate structure (see examples/read function). 
#' @param output Optional file path for output.
#' @param python_indexing If TRUE, month indices are done in python style for 
#'   compatibily. If FALSE then January is 1, not 0. Defaults to FALSE.
#' @return A data.frame with model results.
#'
#' @seealso \link[r3PG]{load_config}.

instance3PG <- function(config, climate = NULL, output = FALSE, python_indexing = FALSE){
  
  if(!is.list(config)){
    config <- load_config(config)
  }

  if(is.null(climate)){
    climate <- read.table(config$IO$input, sep = "\t", header = TRUE) #
  } else if(!is.data.frame(climate)){
    climate <- read.table(climate, sep = "\t", header = TRUE) #
  }
  
  # Process climate columns
    climate <- data.frame(
      T_av = climate[,grep(pattern = "^(Tav|T_av|Tmean|T_mean|Tmn|T_mn)$", x = colnames(climate))],
      VPD = climate[,grep(pattern = "^(VPD|vpd)$", x = colnames(climate))],
      rain = climate[,grep(pattern = "^(Rain|rain)$", x = colnames(climate))],
      solar_rad = climate[,grep(pattern = "^(Solar.rad|Solar_rad|SolarRad)$", x = colnames(climate))],
      frost_days = as.integer(climate[,grep(pattern = "^(Frost.Days|Frost_Days|FrostDays|Frost.days)$", x = colnames(climate))]),
      CaMonthly = climate[,grep(pattern = "^(CaMonthly|Ca|CO2)$", x = colnames(climate))],
      D13Catm = climate[,grep(pattern = "^(D13Catm|D13C|d13C)$", x = colnames(climate))],
      d18Osrc = climate[,grep(pattern = "^(d18O|d18Osrc|d18O|D18O)$", x = colnames(climate))]
    )

  list2env(config$TimeRange, envir = environment())
  list2env(config$SiteCharacteristics, envir = environment())
  list2env(config$InitialState, envir = environment())
  list2env(config$StemMortality, envir = environment())
  
    nYears = EndYear - InitialYear + 1

    # Assign initial state of stand
      stand_age_list <- get_stand_age(lat, InitialYear, InitialMonth, YearPlanted, MonthPlanted, EndAge)
        list2env(stand_age_list, envir = environment())

        # do annual calculation
        if(python_indexing == TRUE){InitialMonth <- InitialMonth + 1; EndMonth <- EndMonth + 1; MonthPlanted <- MonthPlanted + 1}
        metMonth = InitialMonth
        for(year in seq(from = StartAge, to = EndAge, by = 1)){
            print(paste('year ', year, sep = ""))

            # do monthly calculations
            month = InitialMonth 
            for(month_counter in 1:12){
                #print(paste('month: ', month, " and metMonth:", metMonth, sep = ""))

                if(year == StartAge & month == InitialMonth){
                    WS = InitialWS
                    WF = InitialWF
                    WR = InitialWR
                    StemNo = InitialStocking
                    ASW = InitialASW
                    
                    TotalLitter = 0
                    # thinEventNo = 1
                    # defoltnEventNo = 1
                    irrig = 0 # TODO

                    factors_age_list <- calc_factors_age(stand_age, SLA0, SLA1, tSLA, fracBB0, fracBB1, tBB)
                      list2env(factors_age_list, envir = environment())

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

                    # assign meteorological data at this month
                    if(month > 12){month = 1}
                    if(metMonth > nrow(climate)){metMonth = 1}

                      T_av = climate[metMonth, "T_av"]
                      VPD = climate[metMonth, "VPD"]
                      rain = climate[metMonth, "rain"]
                      solar_rad = climate[metMonth, "solar_rad"]
                      frost_days = as.integer(climate[metMonth, "frost_days"])
                      CaMonthly = climate[metMonth, "CaMonthly"]
                      D13Catm = climate[metMonth, "D13Catm"]
                      d18Osrc = climate[metMonth, "d18Osrc"]
                      day_length = get_day_length(lat, month)

                    CounterforShrub = NA

                    # Canopy Production Module
                      canopy_production_list <- canopy_production(T_av, CaMonthly, VPD, ASW, frost_days, stand_age, LAI, solar_rad, month, CounterforShrub, config)
                        list2env(canopy_production_list, envir = environment())

          					# Water Balance Module
                      water_balance_list <- water_balance(solar_rad, VPD, day_length, LAI, rain, irrig, month, ASW, canopy_conductance, LAIShrub, config)
                        list2env(water_balance_list, envir = environment())

                    # Biomass Partion Module
                      modifier_physiology = tail(modifiers, 1)
                      biomass_partition_list = biomass_partition(T_av, LAI, elev, CaMonthly, D13Catm, WF, WR, WS, TotalLitter, NPP, GPPmolc, stand_age, month, avDBH, modifier_physiology, VPD, d18Osrc, canopy_conductance, canopy_transpiration_sec, config)
                        list2env(biomass_partition_list, envir = environment())

                    # Stem Mortality Module
                      stem_mortality_list <- stem_mortality(WF, WR, WS, StemNo, delStemNo, stand_age, config)
                        list2env(stem_mortality_list, envir = environment())

                        #print(paste("LAI: ", get("LAI"), sep = ""))
                        out_var_names <- names(config$Output[config$Output == 1])
                        output_vars <- rbind(output_vars, sapply(X = out_var_names, FUN = dynGet))


            # break
                }

              metMonth = metMonth + 1
              month = month + 1
            }
        }

        if(output == FALSE){
        } else if(is.null(output) | output == "" | output == "config"){
          write.table(x = output_vars, file = config$IO$output, sep = "\t", row.names = FALSE, col.names = TRUE)
        } else {
          write.table(x = output_vars, file = output, sep = "\t", row.names = FALSE, col.names = TRUE)
        }

    return(output_vars)
}

