#' Calculations for partioning biomass
#'
#' Internal functions for partitioning biomass
#'

calc_biomass_partition <- function(NPP, avDBH, modifier_physiology, m0, FR, pfsConst, pfsPower, pRx, pRn){

    # calculate partitioning coefficients
    m = m0 + (1 - m0) * FR
    pFS = pfsConst * (avDBH ^ pfsPower) # foliage and stem partition
    pR = pRx * pRn / (pRn + (pRx - pRn) * modifier_physiology * m) # root partition
    pS = (1 - pR) / (1 + pFS) # stem partition
    pF = 1 - pR - pS # foliage partition

    # calculate biomass increments
    delWF = NPP * pF                        # foliage
    delWR = NPP * pR                        # root
    delWS = NPP * pS                        # stem

    biomass_partition <- list(delWF = delWF, delWR = delWR, delWS = delWS)
    return(biomass_partition)
}

calc_litter_and_rootturnover <- function(WF, WR, stand_age, gammaFx, gammaF0, tgammaF, Rttover){
    # calculate litterfall & root turnover -
    Littfall = gammaFx * gammaF0 / (gammaF0 + (gammaFx - gammaF0) * exp(-12 * log(1 + gammaFx / gammaF0) * stand_age / tgammaF))

    delLitter = Littfall * WF
    delRoots = Rttover * WR

    litter_and_rootturnover_list <- list(delLitter = delLitter, delRoots = delRoots)
    return(litter_and_rootturnover_list)
}

update_endofmonth_biomass <- function(WF, WR, WS, TotalLitter, delWF, delWR, delWS, delLitter, delRoots){
    # Calculate end-of-month biomass

    WF = WF + delWF - delLitter
    WR = WR + delWR - delRoots
    WS = WS + delWS
    TotalW = WF + WR + WS
    TotalLitter = TotalLitter + delLitter
    endofmonth_biomass_list <- list(WF = WF, WR = WR, WS = WS, TotalW = TotalW, TotalLitter = TotalLitter)
    return(endofmonth_biomass_list)
}

biomass_partition <- function(T_av, LAI, elev, CaMonthly, D13Catm, WF, WR, WS, TotalLitter, NPP, GPPmolc, stand_age, month, avDBH, modifier_physiology, VPD, d18Osrc, canopy_conductance, canopy_transpiration_sec, config){

  list2env(config$CanopyProduction, envir = environment())
  list2env(config$BiomassPartition, envir = environment())
      
  pfsPower = log(pFS20 / pFS2) / log(20 / 2)
  pfsConst = pFS2 / (2 ^ pfsPower)

    biomass_partition_list = calc_biomass_partition(NPP, avDBH, modifier_physiology, m0, FR, pfsConst, pfsPower, pRx, pRn)
      list2env(biomass_partition_list, envir = environment())

    litter_and_rootturnover_list = calc_litter_and_rootturnover(WF, WR, stand_age, gammaFx, gammaF0, tgammaF, Rttover)
      list2env(litter_and_rootturnover_list, envir = environment())

    endofmonth_biomass_list = update_endofmonth_biomass(WF, WR, WS, TotalLitter, delWF, delWR, delWS, delLitter, delRoots)
      list2env(endofmonth_biomass_list, envir = environment())

    d13c_list = calc_d13c(T_av, CaMonthly, D13Catm, elev, GPPmolc, month, canopy_conductance, RGcGW, D13CTissueDif, aFracDiffu, bFracRubi)
      list2env(d13c_list, envir = environment())

    d18O_list = d18O(T_av, VPD, d18Osrc, canopy_transpiration_sec)
      list2env(d18O_list, envir = environment())

    biomass_partition_list <- list(WF = WF, WR = WR, WS = WS, TotalW = TotalW, TotalLitter = TotalLitter, D13CTissue = D13CTissue, InterCiPPM = InterCiPPM, delWF = delWF, delWR = delWR, delWS = delWS, d18Oleaf = d18Oleaf, d18Ocell = d18Ocell, d18Ocell_peclet = d18Ocell_peclet)
  
  return(biomass_partition_list)
}
