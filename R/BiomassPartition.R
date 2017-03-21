#"""
#Biomass Partitioning Module
#Included for the version160926
#by YK, on 9/26/2016
#"""
#from __future__ import division
#from math import exp, log, sin, cos, sqrt, acos, pi
#from utils import get_days_in_month

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

calc_d13c <- function(T_av, CaMonthly, D13Catm, elev, GPPmolc, month, canopy_conductance, RGcGW, D13CTissueDif, aFracDiffu, bFracRubi){
    # calculating d13C by Liang Wei

    # Air pressure, kpa
    AirPressure = 101.3 * exp(-1 * elev / 8200)
    # Convert Unit of Atmospheric C, a ppm to part/part
    AtmCa = CaMonthly * 0.000001
    # Canopy conductance for water vapor in mol/m2s, unit conversion
    GwMol = canopy_conductance * 44.6 * (273.15 / (273.15 + T_av)) * (AirPressure / 101.3)
    # Canopy conductance for CO2 in mol/m2s
    GcMol = GwMol * RGcGW

    # GPP per second. Unit: mol/m2 s. GPPmolc divide by 24 hour/day and 3600 s/hr
    GPPmolsec = GPPmolc / (get_days_in_month(month) * 24 * 3600)

    # Calculating monthly average intercellular CO2 concentration. Ci = Ca - A/g
    InterCi = AtmCa - GPPmolsec / GcMol
    InterCiPPM = InterCi * 1000000

    # Calculating monthly d13C of new photosynthate, = d13Catm- a-(b-a) (ci/ca)
    D13CNewPS = D13Catm - aFracDiffu - (bFracRubi - aFracDiffu) * (InterCi / AtmCa)
    D13CTissue = D13CNewPS + D13CTissueDif
    d13c_list <- list(D13CTissue = D13CTissue, InterCiPPM = InterCiPPM)
    return(d13c_list)
}

d18O <- function(T_av, VPD, d18Osrc, canopy_transpiration_sec){
    #"""calculate d18O of leafwater and cellulose from tair, rh, and d18O of source water and vapor
    #"""
    #ratio between 18O and 16O (18O/16O) in VSMOW (Vienna-Standard Mean Ocean Water)
    r_vsmow = 0.0020052
    d18Osource = d18Osrc
    r_source = r_vsmow * (d18Osource / 1000. + 1) #;print d18Osource, r_source
    Tc = T_av
    Tk = Tc + 273.15 #[K]
    # now calculate the saturation and ambient vapor pressures based on rel. hum. and Tair
    esat = 6.112 * exp( 17.67 * Tc / (Tc + 243.5)) #(mbar) SVP versus Temp relationship from Jacobson 1999
    #this assumes that the VPD data being read in are in mbar, as values peak at ~15 (so could not be kPa)
    #also Wei et al. PC&E gives this unit in Table A1 and in the Appendix for VPD modifier function
    ea = esat - VPD
    epsilon_eq = exp(1137. / Tk ^ 2 - 0.4156 / Tk - 0.0020667) - 1.
    alpha_eq = exp(1137. / Tk ^ 2 - 0.4156 / Tk - 0.0020667); #print alpha_eq
    epsilon_eq = 1000. * epsilon_eq #[per mil] #;print(round(epsilon_eq,2))
    epsilon_eq2 = 2.644 - 3.206 * (1.e3 / Tk) + 1.534 * (1e6 / Tk ^ 2) #[per mil] ;print(round(epsilon_eq2,2))  #from Bottinga and Craig 1969
    alpha_eq2 = epsilon_eq2 / 1000. + 1. #print alpha_eq2
    d18Ovapor = d18Osource - epsilon_eq #[per mil] ;print d18Ovapor
    r_vapor = r_source / alpha_eq
    n = 1.0 #The exponent n depends on turbulence intensity - ranges from 0.5 for fully turbulent conditions to 1.0 for pure molecular diffusion [Mathieu and Bariac,1996].
    alpha_k = (1. / 0.97229) ^ n
    epsilon_k = 1000. * (alpha_k - 1) #[per mil]
    gs = 0.150 #[mol/m2/s] leaf stomatal conductance to water vapor
    gb = 1.42 #[mol/m2/s] leaf boundary conductance to water vapor
    epsilon_k2_weighted = (28. * 1 / gs + 22. * 1 / gb)/(1. / gs + 1. / gb) #[per mil] from Luz et al. 2009 (table 2), where gb is constant at 1.42 mol/m2/s
    alpha_k2 = epsilon_k2_weighted / 1000. + 1 # alpha = epsilon/1000 +1
    d18Oleaf = epsilon_eq + (1. - ea / esat) * (d18Osource + epsilon_k2_weighted) + (ea / esat) * d18Ovapor #[per mil]
    r_leaf = alpha_eq * (alpha_k2 * r_source * ((esat - ea) / esat) + r_vapor * (ea / esat))
    d18Oleaf_check = 1000. * (r_leaf / r_vsmow - 1.) #print round(d18Oleaf_check,1)
    #D18Oleaf = (d18Oleaf/1000. - d18Osource/1000.)/(1 + d18Osource/1000.) #expressed as enrichment above source water, i.e. 'big delta'
    D18Ovapor = (d18Ovapor - d18Osource)/(1. + d18Osource / 1000.) #expressed as enrichment above source water, i.e. 'big delta'
    D18Oleaf = epsilon_eq + epsilon_k2_weighted + (D18Ovapor - epsilon_k2_weighted) * (ea / esat)
    # L at steady state varies from ~.01m to ~0.2m (see Fig. 2 from Ferrio)
    L = 0.01 #[m] scaled effective path length (m) for water movement from the veins to the site of evaporation (Kleaf = 31.5 * L**(-0.43),
    C = 55.56e3 #[mol/m3] the molar concentration of water
    a_d = 1. / 1.026; a1 = 100.e-9; a2 = 577.; a3 = 145.
    D = a_d * a1 * exp(-a2 / (Tk - a3))
    ## D should be close to 2.66e-9 for H218O at 32.5 C (see Cuntz et al. 2007, p.897)
    #need to import ET from the model rather than the fixed rate here - add as canopy_transpiration_sec
    #0.05 #[mol/m2/s] leaf transpiration rate
    E = canopy_transpiration_sec
    peclet = (E * L) / (C * D)
    D18Oleaf_peclet = D18Oleaf * ((1 - exp(-peclet)) / peclet)
    epsilon_cell = 27. #[per mil] fractionation factor between carbonyl oxygen and water
    fO = 0.42 #[unitless] fraction of cellulose oxygen that exchanges atoms with xylem water during synthesis
    d18Ocell = fO * (d18Osource + epsilon_cell) + (1. - fO) * (d18Oleaf + epsilon_cell)
    d18Ocell_peclet = fO * (d18Osource + epsilon_cell) + (1. - fO) * (D18Oleaf_peclet + epsilon_cell)
    d18O_list <- list(d18Oleaf = d18Oleaf, d18Ocell = d18Ocell, d18Ocell_peclet = d18Ocell_peclet)
    return(d18O_list)
}

biomass_partition <- function(T_av, LAI, elev, CaMonthly, D13Catm, WF, WR, WS, TotalLitter, NPP, GPPmolc, stand_age, month, avDBH, modifier_physiology, VPD, d18Osrc, canopy_conductance, canopy_transpiration_sec, config){

    config_canopy = config$CanopyProduction
    config_bio = config$BiomassPartition

    m0 = as.numeric(config_bio$m0)
    pRx = as.numeric(config_bio$pRx)
    pRn = as.numeric(config_bio$pRn)

    pFS20 = as.numeric(config_bio$pFS20)
    pFS2 = as.numeric(config_bio$pFS2)
    pfsPower = log(pFS20 / pFS2) / log(20 / 2)
    pfsConst = pFS2 / (2 ^ pfsPower)

    FR = as.numeric(config_canopy$FR)

    gammaFx = as.numeric(config_bio$gammaFx)
    gammaF0 = as.numeric(config_bio$gammaF0)
    tgammaF = as.numeric(config_bio$tgammaF)
    Rttover = as.numeric(config_bio$Rttover)

    RGcGW = as.numeric(config_bio$RGcGW)
    D13CTissueDif = as.numeric(config_bio$D13CTissueDif)
    aFracDiffu = as.numeric(config_bio$aFracDiffu)
    bFracRubi = as.numeric(config_bio$bFracRubi)

    biomass_partition_list = calc_biomass_partition(NPP, avDBH, modifier_physiology, m0, FR, pfsConst, pfsPower, pRx, pRn)
      delWF <- biomass_partition_list$delWF
      delWR <- biomass_partition_list$delWR
      delWS <- biomass_partition_list$delWS

    litter_and_rootturnover_list = calc_litter_and_rootturnover(WF, WR, stand_age, gammaFx, gammaF0, tgammaF, Rttover)
      delLitter <- litter_and_rootturnover_list$delLitter
      delRoots <- litter_and_rootturnover_list$delRoots

    endofmonth_biomass_list = update_endofmonth_biomass(WF, WR, WS, TotalLitter, delWF, delWR, delWS, delLitter, delRoots)
      WF <- endofmonth_biomass_list$WF
      WR <- endofmonth_biomass_list$WR
      WS <- endofmonth_biomass_list$WS
      TotalW <- endofmonth_biomass_list$TotalW
      TotalLitter <- endofmonth_biomass_list$TotalLitter

    d13c_list = calc_d13c(T_av, CaMonthly, D13Catm, elev, GPPmolc, month, canopy_conductance, RGcGW, D13CTissueDif, aFracDiffu, bFracRubi)
      D13CTissue <- d13c_list$D13CTissue
      InterCiPPM <- d13c_list$InterCiPPM

    d18O_list = d18O(T_av, VPD, d18Osrc, canopy_transpiration_sec)
      d18Oleaf <- d18O_list$d18Oleaf
      d18Ocell <- d18O_list$d18Ocell
      d18Ocell_peclet <- d18O_list$d18Ocell_peclet

    biomass_partition_list <- list(WF = WF, WR = WR, WS = WS, TotalW = TotalW, TotalLitter = TotalLitter, D13CTissue = D13CTissue, InterCiPPM = InterCiPPM, delWF = delWF, delWR = delWR, delWS = delWS, d18Oleaf = d18Oleaf, d18Ocell = d18Ocell, d18Ocell_peclet = d18Ocell_peclet)
      return(biomass_partition_list)
}
