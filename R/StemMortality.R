#' Internal calculations for mortality
#'
#' Determines the number of stems to remove to ensure the
#'   self-thinning rule is satisfied. It applies the Newton-Rhapson method
#'   to solve for N to an accuracy of 1 stem or less. To change this,
#'   change the value of "accuracy".#

getMortality <- function(oldN, oldW, mS, wSx1000, thinPower){
    accuracy = 1 / 1000
    n = oldN / 1000
    x1 = 1000 * mS * oldW / oldN
    i = 0
    while(TRUE){
        i = i + 1
        x2 = wSx1000 * (n ^ (1 - thinPower))
        fN = x2 - x1 * n - (1 - mS) * oldW
        dfN = (1 - thinPower) * x2 / n - x1
        dN = -1 * fN / dfN
        n = n + dN
        if(abs(dN) <= accuracy | i >= 5){break()}
    }
    res = oldN - 1000 * n
    return(as.integer(res))
}

calc_mortality <- function(WF, WR, WS, StemNo, delStemNo, wSx1000, thinPower, mF, mR, mS){
    # Calculate mortality
    wSmax = wSx1000 * (1000 / StemNo) ^ thinPower
    AvStemMass = WS * 1000 / StemNo
    delStems = 0
    if(wSmax < AvStemMass){
        delStems = getMortality(StemNo, WS, mS, wSx1000, thinPower)
        WF = WF - mF * delStems * (WF / StemNo)
        WR = WR - mR * delStems * (WR / StemNo)
        WS = WS - mS * delStems * (WS / StemNo)
        # wSmax = wSx1000 * (1000 / StemNo) ** thinPower
    }
    StemNo = StemNo - delStems
    AvStemMass = WS * 1000 / StemNo
    delStemNo = delStemNo + delStems
    mortality_list <- list(WF = WF, WR = WR, WS = WS, AvStemMass = AvStemMass, StemNo = StemNo, delStemNo = delStemNo)
    return(mortality_list)
}

calc_factors_age <- function(stand_age, SLA0, SLA1, tSLA, fracBB0, fracBB1, tBB){
    # update age-dependent factors
    SLA = SLA1 + (SLA0 - SLA1) * exp(-log(2) * (stand_age / tSLA) ^ 2)
    fracBB = fracBB1 + (fracBB0 - fracBB1) * exp(-log(2) * (stand_age / tBB))
    factors_age_list <- list(SLA = SLA, fracBB = fracBB)
    return(factors_age_list)
}

update_stands <- function(stand_age, WF, WS, AvStemMass, StemNo, SLA, fracBB, StemConst, StemPower, Density, HtC0, HtC1){
    # update stsand characteristics
    LAI = WF * SLA * 0.1
    avDBH = (AvStemMass / StemConst) ^ (1 / StemPower)
    BasArea = (((avDBH / 200) ^ 2) * pi) * StemNo
    StandVol = WS * (1 - fracBB) / Density
    if(stand_age > 0){
        MAI = StandVol / stand_age
    } else {
        MAI = 0
    }
    # Height equation (Wykoff 1982) is in English unit,
    # DBH is first convert to inch.
    # Finally Ht is convert form feet to meters._Liang
    Height = (exp(HtC0 + HtC1 / (avDBH / 2.54 + 1)) + 4.5) * 0.3048

    update_stands_list <- list(LAI = LAI, MAI = MAI, avDBH = avDBH, BasArea = BasArea, Height = Height, StandVol = StandVol)
    return(update_stands_list)
}

stem_mortality <- function(WF, WR, WS, StemNo, delStemNo, stand_age, config, doThinning = NA, doDefoliation = NA){

    config_stem = config$StemMortality

    wSx1000 = as.numeric(config_stem$wSx1000)
    thinPower = as.numeric(config_stem$thinPower)
    mF = as.numeric(config_stem$mF)
    mR = as.numeric(config_stem$mR)
    mS = as.numeric(config_stem$mS)

    SLA0 = as.numeric(config_stem$SLA0)
    SLA1 = as.numeric(config_stem$SLA1)
    tSLA = as.numeric(config_stem$tSLA)
    fracBB0 = as.numeric(config_stem$fracBB0)
    fracBB1 = as.numeric(config_stem$fracBB1)
    tBB = as.numeric(config_stem$tBB)

    StemConst = as.numeric(config_stem$StemConst)
    StemPower = as.numeric(config_stem$StemPower)
    Density = as.numeric(config_stem$Density)
    HtC0 = as.numeric(config_stem$HtC0)
    HtC1 = as.numeric(config_stem$HtC1)

    if(!is.na(doThinning)){
        doThinning()
    }
    if(!is.na(doDefoliation)){
        doDefoliation()
    }

    stand_age = stand_age + 1.0 / 12

    mortality_list = calc_mortality(WF, WR, WS, StemNo, delStemNo, wSx1000, thinPower, mF, mR, mS)
      WF <- mortality_list$WF
      WR <- mortality_list$WR
      WS <- mortality_list$WS
      AvStemMass <- mortality_list$AvStemMass
      StemNo <- mortality_list$StemNo
      delStemNo <- mortality_list$delStemNo

    factors_age_list = calc_factors_age(stand_age, SLA0, SLA1, tSLA, fracBB0, fracBB1, tBB)
      SLA <- factors_age_list$SLA
      fracBB <- factors_age_list$fracBB

    update_stands_list = update_stands(stand_age, WF, WS, AvStemMass, StemNo, SLA, fracBB, StemConst, StemPower, Density, HtC0, HtC1)
      LAI <- update_stands_list$LAI
      MAI <- update_stands_list$MAI
      avDBH <- update_stands_list$avDBH
      BasArea <- update_stands_list$BasArea
      Height <- update_stands_list$Height
      StandVol <- update_stands_list$StandVol
    ### YIK update on 8/15/16: add the variable of "AvStemMass" to fix errors related to "AvStemMass = stem_mortalit"
    #    in Model3PG.py

    stem_mortality_list <- list(stand_age = stand_age, LAI = LAI, MAI = MAI, avDBH = avDBH, BasArea = BasArea, Height = Height, StemNo = StemNo, delStemNo = delStemNo, StandVol = StandVol, WF = WF, WR = WR, WS = WS, AvStemMass = AvStemMass)
    return(stem_mortality_list)
    # Original code
    # return stand_age, LAI, MAI, avDBH, BasArea, Height, StemNo, delStemNo, StandVol, WF, WR, WS
}
