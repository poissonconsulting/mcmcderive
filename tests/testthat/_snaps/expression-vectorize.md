# simple expr with iteration var replaced and for loop removed

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        log(eCount[i]) <- b0
      }))
    Output
      log(eCount) <- b0

# avoiding double braces (#21)

    Code
      expression_vectorize(rlang::expr({
        for (i in 1:10) {
          x[i] <- 1
          y[i] <- 2
        }
      }))
    Output
      {
          x <- 1
          y <- 2
      }

# iteration var replaced with squared term

    Code
      expression_vectorize(rlang::expr(for (i in seq_along(LogLength)) {
        eWeightLength[i] <- bWeightLength + bDayte * Dayte[i] + bDayte2 * Dayte[i]^2
      }))
    Output
      eWeightLength <- bWeightLength + bDayte * Dayte + bDayte2 * Dayte^2

# iteration var replaced with prediction, fit and residual term 

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        log(prediction[i]) <- bWeight + eWeightLength[i] * LogLength[i]
        fit[i] <- log(prediction[i])
        residual[i] <- res_lnorm(Weight[i], fit[i], sWeight)
      }))
    Output
      {
          log(prediction) <- bWeight + eWeightLength * LogLength
          fit <- log(prediction)
          residual <- res_lnorm(Weight, fit, sWeight)
      }

# iteration var replaced and cbind added to arrays

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        log(eCount[i]) <- b0 + bYear * Year[i] + bAnnual[Annual[i]] + bSiteAnnual[
          Site[i], Annual[i]]
        fit[i] <- eCount[i]
        residual[i] <- res_gamma_pois(Count[i], fit[i], sSiteAnnualQuadrat)
      }))
    Output
      {
          log(eCount) <- b0 + bYear * Year + bAnnual[Annual] + bSiteAnnual[cbind(Site, 
              Annual)]
          fit <- eCount
          residual <- res_gamma_pois(Count, fit, sSiteAnnualQuadrat)
      }

# expr with mutli lines

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        log(eCount[i]) <- b0 + bYear * Year[i] + bKelpLine * KelpLine[i] + bSite[Site[
          i]] + bAnnual[Annual[i]] + bSiteAnnual[Site[i], Annual[i]]
        log(eCountKelpline[i]) <- b0 + bKelpLine + bYear * Year[i] + bAnnual[Annual[i]] +
          bSite[Site[i]] + bSiteAnnual[Site[i], Annual[i]]
        log(eCountBarren[i]) <- b0 + bYear * Year[i] + bAnnual[Annual[i]] + bSite[
          Site[i]] + bSiteAnnual[Site[i], Annual[i]]
      }))
    Output
      {
          log(eCount) <- b0 + bYear * Year + bKelpLine * KelpLine + 
              bSite[Site] + bAnnual[Annual] + bSiteAnnual[cbind(Site, 
              Annual)]
          log(eCountKelpline) <- b0 + bKelpLine + bYear * Year + bAnnual[Annual] + 
              bSite[Site] + bSiteAnnual[cbind(Site, Annual)]
          log(eCountBarren) <- b0 + bYear * Year + bAnnual[Annual] + 
              bSite[Site] + bSiteAnnual[cbind(Site, Annual)]
      }

# expr with non iteration

    Code
      expression_vectorize(rlang::expr({
        max_age <- round(bA_max)
        age <- 1:max_age
        length <- bL_inf * (1 - exp(-bk * (age - ba0)))
        fecundity <- 10^(-5 + 3 * log(fl2tl(length), base = 2))
        survival <- c(1, rep(bS_J, 2), rep(bS_A, max_age - 1))
        survivorship <- cumprod(survival)
        maturity <- age^14 / (bAs^14 + age^14)
        eggs <- survivorship * maturity * fecundity * 0.5
        prediction <- 1 / sum(eggs)
      }))
    Output
      {
          max_age <- round(bA_max)
          age <- 1:max_age
          length <- bL_inf * (1 - exp(-bk * (age - ba0)))
          fecundity <- 10^(-5 + 3 * log(fl2tl(length), base = 2))
          survival <- c(1, rep(bS_J, 2), rep(bS_A, max_age - 1))
          survivorship <- cumprod(survival)
          maturity <- age^14/(bAs^14 + age^14)
          eggs <- survivorship * maturity * fecundity * 0.5
          prediction <- 1/sum(eggs)
      }

# sum() inside the expression leaves the for loop unchanged

    Code
      expression_vectorize(rlang::expr(for (i in seq_along(Year)) {
        eGrowth[i] <- max(0, (bLinf - LengthAtRelease[i]) * (1 - exp(-sum(eK[Year[i]:
          (Year[i] + dYears[i] - 1)]))))
      }))
    Output
      for (i in seq_along(Year)) {
          eGrowth[i] <- max(0, (bLinf - LengthAtRelease[i]) * (1 - 
              exp(-sum(eK[Year[i]:(Year[i] + dYears[i] - 1)]))))
      }

# cbind with var and constant

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        eDensity[i] <- bDensity[Island[i], Day[i]]
        ePopn[i] <- bPopn[Island[i], 1]
      }))
    Output
      {
          eDensity <- bDensity[cbind(Island, Day)]
          ePopn <- bPopn[cbind(Island, 1)]
      }

# code before for loop

    Code
      expression_vectorize(rlang::expr({
        b0 <- 2
        for (i in 1:nObs) {
          log(eCount[i]) <- b0
        }
      }))
    Output
      {
          b0 <- 2
          log(eCount) <- b0
      }

# more than two dimensions

    Code
      expression_vectorize(rlang::expr(for (i in 1:nObs) {
        log(eCount[i]) <- b0 + bKelpLine * KelpLine[i] + bYear * Year[i] + bSite[Site[
          i]] + bSiteAnnual[Site[i], Annual[i]] + bAnnual[Annual[i]]
        dpois(eCount[i] * bSiteAnnualQuadrat[Site[i], Annual[i], Quadrat[i]])
      }))
    Output
      {
          log(eCount) <- b0 + bKelpLine * KelpLine + bYear * Year + 
              bSite[Site] + bSiteAnnual[cbind(Site, Annual)] + bAnnual[Annual]
          dpois(eCount * bSiteAnnualQuadrat[cbind(Site, Annual, Quadrat)])
      }

