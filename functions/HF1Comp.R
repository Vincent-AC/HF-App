HF1Comp <- function(model,
                    halfLifeHours,
                    Vcentral,
                    Vcartridge,
                    initial_concentration,
                    lastTimePointHours,
                    drugName,
                    VinjectBolus =  NA,
                    dosingIntervalHoursBolus = NA,
                    numberOfDosesBolus = NA,
                    tinfuseHours = NA,
                    VinjectInf =  NA,
                    dosingIntervalHoursInf = NA,
                    numberOfDosesInf = NA,
                    adm.type,
                    constantVolume = T,
                    debit_central_cartridge = 120,
                    Css = NA)
{
  Parameters <- HollowFibre1CompParam(
    halfLifeHours,
    Vcentral,
    Vcartridge,
    initial_concentration,
    lastTimePointHours,
    drugName,
    VinjectBolus,
    dosingIntervalHoursBolus,
    numberOfDosesBolus,
    tinfuseHours,
    VinjectInf,
    dosingIntervalHoursInf,
    numberOfDosesInf,
    adm.type,
    debit_central_cartridge
  )

  SimData <- HollowFibre1CompSimData(
    model,
    halfLifeHours,
    Vcentral,
    Vcartridge,
    initial_concentration,
    lastTimePointHours,
    drugName,
    VinjectBolus,
    dosingIntervalHoursBolus,
    numberOfDosesBolus,
    tinfuseHours,
    VinjectInf,
    dosingIntervalHoursInf,
    numberOfDosesInf,
    adm.type,
    constantVolume,
    debit_central_cartridge,
    Css
  )

  Plot <- HollowFibre1CompPlot(
    SimData,
    lastTimePointHours,
    dosingIntervalHoursBolus,
    dosingIntervalHoursInf,
    adm.type,
    drugName
  )

  Diagram <- HollowFibre1CompDiagram(Parameters,
                                     adm.type,
                                     drugName)


  return(list(Parameters, Plot, Diagram, SimData))
}
