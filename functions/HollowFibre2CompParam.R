HollowFibre2CompParam <- function (halfLifeHours_A = 10,
                                        halfLifeHours_B = 16,
                                        Vcentral = 200,
                                        Vcartridge = 50,
                                        initial_concentration_A = 2,
                                        initial_concentration_B = 10,
                                        lastTimePointHours = 24,
                                        Vinject_ABolus =  NA,
                                        Vinject_B_centralBolus = NA,
                                        Vinject_B_extraBolus = NA,
                                        dosingIntervalHours_ABolus = NA,
                                        dosingIntervalHours_BBolus = NA,
                                        numberOfDosesABolus = NA,
                                        numberOfDosesBBolus = NA,
                                        drugNameA = NA,
                                        drugNameB = NA,
                                        adm.type="Bolus",
                                   tinfuseHours_A=NA,
                                   tinfuseHours_B=NA,
                                   Vinject_AInf =  NA,
                                   Vinject_B_centralInf = NA,
                                   Vinject_B_extraInf = NA,
                                   dosingIntervalHours_AInf = NA,
                                   dosingIntervalHours_BInf = NA,
                                   numberOfDosesAInf = NA,
                                   numberOfDosesBInf = NA,
                                   debit_central_cartridge = 120)
{
  library(plyr)
  library(dplyr)
  halfLifeAmin <- halfLifeHours_A * 60 #hours * minutes/hours
  halfLifeBmin <- halfLifeHours_B * 60 #hours * minutes/hours

  clearanceA <-
    log(2) * (Vcentral + Vcartridge) / halfLifeAmin #ml/min
  clearanceB <-
    log(2) * (Vcentral + Vcartridge) / halfLifeBmin #ml/min
  Vextra <- ifelse(clearanceA-clearanceB != 0, ((clearanceA - clearanceB) / clearanceB) * (Vcentral + Vcartridge) ,0)

  debit_pompe_extra_central <- clearanceA - clearanceB #ml/min
  debit_pompe_central_waste <- clearanceA #ml/min
  debit_pompe_dil_extra <- clearanceA - clearanceB #ml/min
  debit_pompe_dil_central <- clearanceB #ml/min

  #Declare last time point to be simulated
  lastTimePointmin <-
    lastTimePointHours * 60 #in minutes = durée de la manip

  volume_waste_produced <-
    debit_pompe_central_waste * lastTimePointmin #mL
  volume_diluant_to_extra <-
    debit_pompe_extra_central * lastTimePointmin #mL
  volume_diluant_to_central <-
    clearanceB * lastTimePointmin #mL

 if(adm.type=="Bolus")
{
   total_injected_volume_A <- Vinject_ABolus * numberOfDosesABolus
   total_injected_volume_B_central <-
     Vinject_B_centralBolus * numberOfDosesBBolus
   total_injected_volume_B_extra <- Vinject_B_extraBolus * numberOfDosesBBolus
   # IV bolus
         doseA_central <-
                 initial_concentration_A * (Vcentral + Vcartridge) #µg
         conc_bolus_A <- doseA_central / Vinject_ABolus
         doseB_central <-
                 initial_concentration_B * (Vcentral + Vcartridge) #µg
         conc_bolus_B_central <- doseB_central / Vinject_B_centralBolus
         doseB_extra <-
                 doseB_central * ((clearanceA - clearanceB) / clearanceB) #µg
         conc_bolus_B_extra <- doseB_extra / Vinject_B_extraBolus
         #Create a table that summarizes everything
  Parameters <- tibble(
    Group = c(rep("Drugs",9),
              rep("Volume",6),
              rep("Bolus",10),
              rep("Experiment",6)),
    Parameter = c(
      "Drug A name",
      "Drug B name",
      "Cmax central after 1 dose for drug A (µg/mL)",
      "Bolus solution concentration for drug A (µg/mL)",
      "Half life central for drug A (h)",
      "Cmax central after 1 dose for drug B (µg/mL)",
      "Bolus solution concentration for drug B in central (µg/mL)",
      "Bolus solution concentration for drug B in extra (µg/mL)",
      "Half life central for drug B (h)",
      "Central volume (mL)",
      "Cartridge volume (mL)",
      "Extra compartment volume (mL)",
      "Spent volume diluant to central (mL)",
      "Spent volume diluant to extra (mL)",
      "Waste volume (mL)",
      "Bolus volume for 1 dose of drug A (mL)",
      "Bolus volume for 1 dose of drug B in central (mL)",
      "Bolus volume for 1 dose of drug B in extra (mL)",
      "Total number of doses of drug A",
      "Total number of doses of drug B",
      "Dosing interval for drug A (h)",
      "Dosing interval for drug B (h)",
      "Total injected volume of drug A (mL)",
      "Total injected volume of drug B in central (mL)",
      "Total injected volume of drug B in extra (mL)",
      "Duration of experiment (h)",
      "Flow pump diluant extra to extra (mL/min)",
      "Flow pump extra to central (mL/min)",
      "Flow pump diluant central to central (mL/min)",
      "Flow pump central to waste (mL/min)",
      "Flow pump central to cartidge (mL/min)"
    ),
    Value = c(drugNameA,
              drugNameB,
              round(
                c(
                  initial_concentration_A,
                  conc_bolus_A,
                  halfLifeHours_A,
                  initial_concentration_B,
                  conc_bolus_B_central,
                  conc_bolus_B_extra,
                  halfLifeHours_B,
                  Vcentral,
                  Vcartridge,
                  Vextra,
                  volume_diluant_to_central,
                  volume_diluant_to_extra,
                  volume_waste_produced,
                  Vinject_ABolus,
                  Vinject_B_centralBolus,
                  Vinject_B_extraBolus,
                  numberOfDosesABolus,
                  numberOfDosesBBolus,
                  dosingIntervalHours_ABolus,
                  dosingIntervalHours_BBolus,
                  total_injected_volume_A,
                  total_injected_volume_B_central,
                  total_injected_volume_B_extra,
                  lastTimePointHours,
                  debit_pompe_dil_extra,
                  debit_pompe_extra_central,
                  debit_pompe_dil_central,
                  debit_pompe_central_waste,
                  debit_central_cartridge
                ),
                3
              )),
    ID =        c(
      "drugNameA",
      "drugNameB",
      "initial_concentration_A",
      "conc_bolus_A",
      "halfLifeHours_A",
      "initial_concentration_B",
      "conc_bolus_B_central",
      "conc_bolus_B_extra",
      "halfLifeHours_B",
      "Vcentral",
      "Vcartridge",
      "Vextra",
      "volume_diluant_to_central",
      "volume_diluant_to_extra",
      "volume_waste_produced",
      "Vinject_A",
      "Vinject_B_central",
      "Vinject_B_extra",
      "numberOfDosesA",
      "numberOfDosesB",
      "dosingIntervalHours_A",
      "dosingIntervalHours_B",
      "total_injected_volume_A",
      "total_injected_volume_B_central",
      "total_injected_volume_B_extra",
      "lastTimePointHours",
      "debit_pompe_dil_extra",
      "debit_pompe_extra_central",
      "debit_pompe_dil_central",
      "debit_pompe_central_waste",
      "debit_central_cartridge"
    )
  )}
  if(adm.type=="Infusion")
  {
    total_injected_volume_A <- Vinject_AInf * numberOfDosesAInf
    total_injected_volume_B_central <-
      Vinject_B_centralInf * numberOfDosesBInf
    total_injected_volume_B_extra <- Vinject_B_extraInf * numberOfDosesBInf
          tinfuseMin_A <- tinfuseHours_A * 60
          debit_infuse_A <- Vinject_AInf / tinfuseMin_A
          keHours_A <- log(2) / halfLifeHours_A
          dose_infuse_A <-
                  (
                          initial_concentration_A * (Vcentral + Vcartridge) * keHours_A * tinfuseHours_A
                  ) / (1 - exp(-keHours_A *
                                       tinfuseHours_A))#µg
          conc_infuse_A <- dose_infuse_A / Vinject_AInf


          tinfuseMin_B <- tinfuseHours_B * 60
          debit_infuse_B_central <- Vinject_B_centralInf / tinfuseMin_B
          keHours_B <- log(2) / halfLifeHours_B
          dose_infuse_B_central <-
                  (
                          initial_concentration_B * (Vcentral + Vcartridge) * keHours_B * tinfuseHours_B
                  ) / (1 - exp(-keHours_B *
                                       tinfuseHours_B))#µg
          conc_infuse_B_central <- dose_infuse_B_central / Vinject_B_centralInf

          debit_infuse_B_extra <- Vinject_B_extraInf / tinfuseMin_B
          dose_infuse_B_extra <-
                  dose_infuse_B_central * ((clearanceA - clearanceB) / clearanceB) #µg
          conc_infuse_B_extra <- dose_infuse_B_extra / Vinject_B_extraInf
          # IV bolus
          # IV bolus
          #Create a table that summarizes everything
          Parameters <- tibble(
            Group = c(rep("Drugs",9),
                      rep("Volume",6),
                      rep("Infusion",10),
                      rep("Experiment",9)),
                  Parameter = c(
                          "Drug A name",
                          "Drug B name",
                          "Cmax central after 1 dose for drug A (µg/mL)",
                          "Infusion solution concentration for drug A (µg/mL)",
                          "Half life central for drug A (h)",
                          "Cmax central after 1 dose for drug B (µg/mL)",
                          "Infusion solution concentration for drug B in central (µg/mL)",
                          "Infusion solution concentration for drug B in extra (µg/mL)",
                          "Half life central for drug B (h)",
                          "Central volume (mL)",
                          "Cartridge volume (mL)",
                          "Extra compartment volume (mL)",
                          "Spent volume diluant to central (mL)",
                          "Spent volume diluant to extra (mL)",
                          "Waste volume (mL)",
                          "Infusion volume for 1 dose of drug A (mL)",
                          "Infusion volume for 1 dose of drug B in central (mL)",
                          "Infusion volume for 1 dose of drug B in extra (mL)",
                          "Total number of doses of drug A",
                          "Total number of doses of drug B",
                          "Dosing interval for drug A (h)",
                          "Dosing interval for drug B (h)",
                          "Total injected volume of drug A (mL)",
                          "Total injected volume of drug B in central (mL)",
                          "Total injected volume of drug B in extra (mL)",
                          "Duration of experiment (h)",
                          "Flow pump diluant extra to extra (mL/min)",
                          "Flow pump extra to central (mL/min)",
                          "Flow pump diluant central to central (mL/min)",
                          "Flow pump central to waste (mL/min)",
                          "Flow pump central to cartridge (mL/min)",
                          "Flow infusion A to central (mL/min)",
                          "Flow infusion B to central (mL/min)",
                          "Flow infusion B to extra (mL/min)"
                  ),
                  Value = c(drugNameA,
                            drugNameB,
                            round(
                                    c(
                                            initial_concentration_A,
                                            conc_infuse_A,
                                            halfLifeHours_A,
                                            initial_concentration_B,
                                            conc_infuse_B_central,
                                            conc_infuse_B_extra,
                                            halfLifeHours_B,
                                            Vcentral,
                                            Vcartridge,
                                            Vextra,
                                            volume_diluant_to_central,
                                            volume_diluant_to_extra,
                                            volume_waste_produced,
                                            Vinject_AInf,
                                            Vinject_B_centralInf,
                                            Vinject_B_extraInf,
                                            numberOfDosesAInf,
                                            numberOfDosesBInf,
                                            dosingIntervalHours_AInf,
                                            dosingIntervalHours_BInf,
                                            total_injected_volume_A,
                                            total_injected_volume_B_central,
                                            total_injected_volume_B_extra,
                                            lastTimePointHours,
                                            debit_pompe_dil_extra,
                                            debit_pompe_extra_central,
                                            debit_pompe_dil_central,
                                            debit_pompe_central_waste,
                                            debit_central_cartridge,
                                            debit_infuse_A,
                                            debit_infuse_B_central,
                                            debit_infuse_B_extra
                                    ),
                                    3
                            )),
                  ID =        c(
                          "drugNameA",
                          "drugNameB",
                          "initial_concentration_A",
                          "conc_infuse_A",
                          "halfLifeHours_A",
                          "initial_concentration_B",
                          "conc_infuse_B_central",
                          "conc_infuse_B_extra",
                          "halfLifeHours_B",
                          "Vcentral",
                          "Vcartridge",
                          "Vextra",
                          "volume_diluant_to_central",
                          "volume_diluant_to_extra",
                          "volume_waste_produced",
                          "Vinject_A",
                          "Vinject_B_central",
                          "Vinject_B_extra",
                          "numberOfDosesA",
                          "numberOfDosesB",
                          "dosingIntervalHours_A",
                          "dosingIntervalHours_B",
                          "total_injected_volume_A",
                          "total_injected_volume_B_central",
                          "total_injected_volume_B_extra",
                          "lastTimePointHours",
                          "debit_pompe_dil_extra",
                          "debit_pompe_extra_central",
                          "debit_pompe_dil_central",
                          "debit_pompe_central_waste",
                          "debit_central_cartridge",
                          "debit_infuse_A",
                          "debit_infuse_B_central",
                          "debit_infuse_B_extra"
                  )
          )}

  return(Parameters)
}
