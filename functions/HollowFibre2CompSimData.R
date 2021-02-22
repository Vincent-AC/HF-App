HollowFibre2CompSimData  <- function(model,
                                  halfLifeHours_A = 10,
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
                                  adm.type = "Bolus",
                                  tinfuseHours_A = NA,
                                  tinfuseHours_B = NA,
                                  Vinject_AInf =  NA,
                                  Vinject_B_centralInf = NA,
                                  Vinject_B_extraInf = NA,
                                  dosingIntervalHours_AInf = NA,
                                  dosingIntervalHours_BInf = NA,
                                  numberOfDosesAInf = NA,
                                  numberOfDosesBInf = NA,
                                  constantVolume = T)
{
        library(mrgsolve)
        library(dplyr)
        halfLifeAmin <- halfLifeHours_A * 60 #hours * minutes/hours
        halfLifeBmin <- halfLifeHours_B * 60 #hours * minutes/hours

        clearanceA <-
                log(2) * (Vcentral + Vcartridge) / halfLifeAmin #ml/min
        clearanceB <-
                log(2) * (Vcentral + Vcartridge) / halfLifeBmin #ml/min
        Vextra <- ifelse(clearanceA-clearanceB != 0, ((clearanceA - clearanceB) / clearanceB) * (Vcentral + Vcartridge) ,1)
        #mL protection for same halflife

        debit_pompe_extra_central <- clearanceA - clearanceB #ml/min
        debit_pompe_central_waste <- clearanceA #ml/min

        #Declare last time point to be simulated
        lastTimePointMin <-
                lastTimePointHours * 60 #in minutes = durée de la manip

        parameterValues <-
                c(
                        Q1 = debit_pompe_extra_central   ,
                        V1 = Vextra ,
                        Q2 = debit_pompe_central_waste ,
                        V2 = Vcentral + Vcartridge
                )



        if (adm.type == "Bolus")
        {
                dosingIntervalMin_ABolus <- dosingIntervalHours_ABolus * 60
                dosingIntervalMin_BBolus <- dosingIntervalHours_BBolus * 60
                doseA_bolus_central <-
                        initial_concentration_A * (Vcentral + Vcartridge) #µg
                doseB_bolus_central <-
                        initial_concentration_B * (Vcentral + Vcartridge) #µg
                doseB_bolus_extra <-
                        doseB_bolus_central * ((clearanceA - clearanceB) / clearanceB) #µg

                #Dose events for each compartments
                e1_bolus <-
                        ev(
                                amt = doseA_bolus_central,
                                cmt = 1,
                                ii = dosingIntervalMin_ABolus,
                                addl = numberOfDosesABolus - 1
                        )
                e2_bolus <-
                        ev(
                                amt = doseB_bolus_extra,
                                cmt = 2,
                                ii = dosingIntervalMin_BBolus,
                                addl = numberOfDosesBBolus - 1
                        )
                e3_bolus <-
                        ev(
                                amt = doseB_bolus_central,
                                cmt = 3,
                                ii = dosingIntervalMin_BBolus,
                                addl = numberOfDosesBBolus - 1
                        )
                dose <- e1_bolus + e2_bolus + e3_bolus
                dose <- realize_addl(dose)
                if (constantVolume == FALSE)
                {
                        Vadd_bolus_central_A <-
                                ev(
                                        amt = Vinject_ABolus,
                                        cmt = 4,
                                        ii = dosingIntervalMin_ABolus,
                                        addl = numberOfDosesABolus - 1
                                )
                        Vadd_bolus_central_B <-
                                ev(
                                        amt = Vinject_B_centralBolus,
                                        cmt = 5,
                                        ii = dosingIntervalMin_BBolus,
                                        addl = numberOfDosesBBolus - 1
                                )
                        Vadd_bolus_extra_B <-
                                ev(
                                        amt = Vinject_B_extraBolus,
                                        cmt = 6,
                                        ii = dosingIntervalMin_BBolus,
                                        addl = numberOfDosesBBolus - 1
                                )
                        dose <-
                                e1_bolus + e2_bolus + e3_bolus + Vadd_bolus_central_A + Vadd_bolus_central_B +
                                Vadd_bolus_extra_B
                        dose <- realize_addl(dose)
                }



                simulated_data <- model %>%
                        ev(dose) %>%
                        param(parameterValues) %>%
                        mrgsim(end = lastTimePointMin) %>%
                        as_tibble

                #Red is drug A

        }
        if (adm.type == "Infusion")
        {
                tinfuseMin_A <- tinfuseHours_A * 60
                tinfuseMin_B <- tinfuseHours_B * 60

                dosingIntervalMin_AInf <-
                        dosingIntervalHours_AInf * 60
                dosingIntervalMin_BInf <-
                        dosingIntervalHours_BInf * 60

                # IV infusion
                keHours_A <- log(2) / halfLifeHours_A
                keHours_B <- log(2) / halfLifeHours_B
                doseA_infuse_central <-
                        (
                                initial_concentration_A * (Vcentral + Vcartridge) * keHours_A * tinfuseHours_A
                        ) / (1 - exp(-keHours_A *
                                             tinfuseHours_A))#µg
                doseB_infuse_central <-
                        (
                                initial_concentration_B * (Vcentral + Vcartridge) * keHours_B * tinfuseHours_B
                        ) / (1 - exp(-keHours_B *
                                             tinfuseHours_B))#µg
                doseB_infuse_extra <-
                        doseB_infuse_central * ((clearanceA - clearanceB) / clearanceB) #µg
                #Dose events for each compartments
                e1_infuse <-
                        ev(
                                amt = doseA_infuse_central,
                                rate = doseA_infuse_central / tinfuseMin_A,
                                cmt = 1,
                                ii = dosingIntervalMin_AInf,
                                addl = numberOfDosesAInf - 1
                        )
                e2_infuse <-
                        ev(
                                amt = doseB_infuse_extra,
                                rate = doseB_infuse_extra / tinfuseMin_B,
                                cmt = 2,
                                ii = dosingIntervalMin_BInf,
                                addl = numberOfDosesBInf - 1
                        )
                e3_infuse <-
                        ev(
                                amt = doseB_infuse_central,
                                rate = doseB_infuse_central / tinfuseMin_B,
                                cmt = 3,
                                ii = dosingIntervalMin_BInf,
                                addl = numberOfDosesBInf - 1
                        )
                dose <- e1_infuse + e2_infuse + e3_infuse
                dose <- realize_addl(dose)
                if (constantVolume == FALSE)
                {
                        Vadd_infusion_A <-
                                ev(
                                        amt = Vinject_AInf,
                                        rate = Vinject_AInf / tinfuseMin_A,
                                        cmt = 4,
                                        ii = dosingIntervalMin_AInf,
                                        addl = numberOfDosesAInf - 1
                                )
                        Vadd_infusion_B_central <-
                                ev(
                                        amt = Vinject_B_centralInf,
                                        rate = Vinject_B_central / tinfuseMin_B,
                                        cmt = 5,
                                        ii = dosingIntervalMin_BInf,
                                        addl = numberOfDosesBInf - 1
                                )
                        Vadd_infusion_B_extra <-
                                ev(
                                        amt = Vinject_B_extraInf,
                                        rate = Vinject_B_extra / tinfuseMin_B,
                                        cmt = 6,
                                        ii = dosingIntervalMin_BInf,
                                        addl = numberOfDosesBInf - 1
                                )

                        dose <-
                                e1_infuse + e2_infuse + e3_infuse + Vadd_infusion_A + Vadd_infusion_B_extra + Vadd_infusion_B_central
                        dose <- realize_addl(dose)
                }


                simulated_data <- model %>%
                        ev(dose) %>%
                        param(parameterValues) %>%
                        mrgsim(end = lastTimePointMin) %>%
                        as_tibble


        }

        return(simulated_data)
}