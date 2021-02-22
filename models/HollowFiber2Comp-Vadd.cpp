$PROB
- Author: Vincent Aranzana-Climent
- Date: 04/02/2019 - 15/02/18 - 02/05/19
- Structure: Hollow fiber 2 bottles bolus
- Implementation: ODE
- Error model: None
- Random effects: None

$PARAM @annotated
Q1 : 1 : Flow rate of transfer from extra drug bottle to central (mL/min)
V1 : 100: Volume in that extra drug bottle (mL)
Q2 : 0.5 : Flowrate of transfer from central bottle to waste (mL/min)
V2 : 100 : Volume in central bottle (mL)

$CMT @annotated
DRUG_A_CENTRAL : Amount of drug A in central bottle (mcg)
DRUG_B_EXTRA : Amount of drug B in extra drug bottle (mcg)
DRUG_B_CENTRAL : Amount of drug B in central bottle (mcg)
Vadd_A_Central : Volume added by dosing drug A in central bottle (mL)
Vadd_B_Central : Volume added by dosing drug B in central bottle (mL)
Vadd_B_Extra : Volume added by dosing drug B in extra bottle (mL)

$ODE
double Ve = V1 + Vadd_B_Extra;
double Vc = V2 + Vadd_A_Central + Vadd_B_Central;
dxdt_DRUG_A_CENTRAL = -(Q2/Vc) * DRUG_A_CENTRAL;
dxdt_DRUG_B_EXTRA = -(Q1/Ve) * DRUG_B_EXTRA; 
dxdt_DRUG_B_CENTRAL = (Q1/Ve) * DRUG_B_EXTRA  -(Q2/Vc) * DRUG_B_CENTRAL;
dxdt_Vadd_A_Central = 0;
dxdt_Vadd_B_Central = 0;
dxdt_Vadd_B_Extra = 0;


$TABLE
double C_DRUG_A_CENTRAL = (DRUG_A_CENTRAL/Vc);
double C_DRUG_B_EXTRA = (DRUG_B_EXTRA/Ve);
double C_DRUG_B_CENTRAL = (DRUG_B_CENTRAL/Vc);

$CAPTURE @annotated
C_DRUG_A_CENTRAL : Concentration of drug A in central bottle (mcg/mL)
C_DRUG_B_EXTRA : Concentration of drug B in extra drug bottle (mcg/mL)
C_DRUG_B_CENTRAL : Concentration of drug B in central bottle (mcg/mL)
Ve : Volume extra (mL)
Vc : Volume central (mL)

