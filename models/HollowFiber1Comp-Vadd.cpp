$PROB
- Author: Vincent Aranzana-Climent
- Date: 02/05/19
- Structure: Hollow fiber 1 bottle bolus
- Implementation: ODE
- Error model: None
- Random effects: None

$PARAM @annotated
Q1 : 0.5 : Flowrate of transfer from central bottle to waste (mL/min)
V1 : 100 : Volume in central bottle (mL)

$CMT @annotated

DRUG_CENTRAL : Amount of drug in central bottle (mcg)
Vadd : Cumulative added volume at each dose (mL)
$MAIN
DRUG_CENTRAL_0 = 0 ;
$ODE
double V = V1 + Vadd;
dxdt_DRUG_CENTRAL = -(Q1/V) * DRUG_CENTRAL;
dxdt_Vadd = 0;

$TABLE
double C_DRUG_CENTRAL = (DRUG_CENTRAL/V);

$CAPTURE @annotated
C_DRUG_CENTRAL : Concentration of drug in central bottle (mcg/mL)


