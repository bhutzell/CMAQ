Information is based on the mech.def file.
* Fall-off or pressure dependent reaction rate constants (M equals air number density):
 * For rate constants with k<sub>o</sub>, k<sub>i</sub>, n, F values: k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>i</sub>)]F<sup>G</sup>, where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>i</sub>)/n)<sup>2</sup>)<sup>-1</sup> 
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub>M
 * For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> 

* For rate constants with the form A<_Reference_>, k equals A times a reference that represents photolysis rate, a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given.

* In the mechanism definition file, the rate is formatted as
 * A~<_HETEROGENEOUS_>
 * A*K<_REACTION_>
 * A/<_PHOTOLYSIS_>
 * A?<_OPERATOR_>

|Label|Reaction            |Rate Constant Formula| Value<br> molecules/(sec*cm<sup>3</sup>)|   
|:---|:-------------------|:--------------------|:----:|   
| 1   | NO2 ----> NO + O3P  | NO2_06 | Not Available<sup>1</sup> | 
| 2   | O3P + O2 + M ----> O3  |   5.68E-34(T/300)<sup> -2.60</sup> |   5.7721E-34 |
| 3   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| 4   | O3P + NO ----> NO2  | k<sub>o</sub>=  9.00E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6618E-12 |
| 5   | O3P + NO2 ----> NO  |   5.50E-12e<sup>   188.00/T</sup> |   1.0333E-11 |
| 6   | O3P + NO2 ----> NO3  | k<sub>o</sub>=  2.50E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.80</sup><br>k<sub>i</sub> =   2.20E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.70</sup><br>n=     1.00;F=     0.60 |   3.2805E-12 |
| 7   | O3 + NO ----> NO2  |   3.00E-12e<sup> -1500.00/T</sup> |   1.9596E-14 |
| 8   | O3 + NO2 ----> NO3  |   1.40E-13e<sup> -2470.00/T</sup> |   3.5339E-17 |
| 9   | NO + NO3 ---->   2.0000\*NO2  |   1.80E-11e<sup>   110.00/T</sup> |   2.6032E-11 |
| 10   | NO + NO + O2 ---->   2.0000\*NO2  |   3.30E-39e<sup>   530.00/T</sup> |   1.9522E-38 |
| 11   | NO2 + NO3 ----> N2O5  | k<sub>o</sub>=  3.60E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.90E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.33;F=     0.35 |   1.2406E-12 |
| 12   | N2O5 ----> NO2 + NO3  | k<sub>o</sub>=  1.30E-03e<sup>-11000.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   9.70E+14e<sup>-11080.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.33;F=     0.35 |   4.5396E-02 |
| 13   | N2O5 + H2O ---->   2.0000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| 14   | N2O5 + H2O + H2O ---->   2.0000\*HNO3  |   0.0000E+00 |   0.0000E+00 |
| 15   | NO2 + NO3 ----> NO + NO2  |   4.50E-14e<sup> -1260.00/T</sup> |   6.5744E-16 |
| 16   | NO3 ----> NO  | NO3NO_06 | Not Available<sup>1</sup> | 
| 17   | NO3 ----> NO2 + O3P  | NO3NO2_6 | Not Available<sup>1</sup> | 
| 18   | O3 ----> O1D  | O3O1D_06 | Not Available<sup>1</sup> | 
| 19   | O3 ----> O3P  | O3O3P_06 | Not Available<sup>1</sup> | 
| 20   | O1D + H2O ---->   2.0000\*OH  |   1.63E-10e<sup>    60.00/T</sup> |   1.9934E-10 |
| 21   | O1D + M ----> O3P  |   2.38E-11e<sup>    96.00/T</sup> |   3.2841E-11 |
| 22   | OH + NO ----> HONO  | k<sub>o</sub>=  7.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.3998E-12 |
| 23   | HONO ----> OH + NO  | HONO_06 | Not Available<sup>1</sup> | 
| 24   | OH + HONO ----> NO2  |   2.50E-12e<sup>   260.00/T</sup> |   5.9795E-12 |
| 25   | OH + NO2 ----> HNO3  | k<sub>o</sub>=  3.20E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.24;F=     0.41 |   9.8821E-12 |
| 26   | OH + NO3 ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| 27   | OH + HNO3 ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| 28   | HNO3 ----> OH + NO2  | HNO3 | Not Available<sup>1</sup> | 
| 29   | OH + CO ----> HO2 + CO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  3.43E-33e<sup>     0.0/T</sup> |   2.2843E-13 |
| 30   | OH + O3 ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| 31   | HO2 + NO ----> OH + NO2  |   3.60E-12e<sup>   270.00/T</sup> |   8.9042E-12 |
| 32   | HO2 + NO2 ----> HNO4  | k<sub>o</sub>=  2.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   2.90E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.10</sup><br>n=     1.00;F=     0.60 |   1.1385E-12 |
| 33   | HNO4 ----> HO2 + NO2  | k<sub>o</sub>=  3.72E-05e<sup>-10650.0/T</sup>(T/300)<sup> -2.40</sup><br>k<sub>i</sub> =   5.42E+15e<sup>-11170.0/T</sup>(T/300)<sup> -2.30</sup><br>n=     1.00;F=     0.60 |   8.6986E-02 |
| 34   | HNO4 ---->   0.6100\*HO2 +    0.6100\*NO2 +    0.3900\*OH +    0.3900\*NO3  | HNO4_06 | Not Available<sup>1</sup> | 
| 35   | HNO4 + OH ----> NO2  |   1.30E-12e<sup>   380.00/T</sup> |   4.6501E-12 |
| 36   | HO2 + O3 ----> OH  |   2.03E-16e<sup>   693.00/T</sup>(T/300)<sup>  4.57 </sup> |   2.0168E-15 |
| 37   | HO2 + HO2 ----> HO2H  | k<sub>0</sub>=  2.20E-13e<sup>   600.0/T</sup><br>k<sub>1</sub>=  1.90E-33e<sup>   980.0/T</sup> |   2.8975E-12 |
| 38   | HO2 + HO2 + H2O ----> HO2H  | k<sub>0</sub>=  3.08E-34e<sup>  2800.0/T</sup><br>k<sub>1</sub>=  2.66E-54e<sup>  3180.0/T</sup> |   6.4973E-30 |
| 39   | NO3 + HO2 ---->   0.8000\*OH +    0.8000\*NO2 +    0.2000\*HNO3  |   4.0000E-12 |   4.0000E-12 |
| 40   | NO3 + NO3 ---->   2.0000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| 41   | HO2H ---->   2.0000\*OH  | H2O2 | Not Available<sup>1</sup> | 
| 42   | HO2H + OH ----> HO2  |   1.8000E-12 |   1.8000E-12 |
| 43   | OH + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| 44   | OH + SO2 ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  3.30E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.30</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   9.5810E-13 |
| 45   | OH + H2 ----> HO2  |   7.70E-12e<sup> -2100.00/T</sup> |   6.7230E-15 |
| BR01   | MEO2 + NO ----> NO2 + HCHO + HO2  |   2.30E-12e<sup>   360.00/T</sup> |   7.6933E-12 |
| BR02   | MEO2 + HO2 ----> COOH  |   3.46E-13e<sup>   780.00/T</sup>(T/300)<sup>  0.36 </sup> |   4.7237E-12 |
| BR03   | MEO2 + HO2 ----> HCHO  |   3.34E-14e<sup>   780.00/T</sup>(T/300)<sup> -3.53 </sup> |   4.6709E-13 |
| BR04   | MEO2 + NO3 ----> HCHO + HO2 + NO2  |   1.3000E-12 |   1.3000E-12 |
| BR05   | MEO2 + MEO2 ----> MEOH + HCHO  |   6.39E-14e<sup>   365.00/T</sup>(T/300)<sup> -1.80 </sup> |   2.1979E-13 |
| BR06   | MEO2 + MEO2 ---->   2.0000\*HCHO +    2.0000\*HO2  |   7.40E-13e<sup>  -520.00/T</sup> |   1.2936E-13 |
| BR07   | RO2C + NO ----> NO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| BR08   | RO2C + HO2 ----> |   3.80E-13e<sup>   900.00/T</sup> |   7.7759E-12 |
| BR09   | RO2C + NO3 ----> NO2  |   2.3000E-12 |   2.3000E-12 |
| BR10   | RO2C + MEO2 ---->   0.5000\*HO2 +    0.7500\*HCHO +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| BR11   | RO2C + RO2C ----> |   3.5000E-14 |   3.5000E-14 |
| BR12   | RO2XC + NO ----> |   BR07 |   9.3002E-12<sup>7</sup>| 
| BR13   | RO2XC + HO2 ----> |   BR08 |   7.7759E-12<sup>7</sup>| 
| BR14   | RO2XC + NO3 ----> NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR15   | RO2XC + MEO2 ---->   0.5000\*HO2 +    0.7500\*HCHO +    0.2500\*MEOH  |   BR10 |   2.0000E-13<sup>7</sup>| 
| BR16   | RO2XC + RO2C ----> |   BR11 |   3.5000E-14<sup>7</sup>| 
| BR17   | RO2XC + RO2XC ----> |   BR11 |   3.5000E-14<sup>7</sup>| 
| BR18   | MECO3 + NO2 ----> PAN  | k<sub>o</sub>=  2.70E-28e<sup>     0.0/T</sup>(T/300)<sup> -7.10</sup><br>k<sub>i</sub> =   1.21E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.90</sup><br>n=     1.41;F=     0.30 |   9.4624E-12 |
| BR19   | PAN ----> MECO3 + NO2  | k<sub>o</sub>=  4.90E-03e<sup>-12100.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   4.00E+16e<sup>-13600.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.41;F=     0.30 |   4.7483E-04 |
| BR20   | PAN ---->   0.6000\*MECO3 +    0.6000\*NO2 +    0.4000\*MEO2 +    0.4000\*CO2 +    0.4000\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR21   | MECO3 + NO ----> MEO2 + CO2 + NO2  |   7.50E-12e<sup>   290.00/T</sup> |   1.9837E-11 |
| BR22   | MECO3 + HO2 ---->   0.1050\*CCOOOH +    0.0450\*CCOOH +    0.1500\*O3 +    0.4400\*OH +    0.4400\*MEO2 +    0.4400\*CO2  |   5.20E-13e<sup>   980.00/T</sup> |   1.3916E-11 |
| BR23   | MECO3 + NO3 ----> MEO2 + CO2 + NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR24   | MECO3 + MEO2 ---->   0.1000\*CCOOH + HCHO +    0.9000\*HO2 +    0.9000\*MEO2 +    0.9000\*CO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| BR25   | MECO3 + RO2C ----> MEO2 + CO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| BR26   | MECO3 + RO2XC ----> MEO2 + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR27   | MECO3 + MECO3 ---->   2.0000\*MEO2 +    2.0000\*CO2  |   2.90E-12e<sup>   500.00/T</sup> |   1.5514E-11 |
| BR28   | RCO3 + NO2 ----> PAN2  |   1.21E-11e<sup>     0.00/T</sup>(T/300)<sup> -1.07 </sup> |   1.2180E-11 |
| BR29   | PAN2 ----> RCO3 + NO2  |   8.30E+16e<sup>-13940.00/T</sup> |   4.1081E-04 |
| BR30   | PAN2 ---->   0.6000\*RCO3 +    0.6000\*NO2 +    0.4000\*RO2C +    0.4000\*xHO2 +    0.4000\*yROOH +    0.4000\*xCCHO +    0.4000\*CO2 +    0.4000\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR31   | RCO3 + NO ----> NO2 + RO2C + xHO2 + yROOH + xCCHO + CO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| BR32   | RCO3 + HO2 ---->   0.3075\*RCOOOH +    0.1025\*RCOOH +    0.1500\*O3 +    0.4400\*OH +    0.4400\*xHO2 +    0.4400\*RO2C +    0.4400\*CO2 +    0.4400\*xCCHO +    0.4400\*yROOH  |   BR22 |   1.3916E-11<sup>7</sup>| 
| BR33   | RCO3 + NO3 ----> NO2 + RO2C + xHO2 + yROOH + xCCHO + CO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR34   | RCO3 + MEO2 ----> HCHO + HO2 + RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| BR35   | RCO3 + RO2C ----> RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR36   | RCO3 + RO2XC ----> RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR37   | RCO3 + MECO3 ---->   2.0000\*CO2 + MEO2 + RO2C + xHO2 + yROOH + xCCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR38   | RCO3 + RCO3 ---->   2.0000\*RO2C +    2.0000\*xHO2 +    2.0000\*xCCHO +    2.0000\*yROOH +    2.0000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR39   | BZCO3 + NO2 ----> PBZN  |   1.3700E-11 |   1.3700E-11 |
| BR40   | PBZN ----> BZCO3 + NO2  |   7.90E+16e<sup>-14000.00/T</sup> |   3.1974E-04 |
| BR41   | PBZN ---->   0.6000\*BZCO3 +    0.6000\*NO2 +    0.4000\*CO2 +    0.4000\*BZO +    0.4000\*RO2C +    0.4000\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR42   | BZCO3 + NO ----> NO2 + CO2 + BZO + RO2C  |   BR31 |   2.0957E-11<sup>7</sup>| 
| BR43   | BZCO3 + HO2 ---->   0.3075\*RCOOOH +    0.1025\*RCOOH +    0.1500\*O3 +    0.4400\*OH +    0.4400\*BZO +    0.4400\*RO2C +    0.4400\*CO2  |   BR22 |   1.3916E-11<sup>7</sup>| 
| BR44   | BZCO3 + NO3 ----> NO2 + CO2 + BZO + RO2C  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR45   | BZCO3 + MEO2 ----> HCHO + HO2 + RO2C + BZO + CO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| BR46   | BZCO3 + RO2C ----> RO2C + BZO + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR47   | BZCO3 + RO2XC ----> RO2C + BZO + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR48   | BZCO3 + MECO3 ---->   2.0000\*CO2 + MEO2 + BZO + RO2C  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR49   | BZCO3 + RCO3 ---->   2.0000\*CO2 +    2.0000\*RO2C + xHO2 + yROOH + xCCHO + BZO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR50   | BZCO3 + BZCO3 ---->   2.0000\*BZO +    2.0000\*RO2C +    2.0000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR51   | MACO3 + NO2 ----> MAPAN  |   BR28 |   1.2180E-11<sup>7</sup>| 
| BR52   | MAPAN ----> MACO3 + NO2  |   1.60E+16e<sup>-13486.00/T</sup> |   3.6308E-04 |
| BR53   | MAPAN ---->   0.6000\*MACO3 +    0.6000\*NO2 +    0.4000\*CO2 +    0.4000\*HCHO +    0.4000\*MECO3 +    0.4000\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR64   | TBUO + NO2 ----> RNO3  |   2.4000E-11 |   2.4000E-11 |
| BR65   | TBUO ----> ACETONE + MEO2  |   7.50E+14e<sup> -8152.00/T</sup> |   1.0014E+03 |
| BR66   | BZO + NO2 ----> NPHE  |   2.30E-11e<sup>   150.00/T</sup> |   3.8038E-11 |
| BR67   | BZO + HO2 ----> CRES  |   BR08 |   7.7759E-12<sup>7</sup>| 
| BR68   | BZO ----> CRES + RO2C + xHO2  |   1.0000E-03 |   1.0000E-03 |
| R019   | xHO2 + NO ----> NO + HO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R020   | xHO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R021   | xHO2 + NO3 ----> NO3 + HO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R022   | xHO2 + MEO2 ----> MEO2 +    0.5000\*HO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R023   | xHO2 + RO2C ----> RO2C +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R024   | xHO2 + RO2XC ----> RO2XC +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R025   | xHO2 + MECO3 ----> MECO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R026   | xHO2 + RCO3 ----> RCO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R027   | xHO2 + BZCO3 ----> BZCO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R028   | xHO2 + MACO3 ----> MACO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R029   | xOH + NO ----> NO + OH  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R030   | xOH + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R031   | xOH + NO3 ----> NO3 + OH  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R032   | xOH + MEO2 ----> MEO2 +    0.5000\*OH  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R033   | xOH + RO2C ----> RO2C +    0.5000\*OH  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R034   | xOH + RO2XC ----> RO2XC +    0.5000\*OH  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R035   | xOH + MECO3 ----> MECO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R036   | xOH + RCO3 ----> RCO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R037   | xOH + BZCO3 ----> BZCO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R038   | xOH + MACO3 ----> MACO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R039   | xNO2 + NO ----> NO + NO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R040   | xNO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R041   | xNO2 + NO3 ----> NO3 + NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R042   | xNO2 + MEO2 ----> MEO2 +    0.5000\*NO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R043   | xNO2 + RO2C ----> RO2C +    0.5000\*NO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R044   | xNO2 + RO2XC ----> RO2XC +    0.5000\*NO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R045   | xNO2 + MECO3 ----> MECO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R046   | xNO2 + RCO3 ----> RCO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R047   | xNO2 + BZCO3 ----> BZCO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R048   | xNO2 + MACO3 ----> MACO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R049   | xMEO2 + NO ----> NO + MEO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R050   | xMEO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R051   | xMEO2 + NO3 ----> NO3 + MEO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R052   | xMEO2 + MEO2 ---->   1.5000\*MEO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R053   | xMEO2 + RO2C ----> RO2C +    0.5000\*MEO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R054   | xMEO2 + RO2XC ----> RO2XC +    0.5000\*MEO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R055   | xMEO2 + MECO3 ----> MECO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R056   | xMEO2 + RCO3 ----> RCO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R057   | xMEO2 + BZCO3 ----> BZCO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R058   | xMEO2 + MACO3 ----> MACO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R059   | xMECO3 + NO ----> NO + MECO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R060   | xMECO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R061   | xMECO3 + NO3 ----> NO3 + MECO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R062   | xMECO3 + MEO2 ----> MEO2 +    0.5000\*MECO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R063   | xMECO3 + RO2C ----> RO2C +    0.5000\*MECO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R064   | xMECO3 + RO2XC ----> RO2XC +    0.5000\*MECO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R065   | xMECO3 + MECO3 ---->   2.0000\*MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R066   | xMECO3 + RCO3 ----> RCO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R067   | xMECO3 + BZCO3 ----> BZCO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R068   | xMECO3 + MACO3 ----> MACO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R069   | xRCO3 + NO ----> NO + RCO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R070   | xRCO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R071   | xRCO3 + NO3 ----> NO3 + RCO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R072   | xRCO3 + MEO2 ----> MEO2 +    0.5000\*RCO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R073   | xRCO3 + RO2C ----> RO2C +    0.5000\*RCO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R074   | xRCO3 + RO2XC ----> RO2XC +    0.5000\*RCO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R075   | xRCO3 + MECO3 ----> MECO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R076   | xRCO3 + RCO3 ---->   2.0000\*RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R077   | xRCO3 + BZCO3 ----> BZCO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R078   | xRCO3 + MACO3 ----> MACO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R079   | xMACO3 + NO ----> NO + MACO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R080   | xMACO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R081   | xMACO3 + NO3 ----> NO3 + MACO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R082   | xMACO3 + MEO2 ----> MEO2 +    0.5000\*MACO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R083   | xMACO3 + RO2C ----> RO2C +    0.5000\*MACO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R084   | xMACO3 + RO2XC ----> RO2XC +    0.5000\*MACO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R085   | xMACO3 + MECO3 ----> MECO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R086   | xMACO3 + RCO3 ----> RCO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R087   | xMACO3 + BZCO3 ----> BZCO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R088   | xMACO3 + MACO3 ---->   2.0000\*MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R089   | xTBUO + NO ----> NO + TBUO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R090   | xTBUO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R091   | xTBUO + NO3 ----> NO3 + TBUO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R092   | xTBUO + MEO2 ----> MEO2 +    0.5000\*TBUO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R093   | xTBUO + RO2C ----> RO2C +    0.5000\*TBUO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R094   | xTBUO + RO2XC ----> RO2XC +    0.5000\*TBUO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R095   | xTBUO + MECO3 ----> MECO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R096   | xTBUO + RCO3 ----> RCO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R097   | xTBUO + BZCO3 ----> BZCO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R098   | xTBUO + MACO3 ----> MACO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R099   | xCO + NO ----> NO + CO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R100   | xCO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R101   | xCO + NO3 ----> NO3 + CO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R102   | xCO + MEO2 ----> MEO2 +    0.5000\*CO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R103   | xCO + RO2C ----> RO2C +    0.5000\*CO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R104   | xCO + RO2XC ----> RO2XC +    0.5000\*CO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R105   | xCO + MECO3 ----> MECO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R106   | xCO + RCO3 ----> RCO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R107   | xCO + BZCO3 ----> BZCO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R108   | xCO + MACO3 ----> MACO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BP01   | HCHO ---->   2.0000\*HO2 + CO  | HCHOR_06 | Not Available<sup>1</sup> | 
| BP02   | HCHO ----> CO  | HCHOM_06 | Not Available<sup>1</sup> | 
| BP03   | HCHO + OH ----> HO2 + CO  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| BP07   | HCHO + NO3 ----> HNO3 + HO2 + CO  |   2.00E-12e<sup> -2431.00/T</sup> |   5.7539E-16 |
| BP08   | CCHO + OH ----> MECO3  |   4.40E-12e<sup>   365.00/T</sup> |   1.4967E-11 |
| BP09   | CCHO ----> CO + HO2 + MEO2  | CCHO_R | Not Available<sup>1</sup> | 
| BP10   | CCHO + NO3 ----> HNO3 + MECO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| BP11   | RCHO + OH ---->   0.9650\*RCO3 +    0.0350\*RO2C +    0.0350\*xHO2 +    0.0350\*xCO +    0.0350\*xCCHO +    0.0350\*yROOH  |   5.10E-12e<sup>   405.00/T</sup> |   1.9838E-11 |
| BP12   | RCHO ----> RO2C + xHO2 + yROOH + xCCHO + CO + HO2  | C2CHO | Not Available<sup>1</sup> | 
| BP13   | RCHO + NO3 ----> HNO3 + RCO3  |   1.40E-12e<sup> -1601.00/T</sup> |   6.5172E-15 |
| BP14   | ACETONE + OH ----> RO2C + xMECO3 + xHCHO + yROOH  |   4.56E-14e<sup>   429.00/T</sup>(T/300)<sup>  3.65 </sup> |   1.8796E-13 |
| BP15   | ACETONE ---->   0.6200\*MECO3 +    1.3800\*MEO2 +    0.3800\*CO  |   5.0000E-01\*ACET_06 | Not Available<sup>1</sup> | 
| BP16   | MEK + OH ---->   0.9670\*RO2C +    0.0390\*RO2XC +    0.0390\*zRNO3 +    0.3760\*xHO2 +    0.5100\*xMECO3 +    0.0740\*xRCO3 +    0.0880\*xHCHO +    0.5040\*xCCHO +    0.3760\*xRCHO + yROOH  |   1.30E-12e<sup>   -25.00/T</sup>(T/300)<sup>  2.00 </sup> |   1.1807E-12 |
| BP17   | MEK ----> MECO3 + RO2C + xHO2 + xCCHO + yROOH  |   1.7500E-01\*MEK_06 | Not Available<sup>1</sup> | 
| BP18   | MEOH + OH ----> HCHO + HO2  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| BP19   | HCOOH + OH ----> HO2 + CO2  |   4.5000E-13 |   4.5000E-13 |
| BP20   | CCOOH + OH ---->   0.5090\*MEO2 +    0.4910\*RO2C +    0.5090\*CO2 +    0.4910\*xHO2 +    0.4910\*xMGLY +    0.4910\*yROOH  |   4.20E-14e<sup>   855.00/T</sup> |   7.3904E-13 |
| BP21   | RCOOH + OH ----> RO2C + xHO2 +    0.1430\*CO2 +    0.1420\*xCCHO +    0.4000\*xRCHO +    0.4570\*xBACL + yROOH  |   1.2000E-12 |   1.2000E-12 |
| BP22   | COOH + OH ---->   0.3000\*HCHO +    0.3000\*OH +    0.7000\*MEO2  |   3.80E-12e<sup>   200.00/T</sup> |   7.4321E-12 |
| BP23   | COOH ----> HCHO + HO2 + OH  | COOH | Not Available<sup>1</sup> | 
| BP24   | ROOH + OH ---->   0.7440\*OH +    0.2510\*RO2C +    0.0040\*RO2XC +    0.0040\*zRNO3 +    0.7440\*RCHO +    0.2390\*xHO2 +    0.0120\*xOH +    0.0120\*xHCHO +    0.0120\*xCCHO +    0.2050\*xRCHO +    0.0340\*xPROD2 +    0.2560\*yROOH  |   2.5000E-11 |   2.5000E-11 |
| BP25   | ROOH ----> RCHO + HO2 + OH  | COOH | Not Available<sup>1</sup> | 
| BP26   | R6OOH + OH ---->   0.8400\*OH +    0.2220\*RO2C +    0.0290\*RO2XC +    0.0290\*zRNO3 +    0.8400\*PRD2 +    0.0900\*xHO2 +    0.0410\*xOH +    0.0200\*xCCHO +    0.0750\*xRCHO +    0.0840\*xPROD2 +    0.1600\*yROOH  |   5.6000E-11 |   5.6000E-11 |
| BP27   | R6OOH ----> OH +    0.1420\*HO2 +    0.7820\*RO2C +    0.0770\*RO2XC +    0.0770\*zRNO3 +    0.0850\*RCHO +    0.1420\*PRD2 +    0.7820\*xHO2 +    0.0260\*xCCHO +    0.0580\*xRCHO +    0.6980\*xPROD2 +    0.8580\*yR6OOH  | COOH | Not Available<sup>1</sup> | 
| BP28   | RAOOH + OH ---->   0.1390\*OH +    0.1480\*HO2 +    0.5890\*RO2C +    0.1240\*RO2XC +    0.1240\*zRNO3 +    0.0740\*PRD2 +    0.1470\*MGLY +    0.1390\*IPRD +    0.5650\*xHO2 +    0.0240\*xOH +    0.4480\*xRCHO +    0.0260\*xGLY +    0.0300\*xMEK +    0.2520\*xMGLY +    0.0730\*xAFG1 +    0.0730\*xAFG2 +    0.7130\*yR6OOH  |   1.4100E-10 |   1.4100E-10 |
| BP29   | RAOOH ----> OH + HO2 +    0.5000\*GLY +    0.5000\*MGLY +    0.5000\*AFG1 +    0.5000\*AFG2  | COOH | Not Available<sup>1</sup> | 
| BP30   | GLY ---->   2.0000\*CO +    2.0000\*HO2  | GLY_07R | Not Available<sup>1</sup> | 
| BP31   | GLY ----> HCHO + CO  | GLY_07M | Not Available<sup>1</sup> | 
| BP32   | GLY + OH ---->   0.7000\*HO2 +    1.4000\*CO +    0.3000\*HCOCO3  |   3.10E-12e<sup>   342.20/T</sup> |   9.7683E-12 |
| BP33   | GLY + NO3 ----> HNO3 +    0.7000\*HO2 +    1.4000\*CO +    0.3000\*HCOCO3  |   2.80E-12e<sup> -2390.00/T</sup> |   9.2429E-16 |
| BP34   | MGLY ----> HO2 + CO + MECO3  | MGLY_06 | Not Available<sup>1</sup> | 
| BP35   | MGLY + OH ----> CO + MECO3  |   1.5000E-11 |   1.5000E-11 |
| BP36   | MGLY + NO3 ----> HNO3 + CO + MECO3  |   1.40E-12e<sup> -1895.00/T</sup> |   2.4312E-15 |
| BP37   | BACL ---->   2.0000\*MECO3  | BACL_07 | Not Available<sup>1</sup> | 
| BP38   | CRES + OH ---->   0.2000\*BZO +    0.8000\*RO2C +    0.8000\*xHO2 +    0.8000\*yR6OOH +    0.2500\*xMGLY  |   1.70E-12e<sup>   950.00/T</sup> |   4.1138E-11 |
| BP39   | CRES + NO3 ----> HNO3 + BZO  |   1.4000E-11 |   1.4000E-11 |
| BP40   | NPHE + OH ----> BZO  |   3.5000E-12 |   3.5000E-12 |
| BP41   | NPHE ----> HONO  |   1.5000E-03\*NO2_06 | Not Available<sup>1</sup> | 
| BP42   | NPHE ----> |   1.5000E-02\*NO2_06 | Not Available<sup>1</sup> | 
| BP43   | BALD + OH ----> BZCO3  |   1.2000E-11 |   1.2000E-11 |
| BP44   | BALD ----> |   6.0000E-02\*BALD_06 | Not Available<sup>1</sup> | 
| BP45   | BALD + NO3 ----> HNO3 + BZCO3  |   1.34E-12e<sup> -1860.00/T</sup> |   2.6168E-15 |
| BP46   | AFG1 + OH ---->   0.2170\*MACO3 +    0.7230\*RO2C +    0.0600\*RO2XC +    0.0600\*zRNO3 +    0.5210\*xHO2 +    0.2010\*xMECO3 +    0.3340\*xCO +    0.4070\*xRCHO +    0.1290\*xMEK +    0.1070\*xGLY +    0.2670\*xMGLY +    0.7830\*yR6OOH  |   7.4000E-11 |   7.4000E-11 |
| BP47   | AFG1 + O3 ---->   0.8260\*OH +    0.5220\*HO2 +    0.6520\*RO2C +    0.5220\*CO +    0.1740\*CO2 +    0.4320\*GLY +    0.5680\*MGLY +    0.6520\*xRCO3 +    0.6520\*xHCHO +    0.6520\*yR6OOH  |   9.6600E-18 |   9.6600E-18 |
| BP48   | AFG1 ---->   1.0230\*HO2 +    0.1730\*MEO2 +    0.3050\*MECO3 +    0.5000\*MACO3 +    0.6950\*CO +    0.1950\*GLY +    0.3050\*MGLY  | AFG1 | Not Available<sup>1</sup> | 
| BP49   | AFG2 + OH ---->   0.2170\*MACO3 +    0.7230\*RO2C +    0.0600\*RO2XC +    0.0600\*zRNO3 +    0.5210\*xHO2 +    0.2010\*xMECO3 +    0.3340\*xCO +    0.4070\*xRCHO +    0.1290\*xMEK +    0.1070\*xGLY +    0.2670\*xMGLY +    0.7830\*yR6OOH  |   7.4000E-11 |   7.4000E-11 |
| BP50   | AFG2 + O3 ---->   0.8260\*OH +    0.5220\*HO2 +    0.6520\*RO2C +    0.5220\*CO +    0.1740\*CO2 +    0.4320\*GLY +    0.5680\*MGLY +    0.6520\*xRCO3 +    0.6520\*xHCHO +    0.6520\*yR6OOH  |   9.6600E-18 |   9.6600E-18 |
| BP51   | AFG2 ----> PRD2  | AFG1 | Not Available<sup>1</sup> | 
| BP52   | AFG3 + OH ---->   0.2060\*MACO3 +    0.7330\*RO2C +    0.1170\*RO2XC +    0.1170\*zRNO3 +    0.5610\*xHO2 +    0.1170\*xMECO3 +    0.1140\*xCO +    0.2740\*xGLY +    0.1530\*xMGLY +    0.0190\*xBACL +    0.1950\*xAFG1 +    0.1950\*xAFG2 +    0.2310\*xIPRD +    0.7940\*yR6OOH  |   9.3500E-11 |   9.3500E-11 |
| BP53   | AFG3 + O3 ---->   0.4710\*OH +    0.5540\*HO2 +    0.0130\*MECO3 +    0.2580\*RO2C +    0.0070\*RO2XC +    0.0070\*zRNO3 +    0.5800\*CO +    0.1900\*CO2 +    0.3660\*GLY +    0.1840\*MGLY +    0.3500\*AFG1 +    0.3500\*AFG2 +    0.1390\*AFG3 +    0.0030\*MACR +    0.0040\*MVK +    0.0030\*IPRD +    0.0950\*xHO2 +    0.1630\*xRCO3 +    0.1630\*xHCHO +    0.0950\*xMGLY +    0.2640\*yR6OOH  |   1.4300E-17 |   1.4300E-17 |
| BP55   | MACR + O3 ---->   0.2080\*OH +    0.1080\*HO2 +    0.1000\*RO2C +    0.4500\*CO +    0.1170\*CO2 +    0.1000\*HCHO +    0.9000\*MGLY +    0.3330\*HCOOH +    0.1000\*xRCO3 +    0.1000\*xHCHO +    0.1000\*yROOH  |   1.40E-15e<sup> -2100.00/T</sup> |   1.2224E-18 |
| BP57   | MACR + O3P ----> RCHO  |   6.3400E-12 |   6.3400E-12 |
| BP60   | MVK + O3 ---->   0.1640\*OH +    0.0640\*HO2 +    0.0500\*RO2C +    0.0500\*xHO2 +    0.4750\*CO +    0.1240\*CO2 +    0.0500\*HCHO +    0.9500\*MGLY +    0.3510\*HCOOH +    0.0500\*xRCO3 +    0.0500\*xHCHO +    0.0500\*yROOH  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| BP62   | MVK + O3P ---->   0.4500\*RCHO +    0.5500\*MEK  |   4.3200E-12 |   4.3200E-12 |
| BP63   | MVK ---->   0.4000\*MEO2 +    0.6000\*CO +    0.6000\*PRD2 +    0.4000\*MACO3  | MVK_06 | Not Available<sup>1</sup> | 
| BP64   | IPRD + OH ---->   0.2890\*MACO3 +    0.6700\*RO2C +    0.6700\*xHO2 +    0.0410\*RO2XC +    0.0410\*zRNO3 +    0.3360\*xCO +    0.0550\*xHCHO +    0.1290\*xHOCCHO +    0.0130\*xRCHO +    0.1500\*xMEK +    0.3320\*xPROD2 +    0.1500\*xGLY +    0.1740\*xMGLY +    0.7110\*yR6OOH  |   6.1900E-11 |   6.1900E-11 |
| BP65   | IPRD + O3 ---->   0.2850\*OH +    0.4000\*HO2 +    0.0480\*RO2C +    0.0480\*xRCO3 +    0.4980\*CO +    0.1400\*CO2 +    0.1240\*HCHO +    0.2100\*MEK +    0.0230\*GLY +    0.7420\*MGLY +    0.1000\*HCOOH +    0.3720\*RCOOH +    0.0470\*xHOCCHO +    0.0010\*xHCHO +    0.0480\*yR6OOH  |   4.1800E-18 |   4.1800E-18 |
| BP66   | IPRD + NO3 ---->   0.1500\*MACO3 +    0.1500\*HNO3 +    0.7990\*RO2C +    0.7990\*xHO2 +    0.0510\*RO2XC +    0.0510\*zRNO3 +    0.5720\*xCO +    0.2270\*xHCHO +    0.2180\*xRCHO +    0.0080\*xMGLY +    0.5720\*xRNO3 +    0.8500\*yR6OOH  |   1.0000E-13 |   1.0000E-13 |
| BP67   | IPRD ---->   1.2330\*HO2 +    0.4670\*MECO3 +    0.3000\*RCO3 +    1.2330\*CO +    0.3000\*HCHO +    0.4670\*HOCCHO +    0.2330\*MEK  | MACR_06 | Not Available<sup>1</sup> | 
| BP68   | PRD2 + OH ---->   0.4720\*HO2 +    0.3790\*xHO2 +    0.0290\*xMECO3 +    0.0490\*xRCO3 +    0.4730\*RO2C +    0.0710\*RO2XC +    0.0710\*zRNO3 +    0.0020\*HCHO +    0.2110\*xHCHO +    0.0010\*CCHO +    0.0830\*xCCHO +    0.1430\*RCHO +    0.4020\*xRCHO +    0.1150\*xMEK +    0.3290\*PRD2 +    0.0070\*xPROD2 +    0.5280\*yR6OOH  |   1.5500E-11 |   1.5500E-11 |
| BP69   | PRD2 ---->   0.9130\*xHO2 +    0.4000\*MECO3 +    0.6000\*RCO3 +    1.5900\*RO2C +    0.0870\*RO2XC +    0.0870\*zRNO3 +    0.3030\*xHCHO +    0.1630\*xCCHO +    0.7800\*xRCHO + yR6OOH  |   4.8600E-03\*MEK_06 | Not Available<sup>1</sup> | 
| BP70   | RNO3 + OH ---->   0.1890\*HO2 +    0.3050\*xHO2 +    0.0190\*NO2 +    0.3130\*xNO2 +    0.9760\*RO2C +    0.1750\*RO2XC +    0.1750\*zRNO3 +    0.0110\*xHCHO +    0.4290\*xCCHO +    0.0010\*RCHO +    0.0360\*xRCHO +    0.0040\*xACETONE +    0.0100\*MEK +    0.1700\*xMEK +    0.0080\*PRD2 +    0.0310\*xPROD2 +    0.1890\*RNO3 +    0.3050\*xRNO3 +    0.1570\*yROOH +    0.6360\*yR6OOH  |   7.2000E-12 |   7.2000E-12 |
| BP71   | RNO3 ---->   0.3440\*HO2 +    0.5540\*xHO2 + NO2 +    0.7210\*RO2C +    0.1020\*RO2XC +    0.1020\*zRNO3 +    0.0740\*HCHO +    0.0610\*xHCHO +    0.2140\*CCHO +    0.2300\*xCCHO +    0.0740\*RCHO +    0.0630\*xRCHO +    0.0080\*xACETONE +    0.1240\*MEK +    0.0830\*xMEK +    0.1900\*PRD2 +    0.2610\*xPROD2 +    0.0660\*yROOH +    0.5910\*yR6OOH  | IC3ONO2 | Not Available<sup>1</sup> | 
| BP73   | HOCCHO ----> CO +    2.0000\*HO2 + HCHO  | HOCCHO_IUPAC | Not Available<sup>1</sup> | 
| BP74   | HOCCHO + NO3 ----> HNO3 + MECO3  |   BP10 |   2.7340E-15<sup>7</sup>| 
| BP75   | ACROLEIN + OH ---->   0.2500\*xHO2 +    0.7500\*MACO3 +    0.2500\*RO2C +    0.1670\*xCO +    0.0830\*xHCHO +    0.1670\*xCCHO +    0.0830\*xGLY +    0.2500\*yROOH  |   1.9900E-11 |   1.9900E-11 |
| BP76   | ACROLEIN + O3 ---->   0.8300\*HO2 +    0.3300\*OH +    1.0050\*CO +    0.3100\*CO2 +    0.5000\*HCHO +    0.1850\*HCOOH +    0.5000\*GLY  |   1.40E-15e<sup> -2528.00/T</sup> |   2.9091E-19 |
| BP77   | ACROLEIN + NO3 ---->   0.0310\*xHO2 +    0.9670\*MACO3 +    0.0310\*RO2C +    0.0020\*RO2XC +    0.0020\*zRNO3 +    0.9670\*HNO3 +    0.0310\*xCO +    0.0310\*xRNO3 +    0.0330\*yROOH  |   1.1800E-15 |   1.1800E-15 |
| BP78   | ACROLEIN + O3P ----> RCHO  |   2.3700E-12 |   2.3700E-12 |
| BP79   | ACROLEIN ---->   1.0660\*HO2 +    0.1780\*OH +    0.2340\*MEO2 +    0.3300\*MACO3 +    1.1880\*CO +    0.1020\*CO2 +    0.3400\*HCHO +    0.0500\*CCOOH  | ACRO_09 | Not Available<sup>1</sup> | 
| BP80   | CCOOOH + OH ---->   0.9800\*MECO3 +    0.0200\*RO2C +    0.0200\*CO2 +    0.0200\*xOH +    0.0200\*xHCHO +    0.0200\*yROOH  |   5.2800E-12 |   5.2800E-12 |
| BP81   | CCOOOH ----> MEO2 + CO2 + OH  | PAA | Not Available<sup>1</sup> | 
| BP82   | RCOOOH + OH ---->   0.8060\*RCO3 +    0.1940\*RO2C +    0.1940\*yROOH +    0.1100\*CO2 +    0.1100\*xOH +    0.1100\*xCCHO +    0.0840\*xHO2 +    0.0840\*xRCHO  |   6.4200E-12 |   6.4200E-12 |
| BP83   | RCOOOH ----> xHO2 + xCCHO + yROOH + CO2 + OH  | PAA | Not Available<sup>1</sup> | 
| BP84   | HCOCO3 + NO ----> HO2 + CO + CO2 + NO2  |   BR31 |   2.0957E-11<sup>7</sup>| 
| BP85   | HCOCO3 + NO2 ----> HO2 + CO + CO2 + NO3  |   BR28 |   1.2180E-11<sup>7</sup>| 
| BP86   | HCOCO3 + HO2 ---->   0.4400\*OH +    0.4400\*HO2 +    0.4400\*CO +    0.4400\*CO2 +    0.5600\*GLY +    0.1500\*O3  |   BR22 |   1.3916E-11<sup>7</sup>| 
| P001   | xHCHO + NO ----> NO + HCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P002   | xHCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P003   | xHCHO + NO3 ----> NO3 + HCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P004   | xHCHO + MEO2 ----> MEO2 +    0.5000\*HCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P005   | xHCHO + RO2C ----> RO2C +    0.5000\*HCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P006   | xHCHO + RO2XC ----> RO2XC +    0.5000\*HCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P007   | xHCHO + MECO3 ----> MECO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P008   | xHCHO + RCO3 ----> RCO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P009   | xHCHO + BZCO3 ----> BZCO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P010   | xHCHO + MACO3 ----> MACO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P011   | xCCHO + NO ----> NO + CCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P012   | xCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P013   | xCCHO + NO3 ----> NO3 + CCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P014   | xCCHO + MEO2 ----> MEO2 +    0.5000\*CCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P015   | xCCHO + RO2C ----> RO2C +    0.5000\*CCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P016   | xCCHO + RO2XC ----> RO2XC +    0.5000\*CCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P017   | xCCHO + MECO3 ----> MECO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P018   | xCCHO + RCO3 ----> RCO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P019   | xCCHO + BZCO3 ----> BZCO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P020   | xCCHO + MACO3 ----> MACO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P021   | xRCHO + NO ----> NO + RCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P022   | xRCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P023   | xRCHO + NO3 ----> NO3 + RCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P024   | xRCHO + MEO2 ----> MEO2 +    0.5000\*RCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P025   | xRCHO + RO2C ----> RO2C +    0.5000\*RCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P026   | xRCHO + RO2XC ----> RO2XC +    0.5000\*RCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P027   | xRCHO + MECO3 ----> MECO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P028   | xRCHO + RCO3 ----> RCO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P029   | xRCHO + BZCO3 ----> BZCO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P030   | xRCHO + MACO3 ----> MACO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P031   | xACETONE + NO ----> NO + ACETONE  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P032   | xACETONE + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P033   | xACETONE + NO3 ----> NO3 + ACETONE  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P034   | xACETONE + MEO2 ----> MEO2 +    0.5000\*ACETONE  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P035   | xACETONE + RO2C ----> RO2C +    0.5000\*ACETONE  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P036   | xACETONE + RO2XC ----> RO2XC +    0.5000\*ACETONE  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P037   | xACETONE + MECO3 ----> MECO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P038   | xACETONE + RCO3 ----> RCO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P039   | xACETONE + BZCO3 ----> BZCO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P040   | xACETONE + MACO3 ----> MACO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P041   | xMEK + NO ----> NO + MEK  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P042   | xMEK + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P043   | xMEK + NO3 ----> NO3 + MEK  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P044   | xMEK + MEO2 ----> MEO2 +    0.5000\*MEK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P045   | xMEK + RO2C ----> RO2C +    0.5000\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P046   | xMEK + RO2XC ----> RO2XC +    0.5000\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P047   | xMEK + MECO3 ----> MECO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P048   | xMEK + RCO3 ----> RCO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P049   | xMEK + BZCO3 ----> BZCO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P050   | xMEK + MACO3 ----> MACO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P051   | xPROD2 + NO ----> NO + PRD2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P052   | xPROD2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P053   | xPROD2 + NO3 ----> NO3 + PRD2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P054   | xPROD2 + MEO2 ----> MEO2 +    0.5000\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P055   | xPROD2 + RO2C ----> RO2C +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P056   | xPROD2 + RO2XC ----> RO2XC +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P057   | xPROD2 + MECO3 ----> MECO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P058   | xPROD2 + RCO3 ----> RCO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P059   | xPROD2 + BZCO3 ----> BZCO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P060   | xPROD2 + MACO3 ----> MACO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P061   | xGLY + NO ----> NO + GLY  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P062   | xGLY + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P063   | xGLY + NO3 ----> NO3 + GLY  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P064   | xGLY + MEO2 ----> MEO2 +    0.5000\*GLY  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P065   | xGLY + RO2C ----> RO2C +    0.5000\*GLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P066   | xGLY + RO2XC ----> RO2XC +    0.5000\*GLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P067   | xGLY + MECO3 ----> MECO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P068   | xGLY + RCO3 ----> RCO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P069   | xGLY + BZCO3 ----> BZCO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P070   | xGLY + MACO3 ----> MACO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P071   | xMGLY + NO ----> NO + MGLY  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P072   | xMGLY + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P073   | xMGLY + NO3 ----> NO3 + MGLY  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P074   | xMGLY + MEO2 ----> MEO2 +    0.5000\*MGLY  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P075   | xMGLY + RO2C ----> RO2C +    0.5000\*MGLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P076   | xMGLY + RO2XC ----> RO2XC +    0.5000\*MGLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P077   | xMGLY + MECO3 ----> MECO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P078   | xMGLY + RCO3 ----> RCO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P079   | xMGLY + BZCO3 ----> BZCO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P080   | xMGLY + MACO3 ----> MACO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P081   | xBACL + NO ----> NO + BACL  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P082   | xBACL + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P083   | xBACL + NO3 ----> NO3 + BACL  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P084   | xBACL + MEO2 ----> MEO2 +    0.5000\*BACL  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P085   | xBACL + RO2C ----> RO2C +    0.5000\*BACL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P086   | xBACL + RO2XC ----> RO2XC +    0.5000\*BACL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P087   | xBACL + MECO3 ----> MECO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P088   | xBACL + RCO3 ----> RCO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P089   | xBACL + BZCO3 ----> BZCO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P090   | xBACL + MACO3 ----> MACO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P091   | xBALD + NO ----> NO + BALD  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P092   | xBALD + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P093   | xBALD + NO3 ----> NO3 + BALD  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P094   | xBALD + MEO2 ----> MEO2 +    0.5000\*BALD  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P095   | xBALD + RO2C ----> RO2C +    0.5000\*BALD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P096   | xBALD + RO2XC ----> RO2XC +    0.5000\*BALD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P097   | xBALD + MECO3 ----> MECO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P098   | xBALD + RCO3 ----> RCO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P099   | xBALD + BZCO3 ----> BZCO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P100   | xBALD + MACO3 ----> MACO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P101   | xAFG1 + NO ----> NO + AFG1  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P102   | xAFG1 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P103   | xAFG1 + NO3 ----> NO3 + AFG1  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P104   | xAFG1 + MEO2 ----> MEO2 +    0.5000\*AFG1  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P105   | xAFG1 + RO2C ----> RO2C +    0.5000\*AFG1  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P106   | xAFG1 + RO2XC ----> RO2XC +    0.5000\*AFG1  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P107   | xAFG1 + MECO3 ----> MECO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P108   | xAFG1 + RCO3 ----> RCO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P109   | xAFG1 + BZCO3 ----> BZCO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P110   | xAFG1 + MACO3 ----> MACO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P111   | xAFG2 + NO ----> NO + AFG2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P112   | xAFG2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P113   | xAFG2 + NO3 ----> NO3 + AFG2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P114   | xAFG2 + MEO2 ----> MEO2 +    0.5000\*AFG2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P115   | xAFG2 + RO2C ----> RO2C +    0.5000\*AFG2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P116   | xAFG2 + RO2XC ----> RO2XC +    0.5000\*AFG2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P117   | xAFG2 + MECO3 ----> MECO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P118   | xAFG2 + RCO3 ----> RCO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P119   | xAFG2 + BZCO3 ----> BZCO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P120   | xAFG2 + MACO3 ----> MACO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P121   | xAFG3 + NO ----> NO + AFG3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P122   | xAFG3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P123   | xAFG3 + NO3 ----> NO3 + AFG3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P124   | xAFG3 + MEO2 ----> MEO2 +    0.5000\*AFG3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P125   | xAFG3 + RO2C ----> RO2C +    0.5000\*AFG3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P126   | xAFG3 + RO2XC ----> RO2XC +    0.5000\*AFG3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P127   | xAFG3 + MECO3 ----> MECO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P128   | xAFG3 + RCO3 ----> RCO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P129   | xAFG3 + BZCO3 ----> BZCO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P130   | xAFG3 + MACO3 ----> MACO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P131   | xMACR + NO ----> NO + MACR  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P132   | xMACR + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P133   | xMACR + NO3 ----> NO3 + MACR  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P134   | xMACR + MEO2 ----> MEO2 +    0.5000\*MACR  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P135   | xMACR + RO2C ----> RO2C +    0.5000\*MACR  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P136   | xMACR + RO2XC ----> RO2XC +    0.5000\*MACR  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P137   | xMACR + MECO3 ----> MECO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P138   | xMACR + RCO3 ----> RCO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P139   | xMACR + BZCO3 ----> BZCO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P140   | xMACR + MACO3 ----> MACO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P141   | xMVK + NO ----> NO + MVK  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P142   | xMVK + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P143   | xMVK + NO3 ----> NO3 + MVK  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P144   | xMVK + MEO2 ----> MEO2 +    0.5000\*MVK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P145   | xMVK + RO2C ----> RO2C +    0.5000\*MVK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P146   | xMVK + RO2XC ----> RO2XC +    0.5000\*MVK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P147   | xMVK + MECO3 ----> MECO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P148   | xMVK + RCO3 ----> RCO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P149   | xMVK + BZCO3 ----> BZCO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P150   | xMVK + MACO3 ----> MACO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P151   | xIPRD + NO ----> NO + IPRD  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P152   | xIPRD + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P153   | xIPRD + NO3 ----> NO3 + IPRD  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P154   | xIPRD + MEO2 ----> MEO2 +    0.5000\*IPRD  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P155   | xIPRD + RO2C ----> RO2C +    0.5000\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P156   | xIPRD + RO2XC ----> RO2XC +    0.5000\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P157   | xIPRD + MECO3 ----> MECO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P158   | xIPRD + RCO3 ----> RCO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P159   | xIPRD + BZCO3 ----> BZCO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P160   | xIPRD + MACO3 ----> MACO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P161   | xRNO3 + NO ----> NO + RNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P162   | xRNO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P163   | xRNO3 + NO3 ----> NO3 + RNO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P164   | xRNO3 + MEO2 ----> MEO2 +    0.5000\*RNO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P165   | xRNO3 + RO2C ----> RO2C +    0.5000\*RNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P166   | xRNO3 + RO2XC ----> RO2XC +    0.5000\*RNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P167   | xRNO3 + MECO3 ----> MECO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P168   | xRNO3 + RCO3 ----> RCO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P169   | xRNO3 + BZCO3 ----> BZCO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P170   | xRNO3 + MACO3 ----> MACO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PX161   | xMTNO3 + NO ----> NO + MTNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| PX162   | xMTNO3 + HO2 ----> HO2  |   2.65E-13e<sup>  1300.00/T</sup> |   2.0743E-11 |
| PX163   | xMTNO3 + NO3 ----> NO3 + MTNO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| PX164   | xMTNO3 + MEO2 ----> MEO2 +    0.5000\*MTNO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| PX165   | xMTNO3 + RO2C ----> RO2C +    0.5000\*MTNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| PX166   | xMTNO3 + RO2XC ----> RO2XC +    0.5000\*MTNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| PX167   | xMTNO3 + MECO3 ----> MECO3 + MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PX168   | xMTNO3 + RCO3 ----> RCO3 + MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PX169   | xMTNO3 + BZCO3 ----> BZCO3 + MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PX170   | xMTNO3 + MACO3 ----> MACO3 + MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PX170b   | xMTNO3 + IMACO3 ----> MACO3 + MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P171   | yROOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P172   | yROOH + HO2 ----> HO2 + ROOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P173   | yROOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P174   | yROOH + MEO2 ----> MEO2 +    0.5000\*MEK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P175   | yROOH + RO2C ----> RO2C +    0.5000\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P176   | yROOH + RO2XC ----> RO2XC +    0.5000\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P177   | yROOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P178   | yROOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P179   | yROOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P180   | yROOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P181   | yR6OOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P182   | yR6OOH + HO2 ----> HO2 + R6OOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P183   | yR6OOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P184   | yR6OOH + MEO2 ----> MEO2 +    0.5000\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P185   | yR6OOH + RO2C ----> RO2C +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P186   | yR6OOH + RO2XC ----> RO2XC +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P187   | yR6OOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P188   | yR6OOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P189   | yR6OOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P190   | yR6OOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P191   | yRAOOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P192   | yRAOOH + HO2 ----> HO2 + RAOOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P193   | yRAOOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P194   | yRAOOH + MEO2 ----> MEO2 +    0.5000\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P195   | yRAOOH + RO2C ----> RO2C +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P196   | yRAOOH + RO2XC ----> RO2XC +    0.5000\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P197   | yRAOOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P198   | yRAOOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P199   | yRAOOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P200   | yRAOOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P201   | zRNO3 + NO ----> NO + RNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P202   | zRNO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P203   | zRNO3 + NO3 ----> NO3 + PRD2 + HO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P204   | zRNO3 + MEO2 ----> MEO2 +    0.5000\*PRD2 +    0.5000\*HO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P205   | zRNO3 + RO2C ----> RO2C +    0.5000\*PRD2 +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P206   | zRNO3 + RO2XC ----> RO2XC +    0.5000\*PRD2 +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P207   | zRNO3 + MECO3 ----> MECO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P208   | zRNO3 + RCO3 ----> RCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P209   | zRNO3 + BZCO3 ----> BZCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P210   | zRNO3 + MACO3 ----> MACO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PZ201   | zMTNO3 + NO ----> NO + MTNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| PZ202   | zMTNO3 + HO2 ----> HO2  |   2.65E-13e<sup>  1300.00/T</sup> |   2.0743E-11 |
| PZ203   | zMTNO3 + NO3 ----> NO3 + PRD2 + HO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| PZ204   | zMTNO3 + MEO2 ----> MEO2 +    0.5000\*PRD2 +    0.5000\*HO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| PZ205   | zMTNO3 + RO2C ----> RO2C +    0.5000\*PRD2 +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| PZ206   | zMTNO3 + RO2XC ----> RO2XC +    0.5000\*PRD2 +    0.5000\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| PZ207   | zMTNO3 + MECO3 ----> MECO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PZ208   | zMTNO3 + RCO3 ----> RCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PZ209   | zMTNO3 + BZCO3 ----> BZCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PZ210   | zMTNO3 + MACO3 ----> MACO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| PZ210b   | zMTNO3 + IMACO3 ----> IMACO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P211   | xHOCCHO + NO ----> NO + HOCCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P212   | xHOCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P213   | xHOCCHO + NO3 ----> NO3 + HOCCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P214   | xHOCCHO + MEO2 ----> MEO2 +    0.5000\*HOCCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P215   | xHOCCHO + RO2C ----> RO2C +    0.5000\*HOCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P216   | xHOCCHO + RO2XC ----> RO2XC +    0.5000\*HOCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P217   | xHOCCHO + MECO3 ----> MECO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P218   | xHOCCHO + RCO3 ----> RCO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P219   | xHOCCHO + BZCO3 ----> BZCO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P220   | xHOCCHO + MACO3 ----> MACO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P221   | xACROLEIN + NO ----> NO + ACROLEIN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P222   | xACROLEIN + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P223   | xACROLEIN + NO3 ----> NO3 + ACROLEIN  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P224   | xACROLEIN + MEO2 ----> MEO2 +    0.5000\*ACROLEIN  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P225   | xACROLEIN + RO2C ----> RO2C +    0.5000\*ACROLEIN  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P226   | xACROLEIN + RO2XC ----> RO2XC +    0.5000\*ACROLEIN  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P227   | xACROLEIN + MECO3 ----> MECO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P228   | xACROLEIN + RCO3 ----> RCO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P229   | xACROLEIN + BZCO3 ----> BZCO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P230   | xACROLEIN + MACO3 ----> MACO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BE01   | OH + CH4 ----> MEO2  |   1.85E-12e<sup> -1690.00/T</sup> |   6.3895E-15 |
| BE02   | ETHENE + OH ----> xHO2 + RO2C +    1.6100\*xHCHO +    0.1950\*xHOCCHO + yROOH  | k<sub>o</sub>=  1.00E-28e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   8.80E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.00;F=     0.60 |   8.1981E-12 |
| BE03   | ETHENE + O3 ---->   0.1600\*HO2 +    0.1600\*OH +    0.5100\*CO +    0.1200\*CO2 + HCHO +    0.3700\*HCOOH  |   9.14E-15e<sup> -2580.00/T</sup> |   1.5953E-18 |
| BE04   | ETHENE + NO3 ----> xHO2 + RO2C + xRCHO + yROOH  |   3.30E-12e<sup> -2880.00/T</sup> |   2.1058E-16 |
| BE05   | ETHENE + O3P ---->   0.8000\*HO2 +    0.2900\*xHO2 +    0.5100\*MEO2 +    0.2900\*RO2C +    0.5100\*CO +    0.2780\*xCO +    0.2780\*xHCHO +    0.1000\*CCHO +    0.0120\*xGLY +    0.2900\*yROOH  |   1.07E-11e<sup>  -800.00/T</sup> |   7.3127E-13 |
| BT01   | PROPENE + OH ---->   0.9840\*xHO2 +    0.9840\*RO2C +    0.0160\*RO2XC +    0.0160\*zRNO3 +    0.9840\*xHCHO +    0.9840\*xCCHO + yROOH  |   4.85E-12e<sup>   504.00/T</sup> |   2.6296E-11 |
| BT02   | PROPENE + O3 ---->   0.1650\*HO2 +    0.3500\*OH +    0.3550\*MEO2 +    0.5250\*CO +    0.2150\*CO2 +    0.5000\*HCHO +    0.5000\*CCHO +    0.1850\*HCOOH +    0.0750\*CCOOH  |   5.51E-15e<sup> -1878.00/T</sup> |   1.0130E-17 |
| BT03   | PROPENE + NO3 ---->   0.9490\*xHO2 +    0.9490\*RO2C +    0.0510\*RO2XC +    0.0510\*zRNO3 + yROOH  |   4.59E-13e<sup> -1156.00/T</sup> |   9.5049E-15 |
| BT04   | PROPENE + O3P ---->   0.4500\*RCHO +    0.5500\*MEK  |   1.02E-11e<sup>  -280.00/T</sup> |   3.9879E-12 |
| BT05   | BUTADIENE13 + OH ---->   0.9510\*xHO2 +    1.1890\*RO2C +    0.0490\*RO2XC +    0.0490\*zRNO3 +    0.7080\*xHCHO +    0.5800\*xACROLEIN +    0.4710\*xIPRD + yROOH  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| BT06   | BUTADIENE13 + O3 ---->   0.0800\*HO2 +    0.0800\*OH +    0.2550\*CO +    0.1850\*CO2 +    0.5000\*HCHO +    0.1850\*HCOOH +    0.5000\*ACROLEIN +    0.3750\*MVK +    0.1250\*PRD2  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| BT07   | BUTADIENE13 + NO3 ---->   0.8150\*xHO2 +    0.1200\*xNO2 +    1.0550\*RO2C +    0.0650\*RO2XC +    0.0650\*zRNO3 +    0.1150\*xHCHO +    0.4600\*xMVK +    0.1200\*xIPRD +    0.3550\*xRNO3 + yROOH  |   1.0000E-13 |   1.0000E-13 |
| BT08   | BUTADIENE13 + O3P ---->   0.2500\*HO2 +    0.1170\*xHO2 +    0.1180\*xMACO3 +    0.2350\*RO2C +    0.0150\*RO2XC +    0.0150\*zRNO3 +    0.1150\*xCO +    0.1150\*xACROLEIN +    0.0010\*xAFG1 +    0.0010\*xAFG2 +    0.7500\*PRD2 +    0.2500\*yROOH  |   2.26E-11e<sup>   -40.00/T</sup> |   1.9763E-11 |
| BE07   | ISOPRENE + O3 ---->   0.0660\*HO2 +    0.2660\*OH +    0.1920\*xMACO3 +    0.1920\*RO2C +    0.0080\*RO2XC +    0.0080\*zRNO3 +    0.2750\*CO +    0.1220\*CO2 +    0.4000\*HCHO +    0.1920\*xHCHO +    0.2040\*HCOOH +    0.3900\*MACR +    0.1600\*MVK +    0.1500\*IPRD +    0.1000\*PRD2 +    0.2000\*yR6OOH  |   7.86E-15e<sup> -1912.00/T</sup> |   1.2893E-17 |
| BE09   | ISOPRENE + O3P ---->   0.2500\*MEO2 +    0.2400\*xMACO3 +    0.2400\*RO2C +    0.0100\*RO2XC +    0.0100\*zRNO3 +    0.2400\*xHCHO +    0.7500\*PRD2 +    0.2500\*yR6OOH  |   3.5000E-11 |   3.5000E-11 |
| BT09   | APIN + OH ---->   0.7990\*xHO2 +    0.0040\*xRCO3 +    1.0420\*RO2C +    0.1970\*RO2XC +    0.1970\*zRNO3 +    0.0020\*xCO +    0.0220\*xHCHO +    0.7760\*xRCHO +    0.0340\*xACETONE +    0.0200\*xMGLY +    0.0230\*xBACL + yR6OOH + TRPRXN  |   1.21E-11e<sup>   436.00/T</sup> |   5.2225E-11 |
| BT10   | APIN + O3 ---->   0.0090\*HO2 +    0.1020\*xHO2 +    0.7280\*OH +    0.0010\*xMECO3 +    0.2970\*xRCO3 +    1.5110\*RO2C +    0.3370\*RO2XC +    0.3370\*zRNO3 +    0.0290\*CO +    0.0510\*xCO +    0.0170\*CO2 +    0.3440\*xHCHO +    0.2400\*xRCHO +    0.3450\*xACETONE +    0.0080\*MEK +    0.0020\*xGLY +    0.0810\*xBACL +    0.2550\*PRD2 +    0.7370\*yR6OOH + TRPRXN  |   5.00E-16e<sup>  -530.00/T</sup> |   8.4519E-17 |
| BT11   | APIN + NO3 ---->   0.0560\*xHO2 +    0.6430\*xNO2 +    0.0070\*xRCO3 +    1.0500\*RO2C +    0.2930\*RO2XC +    0.2930\*zRNO3 +    0.0050\*xCO +    0.0070\*xHCHO +    0.6840\*xRCHO +    0.0690\*xACETONE +    0.0020\*xMGLY +    0.0560\*xRNO3 + yR6OOH  |   1.19E-12e<sup>   490.00/T</sup> |   6.1560E-12 |
| BT12   | APIN + O3P ----> PRD2 + TRPRXN  |   3.2000E-11 |   3.2000E-11 |
| BE10   | ACETYLENE + OH ---->   0.3000\*HO2 +    0.7000\*OH +    0.3000\*CO +    0.3000\*HCOOH +    0.7000\*GLY  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>n=     1.00;F=     0.60 |   7.6556E-13 |
| BE11   | ACETYLENE + O3 ---->   1.5000\*HO2 +    0.5000\*OH +    1.5000\*CO +    0.5000\*CO2  |   1.00E-14e<sup> -4100.00/T</sup> |   1.0661E-20 |
| BE12   | BENZENE + OH ---->   0.5700\*HO2 +    0.2900\*xHO2 +    0.1160\*OH +    0.2900\*RO2C +    0.0240\*RO2XC +    0.0240\*zRNO3 +    0.2900\*xGLY +    0.5700\*CRES +    0.0290\*xAFG1 +    0.2610\*xAFG2 +    0.1160\*AFG3 +    0.3140\*yRAOOH + BENZRO2  |   2.33E-12e<sup>  -193.00/T</sup> |   1.2196E-12 |
| BT13   | TOLUENE + OH ---->   0.1810\*HO2 +    0.4540\*xHO2 +    0.3120\*OH +    0.4540\*RO2C +    0.0540\*RO2XC +    0.0540\*zRNO3 +    0.2380\*xGLY +    0.1510\*xMGLY +    0.1810\*CRES +    0.0650\*xBALD +    0.1950\*xAFG1 +    0.1950\*xAFG2 +    0.3120\*AFG3 +    0.0730\*yR6OOH +    0.4350\*yRAOOH + TOLRO2  |   1.81E-12e<sup>   338.00/T</sup> |   5.6237E-12 |
| BT14   | MXYL + OH ---->   0.1590\*HO2 +    0.5200\*xHO2 +    0.2390\*OH +    0.5200\*RO2C +    0.0820\*RO2XC +    0.0820\*zRNO3 +    0.1000\*xGLY +    0.3800\*xMGLY +    0.1590\*CRES +    0.0410\*xBALD +    0.3360\*xAFG1 +    0.1440\*xAFG2 +    0.2390\*AFG3 +    0.0470\*yR6OOH +    0.5550\*yRAOOH + XYLRO2  |   2.3100E-11 |   2.3100E-11 |
| BT15   | OXYL + OH ---->   0.1610\*HO2 +    0.5540\*xHO2 +    0.1980\*OH +    0.5540\*RO2C +    0.0870\*RO2XC +    0.0870\*zRNO3 +    0.0840\*xGLY +    0.2380\*xMGLY +    0.1850\*xBACL +    0.1610\*CRES +    0.0470\*xBALD +    0.2530\*xAFG1 +    0.2530\*xAFG2 +    0.1980\*AFG3 +    0.0550\*yR6OOH +    0.5860\*yRAOOH + XYLRO2  |   1.3600E-11 |   1.3600E-11 |
| BT16   | PXYL + OH ---->   0.1590\*HO2 +    0.4870\*xHO2 +    0.2780\*OH +    0.4870\*RO2C +    0.0760\*RO2XC +    0.0760\*zRNO3 +    0.2860\*xGLY +    0.1120\*xMGLY +    0.1590\*CRES +    0.0880\*xBALD +    0.0450\*xAFG1 +    0.0670\*xAFG2 +    0.2780\*AFG3 +    0.2860\*xAFG3 +    0.1020\*yR6OOH +    0.4610\*yRAOOH + XYLRO2  |   1.4300E-11 |   1.4300E-11 |
| BT17   | TMBENZ124 + OH ---->   0.0220\*HO2 +    0.6270\*xHO2 +    0.2300\*OH +    0.6270\*RO2C +    0.1210\*RO2XC +    0.1210\*zRNO3 +    0.0740\*xGLY +    0.4050\*xMGLY +    0.1120\*xBACL +    0.0220\*CRES +    0.0360\*xBALD +    0.0880\*xAFG1 +    0.3520\*xAFG2 +    0.2300\*AFG3 +    0.1510\*xAFG3 +    0.0430\*yR6OOH +    0.7050\*yRAOOH + XYLRO2  |   3.2500E-11 |   3.2500E-11 |
| BT18   | ETOH + OH ---->   0.9500\*HO2 +    0.0500\*xHO2 +    0.0500\*RO2C +    0.0810\*xHCHO +    0.9500\*CCHO +    0.0100\*xHOCCHO +    0.0500\*yROOH  |   5.49E-13e<sup>   530.00/T</sup>(T/300)<sup>  2.00 </sup> |   3.2078E-12 |
| BL01   | ALK1 + OH ----> xHO2 + RO2C + xCCHO + yROOH  |   1.34E-12e<sup>  -499.00/T</sup>(T/300)<sup>  2.00 </sup> |   2.4824E-13 |
| BL02   | ALK2 + OH ---->   0.9650\*xHO2 +    0.9650\*RO2C +    0.0350\*RO2XC +    0.0350\*zRNO3 +    0.2610\*xRCHO +    0.7040\*xACETONE + yROOH  |   1.49E-12e<sup>   -87.00/T</sup>(T/300)<sup>  2.00 </sup> |   1.0992E-12 |
| BL03   | ALK3 + OH ---->   0.6950\*xHO2 +    0.2360\*xTBUO +    1.2530\*RO2C +    0.0700\*RO2XC +    0.0700\*zRNO3 +    0.0260\*xHCHO +    0.4450\*xCCHO +    0.1220\*xRCHO +    0.0240\*xACETONE +    0.3320\*xMEK +    0.9830\*yROOH +    0.0170\*yR6OOH  |   1.51E-12e<sup>   126.00/T</sup> |   2.3042E-12 |
| BL04   | ALK4 + OH ---->   0.8300\*xHO2 +    0.0100\*xMEO2 +    0.0110\*xMECO3 +    1.7630\*RO2C +    0.1490\*RO2XC +    0.1490\*zRNO3 +    0.0020\*xCO +    0.0290\*xHCHO +    0.4380\*xCCHO +    0.2360\*xRCHO +    0.4260\*xACETONE +    0.1060\*xMEK +    0.1460\*xPROD2 + yR6OOH  |   3.75E-12e<sup>    44.00/T</sup> |   4.3463E-12 |
| BL05   | ALK5 + OH ---->   0.6470\*xHO2 +    1.6050\*RO2C +    0.3530\*RO2XC +    0.3530\*zRNO3 +    0.0400\*xHCHO +    0.1060\*xCCHO +    0.2090\*xRCHO +    0.0710\*xACETONE +    0.0860\*xMEK +    0.4070\*xPROD2 + yR6OOH  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| AALK   | SOAALK + OH ----> OH +    0.0060\*SVAVB2 +    0.0520\*SVAVB3 +    0.0810\*SVAVB4  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| BL06   | OLE1 + OH ---->   0.8710\*xHO2 +    0.0010\*xMEO2 +    1.2020\*RO2C +    0.1280\*RO2XC +    0.1280\*zRNO3 +    0.5820\*xHCHO +    0.0100\*xCCHO +    0.0070\*xHOCCHO +    0.6660\*xRCHO +    0.0070\*xACETONE +    0.0360\*xACROLEIN +    0.0010\*xMACR +    0.0120\*xMVK +    0.0090\*xIPRD +    0.1680\*xPROD2 +    0.1690\*yROOH +    0.8310\*yR6OOH  |   6.72E-12e<sup>   501.00/T</sup> |   3.6070E-11 |
| BL07   | OLE1 + O3 ---->   0.0950\*HO2 +    0.0570\*xHO2 +    0.1280\*OH +    0.0900\*RO2C +    0.0050\*RO2XC +    0.0050\*zRNO3 +    0.3030\*CO +    0.0880\*CO2 +    0.5000\*HCHO +    0.0110\*xCCHO +    0.5000\*RCHO +    0.0440\*xRCHO +    0.0030\*xACETONE +    0.0090\*MEK +    0.1850\*HCOOH +    0.1590\*RCOOH +    0.2680\*PRD2 +    0.0110\*yROOH +    0.0520\*yR6OOH  |   3.19E-15e<sup> -1701.00/T</sup> |   1.0618E-17 |
| BL08   | OLE1 + NO3 ---->   0.7720\*xHO2 +    1.4630\*RO2C +    0.2280\*RO2XC +    0.2280\*zRNO3 +    0.0130\*xCCHO +    0.0030\*xRCHO +    0.0340\*xACETONE +    0.7740\*xRNO3 +    0.1690\*yROOH +    0.8310\*yR6OOH  |   5.37E-13e<sup> -1047.00/T</sup> |   1.6028E-14 |
| BL09   | OLE1 + O3P ---->   0.4500\*RCHO +    0.3900\*MEK +    0.1600\*PRD2  |   1.61E-11e<sup>  -326.00/T</sup> |   5.3947E-12 |
| BL10   | OLE2 + OH ---->   0.9120\*xHO2 +    0.9530\*RO2C +    0.0880\*RO2XC +    0.0880\*zRNO3 +    0.1790\*xHCHO +    0.8350\*xCCHO +    0.5100\*xRCHO +    0.1440\*xACETONE +    0.0800\*xMEK +    0.0020\*xMVK +    0.0120\*xIPRD +    0.0230\*xPROD2 +    0.3190\*yROOH +    0.6810\*yR6OOH  |   1.26E-11e<sup>   488.00/T</sup> |   6.4745E-11 |
| BL11   | OLE2 + O3 ---->   0.0940\*HO2 +    0.0410\*xHO2 +    0.4430\*OH +    0.3070\*MEO2 +    0.1560\*xMECO3 +    0.0080\*xRCO3 +    0.2120\*RO2C +    0.0030\*RO2XC +    0.0030\*zRNO3 +    0.2990\*CO +    0.1610\*CO2 +    0.1310\*HCHO +    0.1140\*xHCHO +    0.4530\*CCHO +    0.0710\*xCCHO +    0.3330\*RCHO +    0.0190\*xRCHO +    0.0510\*ACETONE +    0.0330\*MEK +    0.0010\*xMEK +    0.0240\*HCOOH +    0.0650\*CCOOH +    0.2350\*RCOOH +    0.0370\*PRD2 +    0.0730\*yROOH +    0.1360\*yR6OOH  |   8.59E-15e<sup> -1255.00/T</sup> |   1.2762E-16 |
| BL12   | OLE2 + NO3 ---->   0.4000\*xHO2 +    0.4260\*xNO2 +    0.0350\*xMEO2 +    1.1930\*RO2C +    0.1400\*RO2XC +    0.1400\*zRNO3 +    0.0720\*xHCHO +    0.5790\*xCCHO +    0.1630\*xRCHO +    0.1160\*xACETONE +    0.0020\*xMEK +    0.3200\*xRNO3 +    0.3190\*yROOH +    0.6810\*yR6OOH  |   2.31E-13e<sup>   382.00/T</sup> |   8.3185E-13 |
| BL13   | OLE2 + O3P ---->   0.0790\*RCHO +    0.7510\*MEK +    0.1700\*PRD2  |   1.43E-11e<sup>   111.00/T</sup> |   2.0750E-11 |
| BL14   | ARO1 + OH ---->   0.1230\*HO2 +    0.5660\*xHO2 +    0.2020\*OH +    0.5660\*RO2C +    0.1100\*RO2XC +    0.1100\*zRNO3 +    0.1580\*xGLY +    0.1000\*xMGLY +    0.1230\*CRES +    0.0720\*xAFG1 +    0.1850\*xAFG2 +    0.2020\*AFG3 +    0.3090\*xPROD2 +    0.3690\*yR6OOH + TOLRO2  |   7.8400E-12 |   7.8400E-12 |
| BL15a   | ARO2MN + OH ---->   0.0770\*HO2 +    0.6170\*xHO2 +    0.1780\*OH +    0.6170\*RO2C +    0.1280\*RO2XC +    0.1280\*zRNO3 +    0.0880\*xGLY +    0.3120\*xMGLY +    0.1340\*xBACL +    0.0770\*CRES +    0.0260\*xBALD +    0.2210\*xAFG1 +    0.2470\*xAFG2 +    0.1780\*AFG3 +    0.0680\*xAFG3 +    0.0570\*xPROD2 +    0.1010\*yR6OOH + XYLRO2  |   3.0900E-11 |   3.0900E-11 |
| BL15b   | NAPHTHAL + OH ---->   0.0770\*HO2 +    0.6170\*xHO2 +    0.1780\*OH +    0.6170\*RO2C +    0.1280\*RO2XC +    0.1280\*zRNO3 +    0.0880\*xGLY +    0.3120\*xMGLY +    0.1340\*xBACL +    0.0770\*CRES +    0.0260\*xBALD +    0.2210\*xAFG1 +    0.2470\*xAFG2 +    0.1780\*AFG3 +    0.0680\*xAFG3 +    0.0570\*xPROD2 +    0.1010\*yR6OOH + PAHRO2  |   3.0900E-11 |   3.0900E-11 |
| BL16   | TERP + OH ---->   0.7340\*xHO2 +    0.0640\*xRCO3 +    1.2110\*RO2C +    0.2010\*RO2XC +    0.2010\*zMTNO3 +    0.0010\*xCO +    0.4110\*xHCHO +    0.3850\*xRCHO +    0.0370\*xACETONE +    0.0070\*xMEK +    0.0030\*xMGLY +    0.0090\*xBACL +    0.0030\*xMVK +    0.0020\*xIPRD +    0.4090\*xPROD2 + yR6OOH + TRPRXN  |   2.27E-11e<sup>   435.00/T</sup> |   9.7647E-11 |
| BL17   | TERP + O3 ---->   0.0780\*HO2 +    0.0460\*xHO2 +    0.4990\*OH +    0.2020\*xMECO3 +    0.0590\*xRCO3 +    0.4900\*RO2C +    0.1210\*RO2XC +    0.1210\*zMTNO3 +    0.2490\*CO +    0.0630\*CO2 +    0.1270\*HCHO +    0.0330\*xHCHO +    0.2080\*xRCHO +    0.0570\*xACETONE +    0.0020\*MEK +    0.1720\*HCOOH +    0.0680\*RCOOH +    0.0030\*xMGLY +    0.0390\*xBACL +    0.0020\*xMACR +    0.0010\*xIPRD +    0.5020\*PRD2 +    0.4280\*yR6OOH + TRPRXN  |   8.28E-16e<sup>  -785.00/T</sup> |   5.9508E-17 |
| BL18   | TERP + NO3 ----> TERPNRO2  |   1.33E-12e<sup>   490.00/T</sup> |   6.8802E-12 |
| BL18a   | TERPNRO2 + NO ---->   0.8270\*NO2 +    0.6880\*MTNO3 +    0.4240\*RO2C +    0.2270\*HO2 +    0.0260\*RCO3 +    0.0120\*CO +    0.0230\*HCHO +    0.0020\*HOCCHO +    0.4030\*RCHO +    0.2390\*ACETONE +    0.0050\*MACR +    0.0010\*MVK +    0.0040\*IPRD  |   BR07 |   9.3002E-12<sup>7</sup>| 
| BL18b   | TERPNRO2 + HO2 ----> MTNO3  |   2.65E-13e<sup>  1300.00/T</sup> |   2.0743E-11 |
| BL18c   | TERPNRO2 + NO3 ---->   1.5310\*NO2 +    0.4220\*MTNO3 +    0.7860\*RO2C +    0.4200\*HO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0430\*HCHO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BL18d   | TERPNRO2 + MEO2 ---->   0.2660\*NO2 +    0.7110\*MTNO3 +    0.3930\*RO2C +    0.7100\*HO2 +    0.0240\*RCO3 +    0.0110\*CO +    0.7720\*HCHO +    0.0020\*HOCCHO +    0.3730\*RCHO +    0.2220\*ACETONE +    0.0050\*MACR +    0.0010\*MVK +    0.0040\*IPRD +    0.2500\*MEOH  |   BR10 |   2.0000E-13<sup>7</sup>| 
| BL18e   | TERPNRO2 + RO2C ---->   0.2660\*NO2 +    0.7110\*MTNO3 +    0.3930\*RO2C +    0.2100\*HO2 +    0.0240\*RCO3 +    0.0110\*CO +    0.0220\*HCHO +    0.0020\*HOCCHO +    0.3730\*RCHO +    0.2220\*ACETONE +    0.0050\*MACR +    0.0010\*MVK +    0.0040\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| BL18f   | TERPNRO2 + RO2XC ---->   0.2660\*NO2 +    0.7110\*MTNO3 +    0.3930\*RO2C +    0.2100\*HO2 +    0.0240\*RCO3 +    0.0110\*CO +    0.0220\*HCHO +    0.0020\*HOCCHO +    0.3730\*RCHO +    0.2220\*ACETONE +    0.0050\*MACR +    0.0010\*MVK +    0.0040\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| BL18g   | TERPNRO2 + MECO3 ---->   0.5310\*NO2 +    0.4220\*MTNO3 +    0.7860\*RO2C +    0.4200\*HO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0430\*HCHO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD + MEO2 + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BL18h   | TERPNRO2 + RCO3 ---->   0.5310\*NO2 +    0.4220\*MTNO3 +    1.7860\*RO2C +    0.4200\*HO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0430\*HCHO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD + CO2 + xHO2 + xCCHO + yROOH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BL18i   | TERPNRO2 + BZCO3 ---->   0.5310\*NO2 +    0.4220\*MTNO3 +    1.7860\*RO2C +    0.4200\*HO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0430\*HCHO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD + CO2 + BZO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BL19j   | TERPNRO2 + MACO3 ----> CO2 +    1.0430\*HCHO + MECO3 +    0.7860\*RO2C +    0.4200\*HO2 +    0.5310\*NO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD +    0.4220\*MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BL19k   | TERPNRO2 + IMACO3 ----> CO2 +    1.0430\*HCHO + MECO3 +    0.7860\*RO2C +    0.4200\*HO2 +    0.5310\*NO2 +    0.0480\*RCO3 +    0.0220\*CO +    0.0040\*HOCCHO +    0.7460\*RCHO +    0.4430\*ACETONE +    0.0090\*MACR +    0.0020\*MVK +    0.0070\*IPRD +    0.4220\*MTNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BL19   | TERP + O3P ---->   0.2370\*RCHO +    0.7630\*PRD2 + TRPRXN  |   4.0200E-11 |   4.0200E-11 |
| BT19   | SESQ + OH ---->   0.7340\*xHO2 +    0.0640\*xRCO3 +    1.2110\*RO2C +    0.2010\*RO2XC +    0.2010\*zRNO3 +    0.0010\*xCO +    0.4110\*xHCHO +    0.3850\*xRCHO +    0.0370\*xACETONE +    0.0070\*xMEK +    0.0030\*xMGLY +    0.0090\*xBACL +    0.0030\*xMVK +    0.0020\*xIPRD +    0.4090\*xPROD2 + yR6OOH + SESQRXN  |   BL16 |   9.7647E-11<sup>7</sup>| 
| BT20   | SESQ + O3 ---->   0.0780\*HO2 +    0.0460\*xHO2 +    0.4990\*OH +    0.2020\*xMECO3 +    0.0590\*xRCO3 +    0.4900\*RO2C +    0.1210\*RO2XC +    0.1210\*zRNO3 +    0.2490\*CO +    0.0630\*CO2 +    0.1270\*HCHO +    0.0330\*xHCHO +    0.2080\*xRCHO +    0.0570\*xACETONE +    0.0020\*MEK +    0.1720\*HCOOH +    0.0680\*RCOOH +    0.0030\*xMGLY +    0.0390\*xBACL +    0.0020\*xMACR +    0.0010\*xIPRD +    0.5020\*PRD2 +    0.4280\*yR6OOH + SESQRXN  |   BL17 |   5.9508E-17<sup>7</sup>| 
| BT21   | SESQ + NO3 ---->   0.2270\*xHO2 +    0.2870\*xNO2 +    0.0260\*xRCO3 +    1.7860\*RO2C +    0.4600\*RO2XC +    0.4600\*zRNO3 +    0.0120\*xCO +    0.0230\*xHCHO +    0.0020\*xCCHO +    0.4030\*xRCHO +    0.2390\*xACETONE +    0.0050\*xMACR +    0.0010\*xMVK +    0.0040\*xIPRD +    0.2280\*xRNO3 + yR6OOH + SESQRXN  |   BL18 |   6.8802E-12<sup>7</sup>| 
| BT22   | SESQ + O3P ---->   0.2370\*RCHO +    0.7630\*PRD2 + SESQRXN  |   BL19 |   4.0200E-11<sup>7</sup>| 
| CI01   | CL2 ---->   2.0000\*CL  | CL2 | Not Available<sup>1</sup> | 
| CI02   | CL + NO + M ----> CLNO  |   7.60E-32(T/300)<sup> -1.80</sup> |   7.6851E-32 |
| CI03   | CLNO ----> CL + NO  | CLNO_06 | Not Available<sup>1</sup> | 
| CI04   | CL + NO2 ----> CLONO  | k<sub>o</sub>=  1.30E-30e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>k<sub>i</sub> =   1.00E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   1.6244E-11 |
| CI05   | CL + NO2 ----> CLNO2  | k<sub>o</sub>=  1.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>k<sub>i</sub> =   1.00E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   3.5840E-12 |
| CI06   | CLONO ----> CL + NO2  | CLONO | Not Available<sup>1</sup> | 
| CI07   | CLNO2 ----> CL + NO2  | CLNO2 | Not Available<sup>1</sup> | 
| CI08   | CL + HO2 ----> HCL  |   3.44E-11(T/300)<sup> -0.56</sup> |   3.4519E-11 |
| CI09   | CL + HO2 ----> CLO + OH  |   9.41E-12(T/300)<sup>  2.10</sup> |   9.2886E-12 |
| CI10   | CL + O3 ----> CLO  |   2.80E-11e<sup>  -250.00/T</sup> |   1.2106E-11 |
| CI11   | CL + NO3 ----> CLO + NO2  |   2.4000E-11 |   2.4000E-11 |
| CI12   | CLO + NO ----> CL + NO2  |   6.20E-12e<sup>   295.00/T</sup> |   1.6676E-11 |
| CI13   | CLO + NO2 ----> CLONO2  | k<sub>o</sub>=  1.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   1.50E-11e<sup>     0.0/T</sup>(T/300)<sup> -1.90</sup><br>n=     1.00;F=     0.60 |   2.3359E-12 |
| CI14   | CLONO2 ----> CLO + NO2  | CLONO2_1 | Not Available<sup>1</sup> | 
| CI15   | CLONO2 ----> CL + NO3  | CLONO2_2 | Not Available<sup>1</sup> | 
| CI16   | CLONO2 ----> CLO + NO2  | k<sub>o</sub>=  4.48E-05e<sup>-12530.0/T</sup>(T/300)<sup> -1.00</sup><br>k<sub>i</sub> =   3.71E+15e<sup>-12530.0/T</sup>(T/300)<sup>  3.50</sup><br>n=     1.00;F=     0.60 |   3.1797E-04 |
| CI17   | CL + CLONO2 ----> CL2 + NO3  |   6.20E-12e<sup>   145.00/T</sup> |   1.0083E-11 |
| CI18   | CLO + HO2 ----> HOCL  |   2.20E-12e<sup>   340.00/T</sup> |   6.8814E-12 |
| CI19   | HOCL ----> OH + CL  | HOCL_06 | Not Available<sup>1</sup> | 
| CI20   | CLO + CLO ---->   0.2900\*CL2 +    1.4200\*CL  |   1.25E-11e<sup> -1960.00/T</sup> |   1.7455E-14 |
| CI21   | OH + HCL ----> CL  |   1.70E-12e<sup>  -230.00/T</sup> |   7.8600E-13 |
| CI22   | CL + H2 ----> HCL + HO2  |   3.90E-11e<sup> -2310.00/T</sup> |   1.6836E-14 |
| CP01   | HCHO + CL ----> HCL + HO2 + CO  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| CP02   | CCHO + CL ----> HCL + MECO3  |   8.0000E-11 |   8.0000E-11 |
| CP03   | MEOH + CL ----> HCL + HCHO + HO2  |   5.5000E-11 |   5.5000E-11 |
| CP04   | RCHO + CL ----> HCL +    0.9000\*RCO3 +    0.1000\*RO2C +    0.1000\*xCCHO +    0.1000\*xCO +    0.1000\*xHO2 +    0.1000\*yROOH  |   1.2300E-10 |   1.2300E-10 |
| CP05   | ACETONE + CL ----> HCL + RO2C + xHCHO + xMECO3 + yROOH  |   7.70E-11e<sup> -1000.00/T</sup> |   2.6907E-12 |
| CP06   | MEK + CL ----> HCL +    0.9750\*RO2C +    0.0390\*RO2XC +    0.0390\*zRNO3 +    0.8400\*xHO2 +    0.0850\*xMECO3 +    0.0360\*xRCO3 +    0.0650\*xHCHO +    0.0700\*xCCHO +    0.8400\*xRCHO + yROOH  |   3.6000E-11 |   3.6000E-11 |
| CP07   | RNO3 + CL ----> HCL +    0.0380\*NO2 +    0.0550\*HO2 +    1.2820\*RO2C +    0.2020\*RO2XC +    0.2020\*zRNO3 +    0.0090\*RCHO +    0.0180\*MEK +    0.0120\*PRD2 +    0.0550\*RNO3 +    0.1590\*xNO2 +    0.5470\*xHO2 +    0.0450\*xHCHO +    0.3000\*xCCHO +    0.0200\*xRCHO +    0.0030\*xACETONE +    0.0410\*xMEK +    0.0460\*xPROD2 +    0.5470\*xRNO3 +    0.9080\*yR6OOH  |   1.9200E-10 |   1.9200E-10 |
| CP08   | PRD2 + CL ----> HCL +    0.3140\*HO2 +    0.6800\*RO2C +    0.1160\*RO2XC +    0.1160\*zRNO3 +    0.1980\*RCHO +    0.1160\*PRD2 +    0.5410\*xHO2 +    0.0070\*xMECO3 +    0.0220\*xRCO3 +    0.2370\*xHCHO +    0.1090\*xCCHO +    0.5910\*xRCHO +    0.0510\*xMEK +    0.0400\*xPROD2 +    0.6860\*yR6OOH  |   2.0000E-10 |   2.0000E-10 |
| CP09   | GLY + CL ----> HCL +    0.6300\*HO2 +    1.2600\*CO +    0.3700\*RCO3  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| CP10   | MGLY + CL ----> HCL + CO + MECO3  |   8.0000E-11 |   8.0000E-11 |
| CP11   | CRES + CL ----> HCL + xHO2 + xBALD + yR6OOH  |   6.2000E-11 |   6.2000E-11 |
| CP12   | BALD + CL ----> HCL + BZCO3  |   8.0000E-11 |   8.0000E-11 |
| CP13   | ROOH + CL ----> HCL +    0.4140\*OH +    0.5880\*RO2C +    0.4140\*RCHO +    0.1040\*xOH +    0.4820\*xHO2 +    0.1060\*xHCHO +    0.1040\*xCCHO +    0.1970\*xRCHO +    0.2850\*xMEK +    0.5860\*yROOH  |   1.6600E-10 |   1.6600E-10 |
| CP14   | R6OOH + CL ----> HCL +    0.1450\*OH +    1.0780\*RO2C +    0.1170\*RO2XC +    0.1170\*zRNO3 +    0.1450\*PRD2 +    0.5020\*xOH +    0.2370\*xHO2 +    0.1860\*xCCHO +    0.6760\*xRCHO +    0.2800\*xPROD2 +    0.8550\*yR6OOH  |   3.0000E-10 |   3.0000E-10 |
| CP15   | RAOOH + CL ---->   0.4040\*HCL +    0.1390\*OH +    0.1480\*HO2 +    0.5890\*RO2C +    0.1240\*RO2XC +    0.1240\*zRNO3 +    0.0740\*PRD2 +    0.1470\*MGLY +    0.1390\*IPRD +    0.5650\*xHO2 +    0.0240\*xOH +    0.4480\*xRCHO +    0.0260\*xGLY +    0.0300\*xMEK +    0.2520\*xMGLY +    0.0730\*xAFG1 +    0.0730\*xAFG2 +    0.7130\*yR6OOH  |   4.2900E-10 |   4.2900E-10 |
| TP01   | ACROLEIN + CL ---->   0.4840\*xHO2 +    0.2740\*xCL +    0.2160\*MACO3 +    1.0320\*RO2C +    0.0260\*RO2XC +    0.0260\*zRNO3 +    0.2160\*HCL +    0.4840\*xCO +    0.2740\*xHCHO +    0.2740\*xGLY +    0.4840\*xCLCCHO +    0.7840\*yROOH  |   2.9400E-10 |   2.9400E-10 |
| CP17   | MVK + CL ---->   1.2830\*RO2C +    0.0530\*RO2XC +    0.0530\*zRNO3 +    0.3220\*xHO2 +    0.6250\*xMECO3 +    0.9470\*xCLCCHO + yROOH  |   2.3200E-10 |   2.3200E-10 |
| CP18   | IPRD + CL ---->   0.4010\*HCL +    0.0840\*HO2 +    0.1540\*MACO3 +    0.7300\*RO2C +    0.0510\*RO2XC +    0.0510\*zRNO3 +    0.0420\*AFG1 +    0.0420\*AFG2 +    0.7120\*xHO2 +    0.4980\*xCO +    0.1950\*xHCHO +    0.0170\*xMGLY +    0.0090\*xAFG1 +    0.0090\*xAFG2 +    0.1150\*xIPRD +    0.1400\*xCLCCHO +    0.4200\*xCLACET +    0.7620\*yR6OOH  |   4.1200E-10 |   4.1200E-10 |
| CP19   | CLCCHO ----> HO2 + CO + RO2C + xCL + xHCHO + yROOH  | CLCCHO | Not Available<sup>1</sup> | 
| CP20   | CLCCHO + OH ----> RCO3  |   3.1000E-12 |   3.1000E-12 |
| CP21   | CLCCHO + CL ----> HCL + RCO3  |   1.2900E-11 |   1.2900E-11 |
| CP22   | CLACET ----> MECO3 + RO2C + xCL + xHCHO + yROOH  |   5.0000E-01\*CLACET | Not Available<sup>1</sup> | 
| CP29   | xCL + NO ----> NO + CL  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP30   | xCL + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP31   | xCL + NO3 ----> NO3 + CL  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP32   | xCL + MEO2 ----> MEO2 +    0.5000\*CL  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP33   | xCL + RO2C ----> RO2C +    0.5000\*CL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP34   | xCL + RO2XC ----> RO2XC +    0.5000\*CL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP35   | xCL + MECO3 ----> MECO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP36   | xCL + RCO3 ----> RCO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP37   | xCL + BZCO3 ----> BZCO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP38   | xCL + MACO3 ----> MACO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP39   | xCLCCHO + NO ----> NO + CLCCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP40   | xCLCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP41   | xCLCCHO + NO3 ----> NO3 + CLCCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP42   | xCLCCHO + MEO2 ----> MEO2 +    0.5000\*CLCCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP43   | xCLCCHO + RO2C ----> RO2C +    0.5000\*CLCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP44   | xCLCCHO + RO2XC ----> RO2XC +    0.5000\*CLCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP45   | xCLCCHO + MECO3 ----> MECO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP46   | xCLCCHO + RCO3 ----> RCO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP47   | xCLCCHO + BZCO3 ----> BZCO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP48   | xCLCCHO + MACO3 ----> MACO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP49   | xCLACET + NO ----> NO + CLACET  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP50   | xCLACET + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP51   | xCLACET + NO3 ----> NO3 + CLACET  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP52   | xCLACET + MEO2 ----> MEO2 +    0.5000\*CLACET  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP53   | xCLACET + RO2C ----> RO2C +    0.5000\*CLACET  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP54   | xCLACET + RO2XC ----> RO2XC +    0.5000\*CLACET  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP55   | xCLACET + MECO3 ----> MECO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP56   | xCLACET + RCO3 ----> RCO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP57   | xCLACET + BZCO3 ----> BZCO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP58   | xCLACET + MACO3 ----> MACO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CE01   | CL + CH4 ----> HCL + MEO2  |   7.30E-12e<sup> -1280.00/T</sup> |   9.9732E-14 |
| CE02   | ETHENE + CL ----> xHO2 +    2.0000\*RO2C + xHCHO + CLCHO  | k<sub>o</sub>=  1.60E-29e<sup>     0.0/T</sup>(T/300)<sup> -3.30</sup><br>k<sub>i</sub> =   3.10E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   1.0603E-10 |
| TE01   | PROPENE + CL ---->   0.1240\*HCL +    0.9710\*xHO2 +    0.9710\*RO2C +    0.0290\*RO2XC +    0.0290\*zRNO3 +    0.1240\*xACROLEIN +    0.3060\*xCLCCHO +    0.5400\*xCLACET + yROOH  |   2.6700E-10 |   2.6700E-10 |
| TE02   | BUTADIENE13 + CL ---->   0.3900\*xHO2 +    0.5410\*xCL +    1.8840\*RO2C +    0.0690\*RO2XC +    0.0690\*zRNO3 +    0.8630\*xHCHO +    0.4570\*xACROLEIN +    0.4730\*xIPRD + yROOH  |   4.9000E-10 |   4.9000E-10 |
| CE03   | ISOPRENE + CL ---->   0.1500\*HCL +    0.7380\*xHO2 +    0.1770\*xCL +    1.1680\*RO2C +    0.0850\*RO2XC +    0.0850\*zRNO3 +    0.2750\*xHCHO +    0.1770\*xMVK +    0.6710\*xIPRD +    0.0670\*xCLCCHO + yR6OOH  |   4.8000E-10 |   4.8000E-10 |
| TE03   | APIN + CL ---->   0.5480\*HCL +    0.2520\*xHO2 +    0.0680\*xCL +    0.0340\*xMECO3 +    0.0500\*xRCO3 +    0.0160\*xMACO3 +    2.2580\*RO2C +    0.5820\*RO2XC +    0.5820\*zRNO3 +    0.0350\*xCO +    0.1580\*xHCHO +    0.1850\*xRCHO +    0.2740\*xACETONE +    0.0070\*xGLY +    0.0030\*xBACL +    0.0030\*xMVK +    0.1580\*xIPRD +    0.0060\*xAFG1 +    0.0060\*xAFG2 +    0.0010\*xAFG3 +    0.1090\*xCLCCHO + yR6OOH  |   5.4600E-10 |   5.4600E-10 |
| CE04   | ACETYLENE + CL ----> HO2 + CO  | k<sub>o</sub>=  5.20E-30e<sup>     0.0/T</sup>(T/300)<sup> -2.40</sup><br>k<sub>i</sub> =   2.20E-10e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   5.0269E-11 |
| TE04   | TOLUENE + CL ---->   0.8940\*xHO2 +    0.8940\*RO2C +    0.1060\*RO2XC +    0.1060\*zRNO3 +    0.8940\*xBALD  |   6.2000E-11 |   6.2000E-11 |
| TE05   | MXYL + CL ---->   0.8640\*xHO2 +    0.8640\*RO2C +    0.1360\*RO2XC +    0.1360\*zRNO3 +    0.8640\*xBALD  |   1.3500E-10 |   1.3500E-10 |
| TE06   | OXYL + CL ---->   0.8640\*xHO2 +    0.8640\*RO2C +    0.1360\*RO2XC +    0.1360\*zRNO3 +    0.8640\*xBALD  |   1.4000E-10 |   1.4000E-10 |
| TE07   | PXYL + CL ---->   0.8640\*xHO2 +    0.8640\*RO2C +    0.1360\*RO2XC +    0.1360\*zRNO3 +    0.8640\*xBALD  |   1.4400E-10 |   1.4400E-10 |
| TE08   | TMBENZ124 + CL ---->   0.8380\*xHO2 +    0.8380\*RO2C +    0.1620\*RO2XC +    0.1620\*zRNO3 +    0.8380\*xBALD  |   2.4200E-10 |   2.4200E-10 |
| TE09   | ETOH + CL ----> HCL +    0.6880\*HO2 +    0.3120\*xHO2 +    0.3120\*RO2C +    0.5030\*xHCHO +    0.6880\*CCHO +    0.0610\*xHOCCHO +    0.3120\*yROOH  |   8.60E-11e<sup>    45.00/T</sup> |   1.0001E-10 |
| BC01   | ALK1 + CL ----> HCL + xHO2 + RO2C + xCCHO + yROOH  |   8.30E-11e<sup>  -100.00/T</sup> |   5.9349E-11 |
| BC02   | ALK2 + CL ----> HCL +    0.9700\*xHO2 +    0.9700\*RO2C +    0.0300\*RO2XC +    0.0300\*zRNO3 +    0.4820\*xRCHO +    0.4880\*xACETONE + yROOH  |   1.20E-10e<sup>    40.00/T</sup> |   1.3723E-10 |
| BC03   | ALK3 + CL ----> HCL +    0.8350\*xHO2 +    0.0940\*xTBUO +    1.3610\*RO2C +    0.0700\*RO2XC +    0.0700\*zRNO3 +    0.0780\*xHCHO +    0.3400\*xCCHO +    0.3430\*xRCHO +    0.0750\*xACETONE +    0.2530\*xMEK +    0.9830\*yROOH +    0.0170\*yR6OOH  |   1.8600E-10 |   1.8600E-10 |
| BC04   | ALK4 + CL ----> HCL +    0.8270\*xHO2 +    0.0030\*xMEO2 +    0.0040\*xMECO3 +    1.7370\*RO2C +    0.1650\*RO2XC +    0.1650\*zRNO3 +    0.0030\*xCO +    0.0340\*xHCHO +    0.2870\*xCCHO +    0.4120\*xRCHO +    0.2470\*xACETONE +    0.0760\*xMEK +    0.1300\*xPROD2 + yR6OOH  |   2.6300E-10 |   2.6300E-10 |
| BC05   | ALK5 + CL ----> HCL +    0.6470\*xHO2 +    1.5410\*RO2C +    0.3520\*RO2XC +    0.3520\*zRNO3 +    0.0220\*xHCHO +    0.0800\*xCCHO +    0.2580\*xRCHO +    0.0440\*xACETONE +    0.0410\*xMEK +    0.3780\*xPROD2 + yR6OOH  |   4.2100E-10 |   4.2100E-10 |
| BC06   | OLE1 + CL ---->   0.3840\*HCL +    0.8730\*xHO2 +    1.6080\*RO2C +    0.1270\*RO2XC +    0.1270\*zRNO3 +    0.0360\*xHCHO +    0.2060\*xCCHO +    0.0720\*xRCHO +    0.2150\*xACROLEIN +    0.0190\*xMVK +    0.0380\*xIPRD +    0.1920\*xCLCCHO +    0.3370\*xCLACET +    0.1690\*yROOH +    0.8310\*yR6OOH  |   3.9200E-10 |   3.9200E-10 |
| BC07   | OLE2 + CL ---->   0.2790\*HCL +    0.4500\*xHO2 +    0.4420\*xCL +    0.0010\*xMEO2 +    1.4920\*RO2C +    0.1060\*RO2XC +    0.1060\*zRNO3 +    0.1900\*xHCHO +    0.3830\*xCCHO +    0.3170\*xRCHO +    0.0860\*xACETONE +    0.0420\*xMEK +    0.0250\*xMACR +    0.0580\*xMVK +    0.1610\*xIPRD +    0.0130\*xCLCCHO +    0.1910\*xCLACET +    0.3190\*yROOH +    0.6810\*yR6OOH  |   3.7700E-10 |   3.7700E-10 |
| BC08   | ARO1 + CL ---->   0.8400\*xHO2 +    0.8400\*RO2C +    0.1600\*RO2XC +    0.1600\*zRNO3 +    0.8400\*xPROD2  |   2.1600E-10 |   2.1600E-10 |
| BC09a   | ARO2MN + CL ---->   0.8280\*xHO2 +    0.8280\*RO2C +    0.1720\*RO2XC +    0.1720\*zRNO3 +    0.4690\*xBALD +    0.3590\*xPROD2  |   2.6600E-10 |   2.6600E-10 |
| BC09b   | NAPHTHAL + CL ---->   0.8280\*xHO2 +    0.8280\*RO2C +    0.1720\*RO2XC +    0.1720\*zRNO3 +    0.4690\*xBALD +    0.3590\*xPROD2  |   2.6600E-10 |   2.6600E-10 |
| BC10   | TERP + CL ---->   0.5480\*HCL +    0.2520\*xHO2 +    0.0680\*xCL +    0.0340\*xMECO3 +    0.0500\*xRCO3 +    0.0160\*xMACO3 +    2.2580\*RO2C +    0.5820\*RO2XC +    0.5820\*zRNO3 +    0.0350\*xCO +    0.1580\*xHCHO +    0.1850\*xRCHO +    0.2740\*xACETONE +    0.0070\*xGLY +    0.0030\*xBACL +    0.0030\*xMVK +    0.1580\*xIPRD +    0.0060\*xAFG1 +    0.0060\*xAFG2 +    0.0010\*xAFG3 +    0.1090\*xCLCCHO + yR6OOH  |   5.4600E-10 |   5.4600E-10 |
| BC11   | SESQ + CL ---->   0.2520\*xHO2 +    0.0680\*xCL +    0.0340\*xMECO3 +    0.0500\*xRCO3 +    0.0160\*xMACO3 +    2.2580\*RO2C +    0.5820\*RO2XC +    0.5820\*zRNO3 +    0.5480\*HCL +    0.0350\*xCO +    0.1580\*xHCHO +    0.1850\*xRCHO +    0.2740\*xACETONE +    0.0070\*xGLY +    0.0030\*xBACL +    0.0030\*xMVK +    0.1580\*xIPRD +    0.0060\*xAFG1 +    0.0060\*xAFG2 +    0.0010\*xAFG3 +    0.1090\*xCLCCHO + yR6OOH  |   BC10 |   5.4600E-10<sup>7</sup>| 
| AE51   | BENZRO2 + NO ----> NO +    0.0340\*SVAVB2 +    0.3920\*SVAVB4  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE52   | BENZRO2 + HO2 ----> HO2 +    0.1460\*SVAVB1  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE53   | XYLRO2 + NO ----> NO +    0.0150\*SVAVB2 +    0.0230\*SVAVB3 +    0.0600\*SVAVB4  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE54   | XYLRO2 + HO2 ----> HO2 +    0.1930\*SVAVB1  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE55   | TOLRO2 + NO ----> NO +    0.0160\*SVAVB2 +    0.0510\*SVAVB3 +    0.0470\*SVAVB4  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE56   | TOLRO2 + HO2 ----> HO2 +    0.1400\*SVAVB1  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE57   | PAHRO2 + NO ----> NO +    0.0280\*SVAVB2 +    0.2250\*SVAVB3 +    0.1910\*SVAVB4  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE58   | PAHRO2 + HO2 ----> HO2 +    0.4730\*SVAVB1  |   BR08 |   7.7759E-12<sup>7</sup>| 
| TR01   | HCHO_PRIMARY ----> | HCHOR_06 | Not Available<sup>1</sup> | 
| TR02   | HCHO_PRIMARY ----> | HCHOM_06 | Not Available<sup>1</sup> | 
| TR03   | HCHO_PRIMARY + OH ----> OH  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| TR05   | HCHO_PRIMARY + NO3 ----> NO3  |   2.00E-12e<sup> -2431.00/T</sup> |   5.7539E-16 |
| TR06   | HCHO_PRIMARY + CL ----> CL  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| TR07   | CCHO_PRIMARY + OH ----> OH  |   4.40E-12e<sup>   365.00/T</sup> |   1.4967E-11 |
| TR08   | CCHO_PRIMARY ----> | CCHO_R | Not Available<sup>1</sup> | 
| TR09   | CCHO_PRIMARY + NO3 ----> NO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| TR10   | CCHO_PRIMARY + CL ----> CL  |   8.0000E-11 |   8.0000E-11 |
| TR11   | ACRO_PRIMARY + OH ----> OH  |   1.9900E-11 |   1.9900E-11 |
| TR12   | ACRO_PRIMARY + O3 ----> O3  |   1.40E-15e<sup> -2528.00/T</sup> |   2.9091E-19 |
| TR13   | ACRO_PRIMARY + NO3 ----> NO3  |   1.1800E-15 |   1.1800E-15 |
| TR14   | ACRO_PRIMARY + O3P ----> O3P  |   2.3700E-12 |   2.3700E-12 |
| TR15   | ACRO_PRIMARY ----> | ACRO_09 | Not Available<sup>1</sup> | 
| TR16   | ACRO_PRIMARY + CL ----> CL  |   2.9400E-10 |   2.9400E-10 |
| IS1   | ISOPRENE + OH ----> ISOPO2 + ISOPRXN  |   2.54E-11e<sup>   410.00/T</sup> |   1.0047E-10 |
| IS2   | ISOPO2 + NO ---->   0.4000\*MVK +    0.2600\*MACR +    0.8830\*NO2 +    0.0700\*ISOPND +    0.0470\*ISOPNB +    0.6600\*HCHO +    0.1000\*HC5 +    0.0430\*ARO2MN +    0.0800\*DIBOO +    0.8030\*HO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS3   | ISOPO2 + HO2 ---->   0.8800\*ISOPOOH +    0.1200\*OH +    0.0470\*MACR +    0.0730\*MVK +    0.1200\*HO2 +    0.1200\*HCHO  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS4   | ISOPO2 + MEO2 ---->   0.9500\*HO2 +    1.1200\*HCHO +    0.2300\*MVK +    0.1500\*MACR +    0.0500\*DIBOO +    0.0600\*HC5 +    0.0200\*ARO2MN +    0.5000\*PRD2 +    0.2500\*MEOH  |   1.8000E-12 |   1.8000E-12 |
| IS5   | ISOPO2 + RO2C ---->   0.4500\*HO2 +    0.3700\*HCHO +    0.2300\*MVK +    0.1500\*MACR +    0.0500\*DIBOO +    0.0600\*HC5 +    0.0200\*ARO2MN +    0.5000\*PRD2  |   6.8000E-13 |   6.8000E-13 |
| IS6   | ISOPO2 + ISOPO2 ---->   0.9100\*HO2 +    0.7500\*HCHO +    0.4500\*MVK +    0.2900\*MACR +    0.0900\*DIBOO +    0.1100\*HC5 +    0.0500\*ARO2MN + PRD2  |   2.3000E-12 |   2.3000E-12 |
| IS7   | ISOPO2 + MECO3 ----> MEO2 + CO2 +    0.9100\*HO2 +    0.7500\*HCHO +    0.4500\*MVK +    0.2900\*MACR +    0.0900\*DIBOO +    0.1100\*HC5 +    0.0500\*ARO2MN  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS107   | ISOPO2 ----> HO2 + HPALD  |   4.07E+08e<sup> -7694.00/T</sup> |   2.5251E-03 |
| IS137   | HPALD ----> OH + HO2 +    0.5000\*HACET +    0.5000\*MGLY +    0.2500\*HOCCHO +    0.2500\*GLY + HCHO  | HPALD | Not Available<sup>1</sup> | 
| IS138   | HPALD + OH ----> OH + PRD2  |   4.6000E-11 |   4.6000E-11 |
| IS9   | ISOPRENE + NO3 ----> NISOPO2  |   3.03E-12e<sup>  -448.00/T</sup> |   6.7433E-13 |
| IS10   | NISOPO2 + NO3 ---->   0.7000\*NIT1 +    0.0350\*MVK +    0.0350\*MACR +    1.3000\*NO2 +    0.8000\*HO2 +    0.0700\*HCHO +    0.2300\*HC5  |   2.3000E-12 |   2.3000E-12 |
| IS11   | NISOPO2 + NO ---->   0.7000\*NIT1 +    0.0350\*MVK +    0.0350\*MACR +    1.3000\*NO2 +    0.8000\*HO2 +    0.0700\*HCHO +    0.2300\*HC5  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS12   | NISOPO2 + HO2 ----> NISOPOOH  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS13   | NISOPO2 + MEO2 ---->   0.6000\*NIT1 +    0.0175\*MVK +    0.0175\*MACR +    0.1500\*NO2 +    0.9000\*HO2 +    0.7850\*HCHO +    0.1150\*HC5 +    0.2500\*ISOPND +    0.2500\*MEOH  |   1.3000E-12 |   1.3000E-12 |
| IS14   | NISOPO2 + RO2C ---->   0.6000\*NIT1 +    0.0175\*MVK +    0.0175\*MACR +    0.1500\*NO2 +    0.4000\*HO2 +    0.0350\*HCHO +    0.1150\*HC5 +    0.2500\*ISOPND  |   6.0400E-13 |   6.0400E-13 |
| IS140   | NISOPO2 + NISOPO2 ---->   1.2000\*NIT1 +    0.0350\*MVK +    0.0350\*MACR +    0.3000\*NO2 +    0.8000\*HO2 +    0.0700\*HCHO +    0.2300\*HC5 +    0.5000\*ISOPND  |   1.2000E-12 |   1.2000E-12 |
| IS15   | NISOPO2 + MECO3 ----> MEO2 + CO2 +    0.7000\*NIT1 +    0.0350\*MVK +    0.0350\*MACR +    0.3000\*NO2 +    0.8000\*HO2 +    0.0700\*HCHO +    0.2300\*HC5  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS17   | HC5 + OH ----> HC5OO  |   1.42E-11e<sup>   610.00/T</sup> |   1.0986E-10 |
| IS18   | HC5OO + NO ----> NO2 +    0.2340\*HOCCHO +    0.2340\*MGLY +    0.2160\*GLY +    0.2160\*HACET +    0.2900\*DHMOB +    0.1700\*RCOOH +    0.0900\*PRD2 +    0.0900\*CO + HO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS19   | HC5OO + HO2 ----> R6OOH  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS20   | HC5OO + MEO2 ---->   0.1170\*HOCCHO +    0.1170\*MGLY +    0.1080\*GLY +    0.1080\*HACET +    0.1450\*DHMOB +    0.0850\*RCOOH +    0.5450\*PRD2 +    0.0450\*CO + HO2 +    0.7500\*HCHO +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS21   | HC5OO + RO2C ---->   0.1170\*HOCCHO +    0.1170\*MGLY +    0.1080\*GLY +    0.1080\*HACET +    0.1450\*DHMOB +    0.0850\*RCOOH +    0.5450\*PRD2 +    0.0450\*CO +    0.5000\*HO2  |   3.5000E-14 |   3.5000E-14 |
| IS22   | HC5OO + MECO3 ----> MEO2 + CO2 +    0.2340\*HOCCHO +    0.2340\*MGLY +    0.2160\*GLY +    0.2160\*HACET +    0.2900\*DHMOB +    0.1700\*RCOOH +    0.0900\*PRD2 +    0.0900\*CO + HO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS24   | HC5 + O3 ---->   0.5000\*MGLY +    0.3500\*GLY +    0.7900\*OH +    0.0200\*HCHO +    0.3500\*HOCCHO +    0.5900\*CO +    0.1500\*HACET +    0.1300\*RCOOH +    0.0800\*CO2 +    0.6000\*HO2 +    0.3500\*MECO3  |   3.94E-15e<sup> -1520.00/T</sup> |   2.4067E-17 |
| IS25   | ISOPND + OH ----> ISOPNOOD  |   1.20E-11e<sup>   652.00/T</sup> |   1.0688E-10 |
| IS26   | ISOPNOOD + NO ---->   0.3400\*PRD2 +    0.1500\*PROPNN +    0.4400\*HACET +    0.0700\*MVKN +    0.1300\*ETHLN +    0.3100\*HCOOH +    0.3100\*NO3 +    0.7200\*HCHO +    0.1500\*HOCCHO +    1.3400\*NO2 +    0.3500\*HO2  |   2.40E-12e<sup>   360.00/T</sup> |   8.0278E-12 |
| IS141   | ISOPNOOD + HO2 ----> RNO3I  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS142   | ISOPNOOD + MEO2 ---->   0.1700\*PRD2 +    0.0750\*PROPNN +    0.2200\*HACET +    0.0350\*MVKN +    0.0650\*ETHLN +    0.1550\*HCOOH +    0.1550\*NO3 +    1.1100\*HCHO +    0.0750\*HOCCHO +    0.1700\*NO2 +    0.6750\*HO2 +    0.5000\*RNO3I +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS143   | ISOPNOOD + RO2C ---->   0.1700\*PRD2 +    0.0750\*PROPNN +    0.2200\*HACET +    0.0350\*MVKN +    0.0650\*ETHLN +    0.1550\*HCOOH +    0.1550\*NO3 +    0.3600\*HCHO +    0.0750\*HOCCHO +    0.1700\*NO2 +    0.1750\*HO2 +    0.5000\*RNO3I  |   3.5000E-14 |   3.5000E-14 |
| IS144   | ISOPNOOD + MECO3 ----> MEO2 + CO2 +    0.3400\*PRD2 +    0.1500\*PROPNN +    0.4400\*HACET +    0.0700\*MVKN +    0.1300\*ETHLN +    0.3100\*HCOOH +    0.3100\*NO3 +    0.7200\*HCHO +    0.1500\*HOCCHO +    0.3400\*NO2 +    0.3500\*HO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS27   | ISOPND + O3 ---->   0.3600\*ETHLN +    0.2900\*PROPNN +    0.7000\*MGLY +    0.1200\*RCOOH +    0.3900\*HO2 +    0.0380\*HCHO +    0.0290\*CO +    0.7300\*OH +    0.0170\*CO2 +    0.3600\*NO2 +    0.1600\*HACET +    0.3400\*HOCCHO  |   2.9000E-17 |   2.9000E-17 |
| IS28   | ISOPNB + OH ----> ISOPNOOB  |   2.40E-12e<sup>   745.00/T</sup> |   2.9201E-11 |
| IS29   | ISOPNOOB + NO ---->   0.6000\*HOCCHO +    0.6000\*HACET +    0.4000\*HCHO +    0.4000\*HO2 +    0.2600\*MACRN +    0.1400\*MVKN +    1.6000\*NO2  |   2.40E-12e<sup>   360.00/T</sup> |   8.0278E-12 |
| IS145   | ISOPNOOB + HO2 ----> RNO3I  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS146   | ISOPNOOB + MEO2 ---->   0.3000\*HOCCHO +    0.3000\*HACET +    0.9500\*HCHO +    0.7000\*HO2 +    0.1300\*MACRN +    0.0700\*MVKN +    0.3000\*NO2 +    0.5000\*RNO3I +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS147   | ISOPNOOB + RO2C ---->   0.3000\*HOCCHO +    0.3000\*HACET +    0.2000\*HCHO +    0.2000\*HO2 +    0.1300\*MACRN +    0.0700\*MVKN +    0.3000\*NO2 +    0.5000\*RNO3I  |   3.5000E-14 |   3.5000E-14 |
| IS148   | ISOPNOOB + MECO3 ----> MEO2 + CO2 +    0.6000\*HOCCHO +    0.6000\*HACET +    0.4000\*HCHO +    0.4000\*HO2 +    0.2600\*MACRN +    0.1400\*MVKN +    0.6000\*NO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS30   | ISOPNB + O3 ---->   0.1200\*MVKN +    0.3200\*MACRN +    0.3400\*OH +    0.0800\*HO2 +    0.2600\*CO +    0.0700\*CO2 +    0.1600\*HCOOH +    0.5600\*HCHO +    0.2800\*RNO3I +    0.0400\*HACET +    0.2800\*NO2 +    0.2400\*BACL  |   3.7000E-19 |   3.7000E-19 |
| IS31   | NIT1 + NO3 ---->   0.6000\*NIT1NO3OOA +    0.6000\*HNO3 +    0.4000\*NIT1NO3OOB  |   3.15E-13e<sup>  -448.00/T</sup> |   7.0104E-14 |
| IS32   | NIT1NO3OOA + NO3 ----> NO2 + PROPNN + CO + CO2 + HO2  |   4.0000E-12 |   4.0000E-12 |
| IS34   | NIT1NO3OOA + NO ----> NO2 + PROPNN + CO + CO2 + HO2  |   BR31 |   2.0957E-11<sup>7</sup>| 
| IS109   | NIT1NO3OOA + NO2 ----> MAPAN  |   BR28 |   1.2180E-11<sup>7</sup>| 
| IS36   | NIT1NO3OOA + HO2 ---->   0.7500\*RCOOOH +    0.2500\*RCOOH +    0.2500\*O3  |   BR22 |   1.3916E-11<sup>7</sup>| 
| IS38   | NIT1NO3OOA + RO2C ----> PROPNN + CO + CO2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IS40   | NIT1NO3OOA + MEO2 ----> PROPNN + CO + CO2 +    2.0000\*HO2 + HCHO  |   BR24 |   1.0699E-11<sup>7</sup>| 
| IS41   | NIT1NO3OOA + MECO3 ----> MEO2 +    2.0000\*CO2 + PROPNN + CO + HO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IS33   | NIT1NO3OOB + NO3 ----> ISOPNN + GLY + NO2  |   2.3000E-12 |   2.3000E-12 |
| IS35   | NIT1NO3OOB + NO ---->   0.9400\*ISOPNN +    0.9400\*GLY +    0.9400\*NO2 +    0.0600\*RNO3I  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS37   | NIT1NO3OOB + HO2 ----> RNO3I  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS39   | NIT1NO3OOB + RO2C ---->   0.7000\*ISOPNN +    0.7000\*GLY +    0.3000\*RNO3I  |   3.5000E-14 |   3.5000E-14 |
| IS43   | NIT1NO3OOB + MEO2 ---->   0.7000\*ISOPNN +    0.7000\*GLY +    0.3000\*RNO3I +    0.7500\*HCHO +    0.2500\*MEOH +    0.5000\*HO2  |   2.0000E-13 |   2.0000E-13 |
| IS44   | NIT1NO3OOB + MECO3 ----> MEO2 + CO2 + ISOPNN + GLY  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS46   | NIT1 + O3 ---->   0.3000\*PROPNN +    0.4500\*CO +    0.8500\*OH +    0.4500\*HO2 +    0.1500\*CO2 +    0.7000\*GLY +    0.7000\*NO2 +    0.7000\*MGLY  |   4.15E-15e<sup> -1520.00/T</sup> |   2.5349E-17 |
| IS47   | NIT1 + OH ---->   0.3450\*NIT1NO3OOA +    0.6550\*NIT1OHOO  |   7.48E-12e<sup>   410.00/T</sup> |   2.9588E-11 |
| IS48   | NIT1OHOO + NO ---->   0.9190\*PROPNN +    0.9190\*GLY +    0.0150\*CO +    0.0810\*RNO3I +    0.9340\*NO2 +    0.9340\*HO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS50   | NIT1OHOO + HO2 ----> R6OOH  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS51   | NIT1OHOO + RO2C ---->   0.6890\*PROPNN +    0.6890\*GLY +    0.0110\*CO +    0.3110\*RNO3I +    0.7000\*HO2  |   3.5000E-14 |   3.5000E-14 |
| IS52   | NIT1OHOO + MEO2 ---->   0.6890\*PROPNN +    0.6890\*GLY +    0.0110\*CO +    0.3110\*RNO3I +    1.2000\*HO2 +    0.7500\*HCHO +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS53   | NIT1OHOO + MECO3 ----> MEO2 + CO2 +    0.9840\*PROPNN +    0.9840\*GLY +    0.0160\*CO +    0.0160\*RNO3I + HO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS55   | DIBOO + NO ----> NO2 + HO2 +    0.5200\*HOCCHO +    0.5200\*MGLY +    0.4800\*GLY +    0.4800\*HACET  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS102   | DIBOO + HO2 ----> R6OOH  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS103   | DIBOO + MEO2 ----> HO2 +    0.2600\*HOCCHO +    0.2600\*MGLY +    0.2400\*GLY +    0.2400\*HACET +    0.5000\*PRD2 +    0.7500\*HCHO +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS104   | DIBOO + RO2C ---->   0.5000\*HO2 +    0.2600\*HOCCHO +    0.2600\*MGLY +    0.2400\*GLY +    0.2400\*HACET +    0.5000\*PRD2  |   3.5000E-14 |   3.5000E-14 |
| IS105   | DIBOO + MECO3 ----> HO2 +    0.5200\*HOCCHO +    0.5200\*MGLY +    0.4800\*GLY +    0.4800\*HACET + MEO2 + CO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS56   | MVK + OH ----> MVKOO  |   2.60E-12e<sup>   610.00/T</sup> |   2.0115E-11 |
| IS57   | MVKOO + NO ---->   0.6250\*HOCCHO +    0.6250\*MECO3 +    0.2650\*MGLY +    0.2650\*HCHO +    0.2650\*HO2 +    0.1100\*MVKN +    0.8900\*NO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS58   | MVKOO + HO2 ----> ROOH  |   1.82E-13e<sup>  1300.00/T</sup> |   1.4246E-11 |
| IS59   | MVKOO + MEO2 ---->   0.3500\*HOCCHO +    0.3500\*MECO3 +    0.1500\*MGLY +    0.9000\*HCHO +    0.6500\*HO2 +    0.5000\*MEK +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS60   | MVKOO + RO2C ---->   0.3500\*HOCCHO +    0.3500\*MECO3 +    0.1500\*MGLY +    0.1500\*HCHO +    0.1500\*HO2 +    0.5000\*MEK  |   3.5000E-14 |   3.5000E-14 |
| IS61   | MVKOO + MECO3 ----> MEO2 + CO2 +    0.7000\*HOCCHO +    0.7000\*MECO3 +    0.3000\*MGLY +    0.3000\*HCHO +    0.3000\*HO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS63   | MACROO + NO ---->   0.8500\*NO2 +    0.8500\*HO2 +    0.7200\*HACET +    0.7200\*CO +    0.1300\*HCHO +    0.1300\*MGLY +    0.1500\*MACRN  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS64   | MACROO + HO2 ----> ROOH  |   1.82E-13e<sup>  1300.00/T</sup> |   1.4246E-11 |
| IS65   | MACROO + MEO2 ----> HO2 +    0.4240\*HACET +    0.4240\*CO +    0.8260\*HCHO +    0.0760\*MGLY +    0.5000\*PRD2 +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS66   | MACROO + RO2C ---->   0.5000\*HO2 +    0.4240\*HACET +    0.4240\*CO +    0.0760\*HCHO +    0.0760\*MGLY +    0.5000\*PRD2  |   3.5000E-14 |   3.5000E-14 |
| IS67   | MACROO + MECO3 ----> MEO2 + CO2 + HO2 +    0.1500\*MGLY +    0.8500\*HACET +    0.8500\*CO +    0.1500\*HCHO  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS69   | MACO3 + NO ----> NO2 + CO + CO2 + HCHO + MEO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| IS70   | MACO3 + HO2 ---->   0.3075\*RCOOOH +    0.1025\*RCOOH +    0.1500\*O3 +    0.4400\*OH +    0.4400\*HCHO +    0.4400\*MECO3 +    0.4400\*CO2  |   BR22 |   1.3916E-11<sup>7</sup>| 
| IS71   | MACO3 + NO3 ----> NO2 + CO + CO2 + HCHO + MEO2  |   4.0000E-12 |   4.0000E-12 |
| IS72   | MACO3 + MEO2 ---->   2.0000\*HCHO + HO2 + CO + CO2 + MEO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| IS73   | MACO3 + RO2C ----> CO + CO2 + HCHO + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IS74   | MACO3 + RO2XC ----> CO + CO2 + HCHO + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IS75   | MACO3 + MECO3 ---->   2.0000\*CO2 +    2.0000\*MEO2 + CO + HCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IS76   | MACO3 + RCO3 ----> CO +    2.0000\*CO2 + HCHO + MEO2 + RO2C + xHO2 + yROOH + xCCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IS77   | MACO3 + BZCO3 ----> CO +    2.0000\*CO2 + HCHO + MEO2 + BZO + RO2C  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IS78   | MACO3 + MACO3 ---->   2.0000\*CO +    2.0000\*CO2 +    2.0000\*HCHO +    2.0000\*MEO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IS108   | MAPAN + OH ----> HACET + CO + NO2  |   2.9000E-11 |   2.9000E-11 |
| IS79   | HOCCHO + OH ---->   0.7500\*HO2 +    0.2500\*OH +    0.1300\*GLY +    0.5200\*CO +    0.3500\*CO2 +    0.1600\*HCOOH +    0.7100\*HCHO  |   8.0000E-12 |   8.0000E-12 |
| IS80   | HACET + OH ---->   0.7500\*MGLY +    0.8250\*HO2 +    0.1250\*HCOOH +    0.1000\*OH +    0.1250\*MEO2 +    0.2000\*CO2 +    0.0500\*CO +    0.1250\*CCOOH  |   2.15E-12e<sup>   305.00/T</sup> |   5.9801E-12 |
| IS81   | HACET ----> HO2 + MECO3 + HCHO  |   1.7500E-01\*MEK_06 | Not Available<sup>1</sup> | 
| IS82   | ETHLN + OH ----> HCHO + CO2 + NO2  |   2.94E-12e<sup>   365.00/T</sup> |   1.0000E-11 |
| IS111   | ETHLN ----> NO2 + HCHO + HO2 + CO  | NOA | Not Available<sup>1</sup> | 
| IS83   | PROPNN + OH ----> MGLY + NO2  |   4.0000E-13 |   4.0000E-13 |
| IS93   | ISOPNN + OH ----> PROPNN + NO2  |   4.0000E-13 |   4.0000E-13 |
| IS97   | PROPNN ----> MECO3 + HCHO + NO2  | NOA | Not Available<sup>1</sup> | 
| IS98   | ISOPNN ----> MECO3 + HCHO +    2.0000\*NO2  | IC3ONO2 | Not Available<sup>1</sup> | 
| IS84   | MVKN + OH ---->   0.6500\*HCOOH +    0.6500\*MGLY +    0.3500\*HCHO +    0.3500\*PYRUACD + NO3  |   3.50E-12e<sup>   140.00/T</sup> |   5.5975E-12 |
| IS106   | MVKN ----> MECO3 + NO2 + HOCCHO  | NOA | Not Available<sup>1</sup> | 
| IS85   | MACRN + OH ---->   0.0800\*CCOOH +    0.0800\*HCHO +    0.1500\*NO3 +    0.0700\*HCOOH +    0.0700\*MGLY +    0.8500\*HACET +    0.8500\*NO2 +    0.9300\*CO2  |   1.28E-11e<sup>   405.00/T</sup> |   4.9790E-11 |
| IS110   | MACRN ----> HACET + NO2 + CO + HO2  | C2CHO | Not Available<sup>1</sup> | 
| IS86   | DHMOB + OH ---->   1.5000\*CO +    0.5000\*HO2 +    0.5000\*HACET +    0.5000\*PRD2  |   1.0000E-11 |   1.0000E-11 |
| IS87   | PYRUACD ----> CCHO + CO2  | MGLY_06 | Not Available<sup>1</sup> | 
| IS88   | ISOPOOH + OH ----> IEPOX + OH  |   1.90E-11e<sup>   390.00/T</sup> |   7.0281E-11 |
| IS89   | ISOPOOH + OH ---->   0.3870\*ISOPO2 +    0.6130\*HC5 +    0.6130\*OH  |   4.75E-12e<sup>   200.00/T</sup> |   9.2901E-12 |
| IS90   | IEPOX + OH ----> IEPOXOO  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| IS91   | IEPOXOO + HO2 ---->   0.7250\*HACET +    0.2750\*HOCCHO +    0.2750\*GLY +    0.2750\*MGLY +    1.1250\*OH +    0.8250\*HO2 +    0.2000\*CO2 +    0.3750\*HCHO +    0.0740\*HCOOH +    0.2510\*CO  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS96   | IEPOXOO + NO ---->   0.7250\*HACET +    0.2750\*HOCCHO +    0.2750\*GLY +    0.2750\*MGLY +    0.1250\*OH +    0.8250\*HO2 +    0.2000\*CO2 +    0.3750\*HCHO +    0.0740\*HCOOH +    0.2510\*CO + NO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS112   | IEPOXOO + MEO2 ---->   0.3630\*HACET +    0.1380\*HOCCHO +    0.1380\*GLY +    0.1380\*MGLY +    0.0630\*OH +    0.9130\*HO2 +    0.1000\*CO2 +    0.9380\*HCHO +    0.0370\*HCOOH +    0.1260\*CO +    0.5000\*PRD2 +    0.2500\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS113   | IEPOXOO + RO2C ---->   0.3630\*HACET +    0.1380\*HOCCHO +    0.1380\*GLY +    0.1380\*MGLY +    0.0630\*OH +    0.4130\*HO2 +    0.1000\*CO2 +    0.1880\*HCHO +    0.0370\*HCOOH +    0.1260\*CO +    0.5000\*PRD2  |   3.5000E-14 |   3.5000E-14 |
| IS114   | IEPOXOO + MECO3 ---->   0.7250\*HACET +    0.2750\*HOCCHO +    0.2750\*GLY +    0.2750\*MGLY +    0.1250\*OH +    0.8250\*HO2 +    1.2000\*CO2 +    0.3750\*HCHO +    0.0740\*HCOOH +    0.2510\*CO + MEO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| IS92   | ISOPOOH ----> OH +    0.9100\*HO2 +    0.7500\*HCHO +    0.4500\*MVK +    0.2900\*MACR +    0.0900\*DIBOO +    0.1100\*HC5 +    0.0500\*ARO2MN  | COOH | Not Available<sup>1</sup> | 
| IS94   | RNO3I + OH ----> NO2 + HO2 + PRD2  |   8.0000E-12 |   8.0000E-12 |
| IS99   | NISOPOOH + OH ----> RNO3I + OH  |   5.0000E-11 |   5.0000E-11 |
| IS139   | NISOPOOH + OH ---->   0.3000\*NISOPO2 +    0.7000\*OH +    0.7000\*NIT1  |   3.80E-12e<sup>   200.00/T</sup> |   7.4321E-12 |
| IS00   | MACR + OH ---->   0.5300\*MACROO +    0.4700\*IMACO3  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| BP56   | MACR + NO3 ---->   0.5000\*IMACO3 +    0.5000\*RO2C +    0.5000\*HNO3 +    0.5000\*xHO2 +    0.5000\*xCO +    0.5000\*yROOH  |   1.50E-12e<sup> -1815.00/T</sup> |   3.4065E-15 |
| BP58   | MACR ---->   0.3300\*OH +    0.6700\*HO2 +    0.3400\*MECO3 +    0.3300\*IMACO3 +    0.3300\*RO2C +    0.6700\*CO +    0.3400\*HCHO +    0.3300\*xMECO3 +    0.3300\*xHCHO +    0.3300\*yROOH  | MACR_06 | Not Available<sup>1</sup> | 
| CP16   | MACR + CL ---->   0.2500\*HCL +    0.1650\*IMACO3 +    0.8020\*RO2C +    0.0330\*RO2XC +    0.0330\*zRNO3 +    0.8020\*xHO2 +    0.5410\*xCO +    0.0820\*xIPRD +    0.1800\*xCLCCHO +    0.5410\*xCLACET +    0.8350\*yROOH  |   3.8500E-10 |   3.8500E-10 |
| IA69   | IMACO3 + NO ----> NO2 + CO + CO2 + HCHO + MEO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| IA70   | IMACO3 + HO2 ---->   0.7500\*IMPAA +    0.2500\*RCOOH +    0.2500\*O3  |   BR22 |   1.3916E-11<sup>7</sup>| 
| IA71   | IMACO3 + NO3 ----> NO2 + CO + CO2 + HCHO + MEO2  |   4.0000E-12 |   4.0000E-12 |
| IA72   | IMACO3 + MEO2 ---->   2.0000\*HCHO + HO2 + CO + CO2 + MEO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| IA73   | IMACO3 + RO2C ----> CO + CO2 + HCHO + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IA74   | IMACO3 + RO2XC ----> CO + CO2 + HCHO + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IA75   | IMACO3 + MECO3 ---->   2.0000\*CO2 +    2.0000\*MEO2 + CO + HCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IA76   | IMACO3 + RCO3 ----> CO +    2.0000\*CO2 + HCHO + MEO2 + RO2C + xHO2 + yROOH + xCCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IA77   | IMACO3 + BZCO3 ----> CO +    2.0000\*CO2 + HCHO + MEO2 + BZO + RO2C  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IA78   | IMACO3 + MACO3 ---->   2.0000\*CO +    2.0000\*CO2 +    2.0000\*HCHO +    2.0000\*MEO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IA79   | IMACO3 + IMACO3 ---->   2.0000\*CO +    2.0000\*CO2 +    2.0000\*HCHO +    2.0000\*MEO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| IA80   | MACROO ----> HACET + CO + OH  |   2.90E+07e<sup> -5297.00/T</sup> |   5.5799E-01 |
| IA51   | IMACO3 + NO2 ----> IMAPAN  |   BR28 |   1.2180E-11<sup>7</sup>| 
| IA52   | IMAPAN ----> IMACO3 + NO2  |   1.60E+16e<sup>-13486.00/T</sup> |   3.6308E-04 |
| IA53   | IMAPAN ---->   0.6000\*IMACO3 +    0.6000\*NO2 +    0.4000\*CO2 +    0.4000\*HCHO +    0.4000\*MECO3 +    0.4000\*NO3  | PAN | Not Available<sup>1</sup> | 
| IC01   | xCO + IMACO3 ----> IMACO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC02   | xTBUO + IMACO3 ----> IMACO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC03   | xMACO3 + IMACO3 ----> IMACO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC04   | xRCO3 + IMACO3 ----> IMACO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC05   | xMECO3 + IMACO3 ----> IMACO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC06   | xMEO2 + IMACO3 ----> IMACO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC07   | xNO2 + IMACO3 ----> IMACO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC08   | xOH + IMACO3 ----> IMACO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC09   | xHO2 + IMACO3 ----> IMACO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC10   | xACROLEIN + IMACO3 ----> IMACO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC11   | xHOCCHO + IMACO3 ----> IMACO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC12   | zRNO3 + IMACO3 ----> IMACO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC13   | yRAOOH + IMACO3 ----> IMACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC14   | yR6OOH + IMACO3 ----> IMACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC15   | yROOH + IMACO3 ----> IMACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC16   | xRNO3 + IMACO3 ----> IMACO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC17   | xIPRD + IMACO3 ----> IMACO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC18   | xMVK + IMACO3 ----> IMACO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC19   | xMACR + IMACO3 ----> IMACO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC20   | xAFG3 + IMACO3 ----> IMACO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC21   | xAFG2 + IMACO3 ----> IMACO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC22   | xAFG1 + IMACO3 ----> IMACO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC23   | xBALD + IMACO3 ----> IMACO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC24   | xBACL + IMACO3 ----> IMACO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC25   | xMGLY + IMACO3 ----> IMACO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC26   | xGLY + IMACO3 ----> IMACO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC27   | xPROD2 + IMACO3 ----> IMACO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC28   | xMEK + IMACO3 ----> IMACO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC29   | xACETONE + IMACO3 ----> IMACO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC30   | xRCHO + IMACO3 ----> IMACO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC31   | xCCHO + IMACO3 ----> IMACO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC32   | xHCHO + IMACO3 ----> IMACO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC33   | xCL + IMACO3 ----> IMACO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC34   | xCLACET + IMACO3 ----> IMACO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IC35   | xCLCCHO + IMACO3 ----> IMACO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| IA108   | IMAPAN + OH ---->   0.0300\*HACET +    0.0300\*CO +    0.8100\*NO3 +    0.2100\*IMAE +    0.5700\*IHMML +    0.1900\*PAN +    0.1900\*HCHO +    0.1900\*HO2  |   3.0000E-11 |   3.0000E-11 |
| IA90   | IMAE + OH ----> |   1.0000E-12 |   1.0000E-12 |
| IA91   | IHMML + OH ----> |   4.4000E-12 |   4.4000E-12 |
| IA92   | IMPAA + OH ---->   0.8300\*IMACO3 +    0.1700\*IHMML  |   1.6600E-11 |   1.6600E-11 |
| CP07mtp   | MTNO3 + CL ----> HCL +    0.0380\*NO2 +    0.0550\*HO2 +    1.2820\*RO2C +    0.2020\*RO2XC +    0.2020\*zMTNO3 +    0.0090\*RCHO +    0.0180\*MEK +    0.0120\*PRD2 +    0.0550\*MTNO3 +    0.1590\*xNO2 +    0.5470\*xHO2 +    0.0450\*xHCHO +    0.3000\*xCCHO +    0.0200\*xRCHO +    0.0030\*xACETONE +    0.0410\*xMEK +    0.0460\*xPROD2 +    0.5470\*xMTNO3 +    0.9080\*yR6OOH  |   1.9200E-10 |   1.9200E-10 |
| BP70mtp   | MTNO3 + OH ---->   0.1890\*HO2 +    0.3050\*xHO2 +    0.0190\*NO2 +    0.3130\*xNO2 +    0.9760\*RO2C +    0.1750\*RO2XC +    0.1750\*zMTNO3 +    0.0110\*xHCHO +    0.4290\*xCCHO +    0.0010\*RCHO +    0.0360\*xRCHO +    0.0040\*xACETONE +    0.0100\*MEK +    0.1700\*xMEK +    0.0080\*PRD2 +    0.0310\*xPROD2 +    0.1890\*MTNO3 +    0.3050\*xMTNO3 +    0.1570\*yROOH +    0.6360\*yR6OOH  |   7.2000E-12 |   7.2000E-12 |
| BP71mtp   | MTNO3 ---->   0.3440\*HO2 +    0.5540\*xHO2 + NO2 +    0.7210\*RO2C +    0.1020\*RO2XC +    0.1020\*zMTNO3 +    0.0740\*HCHO +    0.0610\*xHCHO +    0.2140\*CCHO +    0.2300\*xCCHO +    0.0740\*RCHO +    0.0630\*xRCHO +    0.0080\*xACETONE +    0.1240\*MEK +    0.0830\*xMEK +    0.1900\*PRD2 +    0.2610\*xPROD2 +    0.0660\*yROOH +    0.5910\*yR6OOH  | IC3ONO2 | Not Available<sup>1</sup> | 
| HET_N02   | NO2 ---->   0.5000\*HONO +    0.5000\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HET_N2O5IJ   | N2O5 ----> HNO3 + H2NO3PIJ  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N2O5K   | N2O5 ----> HNO3 + H2NO3PK  | HETERO_N2O5K | Not Available<sup>2</sup> | 
| HET_H2NO3PIJA   | H2NO3PIJ ----> HNO3  | HETERO_H2NO3PAIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKA   | H2NO3PK ----> HNO3  | HETERO_H2NO3PAK | Not Available<sup>2</sup> | 
| HET_H2NO3PIB   | H2NO3PIJ + ACLI ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PJB   | H2NO3PIJ + ACLJ ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKB   | H2NO3PK + ACLK ----> CLNO2  | HETERO_H2NO3PBK | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_IEPOX   | IEPOX ----> IEPOXP  | HETERO_IEPOX | Not Available<sup>2</sup> | 
| HET_IMAE   | IMAE ----> IMAEP  | HETERO_IMAE | Not Available<sup>2</sup> | 
| HET_IHMML   | IHMML ----> IHMMLP  | HETERO_IMAE | Not Available<sup>2</sup> | 
| HET_TETROL   | IEPOXP ----> AIETETJ  | HETERO_TETROL | Not Available<sup>2</sup> | 
| HET_IEPOXOS   | IEPOXP + ASO4J ----> AIEOSJ  | HETERO_IEPOXOS | Not Available<sup>2</sup> | 
| HET_DIM1   | IEPOXP + AIETETJ ----> ADIMJ  | HETERO_TETROLDIM | Not Available<sup>2</sup> | 
| HET_DIM2   | IEPOXP + AIEOSJ ----> ADIMJ  | HETERO_IEPOXOSDI | Not Available<sup>2</sup> | 
| HET_2MG1   | IMAEP ----> AIMGAJ  | HETERO_2MG | Not Available<sup>2</sup> | 
| HET_IMAEOS1   | IMAEP + ASO4J ----> AIMOSJ  | HETERO_IMAEOS | Not Available<sup>2</sup> | 
| HET_2MG2   | IHMMLP ----> AIMGAJ  | HETERO_2MG | Not Available<sup>2</sup> | 
| HET_IMAEOS2   | IHMMLP + ASO4J ----> AIMOSJ  | HETERO_IMAEOS | Not Available<sup>2</sup> | 
| HET_NO3   | NO3 ----> HNO3  | HETERO_NO3 | Not Available<sup>2</sup> | 
| OLIG_ISOPRENE1   | AISO1J ---->   0.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE2   | AISO2J ---->   0.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_SESQT1   | ASQTJ ---->   1.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC1   | AAVB2J ---->   0.9070\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC2   | AAVB3J ---->   0.9250\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC3   | AAVB4J ---->   0.9430\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| RPOAGEPI   | APOCI + OH ---->   1.2500\*APNCOMI + APOCI + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELI   | APNCOMI + OH ----> OH  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| RPOAGEPJ   | APOCJ + OH ---->   1.2500\*APNCOMJ + APOCJ + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELJ   | APNCOMJ + OH ----> OH  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| PCSOA   | PCVOC + OH ----> OH + PCSOARXN  |   1.2500E-11 |   1.2500E-11 |
| POA_AGE1   | VLVPO1 + OH ----> OH +    0.4857\*VLVPO1 +    0.0062\*VSVPO1 +    0.0025\*VSVPO2 +    0.0026\*VSVPO3 +    0.0023\*VIVPO1 +    0.2944\*VLVOO1 +    0.2021\*VLVOO2 +    0.0019\*VSVOO2 +    0.0023\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE2   | VSVPO1 + OH ----> OH +    0.3003\*VLVPO1 +    0.2862\*VSVPO1 +    0.0041\*VSVPO2 +    0.0035\*VSVPO3 +    0.2239\*VLVOO1 +    0.1820\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE3   | VSVPO2 + OH ----> OH +    0.3856\*VLVPO1 +    0.0950\*VSVPO1 +    0.1373\*VSVPO2 +    0.0005\*VSVPO3 +    0.2051\*VLVOO1 +    0.1764\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE4   | VSVPO3 + OH ----> OH +    0.2181\*VLVPO1 +    0.3063\*VSVPO1 +    0.0153\*VSVPO2 +    0.1043\*VSVPO3 +    0.1893\*VLVOO1 +    0.1668\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE5   | VIVPO1 + OH ----> OH +    0.2412\*VLVPO1 +    0.2089\*VSVPO1 +    0.3000\*VSVPO2 +    0.2028\*VLVOO1 +    0.0471\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE6   | VLVOO1 + OH ----> OH +    0.6664\*VLVOO1 +    0.0143\*VLVOO2 +    0.0123\*VSVOO1 +    0.1239\*VSVOO2 +    0.1831\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE7   | VLVOO2 + OH ----> OH +    0.2858\*VLVOO1 +    0.3931\*VLVOO2 +    0.0139\*VSVOO1 +    0.1027\*VSVOO2 +    0.2045\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE8   | VSVOO1 + OH ----> OH +    0.3303\*VLVOO1 +    0.2272\*VLVOO2 +    0.2607\*VSVOO1 +    0.0702\*VSVOO2 +    0.1116\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE9   | VSVOO2 + OH ----> OH +    0.3444\*VLVOO1 +    0.2749\*VLVOO2 +    0.0491\*VSVOO1 +    0.2577\*VSVOO2 +    0.0739\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE10   | VSVOO3 + OH ----> OH +    0.3886\*VLVOO1 +    0.2421\*VLVOO2 +    0.0640\*VSVOO1 +    0.0385\*VSVOO2 +    0.2667\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| HYD_MT   | AMTNO3J ----> HNO3 + AMTHYDJ  |   9.2590E-05 |   9.2590E-05 |
| HYD_ISOP   | AISOPNNJ ---->   2.0000\*HNO3 +    0.5000\*AMTHYDJ  |   9.2590E-05 |   9.2590E-05 |
| HET_GLY   | GLY ----> AGLYJ  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| HET_MGLY   | MGLY ----> AGLYJ  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molecules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heterogeneous Reaction; depends on predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals fraction of ocean plus surf zones not covered by seaice. P equals air pressure in atmospheres.         
<sup>7</sup>Rate constant multiple of constant for listed reaction   
