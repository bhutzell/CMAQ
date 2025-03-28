<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixE_configuring_WRF.md) - [Home](../README.md) 

<!-- END COMMENT -->

* * *

# Appendix F: Explicit and Lumped cmaq Model Output version 2 (ELMOv2)
The ELMO module makes both raw and highly aggregated predictive data directly available in CMAQ output files rather than requiring follow-up post-processing steps. For example, users may now output NOx, VOC, and PM2.5 mass directly from CMAQ!  
Users specify output file configurations via the CMAQ control namelist (CMAQ_Control.nml). This namelist also contains definitions of ELMO keywords that can be used to simplify output file specification. 
Finally, the chemical control file (CMAQ_Chem_Control_${mech}.nml) contains the definitions of ELMO composite variables that are defined as linear combinations of existing variables. 
These components are described in detail below.

**Why use ELMOv2?**
There are several distinct advantages to using ELMO instead of post-processing CONC and ACONC output with COMBINE:

- Definitions of products like PM25 mass, PMF (Fine PM mass), and PM10 mass automatically adjust as PM species are activated or deactivated by CMAQ users or chemical mechanisms are switched. There is no need to modify a SpecDef file to account for a new or eliminated species.  
   - Note that some of the diagnostic species defined in FINE_ORG, specifically those meant to provide an approximate distinction between primary and secondary or anthropogenic and biogenic organic aerosols, should generally only be used in a qualitative manner. 
A number of emitted compounds forming organic aerosols in the atmosphere can have both anthropogenic and biogenic sources so a quantitative attribution of organic aerosols to specific sources cannot be based on an analysis of concentrations alone and should use ISAM instead. 
Moreover, their interpretation may not be consistent across mechanisms (e.g. cb6r5_aero7 vs. cracmm1) depending on the assumptions made during emissions processing and the mapping of emitted species to CMAQ mechanism species in DESID.

- Complex properties like O:C, OM:OC, particle acidity, etc. can be calculated using species properties available within CMAQ. 
This resolves a potential vulnerability where, for example, the OM:OC of organic species may become out of sync between the SpecDef and the SOA_DEFN table within the model. This could have potentially led to errors in the calculation of OC (organic carbon). With ELMO, there is no such risk.  

- If a user is only interested in aggregate parameters like PM25 mass, they can avoid the I/O time and storage required saving the raw output of every PM variable and then post-processing with COMBINE. This can be particularly helpful when processing 3D data.

- New parameters are available that were not before like N10, N20, N40 and N100, the number of particles above 10, 20, 40 and 100 nm in diameter. AOD and extinction at 550 nm have also been supported as options; these were previously only available on the photolysis diagnostic file.  

- Keywords are available (see section F.4) to select groups of variables of interest. 

- Variables may be added to the ELMO_LIST table in ELMO_DATA.F and then prescribed in ELMO_PROC.F with greater ease.
 
ELMO has no significant quantitative impact on results, but there will be a noticeable advantageous impact on the time invested in post-processing aggregate PM components and storage volumes required for standard runs. 
There can be some slight numerical differences when one compares quantities averaged directly online with ELMO vs. calculated offline using averages. 
For example, total PM2.5 have some small deviations when it is calculated as the hourly average of the sum of species (online) versus the sum of hourly averaged species (offline).  

### F.1 Output File Specification
The interface for prescribing ELMO file properties is located in the [CMAQ Control File](../CMAQ_UG_ch04_model_inputs.md#miscctrl). 
Several variables prescribe how the ELMO output file parameters will be processed. 
```
&ELMO_INIT
  N_Files = 2
  N_Max_Output_Variables = 400
  N_Keywords = 71
  N_Max_Keywords_variables = 150
/
```
The variable N_Files must equal exactly the numbe rof ELMO files you wish. N_Max_Output Variables provides a limit on the total number of variables on any one file. 
N_Keywords must match exactly the number of Keyword variables below. Finally, N_Max_Keywords_Variables should be greater than the ;argest number of components for any one keyword below.

```
&ELMO_Files
  Flabel(1)= 'CONC'
  Tmode(1) = 'instantaneous'
  Lay_Bot(1) = 1
  Lay_Top(1) = 35
  File_Vars(1,:) = 'O3','NO','SO2','NH3','AECI','ASO4J','ANO3K'

  Flabel(2)= 'DEP'
  Tmode(2) = 'aggregate'
  Lay_Bot(2) = 1
  Lay_Top(2) = 1
  File_Vars(2,:) = 'DD_O3','WD_O3','DD_NH3','WD_NH3','DD_SO2','WD_SO2','CO','TA','ACLK'
/
```
Two files are requested for output. File1 is labeled CONC, contains concentration data for 7 CMAQ species, reports those data for instantaneous values at each output time step, and provides 35 model layers. 
The first 4 variables are all gas species. Variable 5 is elemental carbon in the Aitken mode (I), variable 6 is sulfate in the Accumulation mode (J), and variable 7 is nitrate in the coarse mode (K). 
The value of Flabel is completely arbitrary and is appended to the prefix CCTM_ELMO[N]_ where N is the number of the file, 1 in this case. File2, which will be labeled CCTM_ELMO2_DEP_ contains 
6 variables quantifying the deposition of 3 species via 2 pathways (wet deposition, WD; and dry deposition; DD). Because 'aggregate' is selected, these data are summed across the 
output time step (i.e. cumulative sum). Three other species are averaged rather than accumulated: CO for carbon monoxide concentrations, TA for air temperature, and ACLK for chlorine in 
the coarse mode. 
Only one layer is selected. If more layers are selected, CMAQ will put zeros in these cells for deposition variables.  

It is recommended to output all model layers if you are outputting variables for comparison to satellite column data like NO2. Aerosol Optical Depth (AOD_550) may be output as a surface (i.e. just layer 1) or for multiple layers (layer-dependent extinction multiplied by layer thickness). 
When 2D variables are output on a 3D ELMO file, ELMO will put real data in layer 1, and I/O-API missing values above layer 1. 

There are thousands of variables that can be requested in the File_Vars field.


### F.2 Output Variable Types

#### F.2.1 CMAQ Species
ELMO has full capability of outputting all 'raw' CMAQ species for concentration, dry deposition, and wet deposition. If the name of any CMAQ species is provided, it's concentration 
will be output in ppm for gases and ug m-3 for aerosols. If DD_ is prepended, then dry deposition in kg ha-1 is output. If WD_ is prepended, wet deposition in kg ha-1 is output. 

#### F.2.2 ELMO Composites
The Chemical Control Namelist (CMAQ_Chem_Control_${mech}.nml) provides an interface for defining ELMO composite variables as linear combinations of CMAQ species or other ELMO composites. 
The chemical control files provided in the CMAQ repository already contain hundreds of examples. Once an ELMO composite is defined, it may be used in File_Vars to be added to an ELMO 
output file. If it is not used there, it is ignored.

Here is an example for NOx:
```
'NOX'         , 'NOx Concentration',
                'ppmV', 'GAS',
                'NO + NO2',
```
Each definition includes 5 comma-separated fields. The first field is the short-name name of the composite; we recommend keeping these to 10 characters or less. The second field is 
a long description of the composite, and the third field specify the units. Some unit conversions are supported (e.g. ppmV to ug m-3) and are discussed below, but it is recommended that complex unit 
conversions be done by the user offline. The fourth field provides information about the particle sizes corresponding to the composite. If no particle species are included in the 
composite, then the 4th field may read 'GAS'. The 5th field specifies the calculation of the composite, which can include CMAQ species concentrations or deposition, as well as any
ELMO composites that have already been defined.  

Another example specifies how to calculate fine-mode sulfate particle mass:
```
'PMF_SO4'     , 'Fine-Mode Sulfate',
                'ug m-3',  'FINE',
                'ASO4',
```
In this example, the CMAQ species ASO4 points to the CMAQ sulfate chemical species. The 4th field indicates this composite should be summed for FINE particle mass, which CMAQ translates 
internally to the sum of the Aitken and Accumulation modes. The name PMF_SO4 denotes fine-mode PM sulfate. Table F-1 shows the possible values for the 4th field denoting particle size.  

**Table F-1. Definition of values that may be used for phase/size (Field 4) in the ELMO Composite Interface**

|**Phase/Size**  |**Meaning**|
|----------------|----------------------------------|
| **PM01**       | Mass of particles with diameter less than 0.1 um |
| **PM1**        | particles with diameter less than 1.0 um |
| **PM25**       | particles with diameter less than 2.5 um |
| **PM10**       | particles with diameter less than 10.0 um |
| **PM25TO10**   | particles with diameter between 2.5 and 10.0 um |
| **AMS**        | particles predicted to be detected by an aerosol mass spectrometer |
| **INUM10**     | particles with diameter greater than 10 nm |
| **INUM20**     | particles with diameter greater than 20 nm | |
| **INUM40**     | particles with diameter greater than 40 nm |
| **INUM100**    | particles with diameter greater than 100 nm |
| **GAS**        | No particle number, mass, or surface area concentrations used in COMPOSITE definition |

Although these labels indicate mass or number (e.g. PM25 vs. INUM20), they merely define a size range and so could be used for number, mass, or surface area concentration species 
interchangeably. Users may modify the size limits of these options or create new options by editing the 'ELMO_INLET' structure in ELMO_DATA.F.

Once NOx and PMF_SO4 are defined as in the examples above, they may be used in the File_Vars variable in CMAQ_Control.nml as if they were a CMAQ species.

##### Internal Aerosol Composites
For convenience, ELMO automatically assumes that the variables ASO4, ANO3, ANH4, etc. are mapped to the sum of the chemical species mass across all aerosol modes (e.g. ANO3 = ANO3I + ANO3J + ANO3K). 
These species names may be used in File_Vars to request the total concentration across all modes, or they may be combined with DD_ or WD_ to request the deposition across all modes 
(e.g. WD_ANH4 = WD_NH4I + WD_NH4J + WD_NH4K).  

##### Unit Conversions
In most cases, ELMO composites containing only gases will specify ppmV and composites containing only particle mass will specify ug m-3. When gases and particles are mixed, units may 
be more flexible. For example:
```
'PMF_TNO3'    , 'Fine-Mode Nitrate plus Nitric Acid',
                'ug m-3', 'FINE',
                'ANO3 + HNO3',
```
This composite sums particle nitrate in the fine mode with nitric acid converted to ug m-3 using the molecular weight of nitric acid and the time-and-space dependent density of air. 
Sometime, gases are desired in mass concentration units are their own:
```
'NH3_UGM3'    , 'Gas-Phase Ammonia',
                'ug m-3', 'GAS',
                'NH3',
```
Total VOC is now defined in the Chemical Control Namelists provided with each chemical mechanism. Here is an example for CB6:
```
'VOC'         , 'Volatile Organic Compound (VOC) Concentration',
                'ppmC', 'GAS',
                'PAR + 2.0*ETHA + 3.0*PRPA + MEOH + 2.0*ETH + 2.0*ETOH + 2.0*OLE + 3.0*ACET + 7.0*TOL + 8.0*XYLMN + 6.0*BENZENE + FORM + 3.0*GLY + 4.0*KET + 2.0*ETHY
```
The units specified are ppmC. CMAQ sums the species in ppm and coefficients are provided to convert from ppm to ppmC. Units of ppmV, ppbV, ug m-3, umol m-3, ng m-3, are supported. 
If ppmC is desired, the user must provide the coefficients quantifying carbon number in the composite definition.   

For deposition, kg ha-1 is recommended. If kgN ha-1 (kilograms nitrogen per hectare) or kgS ha-1 are desired, the user must supply coefficents to convert from kg to the nitrogen 
or sulfur basis.

#### F.2.3 ELMO Derived Variables
Some output variables are either more complicated to calculate than linear combinations, or they contain so many constituents that change names or properties frequently across 
mechanisms that it makes sense to automate their calculation. ELMO derived variables achieve this purpose. The most commonly used derived variables are total particle mass metrics 
(e.g. PM01, PM1, PM25, PM25TO10, and PM10). Because derived variables are defined in CMAQ source code, they may be requested directly in File_Vars without the user needing to specify 
their contents. This provides convenience and ensures that as chemical mechanisms change, the definitions of these variables remains consistent. Table F-2 defines more derived variables. 
The variable attributes are defined in ELMO_DATA.F.  

**Table F-2. Definition of ELMOv2 derived variables**

|**Derived Variable**  |**Meaning**|
|----------------------|----------------------------------|
| **PM01**     | Mass of particles with diameter less than 0.1 um |
| **PM1**      | Mass of particles with diameter less than 1.0 um |
| **PM25**     | Mass of particles with diameter less than 2.5 um |
| **PM25TO10** | Mass of particles with diameter greater than 2.5 and less than 10. um |
| **PM10**     | Mass of particles with diameter less than 10. um |
| **PMAMS**    | Mass of particles detected by an aerosol mass spectrometer |
| **PMF_OA**   | Fine-mode (Aitken + Accumulation) organic aerosol mass |
| **PMF_OC**   | Fine-mode (Aitken + Accumulation) organic carbon mass |
| **PMF_POA**  | Fine-mode primary organic aerosol mass |
| **PMF_SOA**  | Fine-mode secondary organic aerosol mass |
| **PMF_POC**  | Fine-mode primary organic carbon mass |
| **PMF_SOC**  | Fine-mode secondary organic carbon mass |
| **PMF_ASOA** | Fine-mode anthropogenic secondary organic aerosol mass |
| **PMF_BSOA** | Fine-mode biogenic secondary organic aerosol mass |
| **PMAMS_OA** | Organic aerosol detected in an aerosol mass spectrometer |
| **PM1_OA**   | Organic aerosol mass on particles with diameter smaller than 1 um |
| **PM25_OA**  | Organic aerosol mass on particles with diameter smaller than 2.5 um |
| **PM25_OC**  | Organic carbon mass on particles with diameter smaller than 2.5 um |
| **PM25_FRM** | Federal Reference Method PM2.5 |
| **PMF_FRM**  | Federal Reference Method Fine PM (Aitken + Accumulation ) |
| **PM1_OC**   | Organic carbon mass on particles with diameter smaller than 1 um |
| **Deposition**   |  |
| **DD_PMF_OA** | Dry Deposition of Fine-mode organic aerosol mass |
| **WD_PMF_OA** | Wet Deposition of Fine-mode organic aerosol mass |
| **DD_PMF_OC** | Dry Deposition of Fine-mode organic carbon mass |
| **WD_PMF_OC** | Wet Deposition of Fine-mode organic carbon mass |
| **DD_PMF_POA** | Dry Deposition of Fine-mode primary organic aerosol mass |
| **WD_PMF_POA** | Wet Deposition of Fine-mode primary organic aerosol mass |
| **DD_PMF_POC** | Dry Deposition of Fine-mode primary organic carbon mass |
| **WD_PMF_POC** | Wet Deposition of Fine-mode primary organic carbon mass |
| **DD_PMF_SOA** | Dry Deposition of Fine-mode secondary organic aerosol mass |
| **WD_PMF_SOA** | Wet Deposition of Fine-mode secondary organic aerosol mass |
| **DD_PMF_SOC** | Dry Deposition of Fine-mode secondary organic carbon mass |
| **WD_PMF_SOC** | Wet Deposition of Fine-mode secondary organic carbon mass |

#### F.2.4 Aerosol Property Variables
ELMOv1 was able to output important aerosol properties. ELMOv2 maintains this capability. These variables are defined in ELMO_DATA.F as well. Table F-3 lists the variables that are 
supported. Those with a (m) are expanded to each particle mode if no mode is specified. For example, for STDEV (modal standard deviation), File_Vars could contain STDEV_ACC to output 
just the standard deviation of the Accumulation mode, or STDEV to output the standard deviation of all three particulate modes. 

**Table F-3. Definition of ELMOv2 aerosol property variables**

|**Aerosol Property Variable**  |**Meaning**|
|----------------------|----------------------------------|
| **STDEV(m)**         | Modal standard deviation         |
| **DRY_DG(m)**        | Modal Dry Geometric Mean Diamter |
| **WET_DG(m)**        | Modal Wet Geometric Mean Diamter |
| **WET_M2(m)**        | Modal Wet Second Moment          |
| **DRY_M3(m)**        | Modal Dry Third Moment           |
| **WET_M3(m)**        | Modal Wet Third Moment           |
| **TSP_NUM**          | Total Particle Number            |
| **PMU_NUM**          | Ultrafine Particle Number        |
| **PMF_NUM**          | Fine Particle Number             |
| **PMC_NUM**          | Coarse Particle Number           |
| **N10**              | Number of particle larger than 10 nm |
| **N20**              | Number of particle larger than 20 nm |
| **N40**              | Number of particle larger than 40 nm |
| **N100**             | Number of particle larger than 100 nm |
| **TSP_SRF**          | Total Particle Surface Area      |
| **PMU_SRF**          | Ultrafine Particle Surface Area  |
| **PMF_SRF**          | Fine Particle Surface Area       |
| **PMC_SRF**          | Coarse Particle Surface Area     |
| **DRY_DENS(m)**      | Dry Bulk Particle Density        |
| **WET_DENS(m)**      | Wet Bulk Particle Density        |
| **FPM01(m)**         | Fraction of each mode within PM0.1 |
| **FPM1(m)**          | Fraction of each mode within PM1.0 |
| **FPM25(m)**         | Fraction of each mode within PM2.5 |
| **FPM25TO10(m)**     | Fraction of each mode within PM2.5-10. |
| **FPM10(m)**         | Fraction of each mode within PM10. |
| **PMF_HPMOLAL**      | Fine Particle H+ Concentration   |
| **PMF_PH**           | Fine Particle pH+                |
| **PMF_OMOC**         | Fine Particle OM:OC for organic aerosol compounds |
| **PMF_OTOC**         | Fine Particle O:C for organic aerosol compounds   |
| **PMAMS_OTOC**       | Aerosol mass spectrometer Particle O:C for organic aerosol compounds    |
| **BENAPY_FAERO**     | Aerosol mass fraction of benzo-a-pyrene |

#### F.2.5 Meteorological Variables
ELMOv2 can output many useful meteorological variables on the same files and using the same time averaging as the pollutant fields. These variables are calculated by the weather 
forecasting model and used to drive CMAQ simulations, or they are derived from input environmental conditions. Examples are in Table F-4. Their attributes may be viewed in ELMO_DATA.F.  

**Table F-4. Definition of ELMOv2 meteorological variables**

|**Meteorological Variable**  |**Meaning**  |
|--------------------|----------------------|
| **TA**             | Temperature (K)      |
| **PRES**           | Pressure (Pa)        | 
| **RH**             | Relative Humidity (1)| 
| **SFC_TMP**        | Surface Temp (K)     | 
| **PBL**            | Height of Planetary Boundary Layer (m) | 
| **RGRND**          | Solar Radiation at Ground |
| **PRECIP**         | Precipitation |
| **WSPD10**         | Wind Speed at 10 m |
| **WDIR10**         | Wind Direction at 10 m |
| **WVEL**           | Vertical Wind Velocity |
| **DZ**             | Height of each grid cell computed from top layer height |
| **ZH**             | Height of grid cell midpoints (mass-weighted) |
| **CFRAC**          | Cloud Fraction |
| **PV**             | Potential Vorticity |
| **DENS**           | Advected Density |
| **KZ**             | Vertical diffusivity coefficients |
| **KZMIN**          | Vertical diffusivity coefficients |
| **RHOJ**           | Advected Density x Jacobian/MSFX^2 |
| **USTAR**          | Surface Friction Velocity |
| **GA**             | Surface Aerodynamic Conductance |
| **ZOL**            | Grid Cell Midpoint over Monin-Obukhov length |
| **SHFX**           | Sensible Heat Flux |
| **LHFX**           | Latent Heat FLux |
| **SWC_1**          | Layer 1 soil moisture saturation ratio |
| **SWC_2**          | Layer 2 soil moisture saturation ratio |
| **SOILT_1**        | Layer 1 soil temperature |
| **SOILT_2**        | Layer 2 soil temperature |

#### F.2.6 Chemistry Variables
ELMOv2 can also output useful variables for diagnosing chemical reaction rates. The list of supported variables are mostly relevant for heterogeneous chemistry. We recommend relying 
on process anlysis (IRR) for comprehenisve diagnostics of the gas-phase chemical system. Examples of ELMOv2 chemical variables are in Table F-5. Their attributes may be viewed in ELMO_DATA.F.   

**Table F-5. Definition of ELMOv2 chemical variables**

|**Meteorological Variable**  |**Meaning**  |
|--------------------|----------------------|
| **GAMMA_N2O5**     |  Fine Mode N2O5 Heterogeneous rxn probability |
| **GAMMA_N2O5K**    |  Coarse Mode N2O5 Heterogeneous rxn probability |
| **YIELD_CLNO2**    |  Fine Mode CLNO2 Heterogeneous reaction yield |
| **YIELD_CLNO2K**   |  Coarse Mode CLNO2 Heterogeneous reaction yield |
| **GAMMA_IEPOX**    |  IEPOX heterogeneous uptake coefficient |
| **K_IEPOX**        |  IEPOX 1st order particle phase reaction rate const |
| **GAMMA_IMAE**     |  IMAE+HMML heterogeneous uptake coefficient |
| **VOC_NOX**        |  VOC-limiting (>0.35) or NOx-limiting (<0.35) O3 formation |
| **DZ**             |  Height of each grid cell computed from top layer height |
| **EF_HNO3**        |  Enhancement factor for HNO3 photolysis |
 
#### F.2.7 Optical Variables
Variables that are useful for comparing to satelites or other remote sensing techniques are available as well. Their attributes may be viewed in ELMO_DATA.F

**Table F-6. Definition of ELMOv2 optical variables**

|**Optical Variable**  |**Meaning**  |
|--------------------|----------------------|
| **GAMMA_N2O5**     |  Fine Mode N2O5 Heterogeneous rxn probability |
| **GAMMA_N2O5K**    |  Coarse Mode N2O5 Heterogeneous rxn probability |
| **YIELD_CLNO2**    |  Fine Mode CLNO2 Heterogeneous reaction yield |
| **AOD_550**        |  Aerosol Optical Depth at 550 nm -Angstrom interp |
| **PM_EXT_550**     |  Aerosol Extinction at 550 nm -Angstrom interp |
| **NO2_COLUMN**     |  NO2 column density |
| **SO2_COLUMN**     |  SO2 column density |
| **HCHO_COLUMN**    |  HCHO column density |
| **CO_COLUMN**      |  CO column density |

#### F.2.8 Source-Resolved Variables
In ELMOv2, the instrumented CMAQ source apportionment (ISAM) and sensitivity (DDM) models can now output composite and derived variables directly instead of outputting all CMAQ species 
for every user-defined source and requiring users to post-process them into aggregates offline. For example, if a user defines EGU as a source in ISAM to represent electric generating 
units, then the variable PM25_EGU can be added to File_Vars to request PM2.5 mass just for the EGU source.


#### F.2.9 ELMO Keywords
For convenience and mantainability, ELMO uses Keywords defined at run-time that expand to groups of variables (typically particularly meaningful or useful ones). 
In this way, ELMO improves transparency and reduces the risk of needing to rerun simulations to activate mistakenly omitted variables. ELMO Keywords are used in File_Vars just like 
other ELMO variables and CMAQ species. They are defined below the ELMO File specification section in the CMAQ Control Namelist. Here is a straight-forward example:  
```
  Keywd_name(1) = 'SIMPLE'
  Keywd(1,:) = 'PM25','PM10','O3','SO2','CO','NOX','NH3','ISOPRENE','FORMALD'
```
Now the Keyword 'SIMPLE' may be used in File_Vard and ELMO will substitute in all 9 variables in its contents list.
```
  Flabel(1)= 'SMALL_OUT'
  Tmode(1) = 'aggregate'
  Lay_Bot(1) = 1
  Lay_Top(1) = 1
  File_Vars(1,:) = 'SIMPLE'
```
This new file, with a label CCTM_ELMO1_SMALL_OUT_ will have the 9 variables from the 'SIMPLE' keyword on it.
```
  Flabel(1)= 'A_LITTLE_BIGGER_OUT'
  Tmode(1) = 'aggregate'
  Lay_Bot(1) = 1
  Lay_Top(1) = 1
  File_Vars(1,:) = 'SIMPLE','TOLUENE','PMF_SO4','PMC_NO3','TA','RH','DD_NH3'
``` 
This file, CCTM_ELMO1_A_LITTLE_BIGGER_OUT_ will have everything the previous file had plus toluene, fine particle sulfate, coarse particle nitrate, temperature, relative humdity, 
and the dry deposition flux of ammonia.  

There are 70+ ELMO Keywords provided in the default CMAQ Control Namelist. ELMO Keywords may contain other Keywords in their contents list. In this way, Keywords definitions can be 
built up and made comprehensive. For example, the ELMO Keyword 'AMET' contains all of the variables needed to for a full evaluation with the Atmospheric Model Evaluation Tool, including 
criteria pollutants, HAPs, and meteorological variables. 

In order to provide Users with a useful default setting, and support default differences among chemical mechanisms, ELMOv2 will interpret the Keywords 'DEFAULT' and 'DEFAULT_DEP' in a 
special way if they are added to File_Vars. Within the CMAQ source code, ELMOv2 will retrieve the value of the environment variable 'MECH', and append it to 'DEFAULT' and/or 'DEFAULT_DEP' 
and then look up that Keyword. For example, if a simulation is using CRACMM2, then the string CRACMM2 should be assigned to the environment variable 'MECH' in the Run Script. If 
'DEFAULT' is used in File_Vars, then ELMOv2 will look for the Keyword 'DEFAULT_CRACMM2' and expand File_Vars with its contents. Be careful using the 'DEFAULT' and 'DEFAULT_DEP' 
Keywords if you define your chemical mechanism. Be sure to add them to the Keyword list with appropriate contents. 


### F.3 ELMO Logs




 

<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixE_configuring_WRF.md) - [Home](../README.md) <br>
CMAQv5.5 User's Guide<br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT --> 

[link_F_Data]: ../../../CCTM/src/driver/ELMO_DATA.F
[link_F_Proc]: ../../../CCTM/src/driver/ELMO_PROC.F


<!-- END_OF_COMMENT -->

[link_F_Data]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/ELMO_DATA.F
[link_F_Proc]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/ELMO_PROC.F
