
### CMAQ Chemical Mechanisms

The CMAQ modeling system accounts for chemistry in three phases: gas, aerosol, 
and cloud droplets. Available mechanisms include variations of three 
photochemistry schemes (CRACMM, Carbon Bond, and SAPRC) with different 
representations of halogens, dimethyl sulfide, secondary organic aerosols, and 
Hazardous Air Pollutants. Please consult the release notes for changes to mechanisms 
applicable for a specific version of CMAQ. 

The choice of chemical mechanism is a build time option set in the bldit_cctm.csh script.
Subdirectories of the $CMAQ_REPO/CCTM/src/MECHS directory contain the Fortran modules
and namelist files defining each of the available mechanisms. 
The species and emission control namelists enable setting runtime options for a mechanism. 
Species namelists define names, molecular weights, and whether species undergo certain 
atmospheric processes (e.g., transport, cloud chemistry, and deposition), as well as 
controlling whether the species concentrations and deposition results are written to 
output files. 
Emission control (DESID) namelists specify the mapping between emissions input files and 
model species. 
Two Fortran modules, RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90, define 
photochemistry for a mechanism. The data module specifies reactions and parameters. 
The functions module initializes photochemistry and calculates reaction rate constants. 
Because model source code define photochemistry for a mechanism, an executable 
version of the CMAQ model has a fixed photochemistry. 
Modifying the chemistry, such as by adding reactions or replacing the modules representing
one mechanism with another, requires recompiling the model.

### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to one of the mechanism subdirectories located under $CMAQ_REPO/CCTM/src/MECHS. The table below
lists mechanisms available in this version of the CMAQ model.

Table 1.  CMAQv6.0 Chemical Mechanisms

|**Mechanism Name** | **Photochemistry**                                   | **Model Species<sup>1,2</sup>**    | **Cloud Chemistry Module<sup>3</sup>** |
| ----------------- | ---------------------------------------------------- | -------------------- | ---------------------- |
| cb6r5_ae7_aq      | [Carbon Bond 6 version r5 with aero7 treatment of SOA](mechanism_information/cb6r5_ae7_aq/mech_cb6r5_ae7_aq.md) |  [species table](mechanism_information/cb6r5_ae7_aq/cb6r5_ae7_aq_species_table.md)                 | acm_ae7          |
| cb6r5hap_ae7_aq   | [Carbon Bond 6 version r5 with air toxics and aero7 treatment of SOA](mechanism_information/cb6r5_ae7_aq/mech_cb6r5_ae7_aq.md) | [species table](mechanism_information/cb6r5hap_ae7_aq/cb6r5hap_ae7_aq_species_table.md)               | acm_ae7          |
| cb6r5_ae7_aqkmt2  | [Carbon Bond 6 version r5 with aero7 treatment of SOA](mechanism_information/cb6r5_ae7_aq/mech_cb6r5_ae7_aq.md) | [species table](mechanism_information/cb6r5_ae7_aq/cb6r5_ae7_aq_species_table.md)                   | acm_ae7_kmt2          |
| cb6r5m_ae7_aq     | [Carbon Bond 6 version r5 with aero7 treatment of SOA and DMS and marine halogen chemistry](mechanism_information/cb6r5m_ae7_aq/mech_cb6r5m_ae7_aq.md) | [species table](mechanism_information/cb6r5m_ae7_aq/cb6r5m_ae7_aq_species_table.md)                   | acm_ae7_aq          |
| saprc07tic_ae7i_aq | [State Air Pollution Research Center version 07tc with extended isoprene chemistry and aero7i treatment of SOA]( mechanism_information/saprc07tic_ae7i_aq/mech_saprc07tic_ae7i_aq.md) | [species table](mechanism_information/saprc07tic_ae7i_aq/saprc07tic_ae7i_aq_species_table.md)                   | acm_ae7          |
| saprc07tc_ae6_aq | [State Air Pollution Research Center version 07tc with aero6 treatment of SOA](mechanism_information/saprc07tc_ae6_aq/mech_saprc07tc_ae6_aq.md)  | [species table](mechanism_information/saprc07tc_ae6_aq/saprc07tc_ae6_aq_species_table.md)                   | acm_ae6         |       
| cracmm2 | [Community Regional Atmospheric Chemistry Multiphase Mechanism version 2](mechanism_information/cracmm2/mech_cracmm2.md)  | [species table](mechanism_information/cracmm2/cracmm2_species_table.md)                   | acm     |
| cracmm3 | [Community Regional Atmospheric Chemistry Multiphase Mechanism version 3](mechanism_information/cracmm3/mech_cracmm3.md)  | [species table](mechanism_information/cracmm3/cracmm3_species_table.md)                   | acm     |
| cracmm3haps | [Community Regional Atmospheric Chemistry Multiphase Mechanism version 3 with air toxics](mechanism_information/cracmm3/mech_cracmm3.md)  | [species table](mechanism_information/cracmm3haps/cracmm3haps_species_table.md)                   | acm     |

1. mechanisms can share the same model species but differ cloud chemistry
2. species tables define model species in a mechanism's GC, AE, and NR namelists.
3. _kmt_  and _acm_ refers to the kinetic mass transfer to cloud droplets and the convective cloud/transport representation, respectively


### Creating or modifying a mechanism's photochemistry

Editing a mechanism's Fortran modules is one way to make simple changes to the 
photochemistry scheme. More complex changes (_adding reactions and model 
species_) or creating a new scheme requires 1) creating new namelists with a 
text editor (_if adding new model species_) and 2) using the CMAQ chemical 
mechanism utility, CHEMMECH, to produce new Fortran modules. The CHEMMECH 
utility translates an ASCII file listing reactions for photochemistry into 
the Fortran modules used by CMAQ. For more information, consult the README 
file under $CMAQ_REPO/UTIL/chemmech.
Next, the inline_phot_preproc or jproc utility is used to create CMAQ input 
file(s) for the photolysis module being used. 
Finally, if CMAQ is using the Euler Backward Iterative (EBI) solver, the 
create_ebi utility must be used to create a solver for the new mechanism.
These three utilities use the mechanism data module produced by the CHEMMECH 
utility.  

### Using species namelist files

Species namelists define the four groups of model species: gas (GC), aerosol (AE), 
non-reactive (NR), and tracer (TR) species simulated by the CMAQ model.
CMAQ reads namelists to define processes determining concentrations. For example, 
species namelists can be used to apply uniform scaling factors to several physical 
processes. Dry deposition of NO can be reduced by 50% by applying a factor of 0.5 
to the dry deposition velocity for NO. Similarly, the boundary conditions of 
O<sub>3</sub> can be increased by 50% by applying a factor of 1.5. The gas, aerosol, 
and non-reactive namelists define a specific mechanism. 
The tracer namelist is generally interchangable between mechanisms. It can be employed 
for transport and deposition studies. 
Example tracer namelists are under $CMAQ_MODEL/CCTM/src/MECHS/trac0 (_the version most often used_) and $CMAQ_MODEL/CCTM/src/MECHS/trac1.

### Points to emphasize on chemical mechanisms

-   The Euler Backward Iterative (EBI) solver for photochemistry is hardcoded to the 
Fortran data module representing photochemistry and specific names in the species namelists. 
If either change, a new EBI solver must be generated.
-   The Rosenbrock and SMVGEAR photochemistry solvers are not hardcoded to the above files 
so they more easily allow changing these files.

### Sulfur Tracking Method (STM) option

CMAQ includes a runtime option that provides detailed information on the 
modeled sulfur budget. This option, referred to as the Sulfur Tracking 
Method (STM), tracks sulfate production from gas- and aqueous-phase chemical 
reactions, as well as contributions from emissions and initial and boundary 
conditions. The STM option is activated by setting an environment variable 
in the CCTM runscript:

```
setenv STM_SO4TRACK Y
```

Sulfur tracking species are added to the AE and NR groups at runtime if the 
STM is enabled.  Table 2 provides a list of inorganic sulfur tracking 
species.  Table 3 lists additional tracking species for the loss of inorganic 
sulfate to organosulfate for chemical mechanisms that include this loss 
pathway (SAPRC07TIC_AE7I, CB6R3_AE7, or CB6R5M_AE7 mechanisms).

Table 2. Sulfur Tracking Species

|Species Group|Species Name| MW   | Description |
|:------------|:-----------|:-----|:------------|
|AE           |ASO4AQH2O2J | 96.0 |Accumulation mode sulfate (ASO4J) produced by aqueous-phase hydrogen peroxide oxidation reaction:  H<sub>2</sub>O<sub>2</sub> + S(IV) -> S(VI) + H<sub>2</sub>O |
|AE           |ASO4AQO3J   | 96.0 |ASO4J produced by aqueous-phase ozone oxidation reaction:  O<sub>3</sub> + S(IV) -> S(VI) + O<sub>2</sub> |
|AE           |ASO4AQFEMNJ | 96.0 |ASO4J produced by aqueous-phase oxygen catalyzed by Fe<sup>3+</sup> and Mn<sup>2+</sup> oxidation reaction: O<sub>2</sub> + S(IV) -> S(VI) |
|AE           |ASO4AQMHPJ  | 96.0 |ASO4J produced by aqueous-phase methyl hydrogen peroxide oxidation reaction:  MHP + S(IV) -> S(VI) |
|AE           |ASO4AQPAAJ  | 96.0 |ASO4J produced by aqueous-phase peroxyacetic acid oxidation reaction:  PAA + S(IV) -> S(VI) |
|AE           |ASO4GASJ    | 96.0 |ASO4J condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub> |
|AE           |ASO4EMISJ   | 96.0 |ASO4J from source emissions |
|AE           |ASO4ICBCJ   | 96.0 |ASO4J from boundary and initial conditions |
|AE           |ASO4GASI    | 96.0 |Aitken mode sulfate (ASO4I) nucleation and/or condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub> |
|AE           |ASO4EMISI   | 96.0 |ASO4I from source emissions |
|AE           |ASO4ICBCI   | 96.0 |ASO4I from boundary and initial conditions |
|AE           |ASO4GASK    | 96.0 |Coarse mode sulfate (ASO4K) condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub>  |
|AE           |ASO4EMISK   | 96.0 |ASO4K from source emissions |
|AE           |ASO4ICBCK   | 96.0 |ASO4K from boundary and initial conditions |
|NR           |SULF_ICBC   | 98.0 |Sulfuric acid vapor (SULF) from boundary and initial conditions |

Table 3.  Additional Tracking Species Representing Loss of Inorganic Sulfate to Organosulfate (only included if using SAPRC07TIC_AE6I, SAPRC07TIC_AE7I, CB6R3_AE7, or CB6R5M_AE7 mechanisms).

|Species Group|Species Name| MW   | Description |
|:------------|:-----------|:-----|:------------|
|AE           |OSO4J       | 96.0 |Loss of ASO4J to organosulfate |
|AE           |OSO4AQH2O2J | 96.0 |Loss of ASO4AQH2O2J to organosulfate |
|AE           |OSO4AQO3J   | 96.0 |Loss of ASO4AQO3J to organosulfate |
|AE           |OSO4AQFEMNJ | 96.0 |Loss of ASO4AQFEMNJ to organosulfate |
|AE           |OSO4AQMHPJ  | 96.0 |Loss of ASO4AQMHPJ to organosulfate |
|AE           |OSO4AQPAAJ  | 96.0 |Loss of ASO4AQPAAJ to organosulfate |
|AE           |OSO4GASJ    | 96.0 |Loss of ASO4GASJ to organosulfate |
|AE           |OSO4EMISJ   | 96.0 |Loss of ASO4EMISJ to organosulfate |
|AE           |OSO4ICBCJ   | 96.0 |Loss of ASO4ICBCJ to organosulfate |
