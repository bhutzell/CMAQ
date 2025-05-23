## CMAQ Tutorial ##
### Add ELMO Keywords and Derived Variables ###
Purpose: This tutorial will step you through the processes of adding ELMO Keywords to the CMAQ Control Namelist and 
adding new derived variables to the ELMO source code.  

------------

### Add ELMO Keywords

#### STEP 1: Add a Keyword Name  

Go to the CMAQ Control Namelist ([CMAQ_Control.nml][link_cmaq_ctrl]) under the section "DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS". 
Create a new line and set the value of Keywd_name(x) equal to the name of your new Keyword. The x is an arbitrary 
index indicating the position of your Keyword in the full Keyword list. There is nothing consequential about the order 
of ELMO Keywords, but indices cannot repeat. If you choose a number in the middle of the existing list, you must increment 
the index of every Keyword after it in the list. The easiest choice is just to make x equal to 1 greater than the 
current largest Keyword index.  

#### STEP 2: Add Contents for Your Keyword  

Populate Keywd(x,:) with a list of comma-separated, quoted strings. These strings can be CMAQ species, ELMO derived 
variables, meteorological variables, other Keywords, or any other variable type ELMO knows of.

#### STEP 3: Use Your New Keyword

Your Keyword is ready for use in File_Vars to activate variables for your simulation's output files.

------------

### Add Derived Variables to ELMO Source Code

#### STEP 1: Add an Index for the New Variable

Go to [ELMO_DATA.F][link_elmo_data]. Beginning around line 70, you will find a list of 100+ integers, prefixed with ID_, which map to 
derived ELMO variables and other diagnostics. Add an index for your new variable to the end of the list. We recommend 
prefixing it with ID_.

#### STEP 2: Add New Variable Attributes to ELMO_LIST  

Below the list of ID_ indices is a table called ELMO_LIST which holds the attributes for every ELMO derived variable. 
Because each variable maps to an ID_, this list is order-independent. Add a row in any location you prefer. This row 
should contain values for:
- The name of the variable - a character string that will be used in File_Vars to add the variable to an output file. 
This name can also be used in the contents of any Keyword to activate it with a group of other variables.  
- The ID_ of the variable.
- The variable type: 
    - ET_DRVD - a derived ELMO variable. These should be mass concentrations or mixing ratios. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_DRVD_DDEP - a derived ELMO variable for dry deposition fluxes. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_DRVD_WDEP - a derived ELMO variable for wet deposition fluxes. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_AEROPROP - an aerosol property. These variables describe the aerosol size distribution (e.g. number concentration, 
    diameter, density, etc.) or some chemical property (e.g. O:C, pH).  
    - ET_PHOT - an optical property. This could be AOD, NO2 column, etc.
    - ET_MET - a meteorological variable (e.g. temperature, rain data, etc.)
    - ET_CHEM - a chemical reaction property for heterogeneous reactions like gamma uptake coefficient.

#### STEP 3: Declare Array for New Variable

Declare an allocatable array in the ELMO_DATA module that will store the values for your new variable so they may be accessed by ELMO. 
Use ELMO_AOD_550 as an example. This particular variable is defined with two dimensions. Use three dimensions if your variable is 
dependent on height.  

#### STEP 4: Allocate and Initialize New Variable

Add your variable to the subroutine elmo_init_shared. Again, you may use ELMO_AOD_550 as an example.  

#### STEP 5: Populate New Variable in CMAQ

Use the new array defined and allocated in Steps 3/4 in a CMAQ module to store the data you wish to output. The ELMO_DATA module should 
be used in the subroutine you modify if it is not already. We recommend using the 'Use ELMO_DATA, Only:' approach to protect the rest 
of the ELMO_DATA module and only update your own variable.  

#### STEP 6: Propagate Data to ELMO Output Arrays

In ELMO_DERIVED_CALC.F, add a case to the select case statement for the variable IDG. Your case should reference the new ID_ index of 
your variable. Within the case, set outval equal to the value of your new variable in the current local grid cell (C1,R1,L1), and 
make any appropriate modifications. Again, use the approach for ELMO_AOD_550 as a guide.  

#### STEP 7: Add Variable Name to CMAQ Control File

ELMO is now equipped to output your variable. You may add it to File_Vars in [CMAQ_Control.nml][link_cmaq_ctrl] for any custom output file you like, or 
you may add it to the contents of any ELMO Keyword.  


<!-- START_OF_COMMENT -->

[link_elmo_data]: ../../../CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: ../../../CCTM/src/driver/CMAQ_Control.F

<!-- END_OF_COMMENT -->

[link_elmo_data]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/CMAQ_Control.F


