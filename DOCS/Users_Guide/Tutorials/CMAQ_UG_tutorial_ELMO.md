## CMAQ Tutorial ##
### Add ELMO Keywords and Derived Variables ###
Purpose: This tutorial will step you through the processes of adding ELMO Keywords to the CMAQ Control Namelist and 
adding new derived variables to the ELMO source code.  

------------

### Add ELMO Keywords

#### STEP 1: Add a Keyword Name  

Go to the CMAQ Control Namelist (CMAQ_Control.nml) under the section "DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS". 
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

Go to ELMO_DATA.F. Beginning around line 70, you will find a list of 100+ integers, prefixed with ID_, which map to 
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



<!-- START_OF_COMMENT -->

[link_1]: ../../../POST/combine/

<!-- END_OF_COMMENT -->

[link_1]: https://github.com/USEPA/CMAQ/blob/main/POST/combine/ 
