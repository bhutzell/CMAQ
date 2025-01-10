
      subroutine LOAD_HEADER( HEADER_TXT, N_HEADER_TXT, TOC_TXT, N_TOC_TXT )

      IMPLICIT NONE

      CHARACTER( 90 ) :: HEADER_TXT( 200 ), TOC_TXT( 200 )
      INTEGER         :: N_HEADER_TXT, N_TOC_TXT

      N_HEADER_TXT = 21
      HEADER_TXT( : ) = ''
      N_TOC_TXT = 19
      TOC_TXT( : ) = ''

      HEADER_TXT( 1:N_HEADER_TXT ) = (/   
     &   '#================================================================================#', 
     &   '#|                                                                              |#', 
     &   '#|               The Community Multiscale Air Quality (CMAQ) Model              |#', 
     &   '#|                                   Version 5.5                                |#', 
     &   '#|                                                                              |#', 
     &   '#|                          Built and Maintained by the                         |#', 
     &   '#|                        Office of Research and Development                    |#', 
     &   '#|                   United States Environmental Protection Agency              |#', 
     &   '#|                                                                              |#', 
     &   '#|                            https://www.epa.gov/cmaq                          |#', 
     &   '#|                                                                              |#', 
     &   '#|       Source Code:   https://www.github.com/USEPA/cmaq/tree/master           |#', 
     &   '#|       Documentation: https://www.github.com/USEPA/cmaq/tree/master/DOCS      |#', 
     &   '#|                                                                              |#', 
     &   '#|         The CMAQ Model is tested and released with cooperation from          |#', 
     &   '#|         the Community Modeling and Analysis System (CMAS) Center via         |#', 
     &   '#|         contract support. CMAS is managed by the Institute for the           |#', 
     &   '#|         Environment, University of North Carolina at Chapel Hill.            |#', 
     &   '#|         CMAS URL: (https://www.cmascenter.org)                               |#', 
     &   '#|                                                                              |#', 
     &   '#================================================================================#'  
     &      /)


      TOC_TXT( 1:N_TOC_TXT ) = (/   
     &   '                                                                                  ',
     &   '     ------------------------                                                     ',
     &   '     |>>   Useful Links   <<|                                                     ',
     &   '     ------------------------                                                     ',
     &   '                                                                                  ',
     &   '     Search for the headings below to jump to error checks, reports, and          ', 
     &   '     diagnostic tables documenting emission input, prediction output, etc.        ',
     &   '                                                                                  ',
     &   '     ** It is STRONGLY recommended that users check these sections when           ', 
     &   '        configuring a new scenario or beginning production-level simulations. **  ',
     &   '                                                                                  ',
     &   '     -> INITIALIZE DESID EMISSION MODULE                                          ',
     &   '         - Error Check Emission Mapping Configuration                             ',
     &   '         - DESID Emission Scaling Report                                          ',
     &   '         - DESID Emission Diagnostic Report                                       ',
     &   '     -> INITIALIZE ELMO MODULE                                                    ',
     &   '         - Reading ELMO Composites from Mechanism-Specific Namelist               ',
     &   '         - ELMO Output Variable Report                                            ',
     &   '    ------------------------------------------------------------------------------'
     &      /)

       end subroutine LOAD_HEADER 

