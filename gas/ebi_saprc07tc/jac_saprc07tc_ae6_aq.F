      SUBROUTINE JAC( C, PD, RK )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C                                                                       
C Automatically generated Jacobian for DDM-3D                           
C                                                                       
C =-=-= MECHANISM: SAPRC07TC_AE6_AQ                 =-=-=               
C                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE                                                     

      INCLUDE SUBST_RXCMMN                                              
c     INCLUDE SUBST_GC_SPC                                              
c     INCLUDE SUBST_IOPARMS                                             

      INTEGER, PARAMETER :: NJACSPC = 150                               
      INTEGER, PARAMETER :: NJACRXN = 689                               

      CHARACTER( 16 ), SAVE :: PNAME = 'JAC'                            
      CHARACTER( 132 ) :: MSG                                           

      REAL, DIMENSION( NJACSPC ) :: C                                   
      REAL, DIMENSION( NJACSPC, NJACSPC ) :: PD                         
      REAL(KIND=8), DIMENSION( NJACRXN ) :: RK                          

      INTEGER, SAVE :: LOGDEV                                           
      LOGICAL, SAVE :: FIRSTIME = .TRUE.                                

      INTEGER, PARAMETER :: NO2              =   1                      
      INTEGER, PARAMETER :: NO               =   2                      
      INTEGER, PARAMETER :: O3P              =   3                      
      INTEGER, PARAMETER :: O3               =   4                      
      INTEGER, PARAMETER :: NO3              =   5                      
      INTEGER, PARAMETER :: N2O5             =   6                      
      INTEGER, PARAMETER :: HNO3             =   7                      
      INTEGER, PARAMETER :: O1D              =   8                      
      INTEGER, PARAMETER :: OH               =   9                      
      INTEGER, PARAMETER :: HONO             =  10                      
      INTEGER, PARAMETER :: HO2              =  11                      
      INTEGER, PARAMETER :: CO               =  12                      
      INTEGER, PARAMETER :: CO2              =  13                      
      INTEGER, PARAMETER :: HNO4             =  14                      
      INTEGER, PARAMETER :: HO2H             =  15                      
      INTEGER, PARAMETER :: SO2              =  16                      
      INTEGER, PARAMETER :: SULF             =  17                      
      INTEGER, PARAMETER :: SULRXN           =  18                      
      INTEGER, PARAMETER :: NO2EX            =  19                      
      INTEGER, PARAMETER :: MEO2             =  20                      
      INTEGER, PARAMETER :: HCHO             =  21                      
      INTEGER, PARAMETER :: COOH             =  22                      
      INTEGER, PARAMETER :: MEOH             =  23                      
      INTEGER, PARAMETER :: RO2C             =  24                      
      INTEGER, PARAMETER :: RO2XC            =  25                      
      INTEGER, PARAMETER :: MECO3            =  26                      
      INTEGER, PARAMETER :: PAN              =  27                      
      INTEGER, PARAMETER :: CCOOOH           =  28                      
      INTEGER, PARAMETER :: CCOOH            =  29                      
      INTEGER, PARAMETER :: RCO3             =  30                      
      INTEGER, PARAMETER :: PAN2             =  31                      
      INTEGER, PARAMETER :: xHO2             =  32                      
      INTEGER, PARAMETER :: yROOH            =  33                      
      INTEGER, PARAMETER :: xCCHO            =  34                      
      INTEGER, PARAMETER :: RCOOOH           =  35                      
      INTEGER, PARAMETER :: RCOOH            =  36                      
      INTEGER, PARAMETER :: BZCO3            =  37                      
      INTEGER, PARAMETER :: PBZN             =  38                      
      INTEGER, PARAMETER :: BZO              =  39                      
      INTEGER, PARAMETER :: MACO3            =  40                      
      INTEGER, PARAMETER :: MAPAN            =  41                      
      INTEGER, PARAMETER :: TBUO             =  42                      
      INTEGER, PARAMETER :: RNO3             =  43                      
      INTEGER, PARAMETER :: ACETONE          =  44                      
      INTEGER, PARAMETER :: NPHE             =  45                      
      INTEGER, PARAMETER :: CRES             =  46                      
      INTEGER, PARAMETER :: xOH              =  47                      
      INTEGER, PARAMETER :: xNO2             =  48                      
      INTEGER, PARAMETER :: xMEO2            =  49                      
      INTEGER, PARAMETER :: xMECO3           =  50                      
      INTEGER, PARAMETER :: xRCO3            =  51                      
      INTEGER, PARAMETER :: xMACO3           =  52                      
      INTEGER, PARAMETER :: xTBUO            =  53                      
      INTEGER, PARAMETER :: xCO              =  54                      
      INTEGER, PARAMETER :: CCHO             =  55                      
      INTEGER, PARAMETER :: RCHO             =  56                      
      INTEGER, PARAMETER :: xHCHO            =  57                      
      INTEGER, PARAMETER :: MEK              =  58                      
      INTEGER, PARAMETER :: zRNO3            =  59                      
      INTEGER, PARAMETER :: xRCHO            =  60                      
      INTEGER, PARAMETER :: HCOOH            =  61                      
      INTEGER, PARAMETER :: xMGLY            =  62                      
      INTEGER, PARAMETER :: xBACL            =  63                      
      INTEGER, PARAMETER :: ROOH             =  64                      
      INTEGER, PARAMETER :: xPROD2           =  65                      
      INTEGER, PARAMETER :: R6OOH            =  66                      
      INTEGER, PARAMETER :: PRD2             =  67                      
      INTEGER, PARAMETER :: yR6OOH           =  68                      
      INTEGER, PARAMETER :: RAOOH            =  69                      
      INTEGER, PARAMETER :: MGLY             =  70                      
      INTEGER, PARAMETER :: IPRD             =  71                      
      INTEGER, PARAMETER :: xGLY             =  72                      
      INTEGER, PARAMETER :: xMEK             =  73                      
      INTEGER, PARAMETER :: xAFG1            =  74                      
      INTEGER, PARAMETER :: xAFG2            =  75                      
      INTEGER, PARAMETER :: GLY              =  76                      
      INTEGER, PARAMETER :: AFG1             =  77                      
      INTEGER, PARAMETER :: AFG2             =  78                      
      INTEGER, PARAMETER :: BACL             =  79                      
      INTEGER, PARAMETER :: BALD             =  80                      
      INTEGER, PARAMETER :: AFG3             =  81                      
      INTEGER, PARAMETER :: xIPRD            =  82                      
      INTEGER, PARAMETER :: MACR             =  83                      
      INTEGER, PARAMETER :: MVK              =  84                      
      INTEGER, PARAMETER :: xHOCCHO          =  85                      
      INTEGER, PARAMETER :: xRNO3            =  86                      
      INTEGER, PARAMETER :: HOCCHO           =  87                      
      INTEGER, PARAMETER :: xACETONE         =  88                      
      INTEGER, PARAMETER :: ACROLEIN         =  89                      
      INTEGER, PARAMETER :: xBALD            =  90                      
      INTEGER, PARAMETER :: xAFG3            =  91                      
      INTEGER, PARAMETER :: xMACR            =  92                      
      INTEGER, PARAMETER :: xMVK             =  93                      
      INTEGER, PARAMETER :: yRAOOH           =  94                      
      INTEGER, PARAMETER :: xACROLEIN        =  95                      
      INTEGER, PARAMETER :: ETHENE           =  96                      
      INTEGER, PARAMETER :: PROPENE          =  97                      
      INTEGER, PARAMETER :: BUTADIENE13      =  98                      
      INTEGER, PARAMETER :: ISOPRENE         =  99                      
      INTEGER, PARAMETER :: ISOPRXN          = 100                      
      INTEGER, PARAMETER :: APIN             = 101                      
      INTEGER, PARAMETER :: TRPRXN           = 102                      
      INTEGER, PARAMETER :: ACETYLENE        = 103                      
      INTEGER, PARAMETER :: BENZENE          = 104                      
      INTEGER, PARAMETER :: BENZRO2          = 105                      
      INTEGER, PARAMETER :: TOLUENE          = 106                      
      INTEGER, PARAMETER :: TOLRO2           = 107                      
      INTEGER, PARAMETER :: MXYL             = 108                      
      INTEGER, PARAMETER :: XYLRO2           = 109                      
      INTEGER, PARAMETER :: OXYL             = 110                      
      INTEGER, PARAMETER :: PXYL             = 111                      
      INTEGER, PARAMETER :: TRIMETH_BENZ124  = 112                      
      INTEGER, PARAMETER :: ETOH             = 113                      
      INTEGER, PARAMETER :: ALK1             = 114                      
      INTEGER, PARAMETER :: ALK2             = 115                      
      INTEGER, PARAMETER :: ALK3             = 116                      
      INTEGER, PARAMETER :: ALK4             = 117                      
      INTEGER, PARAMETER :: ALK5             = 118                      
      INTEGER, PARAMETER :: ALK5RXN          = 119                      
      INTEGER, PARAMETER :: OLE1             = 120                      
      INTEGER, PARAMETER :: OLE2             = 121                      
      INTEGER, PARAMETER :: ARO1             = 122                      
      INTEGER, PARAMETER :: ARO2             = 123                      
      INTEGER, PARAMETER :: TERP             = 124                      
      INTEGER, PARAMETER :: SESQ             = 125                      
      INTEGER, PARAMETER :: SESQRXN          = 126                      
      INTEGER, PARAMETER :: CL2              = 127                      
      INTEGER, PARAMETER :: CL               = 128                      
      INTEGER, PARAMETER :: CLNO             = 129                      
      INTEGER, PARAMETER :: CLONO            = 130                      
      INTEGER, PARAMETER :: CLNO2            = 131                      
      INTEGER, PARAMETER :: HCL              = 132                      
      INTEGER, PARAMETER :: CLO              = 133                      
      INTEGER, PARAMETER :: CLONO2           = 134                      
      INTEGER, PARAMETER :: HOCL             = 135                      
      INTEGER, PARAMETER :: xCL              = 136                      
      INTEGER, PARAMETER :: xCLCCHO          = 137                      
      INTEGER, PARAMETER :: xCLACET          = 138                      
      INTEGER, PARAMETER :: CLCCHO           = 139                      
      INTEGER, PARAMETER :: CLACET           = 140                      
      INTEGER, PARAMETER :: CLCHO            = 141                      
      INTEGER, PARAMETER :: BNZNRXN          = 142                      
      INTEGER, PARAMETER :: BNZHRXN          = 143                      
      INTEGER, PARAMETER :: XYLNRXN          = 144                      
      INTEGER, PARAMETER :: XYLHRXN          = 145                      
      INTEGER, PARAMETER :: TOLNRXN          = 146                      
      INTEGER, PARAMETER :: TOLHRXN          = 147                      
      INTEGER, PARAMETER :: HCHO_PRIMARY     = 148                      
      INTEGER, PARAMETER :: CCHO_PRIMARY     = 149                      
      INTEGER, PARAMETER :: ACROLEIN_PRIMARY = 150                      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( FIRSTIME ) THEN                                               

         IF( MECHNAME .NE. 'SAPRC07TC_AE6_AQ                ' ) THEN    
            MSG =  'This Jacobian was built for SAPRC07TC_AE6_AQ        
     &            // MECHNAME // ' is in use. Exiting.'                 
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                     
         ENDIF                                                          

         IF ( N_GC_SPC .NE. NJACSPC ) THEN                              
            MSG =  'ERROR: Number of species in Jacobian '              
     &          // 'does not match N_GC_SPC.'                           
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                     
         ELSE IF ( NRXNS .NE. NJACRXN ) THEN                            
            MSG =  'ERROR: Number of reactions in Jacobian '            
     &          // 'does not match NRXNS.'                              
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                     
         ENDIF                                                          

         FIRSTIME = .FALSE.                                             

      ENDIF                                                             

      PD = 0.0                                                          

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   1: NO2                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO2             ,NO2              ) = -           RK( 001 )   
     &                                          -           RK( 005 ) * 
     &                                          -           RK( 006 ) * 
     &                                          -           RK( 008 ) * 
     &                                          -           RK( 011 ) * 
     &                                          -           RK( 025 ) * 
     &                                          -           RK( 032 ) * 
     &                                          -           RK( 046 )   
     &                                          -           RK( 067 ) * 
     &                                          -           RK( 077 ) * 
     &                                          -           RK( 088 ) * 
     &                                          -           RK( 100 ) * 
     &                                          -           RK( 113 ) * 
     &                                          -           RK( 115 ) * 
     &                                          -           RK( 573 ) * 
     &                                          -           RK( 574 ) * 
     &                                          -           RK( 582 ) * 
      PD( NO2             ,NO               ) =             RK( 004 ) * 
     &                                          +           RK( 007 ) * 
     &                                          + 2.00000 * RK( 009 ) * 
     &                                          + 4.00000 * RK( 010 ) * 
     &                                          +           RK( 031 ) * 
     &                                          +           RK( 050 ) * 
     &                                          +           RK( 056 ) * 
     &                                          +           RK( 070 ) * 
     &                                          +           RK( 080 ) * 
     &                                          +           RK( 091 ) * 
     &                                          +           RK( 103 ) * 
     &                                          +           RK( 138 ) * 
     &                                          +           RK( 581 ) * 
      PD( NO2             ,O3P              ) =             RK( 004 ) * 
     &                                          -           RK( 005 ) * 
     &                                          -           RK( 006 ) * 
      PD( NO2             ,O3               ) =             RK( 007 ) * 
     &                                          -           RK( 008 ) * 
      PD( NO2             ,NO3              ) =   2.00000 * RK( 009 ) * 
     &                                          -           RK( 011 ) * 
     &                                          +           RK( 017 )   
     &                                          +           RK( 026 ) * 
     &                                          + 0.80000 * RK( 039 ) * 
     &                                          + 4.00000 * RK( 040 ) * 
     &                                          +           RK( 053 ) * 
     &                                          +           RK( 058 ) * 
     &                                          +           RK( 063 ) * 
     &                                          +           RK( 072 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 105 ) * 
     &                                          +           RK( 140 ) * 
     &                                          +           RK( 580 ) * 
      PD( NO2             ,N2O5             ) =             RK( 012 )   
      PD( NO2             ,HNO3             ) =             RK( 028 )   
      PD( NO2             ,OH               ) =             RK( 024 ) * 
     &                                          -           RK( 025 ) * 
     &                                          +           RK( 026 ) * 
     &                                          +           RK( 035 ) * 
     &                                          + 0.01900 * RK( 273 ) * 
      PD( NO2             ,HONO             ) =             RK( 024 ) * 
      PD( NO2             ,HO2              ) =             RK( 031 ) * 
     &                                          -           RK( 032 ) * 
     &                                          + 0.80000 * RK( 039 ) * 
      PD( NO2             ,HNO4             ) =             RK( 033 )   
     &                                          + 0.61000 * RK( 034 )   
     &                                          +           RK( 035 ) * 
      PD( NO2             ,NO2EX            ) =             RK( 047 )   
     &                                          +           RK( 048 )   
      PD( NO2             ,MEO2             ) =             RK( 050 ) * 
     &                                          +           RK( 053 ) * 
     &                                          + 0.50000 * RK( 141 ) * 
      PD( NO2             ,RO2C             ) =             RK( 056 ) * 
     &                                          +           RK( 058 ) * 
     &                                          + 0.50000 * RK( 142 ) * 
      PD( NO2             ,RO2XC            ) =             RK( 063 ) * 
     &                                          + 0.50000 * RK( 143 ) * 
      PD( NO2             ,MECO3            ) = -           RK( 067 ) * 
     &                                          +           RK( 070 ) * 
     &                                          +           RK( 072 ) * 
     &                                          +           RK( 144 ) * 
      PD( NO2             ,PAN              ) =             RK( 068 )   
     &                                          + 0.60000 * RK( 069 )   
      PD( NO2             ,RCO3             ) = -           RK( 077 ) * 
     &                                          +           RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 145 ) * 
      PD( NO2             ,PAN2             ) =             RK( 078 )   
     &                                          + 0.60000 * RK( 079 )   
      PD( NO2             ,BZCO3            ) = -           RK( 088 ) * 
     &                                          +           RK( 091 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 146 ) * 
      PD( NO2             ,PBZN             ) =             RK( 089 )   
     &                                          + 0.60000 * RK( 090 )   
      PD( NO2             ,BZO              ) = -           RK( 115 ) * 
      PD( NO2             ,MACO3            ) = -           RK( 100 ) * 
     &                                          +           RK( 103 ) * 
     &                                          +           RK( 105 ) * 
     &                                          +           RK( 147 ) * 
      PD( NO2             ,MAPAN            ) =             RK( 101 )   
     &                                          + 0.60000 * RK( 102 )   
      PD( NO2             ,TBUO             ) = -           RK( 113 ) * 
      PD( NO2             ,RNO3             ) =   0.01900 * RK( 273 ) * 
     &                                          +           RK( 274 )   
     &                                          + 0.03800 * RK( 598 ) * 
      PD( NO2             ,xNO2             ) =             RK( 138 ) * 
     &                                          +           RK( 140 ) * 
     &                                          + 0.50000 * RK( 141 ) * 
     &                                          + 0.50000 * RK( 142 ) * 
     &                                          + 0.50000 * RK( 143 ) * 
     &                                          +           RK( 144 ) * 
     &                                          +           RK( 145 ) * 
     &                                          +           RK( 146 ) * 
     &                                          +           RK( 147 ) * 
      PD( NO2             ,CL               ) = -           RK( 573 ) * 
     &                                          -           RK( 574 ) * 
     &                                          +           RK( 580 ) * 
     &                                          + 0.03800 * RK( 598 ) * 
      PD( NO2             ,CLONO            ) =             RK( 575 )   
      PD( NO2             ,CLNO2            ) =             RK( 576 )   
      PD( NO2             ,CLO              ) =             RK( 581 ) * 
     &                                          -           RK( 582 ) * 
      PD( NO2             ,CLONO2           ) =             RK( 583 )   
     &                                          +           RK( 585 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   2: NO                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO              ,NO2              ) =             RK( 001 )   
     &                                          +           RK( 005 ) * 
     &                                          +           RK( 015 ) * 
      PD( NO              ,NO               ) = -           RK( 004 ) * 
     &                                          -           RK( 007 ) * 
     &                                          -           RK( 009 ) * 
     &                                          - 4.00000 * RK( 010 ) * 
     &                                          -           RK( 022 ) * 
     &                                          -           RK( 031 ) * 
     &                                          -           RK( 050 ) * 
     &                                          -           RK( 056 ) * 
     &                                          -           RK( 061 ) * 
     &                                          -           RK( 070 ) * 
     &                                          -           RK( 080 ) * 
     &                                          -           RK( 091 ) * 
     &                                          -           RK( 103 ) * 
     &                                          -           RK( 571 ) * 
     &                                          -           RK( 581 ) * 
      PD( NO              ,O3P              ) = -           RK( 004 ) * 
     &                                          +           RK( 005 ) * 
      PD( NO              ,O3               ) = -           RK( 007 ) * 
      PD( NO              ,NO3              ) = -           RK( 009 ) * 
     &                                          +           RK( 015 ) * 
     &                                          +           RK( 016 )   
      PD( NO              ,OH               ) = -           RK( 022 ) * 
      PD( NO              ,HONO             ) =             RK( 023 )   
      PD( NO              ,HO2              ) = -           RK( 031 ) * 
      PD( NO              ,MEO2             ) = -           RK( 050 ) * 
      PD( NO              ,RO2C             ) = -           RK( 056 ) * 
      PD( NO              ,RO2XC            ) = -           RK( 061 ) * 
      PD( NO              ,MECO3            ) = -           RK( 070 ) * 
      PD( NO              ,RCO3             ) = -           RK( 080 ) * 
      PD( NO              ,BZCO3            ) = -           RK( 091 ) * 
      PD( NO              ,MACO3            ) = -           RK( 103 ) * 
      PD( NO              ,CL               ) = -           RK( 571 ) * 
      PD( NO              ,CLNO             ) =             RK( 572 )   
      PD( NO              ,CLO              ) = -           RK( 581 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   3: O3P                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O3P             ,NO2              ) =             RK( 001 )   
     &                                          -           RK( 005 ) * 
     &                                          -           RK( 006 ) * 
      PD( O3P             ,NO               ) = -           RK( 004 ) * 
      PD( O3P             ,O3P              ) = -           RK( 002 )   
     &                                          -           RK( 003 ) * 
     &                                          -           RK( 004 ) * 
     &                                          -           RK( 005 ) * 
     &                                          -           RK( 006 ) * 
     &                                          -           RK( 261 ) * 
     &                                          -           RK( 265 ) * 
     &                                          -           RK( 281 ) * 
     &                                          -           RK( 521 ) * 
     &                                          -           RK( 525 ) * 
     &                                          -           RK( 529 ) * 
     &                                          -           RK( 533 ) * 
     &                                          -           RK( 537 ) * 
     &                                          -           RK( 555 ) * 
     &                                          -           RK( 559 ) * 
     &                                          -           RK( 565 ) * 
     &                                          -           RK( 569 ) * 
      PD( O3P             ,O3               ) = -           RK( 003 ) * 
     &                                          +           RK( 019 )   
      PD( O3P             ,NO3              ) =             RK( 017 )   
      PD( O3P             ,O1D              ) =             RK( 021 )   
      PD( O3P             ,MACR             ) = -           RK( 261 ) * 
      PD( O3P             ,MVK              ) = -           RK( 265 ) * 
      PD( O3P             ,ACROLEIN         ) = -           RK( 281 ) * 
      PD( O3P             ,ETHENE           ) = -           RK( 521 ) * 
      PD( O3P             ,PROPENE          ) = -           RK( 525 ) * 
      PD( O3P             ,BUTADIENE13      ) = -           RK( 529 ) * 
      PD( O3P             ,ISOPRENE         ) = -           RK( 533 ) * 
      PD( O3P             ,APIN             ) = -           RK( 537 ) * 
      PD( O3P             ,OLE1             ) = -           RK( 555 ) * 
      PD( O3P             ,OLE2             ) = -           RK( 559 ) * 
      PD( O3P             ,TERP             ) = -           RK( 565 ) * 
      PD( O3P             ,SESQ             ) = -           RK( 569 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   4: O3                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O3              ,NO2              ) = -           RK( 008 ) * 
      PD( O3              ,NO               ) = -           RK( 007 ) * 
      PD( O3              ,O3P              ) =             RK( 002 )   
     &                                          -           RK( 003 ) * 
      PD( O3              ,O3               ) = -           RK( 003 ) * 
     &                                          -           RK( 007 ) * 
     &                                          -           RK( 008 ) * 
     &                                          -           RK( 018 )   
     &                                          -           RK( 019 )   
     &                                          -           RK( 030 ) * 
     &                                          -           RK( 036 ) * 
     &                                          -           RK( 251 ) * 
     &                                          -           RK( 254 ) * 
     &                                          -           RK( 257 ) * 
     &                                          -           RK( 259 ) * 
     &                                          -           RK( 264 ) * 
     &                                          -           RK( 268 ) * 
     &                                          -           RK( 279 ) * 
     &                                          -           RK( 519 ) * 
     &                                          -           RK( 523 ) * 
     &                                          -           RK( 527 ) * 
     &                                          -           RK( 531 ) * 
     &                                          -           RK( 535 ) * 
     &                                          -           RK( 539 ) * 
     &                                          -           RK( 553 ) * 
     &                                          -           RK( 557 ) * 
     &                                          -           RK( 563 ) * 
     &                                          -           RK( 567 ) * 
     &                                          -           RK( 579 ) * 
      PD( O3              ,OH               ) = -           RK( 030 ) * 
      PD( O3              ,HO2              ) = -           RK( 036 ) * 
     &                                          + 0.30000 * RK( 071 ) * 
     &                                          + 0.25000 * RK( 081 ) * 
     &                                          + 0.25000 * RK( 092 ) * 
     &                                          + 0.25000 * RK( 104 ) * 
      PD( O3              ,MECO3            ) =   0.30000 * RK( 071 ) * 
      PD( O3              ,RCO3             ) =   0.25000 * RK( 081 ) * 
      PD( O3              ,BZCO3            ) =   0.25000 * RK( 092 ) * 
      PD( O3              ,MACO3            ) =   0.25000 * RK( 104 ) * 
      PD( O3              ,IPRD             ) = -           RK( 268 ) * 
      PD( O3              ,AFG1             ) = -           RK( 251 ) * 
      PD( O3              ,AFG2             ) = -           RK( 254 ) * 
      PD( O3              ,AFG3             ) = -           RK( 257 ) * 
      PD( O3              ,MACR             ) = -           RK( 259 ) * 
      PD( O3              ,MVK              ) = -           RK( 264 ) * 
      PD( O3              ,ACROLEIN         ) = -           RK( 279 ) * 
      PD( O3              ,ETHENE           ) = -           RK( 519 ) * 
      PD( O3              ,PROPENE          ) = -           RK( 523 ) * 
      PD( O3              ,BUTADIENE13      ) = -           RK( 527 ) * 
      PD( O3              ,ISOPRENE         ) = -           RK( 531 ) * 
      PD( O3              ,APIN             ) = -           RK( 535 ) * 
      PD( O3              ,ACETYLENE        ) = -           RK( 539 ) * 
      PD( O3              ,OLE1             ) = -           RK( 553 ) * 
      PD( O3              ,OLE2             ) = -           RK( 557 ) * 
      PD( O3              ,TERP             ) = -           RK( 563 ) * 
      PD( O3              ,SESQ             ) = -           RK( 567 ) * 
      PD( O3              ,CL               ) = -           RK( 579 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   5: NO3                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO3             ,NO2              ) =             RK( 006 ) * 
     &                                          +           RK( 008 ) * 
     &                                          -           RK( 011 ) * 
     &                                          -           RK( 015 ) * 
      PD( NO3             ,NO               ) = -           RK( 009 ) * 
      PD( NO3             ,O3P              ) =             RK( 006 ) * 
      PD( NO3             ,O3               ) =             RK( 008 ) * 
      PD( NO3             ,NO3              ) = -           RK( 009 ) * 
     &                                          -           RK( 011 ) * 
     &                                          -           RK( 015 ) * 
     &                                          -           RK( 016 )   
     &                                          -           RK( 017 )   
     &                                          -           RK( 026 ) * 
     &                                          -           RK( 039 ) * 
     &                                          - 4.00000 * RK( 040 ) * 
     &                                          -           RK( 053 ) * 
     &                                          -           RK( 058 ) * 
     &                                          -           RK( 063 ) * 
     &                                          -           RK( 072 ) * 
     &                                          -           RK( 082 ) * 
     &                                          -           RK( 093 ) * 
     &                                          -           RK( 105 ) * 
     &                                          -           RK( 211 ) * 
     &                                          -           RK( 214 ) * 
     &                                          -           RK( 217 ) * 
     &                                          -           RK( 237 ) * 
     &                                          -           RK( 240 ) * 
     &                                          -           RK( 243 ) * 
     &                                          -           RK( 249 ) * 
     &                                          -           RK( 260 ) * 
     &                                          -           RK( 269 ) * 
     &                                          -           RK( 277 ) * 
     &                                          -           RK( 280 ) * 
     &                                          -           RK( 520 ) * 
     &                                          -           RK( 524 ) * 
     &                                          -           RK( 528 ) * 
     &                                          -           RK( 532 ) * 
     &                                          -           RK( 536 ) * 
     &                                          -           RK( 554 ) * 
     &                                          -           RK( 558 ) * 
     &                                          -           RK( 564 ) * 
     &                                          -           RK( 568 ) * 
     &                                          -           RK( 580 ) * 
      PD( NO3             ,N2O5             ) =             RK( 012 )   
      PD( NO3             ,HNO3             ) =             RK( 027 ) * 
      PD( NO3             ,OH               ) = -           RK( 026 ) * 
     &                                          +           RK( 027 ) * 
      PD( NO3             ,HO2              ) = -           RK( 039 ) * 
      PD( NO3             ,HNO4             ) =   0.39000 * RK( 034 )   
      PD( NO3             ,MEO2             ) = -           RK( 053 ) * 
      PD( NO3             ,HCHO             ) = -           RK( 211 ) * 
      PD( NO3             ,RO2C             ) = -           RK( 058 ) * 
      PD( NO3             ,RO2XC            ) = -           RK( 063 ) * 
      PD( NO3             ,MECO3            ) = -           RK( 072 ) * 
      PD( NO3             ,PAN              ) =   0.40000 * RK( 069 )   
      PD( NO3             ,RCO3             ) = -           RK( 082 ) * 
      PD( NO3             ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( NO3             ,BZCO3            ) = -           RK( 093 ) * 
      PD( NO3             ,PBZN             ) =   0.40000 * RK( 090 )   
      PD( NO3             ,MACO3            ) = -           RK( 105 ) * 
      PD( NO3             ,MAPAN            ) =   0.40000 * RK( 102 )   
      PD( NO3             ,CRES             ) = -           RK( 243 ) * 
      PD( NO3             ,CCHO             ) = -           RK( 214 ) * 
      PD( NO3             ,RCHO             ) = -           RK( 217 ) * 
      PD( NO3             ,MGLY             ) = -           RK( 240 ) * 
      PD( NO3             ,IPRD             ) = -           RK( 269 ) * 
      PD( NO3             ,GLY              ) = -           RK( 237 ) * 
      PD( NO3             ,BALD             ) = -           RK( 249 ) * 
      PD( NO3             ,MACR             ) = -           RK( 260 ) * 
      PD( NO3             ,HOCCHO           ) = -           RK( 277 ) * 
      PD( NO3             ,ACROLEIN         ) = -           RK( 280 ) * 
      PD( NO3             ,ETHENE           ) = -           RK( 520 ) * 
      PD( NO3             ,PROPENE          ) = -           RK( 524 ) * 
      PD( NO3             ,BUTADIENE13      ) = -           RK( 528 ) * 
      PD( NO3             ,ISOPRENE         ) = -           RK( 532 ) * 
      PD( NO3             ,APIN             ) = -           RK( 536 ) * 
      PD( NO3             ,OLE1             ) = -           RK( 554 ) * 
      PD( NO3             ,OLE2             ) = -           RK( 558 ) * 
      PD( NO3             ,TERP             ) = -           RK( 564 ) * 
      PD( NO3             ,SESQ             ) = -           RK( 568 ) * 
      PD( NO3             ,CL               ) = -           RK( 580 ) * 
     &                                          +           RK( 586 ) * 
      PD( NO3             ,CLONO2           ) =             RK( 584 )   
     &                                          +           RK( 586 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   6: N2O5                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( N2O5            ,NO2              ) =             RK( 011 ) * 
      PD( N2O5            ,NO3              ) =             RK( 011 ) * 
      PD( N2O5            ,N2O5             ) = -           RK( 012 )   
     &                                          -           RK( 013 )   
     &                                          -           RK( 014 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   7: HNO3                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HNO3            ,NO2              ) =             RK( 025 ) * 
      PD( HNO3            ,NO3              ) =   0.20000 * RK( 039 ) * 
     &                                          +           RK( 211 ) * 
     &                                          +           RK( 214 ) * 
     &                                          +           RK( 217 ) * 
     &                                          +           RK( 237 ) * 
     &                                          +           RK( 240 ) * 
     &                                          +           RK( 243 ) * 
     &                                          +           RK( 249 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.15000 * RK( 269 ) * 
     &                                          +           RK( 277 ) * 
     &                                          + 0.96700 * RK( 280 ) * 
      PD( HNO3            ,N2O5             ) =   2.00000 * RK( 013 )   
     &                                          + 2.00000 * RK( 014 )   
      PD( HNO3            ,HNO3             ) = -           RK( 027 ) * 
     &                                          -           RK( 028 )   
      PD( HNO3            ,OH               ) =             RK( 025 ) * 
     &                                          -           RK( 027 ) * 
      PD( HNO3            ,HO2              ) =   0.20000 * RK( 039 ) * 
      PD( HNO3            ,HCHO             ) =             RK( 211 ) * 
      PD( HNO3            ,CRES             ) =             RK( 243 ) * 
      PD( HNO3            ,CCHO             ) =             RK( 214 ) * 
      PD( HNO3            ,RCHO             ) =             RK( 217 ) * 
      PD( HNO3            ,MGLY             ) =             RK( 240 ) * 
      PD( HNO3            ,IPRD             ) =   0.15000 * RK( 269 ) * 
      PD( HNO3            ,GLY              ) =             RK( 237 ) * 
      PD( HNO3            ,BALD             ) =             RK( 249 ) * 
      PD( HNO3            ,MACR             ) =   0.50000 * RK( 260 ) * 
      PD( HNO3            ,HOCCHO           ) =             RK( 277 ) * 
      PD( HNO3            ,ACROLEIN         ) =   0.96700 * RK( 280 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   8: O1D                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O1D             ,O3               ) =             RK( 018 )   
      PD( O1D             ,O1D              ) = -           RK( 020 )   
     &                                          -           RK( 021 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   9: OH                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OH              ,NO2              ) = -           RK( 025 ) * 
      PD( OH              ,NO               ) = -           RK( 022 ) * 
     &                                          +           RK( 031 ) * 
     &                                          +           RK( 128 ) * 
      PD( OH              ,O3               ) = -           RK( 030 ) * 
     &                                          +           RK( 036 ) * 
     &                                          + 0.82600 * RK( 251 ) * 
     &                                          + 0.82600 * RK( 254 ) * 
     &                                          + 0.47100 * RK( 257 ) * 
     &                                          + 0.20800 * RK( 259 ) * 
     &                                          + 0.16400 * RK( 264 ) * 
     &                                          + 0.28500 * RK( 268 ) * 
     &                                          + 0.33000 * RK( 279 ) * 
     &                                          + 0.16000 * RK( 519 ) * 
     &                                          + 0.35000 * RK( 523 ) * 
     &                                          + 0.08000 * RK( 527 ) * 
     &                                          + 0.26600 * RK( 531 ) * 
     &                                          + 0.72800 * RK( 535 ) * 
     &                                          + 0.50000 * RK( 539 ) * 
     &                                          + 0.12800 * RK( 553 ) * 
     &                                          + 0.44300 * RK( 557 ) * 
     &                                          + 0.49900 * RK( 563 ) * 
     &                                          + 0.49900 * RK( 567 ) * 
      PD( OH              ,NO3              ) = -           RK( 026 ) * 
     &                                          + 0.80000 * RK( 039 ) * 
     &                                          +           RK( 130 ) * 
      PD( OH              ,HNO3             ) = -           RK( 027 ) * 
     &                                          +           RK( 028 )   
      PD( OH              ,O1D              ) =   2.00000 * RK( 020 )   
      PD( OH              ,OH               ) = -           RK( 022 ) * 
     &                                          -           RK( 024 ) * 
     &                                          -           RK( 025 ) * 
     &                                          -           RK( 026 ) * 
     &                                          -           RK( 027 ) * 
     &                                          -           RK( 029 ) * 
     &                                          -           RK( 030 ) * 
     &                                          -           RK( 035 ) * 
     &                                          -           RK( 042 ) * 
     &                                          -           RK( 043 ) * 
     &                                          -           RK( 044 ) * 
     &                                          -           RK( 045 )   
     &                                          -           RK( 210 ) * 
     &                                          -           RK( 212 ) * 
     &                                          -           RK( 215 ) * 
     &                                          -           RK( 218 ) * 
     &                                          -           RK( 220 ) * 
     &                                          -           RK( 222 ) * 
     &                                          -           RK( 223 ) * 
     &                                          -           RK( 224 ) * 
     &                                          -           RK( 225 ) * 
     &                                          - 0.70000 * RK( 226 ) * 
     &                                          - 0.25600 * RK( 228 ) * 
     &                                          - 0.16000 * RK( 230 ) * 
     &                                          - 0.86100 * RK( 232 ) * 
     &                                          -           RK( 236 ) * 
     &                                          -           RK( 239 ) * 
     &                                          -           RK( 242 ) * 
     &                                          -           RK( 244 ) * 
     &                                          -           RK( 247 ) * 
     &                                          -           RK( 250 ) * 
     &                                          -           RK( 253 ) * 
     &                                          -           RK( 256 ) * 
     &                                          -           RK( 258 ) * 
     &                                          -           RK( 263 ) * 
     &                                          -           RK( 267 ) * 
     &                                          -           RK( 271 ) * 
     &                                          -           RK( 273 ) * 
     &                                          -           RK( 275 ) * 
     &                                          -           RK( 278 ) * 
     &                                          -           RK( 283 ) * 
     &                                          -           RK( 285 ) * 
     &                                          -           RK( 517 )   
     &                                          -           RK( 518 ) * 
     &                                          -           RK( 522 ) * 
     &                                          -           RK( 526 ) * 
     &                                          -           RK( 530 ) * 
     &                                          -           RK( 534 ) * 
     &                                          - 0.30000 * RK( 538 ) * 
     &                                          - 0.88400 * RK( 540 ) * 
     &                                          - 0.68800 * RK( 541 ) * 
     &                                          - 0.76100 * RK( 542 ) * 
     &                                          - 0.80200 * RK( 543 ) * 
     &                                          - 0.72200 * RK( 544 ) * 
     &                                          - 0.77000 * RK( 545 ) * 
     &                                          -           RK( 546 ) * 
     &                                          -           RK( 547 ) * 
     &                                          -           RK( 548 ) * 
     &                                          -           RK( 549 ) * 
     &                                          -           RK( 550 ) * 
     &                                          -           RK( 551 ) * 
     &                                          -           RK( 552 ) * 
     &                                          -           RK( 556 ) * 
     &                                          - 0.79800 * RK( 560 ) * 
     &                                          - 0.82200 * RK( 561 ) * 
     &                                          -           RK( 562 ) * 
     &                                          -           RK( 566 ) * 
     &                                          -           RK( 590 ) * 
     &                                          -           RK( 612 ) * 
      PD( OH              ,HONO             ) =             RK( 023 )   
     &                                          -           RK( 024 ) * 
      PD( OH              ,HO2              ) =             RK( 031 ) * 
     &                                          +           RK( 036 ) * 
     &                                          + 0.80000 * RK( 039 ) * 
     &                                          -           RK( 043 ) * 
     &                                          +           RK( 578 ) * 
      PD( OH              ,CO               ) = -           RK( 029 ) * 
      PD( OH              ,HNO4             ) =   0.39000 * RK( 034 )   
     &                                          -           RK( 035 ) * 
      PD( OH              ,HO2H             ) =   2.00000 * RK( 041 )   
     &                                          -           RK( 042 ) * 
      PD( OH              ,SO2              ) = -           RK( 044 ) * 
      PD( OH              ,NO2EX            ) =             RK( 049 )   
      PD( OH              ,MEO2             ) =   0.50000 * RK( 131 ) * 
      PD( OH              ,HCHO             ) = -           RK( 210 ) * 
      PD( OH              ,COOH             ) = - 0.70000 * RK( 226 ) * 
     &                                          +           RK( 227 )   
      PD( OH              ,MEOH             ) = -           RK( 222 ) * 
      PD( OH              ,RO2C             ) =   0.50000 * RK( 132 ) * 
      PD( OH              ,RO2XC            ) =   0.50000 * RK( 133 ) * 
      PD( OH              ,MECO3            ) =             RK( 134 ) * 
      PD( OH              ,CCOOOH           ) = -           RK( 283 ) * 
     &                                          +           RK( 284 )   
      PD( OH              ,CCOOH            ) = -           RK( 224 ) * 
      PD( OH              ,RCO3             ) =             RK( 135 ) * 
      PD( OH              ,RCOOOH           ) = -           RK( 285 ) * 
     &                                          +           RK( 286 )   
      PD( OH              ,RCOOH            ) = -           RK( 225 ) * 
      PD( OH              ,BZCO3            ) =             RK( 136 ) * 
      PD( OH              ,MACO3            ) =             RK( 137 ) * 
      PD( OH              ,RNO3             ) = -           RK( 273 ) * 
      PD( OH              ,ACETONE          ) = -           RK( 218 ) * 
      PD( OH              ,NPHE             ) = -           RK( 244 ) * 
      PD( OH              ,CRES             ) = -           RK( 242 ) * 
      PD( OH              ,xOH              ) =             RK( 128 ) * 
     &                                          +           RK( 130 ) * 
     &                                          + 0.50000 * RK( 131 ) * 
     &                                          + 0.50000 * RK( 132 ) * 
     &                                          + 0.50000 * RK( 133 ) * 
     &                                          +           RK( 134 ) * 
     &                                          +           RK( 135 ) * 
     &                                          +           RK( 136 ) * 
     &                                          +           RK( 137 ) * 
      PD( OH              ,CCHO             ) = -           RK( 212 ) * 
      PD( OH              ,RCHO             ) = -           RK( 215 ) * 
      PD( OH              ,MEK              ) = -           RK( 220 ) * 
      PD( OH              ,HCOOH            ) = -           RK( 223 ) * 
      PD( OH              ,ROOH             ) = - 0.25600 * RK( 228 ) * 
     &                                          +           RK( 229 )   
     &                                          + 0.41400 * RK( 604 ) * 
      PD( OH              ,R6OOH            ) = - 0.16000 * RK( 230 ) * 
     &                                          +           RK( 231 )   
     &                                          + 0.14500 * RK( 605 ) * 
      PD( OH              ,PRD2             ) = -           RK( 271 ) * 
      PD( OH              ,RAOOH            ) = - 0.86100 * RK( 232 ) * 
     &                                          +           RK( 233 )   
     &                                          + 0.13900 * RK( 606 ) * 
      PD( OH              ,MGLY             ) = -           RK( 239 ) * 
      PD( OH              ,IPRD             ) = -           RK( 267 ) * 
     &                                          + 0.28500 * RK( 268 ) * 
      PD( OH              ,GLY              ) = -           RK( 236 ) * 
      PD( OH              ,AFG1             ) = -           RK( 250 ) * 
     &                                          + 0.82600 * RK( 251 ) * 
      PD( OH              ,AFG2             ) = -           RK( 253 ) * 
     &                                          + 0.82600 * RK( 254 ) * 
      PD( OH              ,BALD             ) = -           RK( 247 ) * 
      PD( OH              ,AFG3             ) = -           RK( 256 ) * 
     &                                          + 0.47100 * RK( 257 ) * 
      PD( OH              ,MACR             ) = -           RK( 258 ) * 
     &                                          + 0.20800 * RK( 259 ) * 
     &                                          + 0.33000 * RK( 262 )   
      PD( OH              ,MVK              ) = -           RK( 263 ) * 
     &                                          + 0.16400 * RK( 264 ) * 
      PD( OH              ,HOCCHO           ) = -           RK( 275 ) * 
      PD( OH              ,ACROLEIN         ) = -           RK( 278 ) * 
     &                                          + 0.33000 * RK( 279 ) * 
     &                                          + 0.17800 * RK( 282 )   
      PD( OH              ,ETHENE           ) = -           RK( 518 ) * 
     &                                          + 0.16000 * RK( 519 ) * 
      PD( OH              ,PROPENE          ) = -           RK( 522 ) * 
     &                                          + 0.35000 * RK( 523 ) * 
      PD( OH              ,BUTADIENE13      ) = -           RK( 526 ) * 
     &                                          + 0.08000 * RK( 527 ) * 
      PD( OH              ,ISOPRENE         ) = -           RK( 530 ) * 
     &                                          + 0.26600 * RK( 531 ) * 
      PD( OH              ,APIN             ) = -           RK( 534 ) * 
     &                                          + 0.72800 * RK( 535 ) * 
      PD( OH              ,ACETYLENE        ) = - 0.30000 * RK( 538 ) * 
     &                                          + 0.50000 * RK( 539 ) * 
      PD( OH              ,BENZENE          ) = - 0.88400 * RK( 540 ) * 
      PD( OH              ,TOLUENE          ) = - 0.68800 * RK( 541 ) * 
      PD( OH              ,MXYL             ) = - 0.76100 * RK( 542 ) * 
      PD( OH              ,OXYL             ) = - 0.80200 * RK( 543 ) * 
      PD( OH              ,PXYL             ) = - 0.72200 * RK( 544 ) * 
      PD( OH              ,TRIMETH_BENZ124  ) = - 0.77000 * RK( 545 ) * 
      PD( OH              ,ETOH             ) = -           RK( 546 ) * 
      PD( OH              ,ALK1             ) = -           RK( 547 ) * 
      PD( OH              ,ALK2             ) = -           RK( 548 ) * 
      PD( OH              ,ALK3             ) = -           RK( 549 ) * 
      PD( OH              ,ALK4             ) = -           RK( 550 ) * 
      PD( OH              ,ALK5             ) = -           RK( 551 ) * 
      PD( OH              ,OLE1             ) = -           RK( 552 ) * 
     &                                          + 0.12800 * RK( 553 ) * 
      PD( OH              ,OLE2             ) = -           RK( 556 ) * 
     &                                          + 0.44300 * RK( 557 ) * 
      PD( OH              ,ARO1             ) = - 0.79800 * RK( 560 ) * 
      PD( OH              ,ARO2             ) = - 0.82200 * RK( 561 ) * 
      PD( OH              ,TERP             ) = -           RK( 562 ) * 
     &                                          + 0.49900 * RK( 563 ) * 
      PD( OH              ,SESQ             ) = -           RK( 566 ) * 
     &                                          + 0.49900 * RK( 567 ) * 
      PD( OH              ,CL               ) =             RK( 578 ) * 
     &                                          + 0.41400 * RK( 604 ) * 
     &                                          + 0.14500 * RK( 605 ) * 
     &                                          + 0.13900 * RK( 606 ) * 
      PD( OH              ,HCL              ) = -           RK( 590 ) * 
      PD( OH              ,HOCL             ) =             RK( 588 )   
      PD( OH              ,CLCCHO           ) = -           RK( 612 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  10: HONO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HONO            ,NO               ) =             RK( 022 ) * 
      PD( HONO            ,OH               ) =             RK( 022 ) * 
     &                                          -           RK( 024 ) * 
      PD( HONO            ,HONO             ) = -           RK( 023 )   
     &                                          -           RK( 024 ) * 
      PD( HONO            ,NO2EX            ) =             RK( 049 )   
      PD( HONO            ,NPHE             ) =             RK( 245 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  11: HO2                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HO2             ,NO2              ) = -           RK( 032 ) * 
      PD( HO2             ,NO               ) = -           RK( 031 ) * 
     &                                          +           RK( 050 ) * 
     &                                          +           RK( 118 ) * 
      PD( HO2             ,O3P              ) =   0.80000 * RK( 521 ) * 
     &                                          + 0.25000 * RK( 529 ) * 
      PD( HO2             ,O3               ) =             RK( 030 ) * 
     &                                          -           RK( 036 ) * 
     &                                          + 0.52200 * RK( 251 ) * 
     &                                          + 0.52200 * RK( 254 ) * 
     &                                          + 0.55400 * RK( 257 ) * 
     &                                          + 0.10800 * RK( 259 ) * 
     &                                          + 0.06400 * RK( 264 ) * 
     &                                          + 0.40000 * RK( 268 ) * 
     &                                          + 0.83000 * RK( 279 ) * 
     &                                          + 0.16000 * RK( 519 ) * 
     &                                          + 0.16500 * RK( 523 ) * 
     &                                          + 0.08000 * RK( 527 ) * 
     &                                          + 0.06600 * RK( 531 ) * 
     &                                          + 0.00900 * RK( 535 ) * 
     &                                          + 1.50000 * RK( 539 ) * 
     &                                          + 0.09500 * RK( 553 ) * 
     &                                          + 0.09400 * RK( 557 ) * 
     &                                          + 0.07800 * RK( 563 ) * 
     &                                          + 0.07800 * RK( 567 ) * 
      PD( HO2             ,NO3              ) =             RK( 026 ) * 
     &                                          -           RK( 039 ) * 
     &                                          +           RK( 053 ) * 
     &                                          +           RK( 120 ) * 
     &                                          +           RK( 211 ) * 
     &                                          + 0.63000 * RK( 237 ) * 
     &                                          +           RK( 489 ) * 
      PD( HO2             ,OH               ) =             RK( 026 ) * 
     &                                          +           RK( 029 ) * 
     &                                          +           RK( 030 ) * 
     &                                          +           RK( 042 ) * 
     &                                          -           RK( 043 ) * 
     &                                          +           RK( 044 ) * 
     &                                          +           RK( 045 )   
     &                                          +           RK( 210 ) * 
     &                                          +           RK( 222 ) * 
     &                                          +           RK( 223 ) * 
     &                                          + 0.14800 * RK( 232 ) * 
     &                                          + 0.63000 * RK( 236 ) * 
     &                                          + 0.47200 * RK( 271 ) * 
     &                                          + 0.18900 * RK( 273 ) * 
     &                                          + 0.30000 * RK( 538 ) * 
     &                                          + 0.57000 * RK( 540 ) * 
     &                                          + 0.18100 * RK( 541 ) * 
     &                                          + 0.15900 * RK( 542 ) * 
     &                                          + 0.16100 * RK( 543 ) * 
     &                                          + 0.15900 * RK( 544 ) * 
     &                                          + 0.02200 * RK( 545 ) * 
     &                                          + 0.95000 * RK( 546 ) * 
     &                                          + 0.12300 * RK( 560 ) * 
     &                                          + 0.07700 * RK( 561 ) * 
      PD( HO2             ,HO2              ) = -           RK( 031 ) * 
     &                                          -           RK( 032 ) * 
     &                                          -           RK( 036 ) * 
     &                                          - 4.00000 * RK( 037 ) * 
     &                                          - 4.00000 * RK( 038 ) * 
     &                                          -           RK( 039 ) * 
     &                                          -           RK( 043 ) * 
     &                                          -           RK( 051 ) * 
     &                                          -           RK( 052 ) * 
     &                                          -           RK( 057 ) * 
     &                                          -           RK( 062 ) * 
     &                                          -           RK( 071 ) * 
     &                                          -           RK( 081 ) * 
     &                                          -           RK( 092 ) * 
     &                                          -           RK( 104 ) * 
     &                                          -           RK( 116 ) * 
     &                                          -           RK( 577 ) * 
     &                                          -           RK( 578 ) * 
     &                                          -           RK( 587 ) * 
      PD( HO2             ,CO               ) =             RK( 029 ) * 
      PD( HO2             ,HNO4             ) =             RK( 033 )   
     &                                          + 0.61000 * RK( 034 )   
      PD( HO2             ,HO2H             ) =             RK( 042 ) * 
      PD( HO2             ,SO2              ) =             RK( 044 ) * 
      PD( HO2             ,MEO2             ) =             RK( 050 ) * 
     &                                          -           RK( 051 ) * 
     &                                          -           RK( 052 ) * 
     &                                          +           RK( 053 ) * 
     &                                          + 4.00000 * RK( 055 ) * 
     &                                          + 0.50000 * RK( 059 ) * 
     &                                          + 0.50000 * RK( 064 ) * 
     &                                          + 0.90000 * RK( 073 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 106 ) * 
     &                                          + 0.50000 * RK( 121 ) * 
     &                                          + 0.50000 * RK( 490 ) * 
      PD( HO2             ,HCHO             ) =   2.00000 * RK( 208 )   
     &                                          +           RK( 210 ) * 
     &                                          +           RK( 211 ) * 
     &                                          +           RK( 592 ) * 
      PD( HO2             ,COOH             ) =             RK( 227 )   
      PD( HO2             ,MEOH             ) =             RK( 222 ) * 
     &                                          +           RK( 594 ) * 
      PD( HO2             ,RO2C             ) = -           RK( 057 ) * 
     &                                          + 0.50000 * RK( 059 ) * 
     &                                          + 0.50000 * RK( 122 ) * 
     &                                          + 0.50000 * RK( 491 ) * 
      PD( HO2             ,RO2XC            ) = -           RK( 062 ) * 
     &                                          + 0.50000 * RK( 064 ) * 
     &                                          + 0.50000 * RK( 123 ) * 
     &                                          + 0.50000 * RK( 492 ) * 
      PD( HO2             ,MECO3            ) = -           RK( 071 ) * 
     &                                          + 0.90000 * RK( 073 ) * 
     &                                          +           RK( 124 ) * 
     &                                          +           RK( 493 ) * 
      PD( HO2             ,RCO3             ) = -           RK( 081 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 125 ) * 
     &                                          +           RK( 494 ) * 
      PD( HO2             ,xHO2             ) =             RK( 118 ) * 
     &                                          +           RK( 120 ) * 
     &                                          + 0.50000 * RK( 121 ) * 
     &                                          + 0.50000 * RK( 122 ) * 
     &                                          + 0.50000 * RK( 123 ) * 
     &                                          +           RK( 124 ) * 
     &                                          +           RK( 125 ) * 
     &                                          +           RK( 126 ) * 
     &                                          +           RK( 127 ) * 
      PD( HO2             ,BZCO3            ) = -           RK( 092 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 126 ) * 
     &                                          +           RK( 495 ) * 
      PD( HO2             ,BZO              ) = -           RK( 116 ) * 
      PD( HO2             ,MACO3            ) = -           RK( 104 ) * 
     &                                          +           RK( 106 ) * 
     &                                          +           RK( 127 ) * 
     &                                          +           RK( 496 ) * 
      PD( HO2             ,RNO3             ) =   0.18900 * RK( 273 ) * 
     &                                          + 0.34400 * RK( 274 )   
     &                                          + 0.05500 * RK( 598 ) * 
      PD( HO2             ,CCHO             ) =             RK( 213 )   
      PD( HO2             ,RCHO             ) =             RK( 216 )   
      PD( HO2             ,zRNO3            ) =             RK( 489 ) * 
     &                                          + 0.50000 * RK( 490 ) * 
     &                                          + 0.50000 * RK( 491 ) * 
     &                                          + 0.50000 * RK( 492 ) * 
     &                                          +           RK( 493 ) * 
     &                                          +           RK( 494 ) * 
     &                                          +           RK( 495 ) * 
     &                                          +           RK( 496 ) * 
      PD( HO2             ,HCOOH            ) =             RK( 223 ) * 
      PD( HO2             ,ROOH             ) =             RK( 229 )   
      PD( HO2             ,R6OOH            ) =   0.14200 * RK( 231 )   
      PD( HO2             ,PRD2             ) =   0.47200 * RK( 271 ) * 
     &                                          + 0.31400 * RK( 599 ) * 
      PD( HO2             ,RAOOH            ) =   0.14800 * RK( 232 ) * 
     &                                          +           RK( 233 )   
     &                                          + 0.14800 * RK( 606 ) * 
      PD( HO2             ,MGLY             ) =             RK( 238 )   
      PD( HO2             ,IPRD             ) =   0.40000 * RK( 268 ) * 
     &                                          + 1.23300 * RK( 270 )   
     &                                          + 0.08400 * RK( 610 ) * 
      PD( HO2             ,GLY              ) =   2.00000 * RK( 234 )   
     &                                          + 0.63000 * RK( 236 ) * 
     &                                          + 0.63000 * RK( 237 ) * 
     &                                          + 0.63000 * RK( 600 ) * 
      PD( HO2             ,AFG1             ) =   0.52200 * RK( 251 ) * 
     &                                          + 1.02300 * RK( 252 )   
      PD( HO2             ,AFG2             ) =   0.52200 * RK( 254 ) * 
      PD( HO2             ,AFG3             ) =   0.55400 * RK( 257 ) * 
      PD( HO2             ,MACR             ) =   0.10800 * RK( 259 ) * 
     &                                          + 0.67000 * RK( 262 )   
      PD( HO2             ,MVK              ) =   0.06400 * RK( 264 ) * 
      PD( HO2             ,HOCCHO           ) =   2.00000 * RK( 276 )   
      PD( HO2             ,ACROLEIN         ) =   0.83000 * RK( 279 ) * 
     &                                          + 1.06600 * RK( 282 )   
      PD( HO2             ,ETHENE           ) =   0.16000 * RK( 519 ) * 
     &                                          + 0.80000 * RK( 521 ) * 
      PD( HO2             ,PROPENE          ) =   0.16500 * RK( 523 ) * 
      PD( HO2             ,BUTADIENE13      ) =   0.08000 * RK( 527 ) * 
     &                                          + 0.25000 * RK( 529 ) * 
      PD( HO2             ,ISOPRENE         ) =   0.06600 * RK( 531 ) * 
      PD( HO2             ,APIN             ) =   0.00900 * RK( 535 ) * 
      PD( HO2             ,ACETYLENE        ) =   0.30000 * RK( 538 ) * 
     &                                          + 1.50000 * RK( 539 ) * 
     &                                          +           RK( 651 ) * 
      PD( HO2             ,BENZENE          ) =   0.57000 * RK( 540 ) * 
      PD( HO2             ,TOLUENE          ) =   0.18100 * RK( 541 ) * 
      PD( HO2             ,MXYL             ) =   0.15900 * RK( 542 ) * 
      PD( HO2             ,OXYL             ) =   0.16100 * RK( 543 ) * 
      PD( HO2             ,PXYL             ) =   0.15900 * RK( 544 ) * 
      PD( HO2             ,TRIMETH_BENZ124  ) =   0.02200 * RK( 545 ) * 
      PD( HO2             ,ETOH             ) =   0.95000 * RK( 546 ) * 
     &                                          + 0.68800 * RK( 657 ) * 
      PD( HO2             ,OLE1             ) =   0.09500 * RK( 553 ) * 
      PD( HO2             ,OLE2             ) =   0.09400 * RK( 557 ) * 
      PD( HO2             ,ARO1             ) =   0.12300 * RK( 560 ) * 
      PD( HO2             ,ARO2             ) =   0.07700 * RK( 561 ) * 
      PD( HO2             ,TERP             ) =   0.07800 * RK( 563 ) * 
      PD( HO2             ,SESQ             ) =   0.07800 * RK( 567 ) * 
      PD( HO2             ,CL               ) = -           RK( 577 ) * 
     &                                          -           RK( 578 ) * 
     &                                          +           RK( 591 )   
     &                                          +           RK( 592 ) * 
     &                                          +           RK( 594 ) * 
     &                                          + 0.05500 * RK( 598 ) * 
     &                                          + 0.31400 * RK( 599 ) * 
     &                                          + 0.63000 * RK( 600 ) * 
     &                                          + 0.14800 * RK( 606 ) * 
     &                                          + 0.08400 * RK( 610 ) * 
     &                                          +           RK( 651 ) * 
     &                                          + 0.68800 * RK( 657 ) * 
      PD( HO2             ,CLO              ) = -           RK( 587 ) * 
      PD( HO2             ,CLCCHO           ) =             RK( 611 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  12: CO                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CO              ,NO               ) =             RK( 198 ) * 
      PD( CO              ,O3P              ) =   0.51000 * RK( 521 ) * 
      PD( CO              ,O3               ) =   0.52200 * RK( 251 ) * 
     &                                          + 0.52200 * RK( 254 ) * 
     &                                          + 0.58000 * RK( 257 ) * 
     &                                          + 0.45000 * RK( 259 ) * 
     &                                          + 0.47500 * RK( 264 ) * 
     &                                          + 0.49800 * RK( 268 ) * 
     &                                          + 1.00500 * RK( 279 ) * 
     &                                          + 0.51000 * RK( 519 ) * 
     &                                          + 0.52500 * RK( 523 ) * 
     &                                          + 0.25500 * RK( 527 ) * 
     &                                          + 0.27500 * RK( 531 ) * 
     &                                          + 0.02900 * RK( 535 ) * 
     &                                          + 1.50000 * RK( 539 ) * 
     &                                          + 0.30300 * RK( 553 ) * 
     &                                          + 0.29900 * RK( 557 ) * 
     &                                          + 0.24900 * RK( 563 ) * 
     &                                          + 0.24900 * RK( 567 ) * 
      PD( CO              ,NO3              ) =             RK( 200 ) * 
     &                                          +           RK( 211 ) * 
     &                                          + 1.26000 * RK( 237 ) * 
     &                                          +           RK( 240 ) * 
      PD( CO              ,OH               ) = -           RK( 029 ) * 
     &                                          +           RK( 210 ) * 
     &                                          + 1.26000 * RK( 236 ) * 
     &                                          +           RK( 239 ) * 
     &                                          + 0.30000 * RK( 538 ) * 
      PD( CO              ,CO               ) = -           RK( 029 ) * 
      PD( CO              ,MEO2             ) =   0.50000 * RK( 201 ) * 
      PD( CO              ,HCHO             ) =             RK( 208 )   
     &                                          +           RK( 209 )   
     &                                          +           RK( 210 ) * 
     &                                          +           RK( 211 ) * 
     &                                          +           RK( 592 ) * 
      PD( CO              ,RO2C             ) =   0.50000 * RK( 202 ) * 
      PD( CO              ,RO2XC            ) =   0.50000 * RK( 203 ) * 
      PD( CO              ,MECO3            ) =             RK( 204 ) * 
      PD( CO              ,RCO3             ) =             RK( 205 ) * 
      PD( CO              ,BZCO3            ) =             RK( 206 ) * 
      PD( CO              ,MACO3            ) =             RK( 207 ) * 
      PD( CO              ,ACETONE          ) =   0.38000 * RK( 219 )   
      PD( CO              ,xCO              ) =             RK( 198 ) * 
     &                                          +           RK( 200 ) * 
     &                                          + 0.50000 * RK( 201 ) * 
     &                                          + 0.50000 * RK( 202 ) * 
     &                                          + 0.50000 * RK( 203 ) * 
     &                                          +           RK( 204 ) * 
     &                                          +           RK( 205 ) * 
     &                                          +           RK( 206 ) * 
     &                                          +           RK( 207 ) * 
      PD( CO              ,CCHO             ) =             RK( 213 )   
      PD( CO              ,RCHO             ) =             RK( 216 )   
      PD( CO              ,MGLY             ) =             RK( 238 )   
     &                                          +           RK( 239 ) * 
     &                                          +           RK( 240 ) * 
     &                                          +           RK( 601 ) * 
      PD( CO              ,IPRD             ) =   0.49800 * RK( 268 ) * 
     &                                          + 1.23300 * RK( 270 )   
      PD( CO              ,GLY              ) =   2.00000 * RK( 234 )   
     &                                          +           RK( 235 )   
     &                                          + 1.26000 * RK( 236 ) * 
     &                                          + 1.26000 * RK( 237 ) * 
     &                                          + 1.26000 * RK( 600 ) * 
      PD( CO              ,AFG1             ) =   0.52200 * RK( 251 ) * 
     &                                          + 0.69500 * RK( 252 )   
      PD( CO              ,AFG2             ) =   0.52200 * RK( 254 ) * 
      PD( CO              ,AFG3             ) =   0.58000 * RK( 257 ) * 
      PD( CO              ,MACR             ) =   0.45000 * RK( 259 ) * 
     &                                          + 0.67000 * RK( 262 )   
      PD( CO              ,MVK              ) =   0.47500 * RK( 264 ) * 
     &                                          + 0.60000 * RK( 266 )   
      PD( CO              ,HOCCHO           ) =             RK( 276 )   
      PD( CO              ,ACROLEIN         ) =   1.00500 * RK( 279 ) * 
     &                                          + 1.18800 * RK( 282 )   
      PD( CO              ,ETHENE           ) =   0.51000 * RK( 519 ) * 
     &                                          + 0.51000 * RK( 521 ) * 
      PD( CO              ,PROPENE          ) =   0.52500 * RK( 523 ) * 
      PD( CO              ,BUTADIENE13      ) =   0.25500 * RK( 527 ) * 
      PD( CO              ,ISOPRENE         ) =   0.27500 * RK( 531 ) * 
      PD( CO              ,APIN             ) =   0.02900 * RK( 535 ) * 
      PD( CO              ,ACETYLENE        ) =   0.30000 * RK( 538 ) * 
     &                                          + 1.50000 * RK( 539 ) * 
     &                                          +           RK( 651 ) * 
      PD( CO              ,OLE1             ) =   0.30300 * RK( 553 ) * 
      PD( CO              ,OLE2             ) =   0.29900 * RK( 557 ) * 
      PD( CO              ,TERP             ) =   0.24900 * RK( 563 ) * 
      PD( CO              ,SESQ             ) =   0.24900 * RK( 567 ) * 
      PD( CO              ,CL               ) =             RK( 592 ) * 
     &                                          + 1.26000 * RK( 600 ) * 
     &                                          +           RK( 601 ) * 
     &                                          +           RK( 651 ) * 
      PD( CO              ,CLCCHO           ) =             RK( 611 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  13: CO2                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CO2             ,NO               ) =             RK( 070 ) * 
     &                                          +           RK( 080 ) * 
     &                                          +           RK( 091 ) * 
     &                                          +           RK( 103 ) * 
      PD( CO2             ,O3               ) =   0.17400 * RK( 251 ) * 
     &                                          + 0.17400 * RK( 254 ) * 
     &                                          + 0.19000 * RK( 257 ) * 
     &                                          + 0.11700 * RK( 259 ) * 
     &                                          + 0.12400 * RK( 264 ) * 
     &                                          + 0.14000 * RK( 268 ) * 
     &                                          + 0.31000 * RK( 279 ) * 
     &                                          + 0.12000 * RK( 519 ) * 
     &                                          + 0.21500 * RK( 523 ) * 
     &                                          + 0.18500 * RK( 527 ) * 
     &                                          + 0.12200 * RK( 531 ) * 
     &                                          + 0.01700 * RK( 535 ) * 
     &                                          + 0.50000 * RK( 539 ) * 
     &                                          + 0.08800 * RK( 553 ) * 
     &                                          + 0.16100 * RK( 557 ) * 
     &                                          + 0.06300 * RK( 563 ) * 
     &                                          + 0.06300 * RK( 567 ) * 
      PD( CO2             ,NO3              ) =             RK( 072 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 105 ) * 
      PD( CO2             ,OH               ) =             RK( 029 ) * 
     &                                          +           RK( 223 ) * 
     &                                          + 0.50900 * RK( 224 ) * 
     &                                          + 0.14300 * RK( 225 ) * 
     &                                          + 0.02000 * RK( 283 ) * 
     &                                          + 0.11000 * RK( 285 ) * 
      PD( CO2             ,CO               ) =             RK( 029 ) * 
      PD( CO2             ,MEO2             ) =   0.90000 * RK( 073 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 106 ) * 
      PD( CO2             ,RO2C             ) =             RK( 074 ) * 
     &                                          +           RK( 084 ) * 
     &                                          +           RK( 095 ) * 
     &                                          +           RK( 107 ) * 
      PD( CO2             ,RO2XC            ) =             RK( 075 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 096 ) * 
     &                                          +           RK( 108 ) * 
      PD( CO2             ,MECO3            ) =             RK( 070 ) * 
     &                                          +           RK( 072 ) * 
     &                                          + 0.90000 * RK( 073 ) * 
     &                                          +           RK( 074 ) * 
     &                                          +           RK( 075 ) * 
     &                                          + 4.00000 * RK( 076 ) * 
     &                                          + 2.00000 * RK( 086 ) * 
     &                                          + 2.00000 * RK( 097 ) * 
     &                                          + 2.00000 * RK( 109 ) * 
      PD( CO2             ,PAN              ) =   0.40000 * RK( 069 )   
      PD( CO2             ,CCOOOH           ) =   0.02000 * RK( 283 ) * 
     &                                          +           RK( 284 )   
      PD( CO2             ,CCOOH            ) =   0.50900 * RK( 224 ) * 
      PD( CO2             ,RCO3             ) =             RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 084 ) * 
     &                                          +           RK( 085 ) * 
     &                                          + 2.00000 * RK( 086 ) * 
     &                                          + 4.00000 * RK( 087 ) * 
     &                                          + 2.00000 * RK( 098 ) * 
     &                                          + 2.00000 * RK( 110 ) * 
      PD( CO2             ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( CO2             ,RCOOOH           ) =   0.11000 * RK( 285 ) * 
     &                                          +           RK( 286 )   
      PD( CO2             ,RCOOH            ) =   0.14300 * RK( 225 ) * 
      PD( CO2             ,BZCO3            ) =             RK( 091 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 095 ) * 
     &                                          +           RK( 096 ) * 
     &                                          + 2.00000 * RK( 097 ) * 
     &                                          + 2.00000 * RK( 098 ) * 
     &                                          + 4.00000 * RK( 099 ) * 
     &                                          + 2.00000 * RK( 111 ) * 
      PD( CO2             ,PBZN             ) =   0.40000 * RK( 090 )   
      PD( CO2             ,MACO3            ) =             RK( 103 ) * 
     &                                          +           RK( 105 ) * 
     &                                          +           RK( 106 ) * 
     &                                          +           RK( 107 ) * 
     &                                          +           RK( 108 ) * 
     &                                          + 2.00000 * RK( 109 ) * 
     &                                          + 2.00000 * RK( 110 ) * 
     &                                          + 2.00000 * RK( 111 ) * 
     &                                          + 4.00000 * RK( 112 ) * 
      PD( CO2             ,MAPAN            ) =   0.40000 * RK( 102 )   
      PD( CO2             ,HCOOH            ) =             RK( 223 ) * 
      PD( CO2             ,IPRD             ) =   0.14000 * RK( 268 ) * 
      PD( CO2             ,AFG1             ) =   0.17400 * RK( 251 ) * 
      PD( CO2             ,AFG2             ) =   0.17400 * RK( 254 ) * 
      PD( CO2             ,AFG3             ) =   0.19000 * RK( 257 ) * 
      PD( CO2             ,MACR             ) =   0.11700 * RK( 259 ) * 
      PD( CO2             ,MVK              ) =   0.12400 * RK( 264 ) * 
      PD( CO2             ,ACROLEIN         ) =   0.31000 * RK( 279 ) * 
     &                                          + 0.10200 * RK( 282 )   
      PD( CO2             ,ETHENE           ) =   0.12000 * RK( 519 ) * 
      PD( CO2             ,PROPENE          ) =   0.21500 * RK( 523 ) * 
      PD( CO2             ,BUTADIENE13      ) =   0.18500 * RK( 527 ) * 
      PD( CO2             ,ISOPRENE         ) =   0.12200 * RK( 531 ) * 
      PD( CO2             ,APIN             ) =   0.01700 * RK( 535 ) * 
      PD( CO2             ,ACETYLENE        ) =   0.50000 * RK( 539 ) * 
      PD( CO2             ,OLE1             ) =   0.08800 * RK( 553 ) * 
      PD( CO2             ,OLE2             ) =   0.16100 * RK( 557 ) * 
      PD( CO2             ,TERP             ) =   0.06300 * RK( 563 ) * 
      PD( CO2             ,SESQ             ) =   0.06300 * RK( 567 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  14: HNO4                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HNO4            ,NO2              ) =             RK( 032 ) * 
      PD( HNO4            ,OH               ) = -           RK( 035 ) * 
      PD( HNO4            ,HO2              ) =             RK( 032 ) * 
      PD( HNO4            ,HNO4             ) = -           RK( 033 )   
     &                                          -           RK( 034 )   
     &                                          -           RK( 035 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  15: HO2H                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HO2H            ,OH               ) = -           RK( 042 ) * 
      PD( HO2H            ,HO2              ) =   4.00000 * RK( 037 ) * 
     &                                          + 4.00000 * RK( 038 ) * 
      PD( HO2H            ,HO2H             ) = -           RK( 041 )   
     &                                          -           RK( 042 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  16: SO2                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SO2             ,OH               ) = -           RK( 044 ) * 
      PD( SO2             ,SO2              ) = -           RK( 044 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  17: SULF                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULF            ,OH               ) =             RK( 044 ) * 
      PD( SULF            ,SO2              ) =             RK( 044 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  18: SULRXN                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULRXN          ,OH               ) =             RK( 044 ) * 
      PD( SULRXN          ,SO2              ) =             RK( 044 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  19: NO2EX                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO2EX           ,NO2              ) =             RK( 046 )   
      PD( NO2EX           ,NO2EX            ) = -           RK( 047 )   
     &                                          -           RK( 048 )   
     &                                          -           RK( 049 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  20: MEO2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEO2            ,NO               ) = -           RK( 050 ) * 
     &                                          +           RK( 070 ) * 
     &                                          +           RK( 148 ) * 
      PD( MEO2            ,O3P              ) =   0.51000 * RK( 521 ) * 
     &                                          + 0.25000 * RK( 533 ) * 
      PD( MEO2            ,O3               ) =   0.35500 * RK( 523 ) * 
     &                                          + 0.30700 * RK( 557 ) * 
      PD( MEO2            ,NO3              ) = -           RK( 053 ) * 
     &                                          +           RK( 072 ) * 
     &                                          +           RK( 150 ) * 
      PD( MEO2            ,OH               ) =   0.50900 * RK( 224 ) * 
     &                                          + 0.70000 * RK( 226 ) * 
     &                                          +           RK( 517 )   
      PD( MEO2            ,HO2              ) = -           RK( 051 ) * 
     &                                          -           RK( 052 ) * 
      PD( MEO2            ,MEO2             ) = -           RK( 050 ) * 
     &                                          -           RK( 051 ) * 
     &                                          -           RK( 052 ) * 
     &                                          -           RK( 053 ) * 
     &                                          - 4.00000 * RK( 054 ) * 
     &                                          - 4.00000 * RK( 055 ) * 
     &                                          -           RK( 059 ) * 
     &                                          -           RK( 064 ) * 
     &                                          - 0.10000 * RK( 073 ) * 
     &                                          -           RK( 083 ) * 
     &                                          -           RK( 094 ) * 
     &                                          -           RK( 106 ) * 
     &                                          - 0.50000 * RK( 151 ) * 
      PD( MEO2            ,COOH             ) =   0.70000 * RK( 226 ) * 
      PD( MEO2            ,RO2C             ) = -           RK( 059 ) * 
     &                                          +           RK( 074 ) * 
     &                                          + 0.50000 * RK( 152 ) * 
      PD( MEO2            ,RO2XC            ) = -           RK( 064 ) * 
     &                                          +           RK( 075 ) * 
     &                                          + 0.50000 * RK( 153 ) * 
      PD( MEO2            ,MECO3            ) =             RK( 070 ) * 
     &                                          +           RK( 072 ) * 
     &                                          - 0.10000 * RK( 073 ) * 
     &                                          +           RK( 074 ) * 
     &                                          +           RK( 075 ) * 
     &                                          + 4.00000 * RK( 076 ) * 
     &                                          +           RK( 086 ) * 
     &                                          +           RK( 097 ) * 
     &                                          +           RK( 109 ) * 
     &                                          +           RK( 154 ) * 
      PD( MEO2            ,PAN              ) =   0.40000 * RK( 069 )   
      PD( MEO2            ,CCOOOH           ) =             RK( 284 )   
      PD( MEO2            ,CCOOH            ) =   0.50900 * RK( 224 ) * 
      PD( MEO2            ,RCO3             ) = -           RK( 083 ) * 
     &                                          +           RK( 086 ) * 
     &                                          +           RK( 155 ) * 
      PD( MEO2            ,BZCO3            ) = -           RK( 094 ) * 
     &                                          +           RK( 097 ) * 
     &                                          +           RK( 156 ) * 
      PD( MEO2            ,MACO3            ) = -           RK( 106 ) * 
     &                                          +           RK( 109 ) * 
     &                                          +           RK( 157 ) * 
      PD( MEO2            ,TBUO             ) =             RK( 114 )   
      PD( MEO2            ,ACETONE          ) =   1.38000 * RK( 219 )   
      PD( MEO2            ,xMEO2            ) =             RK( 148 ) * 
     &                                          +           RK( 150 ) * 
     &                                          - 0.50000 * RK( 151 ) * 
     &                                          + 0.50000 * RK( 152 ) * 
     &                                          + 0.50000 * RK( 153 ) * 
     &                                          +           RK( 154 ) * 
     &                                          +           RK( 155 ) * 
     &                                          +           RK( 156 ) * 
     &                                          +           RK( 157 ) * 
      PD( MEO2            ,CCHO             ) =             RK( 213 )   
      PD( MEO2            ,AFG1             ) =   0.17300 * RK( 252 )   
      PD( MEO2            ,MVK              ) =   0.40000 * RK( 266 )   
      PD( MEO2            ,ACROLEIN         ) =   0.23400 * RK( 282 )   
      PD( MEO2            ,ETHENE           ) =   0.51000 * RK( 521 ) * 
      PD( MEO2            ,PROPENE          ) =   0.35500 * RK( 523 ) * 
      PD( MEO2            ,ISOPRENE         ) =   0.25000 * RK( 533 ) * 
      PD( MEO2            ,OLE2             ) =   0.30700 * RK( 557 ) * 
      PD( MEO2            ,CL               ) =             RK( 645 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  21: HCHO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCHO            ,NO               ) =             RK( 050 ) * 
     &                                          +           RK( 103 ) * 
     &                                          +           RK( 287 ) * 
      PD( HCHO            ,O3               ) =   0.10000 * RK( 259 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.12400 * RK( 268 ) * 
     &                                          + 0.50000 * RK( 279 ) * 
     &                                          +           RK( 519 ) * 
     &                                          + 0.50000 * RK( 523 ) * 
     &                                          + 0.50000 * RK( 527 ) * 
     &                                          + 0.40000 * RK( 531 ) * 
     &                                          + 0.50000 * RK( 553 ) * 
     &                                          + 0.13100 * RK( 557 ) * 
     &                                          + 0.12700 * RK( 563 ) * 
     &                                          + 0.12700 * RK( 567 ) * 
      PD( HCHO            ,NO3              ) =             RK( 053 ) * 
     &                                          +           RK( 105 ) * 
     &                                          -           RK( 211 ) * 
     &                                          +           RK( 289 ) * 
      PD( HCHO            ,OH               ) = -           RK( 210 ) * 
     &                                          +           RK( 222 ) * 
     &                                          + 0.30000 * RK( 226 ) * 
     &                                          + 0.00200 * RK( 271 ) * 
      PD( HCHO            ,HO2              ) =             RK( 052 ) * 
      PD( HCHO            ,MEO2             ) =             RK( 050 ) * 
     &                                          +           RK( 052 ) * 
     &                                          +           RK( 053 ) * 
     &                                          + 4.00000 * RK( 054 ) * 
     &                                          + 4.00000 * RK( 055 ) * 
     &                                          + 0.75000 * RK( 059 ) * 
     &                                          + 0.75000 * RK( 064 ) * 
     &                                          + 0.90000 * RK( 073 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 094 ) * 
     &                                          + 2.00000 * RK( 106 ) * 
     &                                          + 0.50000 * RK( 290 ) * 
      PD( HCHO            ,HCHO             ) = -           RK( 208 )   
     &                                          -           RK( 209 )   
     &                                          -           RK( 210 ) * 
     &                                          -           RK( 211 ) * 
     &                                          -           RK( 592 ) * 
      PD( HCHO            ,COOH             ) =   0.30000 * RK( 226 ) * 
     &                                          +           RK( 227 )   
      PD( HCHO            ,MEOH             ) =             RK( 222 ) * 
     &                                          +           RK( 594 ) * 
      PD( HCHO            ,RO2C             ) =   0.75000 * RK( 059 ) * 
     &                                          +           RK( 107 ) * 
     &                                          + 0.50000 * RK( 291 ) * 
      PD( HCHO            ,RO2XC            ) =   0.75000 * RK( 064 ) * 
     &                                          +           RK( 108 ) * 
     &                                          + 0.50000 * RK( 292 ) * 
      PD( HCHO            ,MECO3            ) =   0.90000 * RK( 073 ) * 
     &                                          +           RK( 109 ) * 
     &                                          +           RK( 293 ) * 
      PD( HCHO            ,RCO3             ) =             RK( 083 ) * 
     &                                          +           RK( 110 ) * 
     &                                          +           RK( 294 ) * 
      PD( HCHO            ,BZCO3            ) =             RK( 094 ) * 
     &                                          +           RK( 111 ) * 
     &                                          +           RK( 295 ) * 
      PD( HCHO            ,MACO3            ) =             RK( 103 ) * 
     &                                          +           RK( 105 ) * 
     &                                          + 2.00000 * RK( 106 ) * 
     &                                          +           RK( 107 ) * 
     &                                          +           RK( 108 ) * 
     &                                          +           RK( 109 ) * 
     &                                          +           RK( 110 ) * 
     &                                          +           RK( 111 ) * 
     &                                          + 4.00000 * RK( 112 ) * 
     &                                          +           RK( 296 ) * 
      PD( HCHO            ,MAPAN            ) =   0.40000 * RK( 102 )   
      PD( HCHO            ,RNO3             ) =   0.07400 * RK( 274 )   
      PD( HCHO            ,xHCHO            ) =             RK( 287 ) * 
     &                                          +           RK( 289 ) * 
     &                                          + 0.50000 * RK( 290 ) * 
     &                                          + 0.50000 * RK( 291 ) * 
     &                                          + 0.50000 * RK( 292 ) * 
     &                                          +           RK( 293 ) * 
     &                                          +           RK( 294 ) * 
     &                                          +           RK( 295 ) * 
     &                                          +           RK( 296 ) * 
      PD( HCHO            ,PRD2             ) =   0.00200 * RK( 271 ) * 
      PD( HCHO            ,IPRD             ) =   0.12400 * RK( 268 ) * 
     &                                          + 0.30000 * RK( 270 )   
      PD( HCHO            ,GLY              ) =             RK( 235 )   
      PD( HCHO            ,MACR             ) =   0.10000 * RK( 259 ) * 
     &                                          + 0.34000 * RK( 262 )   
      PD( HCHO            ,MVK              ) =   0.05000 * RK( 264 ) * 
      PD( HCHO            ,HOCCHO           ) =             RK( 276 )   
      PD( HCHO            ,ACROLEIN         ) =   0.50000 * RK( 279 ) * 
     &                                          + 0.34000 * RK( 282 )   
      PD( HCHO            ,ETHENE           ) =             RK( 519 ) * 
      PD( HCHO            ,PROPENE          ) =   0.50000 * RK( 523 ) * 
      PD( HCHO            ,BUTADIENE13      ) =   0.50000 * RK( 527 ) * 
      PD( HCHO            ,ISOPRENE         ) =   0.40000 * RK( 531 ) * 
      PD( HCHO            ,OLE1             ) =   0.50000 * RK( 553 ) * 
      PD( HCHO            ,OLE2             ) =   0.13100 * RK( 557 ) * 
      PD( HCHO            ,TERP             ) =   0.12700 * RK( 563 ) * 
      PD( HCHO            ,SESQ             ) =   0.12700 * RK( 567 ) * 
      PD( HCHO            ,CL               ) = -           RK( 592 ) * 
     &                                          +           RK( 594 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  22: COOH                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( COOH            ,OH               ) = -           RK( 226 ) * 
      PD( COOH            ,HO2              ) =             RK( 051 ) * 
      PD( COOH            ,MEO2             ) =             RK( 051 ) * 
      PD( COOH            ,COOH             ) = -           RK( 226 ) * 
     &                                          -           RK( 227 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  23: MEOH                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEOH            ,OH               ) = -           RK( 222 ) * 
      PD( MEOH            ,MEO2             ) =   4.00000 * RK( 054 ) * 
     &                                          + 0.25000 * RK( 059 ) * 
     &                                          + 0.25000 * RK( 064 ) * 
      PD( MEOH            ,MEOH             ) = -           RK( 222 ) * 
     &                                          -           RK( 594 ) * 
      PD( MEOH            ,RO2C             ) =   0.25000 * RK( 059 ) * 
      PD( MEOH            ,RO2XC            ) =   0.25000 * RK( 064 ) * 
      PD( MEOH            ,CL               ) = -           RK( 594 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  24: RO2C                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RO2C            ,NO               ) = -           RK( 056 ) * 
     &                                          +           RK( 080 ) * 
     &                                          +           RK( 091 ) * 
      PD( RO2C            ,O3P              ) =   0.29000 * RK( 521 ) * 
     &                                          + 0.23500 * RK( 529 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
      PD( RO2C            ,O3               ) =   0.65200 * RK( 251 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
     &                                          + 0.25800 * RK( 257 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.04800 * RK( 268 ) * 
     &                                          + 0.19200 * RK( 531 ) * 
     &                                          + 1.51100 * RK( 535 ) * 
     &                                          + 0.09000 * RK( 553 ) * 
     &                                          + 0.21200 * RK( 557 ) * 
     &                                          + 0.49000 * RK( 563 ) * 
     &                                          + 0.49000 * RK( 567 ) * 
      PD( RO2C            ,NO3              ) = -           RK( 058 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 093 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.79900 * RK( 269 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.94900 * RK( 524 ) * 
     &                                          + 1.05500 * RK( 528 ) * 
     &                                          + 0.93600 * RK( 532 ) * 
     &                                          + 1.05000 * RK( 536 ) * 
     &                                          + 1.46300 * RK( 554 ) * 
     &                                          + 1.19300 * RK( 558 ) * 
     &                                          + 1.78600 * RK( 564 ) * 
     &                                          + 1.78600 * RK( 568 ) * 
      PD( RO2C            ,OH               ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 218 ) * 
     &                                          + 0.96700 * RK( 220 ) * 
     &                                          + 0.49100 * RK( 224 ) * 
     &                                          +           RK( 225 ) * 
     &                                          + 0.25100 * RK( 228 ) * 
     &                                          + 0.22200 * RK( 230 ) * 
     &                                          + 0.58900 * RK( 232 ) * 
     &                                          + 0.80000 * RK( 242 ) * 
     &                                          + 0.72300 * RK( 250 ) * 
     &                                          + 0.72300 * RK( 253 ) * 
     &                                          + 0.73300 * RK( 256 ) * 
     &                                          + 0.50000 * RK( 258 ) * 
     &                                          + 0.97500 * RK( 263 ) * 
     &                                          + 0.67000 * RK( 267 ) * 
     &                                          + 0.47300 * RK( 271 ) * 
     &                                          + 0.97600 * RK( 273 ) * 
     &                                          + 0.25000 * RK( 278 ) * 
     &                                          + 0.02000 * RK( 283 ) * 
     &                                          + 0.19400 * RK( 285 ) * 
     &                                          +           RK( 518 ) * 
     &                                          + 0.98400 * RK( 522 ) * 
     &                                          + 1.18900 * RK( 526 ) * 
     &                                          + 0.98600 * RK( 530 ) * 
     &                                          + 1.04200 * RK( 534 ) * 
     &                                          + 0.29000 * RK( 540 ) * 
     &                                          + 0.45400 * RK( 541 ) * 
     &                                          + 0.52000 * RK( 542 ) * 
     &                                          + 0.55400 * RK( 543 ) * 
     &                                          + 0.48700 * RK( 544 ) * 
     &                                          + 0.62700 * RK( 545 ) * 
     &                                          + 0.05000 * RK( 546 ) * 
     &                                          +           RK( 547 ) * 
     &                                          + 0.96500 * RK( 548 ) * 
     &                                          + 1.25300 * RK( 549 ) * 
     &                                          + 1.76300 * RK( 550 ) * 
     &                                          + 1.60500 * RK( 551 ) * 
     &                                          + 1.20200 * RK( 552 ) * 
     &                                          + 0.95300 * RK( 556 ) * 
     &                                          + 0.56600 * RK( 560 ) * 
     &                                          + 0.61700 * RK( 561 ) * 
     &                                          + 1.21100 * RK( 562 ) * 
     &                                          + 1.21100 * RK( 566 ) * 
      PD( RO2C            ,HO2              ) = -           RK( 057 ) * 
      PD( RO2C            ,MEO2             ) = -           RK( 059 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 094 ) * 
      PD( RO2C            ,RO2C             ) = -           RK( 056 ) * 
     &                                          -           RK( 057 ) * 
     &                                          -           RK( 058 ) * 
     &                                          -           RK( 059 ) * 
     &                                          - 4.00000 * RK( 060 ) * 
     &                                          -           RK( 065 ) * 
     &                                          -           RK( 074 ) * 
     &                                          -           RK( 107 ) * 
      PD( RO2C            ,RO2XC            ) = -           RK( 065 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 096 ) * 
      PD( RO2C            ,MECO3            ) = -           RK( 074 ) * 
     &                                          +           RK( 086 ) * 
     &                                          +           RK( 097 ) * 
      PD( RO2C            ,CCOOOH           ) =   0.02000 * RK( 283 ) * 
      PD( RO2C            ,CCOOH            ) =   0.49100 * RK( 224 ) * 
      PD( RO2C            ,RCO3             ) =             RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 086 ) * 
     &                                          + 4.00000 * RK( 087 ) * 
     &                                          +           RK( 098 ) * 
     &                                          +           RK( 110 ) * 
      PD( RO2C            ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( RO2C            ,RCOOOH           ) =   0.19400 * RK( 285 ) * 
      PD( RO2C            ,RCOOH            ) =             RK( 225 ) * 
      PD( RO2C            ,BZCO3            ) =             RK( 091 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 096 ) * 
     &                                          +           RK( 097 ) * 
     &                                          +           RK( 098 ) * 
     &                                          + 4.00000 * RK( 099 ) * 
     &                                          +           RK( 111 ) * 
      PD( RO2C            ,PBZN             ) =   0.40000 * RK( 090 )   
      PD( RO2C            ,BZO              ) =             RK( 117 )   
      PD( RO2C            ,MACO3            ) = -           RK( 107 ) * 
     &                                          +           RK( 110 ) * 
     &                                          +           RK( 111 ) * 
      PD( RO2C            ,RNO3             ) =   0.97600 * RK( 273 ) * 
     &                                          + 0.72100 * RK( 274 )   
     &                                          + 1.28200 * RK( 598 ) * 
      PD( RO2C            ,ACETONE          ) =             RK( 218 ) * 
     &                                          +           RK( 596 ) * 
      PD( RO2C            ,CRES             ) =   0.80000 * RK( 242 ) * 
      PD( RO2C            ,RCHO             ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 216 )   
     &                                          + 0.10000 * RK( 595 ) * 
      PD( RO2C            ,MEK              ) =   0.96700 * RK( 220 ) * 
     &                                          +           RK( 221 )   
     &                                          + 0.97500 * RK( 597 ) * 
      PD( RO2C            ,ROOH             ) =   0.25100 * RK( 228 ) * 
     &                                          + 0.58800 * RK( 604 ) * 
      PD( RO2C            ,R6OOH            ) =   0.22200 * RK( 230 ) * 
     &                                          + 0.78200 * RK( 231 )   
     &                                          + 1.07800 * RK( 605 ) * 
      PD( RO2C            ,PRD2             ) =   0.47300 * RK( 271 ) * 
     &                                          + 1.59000 * RK( 272 )   
     &                                          + 0.68000 * RK( 599 ) * 
      PD( RO2C            ,RAOOH            ) =   0.58900 * RK( 232 ) * 
     &                                          + 0.58900 * RK( 606 ) * 
      PD( RO2C            ,IPRD             ) =   0.67000 * RK( 267 ) * 
     &                                          + 0.04800 * RK( 268 ) * 
     &                                          + 0.79900 * RK( 269 ) * 
     &                                          + 0.73000 * RK( 610 ) * 
      PD( RO2C            ,AFG1             ) =   0.72300 * RK( 250 ) * 
     &                                          + 0.65200 * RK( 251 ) * 
      PD( RO2C            ,AFG2             ) =   0.72300 * RK( 253 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
      PD( RO2C            ,AFG3             ) =   0.73300 * RK( 256 ) * 
     &                                          + 0.25800 * RK( 257 ) * 
      PD( RO2C            ,MACR             ) =   0.50000 * RK( 258 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.33000 * RK( 262 )   
     &                                          + 0.80200 * RK( 608 ) * 
      PD( RO2C            ,MVK              ) =   0.97500 * RK( 263 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 1.28300 * RK( 609 ) * 
      PD( RO2C            ,ACROLEIN         ) =   0.25000 * RK( 278 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          + 1.03200 * RK( 607 ) * 
      PD( RO2C            ,ETHENE           ) =             RK( 518 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.29000 * RK( 521 ) * 
     &                                          + 2.00000 * RK( 646 ) * 
      PD( RO2C            ,PROPENE          ) =   0.98400 * RK( 522 ) * 
     &                                          + 0.94900 * RK( 524 ) * 
     &                                          + 0.97100 * RK( 647 ) * 
      PD( RO2C            ,BUTADIENE13      ) =   1.18900 * RK( 526 ) * 
     &                                          + 1.05500 * RK( 528 ) * 
     &                                          + 0.23500 * RK( 529 ) * 
     &                                          + 1.88400 * RK( 648 ) * 
      PD( RO2C            ,ISOPRENE         ) =   0.98600 * RK( 530 ) * 
     &                                          + 0.19200 * RK( 531 ) * 
     &                                          + 0.93600 * RK( 532 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
     &                                          + 1.16800 * RK( 649 ) * 
      PD( RO2C            ,APIN             ) =   1.04200 * RK( 534 ) * 
     &                                          + 1.51100 * RK( 535 ) * 
     &                                          + 1.05000 * RK( 536 ) * 
     &                                          + 2.25800 * RK( 650 ) * 
      PD( RO2C            ,BENZENE          ) =   0.29000 * RK( 540 ) * 
      PD( RO2C            ,TOLUENE          ) =   0.45400 * RK( 541 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
      PD( RO2C            ,MXYL             ) =   0.52000 * RK( 542 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
      PD( RO2C            ,OXYL             ) =   0.55400 * RK( 543 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
      PD( RO2C            ,PXYL             ) =   0.48700 * RK( 544 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
      PD( RO2C            ,TRIMETH_BENZ124  ) =   0.62700 * RK( 545 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
      PD( RO2C            ,ETOH             ) =   0.05000 * RK( 546 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
      PD( RO2C            ,ALK1             ) =             RK( 547 ) * 
     &                                          +           RK( 658 ) * 
      PD( RO2C            ,ALK2             ) =   0.96500 * RK( 548 ) * 
     &                                          + 0.97000 * RK( 659 ) * 
      PD( RO2C            ,ALK3             ) =   1.25300 * RK( 549 ) * 
     &                                          + 1.36100 * RK( 660 ) * 
      PD( RO2C            ,ALK4             ) =   1.76300 * RK( 550 ) * 
     &                                          + 1.73700 * RK( 661 ) * 
      PD( RO2C            ,ALK5             ) =   1.60500 * RK( 551 ) * 
     &                                          + 1.54100 * RK( 662 ) * 
      PD( RO2C            ,OLE1             ) =   1.20200 * RK( 552 ) * 
     &                                          + 0.09000 * RK( 553 ) * 
     &                                          + 1.46300 * RK( 554 ) * 
     &                                          + 1.60800 * RK( 663 ) * 
      PD( RO2C            ,OLE2             ) =   0.95300 * RK( 556 ) * 
     &                                          + 0.21200 * RK( 557 ) * 
     &                                          + 1.19300 * RK( 558 ) * 
     &                                          + 1.49200 * RK( 664 ) * 
      PD( RO2C            ,ARO1             ) =   0.56600 * RK( 560 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
      PD( RO2C            ,ARO2             ) =   0.61700 * RK( 561 ) * 
     &                                          + 0.82800 * RK( 666 ) * 
      PD( RO2C            ,TERP             ) =   1.21100 * RK( 562 ) * 
     &                                          + 0.49000 * RK( 563 ) * 
     &                                          + 1.78600 * RK( 564 ) * 
     &                                          + 2.25800 * RK( 667 ) * 
      PD( RO2C            ,SESQ             ) =   1.21100 * RK( 566 ) * 
     &                                          + 0.49000 * RK( 567 ) * 
     &                                          + 1.78600 * RK( 568 ) * 
     &                                          + 2.25800 * RK( 668 ) * 
      PD( RO2C            ,CL               ) =   0.10000 * RK( 595 ) * 
     &                                          +           RK( 596 ) * 
     &                                          + 0.97500 * RK( 597 ) * 
     &                                          + 1.28200 * RK( 598 ) * 
     &                                          + 0.68000 * RK( 599 ) * 
     &                                          + 0.58800 * RK( 604 ) * 
     &                                          + 1.07800 * RK( 605 ) * 
     &                                          + 0.58900 * RK( 606 ) * 
     &                                          + 1.03200 * RK( 607 ) * 
     &                                          + 0.80200 * RK( 608 ) * 
     &                                          + 1.28300 * RK( 609 ) * 
     &                                          + 0.73000 * RK( 610 ) * 
     &                                          + 2.00000 * RK( 646 ) * 
     &                                          + 0.97100 * RK( 647 ) * 
     &                                          + 1.88400 * RK( 648 ) * 
     &                                          + 1.16800 * RK( 649 ) * 
     &                                          + 2.25800 * RK( 650 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
     &                                          +           RK( 658 ) * 
     &                                          + 0.97000 * RK( 659 ) * 
     &                                          + 1.36100 * RK( 660 ) * 
     &                                          + 1.73700 * RK( 661 ) * 
     &                                          + 1.54100 * RK( 662 ) * 
     &                                          + 1.60800 * RK( 663 ) * 
     &                                          + 1.49200 * RK( 664 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
     &                                          + 0.82800 * RK( 666 ) * 
     &                                          + 2.25800 * RK( 667 ) * 
     &                                          + 2.25800 * RK( 668 ) * 
      PD( RO2C            ,CLCCHO           ) =             RK( 611 )   
      PD( RO2C            ,CLACET           ) =             RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  25: RO2XC                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RO2XC           ,NO               ) = -           RK( 061 ) * 
      PD( RO2XC           ,O3P              ) =   0.01500 * RK( 529 ) * 
     &                                          + 0.01000 * RK( 533 ) * 
      PD( RO2XC           ,O3               ) =   0.00700 * RK( 257 ) * 
     &                                          + 0.00800 * RK( 531 ) * 
     &                                          + 0.33700 * RK( 535 ) * 
     &                                          + 0.00500 * RK( 553 ) * 
     &                                          + 0.00300 * RK( 557 ) * 
     &                                          + 0.12100 * RK( 563 ) * 
     &                                          + 0.12100 * RK( 567 ) * 
      PD( RO2XC           ,NO3              ) = -           RK( 063 ) * 
     &                                          + 0.05100 * RK( 269 ) * 
     &                                          + 0.00200 * RK( 280 ) * 
     &                                          + 0.05100 * RK( 524 ) * 
     &                                          + 0.06500 * RK( 528 ) * 
     &                                          + 0.06400 * RK( 532 ) * 
     &                                          + 0.29300 * RK( 536 ) * 
     &                                          + 0.22800 * RK( 554 ) * 
     &                                          + 0.14000 * RK( 558 ) * 
     &                                          + 0.46000 * RK( 564 ) * 
     &                                          + 0.46000 * RK( 568 ) * 
      PD( RO2XC           ,OH               ) =   0.03900 * RK( 220 ) * 
     &                                          + 0.00400 * RK( 228 ) * 
     &                                          + 0.02900 * RK( 230 ) * 
     &                                          + 0.12400 * RK( 232 ) * 
     &                                          + 0.06000 * RK( 250 ) * 
     &                                          + 0.06000 * RK( 253 ) * 
     &                                          + 0.11700 * RK( 256 ) * 
     &                                          + 0.02500 * RK( 263 ) * 
     &                                          + 0.04100 * RK( 267 ) * 
     &                                          + 0.07100 * RK( 271 ) * 
     &                                          + 0.17500 * RK( 273 ) * 
     &                                          + 0.01600 * RK( 522 ) * 
     &                                          + 0.04900 * RK( 526 ) * 
     &                                          + 0.09300 * RK( 530 ) * 
     &                                          + 0.19700 * RK( 534 ) * 
     &                                          + 0.02400 * RK( 540 ) * 
     &                                          + 0.05400 * RK( 541 ) * 
     &                                          + 0.08200 * RK( 542 ) * 
     &                                          + 0.08700 * RK( 543 ) * 
     &                                          + 0.07600 * RK( 544 ) * 
     &                                          + 0.12100 * RK( 545 ) * 
     &                                          + 0.03500 * RK( 548 ) * 
     &                                          + 0.07000 * RK( 549 ) * 
     &                                          + 0.14900 * RK( 550 ) * 
     &                                          + 0.35300 * RK( 551 ) * 
     &                                          + 0.12800 * RK( 552 ) * 
     &                                          + 0.08800 * RK( 556 ) * 
     &                                          + 0.11000 * RK( 560 ) * 
     &                                          + 0.12800 * RK( 561 ) * 
     &                                          + 0.20100 * RK( 562 ) * 
     &                                          + 0.20100 * RK( 566 ) * 
      PD( RO2XC           ,HO2              ) = -           RK( 062 ) * 
      PD( RO2XC           ,MEO2             ) = -           RK( 064 ) * 
      PD( RO2XC           ,RO2C             ) = -           RK( 065 ) * 
      PD( RO2XC           ,RO2XC            ) = -           RK( 061 ) * 
     &                                          -           RK( 062 ) * 
     &                                          -           RK( 063 ) * 
     &                                          -           RK( 064 ) * 
     &                                          -           RK( 065 ) * 
     &                                          - 4.00000 * RK( 066 ) * 
     &                                          -           RK( 075 ) * 
     &                                          -           RK( 085 ) * 
     &                                          -           RK( 096 ) * 
     &                                          -           RK( 108 ) * 
      PD( RO2XC           ,MECO3            ) = -           RK( 075 ) * 
      PD( RO2XC           ,RCO3             ) = -           RK( 085 ) * 
      PD( RO2XC           ,BZCO3            ) = -           RK( 096 ) * 
      PD( RO2XC           ,MACO3            ) = -           RK( 108 ) * 
      PD( RO2XC           ,RNO3             ) =   0.17500 * RK( 273 ) * 
     &                                          + 0.10200 * RK( 274 )   
     &                                          + 0.20200 * RK( 598 ) * 
      PD( RO2XC           ,MEK              ) =   0.03900 * RK( 220 ) * 
     &                                          + 0.03900 * RK( 597 ) * 
      PD( RO2XC           ,ROOH             ) =   0.00400 * RK( 228 ) * 
      PD( RO2XC           ,R6OOH            ) =   0.02900 * RK( 230 ) * 
     &                                          + 0.07700 * RK( 231 )   
     &                                          + 0.11700 * RK( 605 ) * 
      PD( RO2XC           ,PRD2             ) =   0.07100 * RK( 271 ) * 
     &                                          + 0.08700 * RK( 272 )   
     &                                          + 0.11600 * RK( 599 ) * 
      PD( RO2XC           ,RAOOH            ) =   0.12400 * RK( 232 ) * 
     &                                          + 0.12400 * RK( 606 ) * 
      PD( RO2XC           ,IPRD             ) =   0.04100 * RK( 267 ) * 
     &                                          + 0.05100 * RK( 269 ) * 
     &                                          + 0.05100 * RK( 610 ) * 
      PD( RO2XC           ,AFG1             ) =   0.06000 * RK( 250 ) * 
      PD( RO2XC           ,AFG2             ) =   0.06000 * RK( 253 ) * 
      PD( RO2XC           ,AFG3             ) =   0.11700 * RK( 256 ) * 
     &                                          + 0.00700 * RK( 257 ) * 
      PD( RO2XC           ,MACR             ) =   0.03300 * RK( 608 ) * 
      PD( RO2XC           ,MVK              ) =   0.02500 * RK( 263 ) * 
     &                                          + 0.05300 * RK( 609 ) * 
      PD( RO2XC           ,ACROLEIN         ) =   0.00200 * RK( 280 ) * 
     &                                          + 0.02600 * RK( 607 ) * 
      PD( RO2XC           ,PROPENE          ) =   0.01600 * RK( 522 ) * 
     &                                          + 0.05100 * RK( 524 ) * 
     &                                          + 0.02900 * RK( 647 ) * 
      PD( RO2XC           ,BUTADIENE13      ) =   0.04900 * RK( 526 ) * 
     &                                          + 0.06500 * RK( 528 ) * 
     &                                          + 0.01500 * RK( 529 ) * 
     &                                          + 0.06900 * RK( 648 ) * 
      PD( RO2XC           ,ISOPRENE         ) =   0.09300 * RK( 530 ) * 
     &                                          + 0.00800 * RK( 531 ) * 
     &                                          + 0.06400 * RK( 532 ) * 
     &                                          + 0.01000 * RK( 533 ) * 
     &                                          + 0.08500 * RK( 649 ) * 
      PD( RO2XC           ,APIN             ) =   0.19700 * RK( 534 ) * 
     &                                          + 0.33700 * RK( 535 ) * 
     &                                          + 0.29300 * RK( 536 ) * 
     &                                          + 0.58200 * RK( 650 ) * 
      PD( RO2XC           ,BENZENE          ) =   0.02400 * RK( 540 ) * 
      PD( RO2XC           ,TOLUENE          ) =   0.05400 * RK( 541 ) * 
     &                                          + 0.10600 * RK( 652 ) * 
      PD( RO2XC           ,MXYL             ) =   0.08200 * RK( 542 ) * 
     &                                          + 0.13600 * RK( 653 ) * 
      PD( RO2XC           ,OXYL             ) =   0.08700 * RK( 543 ) * 
     &                                          + 0.13600 * RK( 654 ) * 
      PD( RO2XC           ,PXYL             ) =   0.07600 * RK( 544 ) * 
     &                                          + 0.13600 * RK( 655 ) * 
      PD( RO2XC           ,TRIMETH_BENZ124  ) =   0.12100 * RK( 545 ) * 
     &                                          + 0.16200 * RK( 656 ) * 
      PD( RO2XC           ,ALK2             ) =   0.03500 * RK( 548 ) * 
     &                                          + 0.03000 * RK( 659 ) * 
      PD( RO2XC           ,ALK3             ) =   0.07000 * RK( 549 ) * 
     &                                          + 0.07000 * RK( 660 ) * 
      PD( RO2XC           ,ALK4             ) =   0.14900 * RK( 550 ) * 
     &                                          + 0.16500 * RK( 661 ) * 
      PD( RO2XC           ,ALK5             ) =   0.35300 * RK( 551 ) * 
     &                                          + 0.35200 * RK( 662 ) * 
      PD( RO2XC           ,OLE1             ) =   0.12800 * RK( 552 ) * 
     &                                          + 0.00500 * RK( 553 ) * 
     &                                          + 0.22800 * RK( 554 ) * 
     &                                          + 0.12700 * RK( 663 ) * 
      PD( RO2XC           ,OLE2             ) =   0.08800 * RK( 556 ) * 
     &                                          + 0.00300 * RK( 557 ) * 
     &                                          + 0.14000 * RK( 558 ) * 
     &                                          + 0.10600 * RK( 664 ) * 
      PD( RO2XC           ,ARO1             ) =   0.11000 * RK( 560 ) * 
     &                                          + 0.16000 * RK( 665 ) * 
      PD( RO2XC           ,ARO2             ) =   0.12800 * RK( 561 ) * 
     &                                          + 0.17200 * RK( 666 ) * 
      PD( RO2XC           ,TERP             ) =   0.20100 * RK( 562 ) * 
     &                                          + 0.12100 * RK( 563 ) * 
     &                                          + 0.46000 * RK( 564 ) * 
     &                                          + 0.58200 * RK( 667 ) * 
      PD( RO2XC           ,SESQ             ) =   0.20100 * RK( 566 ) * 
     &                                          + 0.12100 * RK( 567 ) * 
     &                                          + 0.46000 * RK( 568 ) * 
     &                                          + 0.58200 * RK( 668 ) * 
      PD( RO2XC           ,CL               ) =   0.03900 * RK( 597 ) * 
     &                                          + 0.20200 * RK( 598 ) * 
     &                                          + 0.11600 * RK( 599 ) * 
     &                                          + 0.11700 * RK( 605 ) * 
     &                                          + 0.12400 * RK( 606 ) * 
     &                                          + 0.02600 * RK( 607 ) * 
     &                                          + 0.03300 * RK( 608 ) * 
     &                                          + 0.05300 * RK( 609 ) * 
     &                                          + 0.05100 * RK( 610 ) * 
     &                                          + 0.02900 * RK( 647 ) * 
     &                                          + 0.06900 * RK( 648 ) * 
     &                                          + 0.08500 * RK( 649 ) * 
     &                                          + 0.58200 * RK( 650 ) * 
     &                                          + 0.10600 * RK( 652 ) * 
     &                                          + 0.13600 * RK( 653 ) * 
     &                                          + 0.13600 * RK( 654 ) * 
     &                                          + 0.13600 * RK( 655 ) * 
     &                                          + 0.16200 * RK( 656 ) * 
     &                                          + 0.03000 * RK( 659 ) * 
     &                                          + 0.07000 * RK( 660 ) * 
     &                                          + 0.16500 * RK( 661 ) * 
     &                                          + 0.35200 * RK( 662 ) * 
     &                                          + 0.12700 * RK( 663 ) * 
     &                                          + 0.10600 * RK( 664 ) * 
     &                                          + 0.16000 * RK( 665 ) * 
     &                                          + 0.17200 * RK( 666 ) * 
     &                                          + 0.58200 * RK( 667 ) * 
     &                                          + 0.58200 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  26: MECO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MECO3           ,NO2              ) = -           RK( 067 ) * 
      PD( MECO3           ,NO               ) = -           RK( 070 ) * 
     &                                          +           RK( 103 ) * 
     &                                          +           RK( 158 ) * 
      PD( MECO3           ,O3               ) =   0.01300 * RK( 257 ) * 
      PD( MECO3           ,NO3              ) = -           RK( 072 ) * 
     &                                          +           RK( 105 ) * 
     &                                          +           RK( 160 ) * 
     &                                          +           RK( 214 ) * 
     &                                          +           RK( 240 ) * 
     &                                          +           RK( 277 ) * 
      PD( MECO3           ,OH               ) =             RK( 212 ) * 
     &                                          +           RK( 239 ) * 
     &                                          +           RK( 275 ) * 
     &                                          + 0.98000 * RK( 283 ) * 
      PD( MECO3           ,HO2              ) = -           RK( 071 ) * 
      PD( MECO3           ,MEO2             ) = -           RK( 073 ) * 
     &                                          +           RK( 106 ) * 
     &                                          + 0.50000 * RK( 161 ) * 
      PD( MECO3           ,RO2C             ) = -           RK( 074 ) * 
     &                                          +           RK( 107 ) * 
     &                                          + 0.50000 * RK( 162 ) * 
      PD( MECO3           ,RO2XC            ) = -           RK( 075 ) * 
     &                                          +           RK( 108 ) * 
     &                                          + 0.50000 * RK( 163 ) * 
      PD( MECO3           ,MECO3            ) = -           RK( 067 ) * 
     &                                          -           RK( 070 ) * 
     &                                          -           RK( 071 ) * 
     &                                          -           RK( 072 ) * 
     &                                          -           RK( 073 ) * 
     &                                          -           RK( 074 ) * 
     &                                          -           RK( 075 ) * 
     &                                          - 4.00000 * RK( 076 ) * 
     &                                          -           RK( 086 ) * 
     &                                          -           RK( 097 ) * 
      PD( MECO3           ,PAN              ) =             RK( 068 )   
     &                                          + 0.60000 * RK( 069 )   
      PD( MECO3           ,CCOOOH           ) =   0.98000 * RK( 283 ) * 
      PD( MECO3           ,RCO3             ) = -           RK( 086 ) * 
     &                                          +           RK( 110 ) * 
     &                                          +           RK( 165 ) * 
      PD( MECO3           ,BZCO3            ) = -           RK( 097 ) * 
     &                                          +           RK( 111 ) * 
     &                                          +           RK( 166 ) * 
      PD( MECO3           ,MACO3            ) =             RK( 103 ) * 
     &                                          +           RK( 105 ) * 
     &                                          +           RK( 106 ) * 
     &                                          +           RK( 107 ) * 
     &                                          +           RK( 108 ) * 
     &                                          +           RK( 110 ) * 
     &                                          +           RK( 111 ) * 
     &                                          + 4.00000 * RK( 112 ) * 
     &                                          +           RK( 167 ) * 
      PD( MECO3           ,MAPAN            ) =   0.40000 * RK( 102 )   
      PD( MECO3           ,ACETONE          ) =   0.62000 * RK( 219 )   
      PD( MECO3           ,xMECO3           ) =             RK( 158 ) * 
     &                                          +           RK( 160 ) * 
     &                                          + 0.50000 * RK( 161 ) * 
     &                                          + 0.50000 * RK( 162 ) * 
     &                                          + 0.50000 * RK( 163 ) * 
     &                                          +           RK( 165 ) * 
     &                                          +           RK( 166 ) * 
     &                                          +           RK( 167 ) * 
      PD( MECO3           ,CCHO             ) =             RK( 212 ) * 
     &                                          +           RK( 214 ) * 
     &                                          +           RK( 593 ) * 
      PD( MECO3           ,MEK              ) =             RK( 221 )   
      PD( MECO3           ,PRD2             ) =   0.40000 * RK( 272 )   
      PD( MECO3           ,MGLY             ) =             RK( 238 )   
     &                                          +           RK( 239 ) * 
     &                                          +           RK( 240 ) * 
     &                                          +           RK( 601 ) * 
      PD( MECO3           ,IPRD             ) =   0.46700 * RK( 270 )   
      PD( MECO3           ,AFG1             ) =   0.30500 * RK( 252 )   
      PD( MECO3           ,BACL             ) =   2.00000 * RK( 241 )   
      PD( MECO3           ,AFG3             ) =   0.01300 * RK( 257 ) * 
      PD( MECO3           ,MACR             ) =   0.34000 * RK( 262 )   
      PD( MECO3           ,HOCCHO           ) =             RK( 275 ) * 
     &                                          +           RK( 277 ) * 
      PD( MECO3           ,CL               ) =             RK( 593 ) * 
     &                                          +           RK( 601 ) * 
      PD( MECO3           ,CLACET           ) =             RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  27: PAN                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAN             ,NO2              ) =             RK( 067 ) * 
      PD( PAN             ,MECO3            ) =             RK( 067 ) * 
      PD( PAN             ,PAN              ) = -           RK( 068 )   
     &                                          -           RK( 069 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  28: CCOOOH                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CCOOOH          ,OH               ) = -           RK( 283 ) * 
      PD( CCOOOH          ,HO2              ) =   0.70000 * RK( 071 ) * 
      PD( CCOOOH          ,MECO3            ) =   0.70000 * RK( 071 ) * 
      PD( CCOOOH          ,CCOOOH           ) = -           RK( 283 ) * 
     &                                          -           RK( 284 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  29: CCOOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CCOOH           ,O3               ) =   0.07500 * RK( 523 ) * 
     &                                          + 0.06500 * RK( 557 ) * 
      PD( CCOOH           ,OH               ) = -           RK( 224 ) * 
      PD( CCOOH           ,HO2              ) =   0.30000 * RK( 071 ) * 
      PD( CCOOH           ,MEO2             ) =   0.10000 * RK( 073 ) * 
      PD( CCOOH           ,MECO3            ) =   0.30000 * RK( 071 ) * 
     &                                          + 0.10000 * RK( 073 ) * 
      PD( CCOOH           ,CCOOH            ) = -           RK( 224 ) * 
      PD( CCOOH           ,ACROLEIN         ) =   0.05000 * RK( 282 )   
      PD( CCOOH           ,PROPENE          ) =   0.07500 * RK( 523 ) * 
      PD( CCOOH           ,OLE2             ) =   0.06500 * RK( 557 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  30: RCO3                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RCO3            ,NO2              ) = -           RK( 077 ) * 
      PD( RCO3            ,NO               ) = -           RK( 080 ) * 
     &                                          +           RK( 168 ) * 
      PD( RCO3            ,NO3              ) = -           RK( 082 ) * 
     &                                          +           RK( 170 ) * 
     &                                          +           RK( 217 ) * 
     &                                          + 0.37000 * RK( 237 ) * 
      PD( RCO3            ,OH               ) =   0.96500 * RK( 215 ) * 
     &                                          + 0.37000 * RK( 236 ) * 
     &                                          + 0.80600 * RK( 285 ) * 
     &                                          +           RK( 612 ) * 
      PD( RCO3            ,HO2              ) = -           RK( 081 ) * 
      PD( RCO3            ,MEO2             ) = -           RK( 083 ) * 
     &                                          + 0.50000 * RK( 171 ) * 
      PD( RCO3            ,RO2C             ) = -           RK( 084 ) * 
     &                                          + 0.50000 * RK( 172 ) * 
      PD( RCO3            ,RO2XC            ) = -           RK( 085 ) * 
     &                                          + 0.50000 * RK( 173 ) * 
      PD( RCO3            ,MECO3            ) = -           RK( 086 ) * 
     &                                          +           RK( 174 ) * 
      PD( RCO3            ,RCO3             ) = -           RK( 077 ) * 
     &                                          -           RK( 080 ) * 
     &                                          -           RK( 081 ) * 
     &                                          -           RK( 082 ) * 
     &                                          -           RK( 083 ) * 
     &                                          -           RK( 084 ) * 
     &                                          -           RK( 085 ) * 
     &                                          -           RK( 086 ) * 
     &                                          - 4.00000 * RK( 087 ) * 
     &                                          -           RK( 098 ) * 
     &                                          -           RK( 110 ) * 
      PD( RCO3            ,PAN2             ) =             RK( 078 )   
     &                                          + 0.60000 * RK( 079 )   
      PD( RCO3            ,RCOOOH           ) =   0.80600 * RK( 285 ) * 
      PD( RCO3            ,BZCO3            ) = -           RK( 098 ) * 
     &                                          +           RK( 176 ) * 
      PD( RCO3            ,MACO3            ) = -           RK( 110 ) * 
     &                                          +           RK( 177 ) * 
      PD( RCO3            ,xRCO3            ) =             RK( 168 ) * 
     &                                          +           RK( 170 ) * 
     &                                          + 0.50000 * RK( 171 ) * 
     &                                          + 0.50000 * RK( 172 ) * 
     &                                          + 0.50000 * RK( 173 ) * 
     &                                          +           RK( 174 ) * 
     &                                          +           RK( 176 ) * 
     &                                          +           RK( 177 ) * 
      PD( RCO3            ,RCHO             ) =   0.96500 * RK( 215 ) * 
     &                                          +           RK( 217 ) * 
     &                                          + 0.90000 * RK( 595 ) * 
      PD( RCO3            ,PRD2             ) =   0.60000 * RK( 272 )   
      PD( RCO3            ,IPRD             ) =   0.30000 * RK( 270 )   
      PD( RCO3            ,GLY              ) =   0.37000 * RK( 236 ) * 
     &                                          + 0.37000 * RK( 237 ) * 
     &                                          + 0.37000 * RK( 600 ) * 
      PD( RCO3            ,CL               ) =   0.90000 * RK( 595 ) * 
     &                                          + 0.37000 * RK( 600 ) * 
     &                                          +           RK( 613 ) * 
      PD( RCO3            ,CLCCHO           ) =             RK( 612 ) * 
     &                                          +           RK( 613 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  31: PAN2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAN2            ,NO2              ) =             RK( 077 ) * 
      PD( PAN2            ,RCO3             ) =             RK( 077 ) * 
      PD( PAN2            ,PAN2             ) = -           RK( 078 )   
     &                                          -           RK( 079 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  32: xHO2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xHO2            ,NO               ) =             RK( 080 ) * 
     &                                          -           RK( 118 ) * 
      PD( xHO2            ,O3P              ) =   0.29000 * RK( 521 ) * 
     &                                          + 0.11700 * RK( 529 ) * 
      PD( xHO2            ,O3               ) =   0.09500 * RK( 257 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.10200 * RK( 535 ) * 
     &                                          + 0.05700 * RK( 553 ) * 
     &                                          + 0.04100 * RK( 557 ) * 
     &                                          + 0.04600 * RK( 563 ) * 
     &                                          + 0.04600 * RK( 567 ) * 
      PD( xHO2            ,NO3              ) =             RK( 082 ) * 
     &                                          -           RK( 120 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.79900 * RK( 269 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.94900 * RK( 524 ) * 
     &                                          + 0.81500 * RK( 528 ) * 
     &                                          + 0.74900 * RK( 532 ) * 
     &                                          + 0.05600 * RK( 536 ) * 
     &                                          + 0.77200 * RK( 554 ) * 
     &                                          + 0.40000 * RK( 558 ) * 
     &                                          + 0.22700 * RK( 564 ) * 
     &                                          + 0.22700 * RK( 568 ) * 
      PD( xHO2            ,OH               ) =   0.03500 * RK( 215 ) * 
     &                                          + 0.37600 * RK( 220 ) * 
     &                                          + 0.49100 * RK( 224 ) * 
     &                                          +           RK( 225 ) * 
     &                                          + 0.23900 * RK( 228 ) * 
     &                                          + 0.09000 * RK( 230 ) * 
     &                                          + 0.56500 * RK( 232 ) * 
     &                                          + 0.80000 * RK( 242 ) * 
     &                                          + 0.52100 * RK( 250 ) * 
     &                                          + 0.52100 * RK( 253 ) * 
     &                                          + 0.56100 * RK( 256 ) * 
     &                                          + 0.50000 * RK( 258 ) * 
     &                                          + 0.30000 * RK( 263 ) * 
     &                                          + 0.67000 * RK( 267 ) * 
     &                                          + 0.37900 * RK( 271 ) * 
     &                                          + 0.30500 * RK( 273 ) * 
     &                                          + 0.25000 * RK( 278 ) * 
     &                                          + 0.08400 * RK( 285 ) * 
     &                                          +           RK( 518 ) * 
     &                                          + 0.98400 * RK( 522 ) * 
     &                                          + 0.95100 * RK( 526 ) * 
     &                                          + 0.90700 * RK( 530 ) * 
     &                                          + 0.79900 * RK( 534 ) * 
     &                                          + 0.29000 * RK( 540 ) * 
     &                                          + 0.45400 * RK( 541 ) * 
     &                                          + 0.52000 * RK( 542 ) * 
     &                                          + 0.55400 * RK( 543 ) * 
     &                                          + 0.48700 * RK( 544 ) * 
     &                                          + 0.62700 * RK( 545 ) * 
     &                                          + 0.05000 * RK( 546 ) * 
     &                                          +           RK( 547 ) * 
     &                                          + 0.96500 * RK( 548 ) * 
     &                                          + 0.69500 * RK( 549 ) * 
     &                                          + 0.83000 * RK( 550 ) * 
     &                                          + 0.64700 * RK( 551 ) * 
     &                                          + 0.87100 * RK( 552 ) * 
     &                                          + 0.91200 * RK( 556 ) * 
     &                                          + 0.56600 * RK( 560 ) * 
     &                                          + 0.61700 * RK( 561 ) * 
     &                                          + 0.73400 * RK( 562 ) * 
     &                                          + 0.73400 * RK( 566 ) * 
      PD( xHO2            ,HO2              ) = -           RK( 119 ) * 
      PD( xHO2            ,MEO2             ) =             RK( 083 ) * 
     &                                          -           RK( 121 ) * 
      PD( xHO2            ,RO2C             ) =             RK( 084 ) * 
     &                                          -           RK( 122 ) * 
      PD( xHO2            ,RO2XC            ) =             RK( 085 ) * 
     &                                          -           RK( 123 ) * 
      PD( xHO2            ,MECO3            ) =             RK( 086 ) * 
     &                                          -           RK( 124 ) * 
      PD( xHO2            ,CCOOH            ) =   0.49100 * RK( 224 ) * 
      PD( xHO2            ,RCO3             ) =             RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 084 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 086 ) * 
     &                                          + 4.00000 * RK( 087 ) * 
     &                                          +           RK( 098 ) * 
     &                                          +           RK( 110 ) * 
     &                                          -           RK( 125 ) * 
      PD( xHO2            ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( xHO2            ,xHO2             ) = -           RK( 118 ) * 
     &                                          -           RK( 119 ) * 
     &                                          -           RK( 120 ) * 
     &                                          -           RK( 121 ) * 
     &                                          -           RK( 122 ) * 
     &                                          -           RK( 123 ) * 
     &                                          -           RK( 124 ) * 
     &                                          -           RK( 125 ) * 
     &                                          -           RK( 126 ) * 
     &                                          -           RK( 127 ) * 
      PD( xHO2            ,RCOOOH           ) =   0.08400 * RK( 285 ) * 
     &                                          +           RK( 286 )   
      PD( xHO2            ,RCOOH            ) =             RK( 225 ) * 
      PD( xHO2            ,BZCO3            ) =             RK( 098 ) * 
     &                                          -           RK( 126 ) * 
      PD( xHO2            ,BZO              ) =             RK( 117 )   
      PD( xHO2            ,MACO3            ) =             RK( 110 ) * 
     &                                          -           RK( 127 ) * 
      PD( xHO2            ,RNO3             ) =   0.30500 * RK( 273 ) * 
     &                                          + 0.55400 * RK( 274 )   
     &                                          + 0.54700 * RK( 598 ) * 
      PD( xHO2            ,CRES             ) =   0.80000 * RK( 242 ) * 
     &                                          +           RK( 602 ) * 
      PD( xHO2            ,RCHO             ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 216 )   
     &                                          + 0.10000 * RK( 595 ) * 
      PD( xHO2            ,MEK              ) =   0.37600 * RK( 220 ) * 
     &                                          +           RK( 221 )   
     &                                          + 0.84000 * RK( 597 ) * 
      PD( xHO2            ,ROOH             ) =   0.23900 * RK( 228 ) * 
     &                                          + 0.48200 * RK( 604 ) * 
      PD( xHO2            ,R6OOH            ) =   0.09000 * RK( 230 ) * 
     &                                          + 0.78200 * RK( 231 )   
     &                                          + 0.23700 * RK( 605 ) * 
      PD( xHO2            ,PRD2             ) =   0.37900 * RK( 271 ) * 
     &                                          + 0.91300 * RK( 272 )   
     &                                          + 0.54100 * RK( 599 ) * 
      PD( xHO2            ,RAOOH            ) =   0.56500 * RK( 232 ) * 
     &                                          + 0.56500 * RK( 606 ) * 
      PD( xHO2            ,IPRD             ) =   0.67000 * RK( 267 ) * 
     &                                          + 0.79900 * RK( 269 ) * 
     &                                          + 0.71200 * RK( 610 ) * 
      PD( xHO2            ,AFG1             ) =   0.52100 * RK( 250 ) * 
      PD( xHO2            ,AFG2             ) =   0.52100 * RK( 253 ) * 
      PD( xHO2            ,AFG3             ) =   0.56100 * RK( 256 ) * 
     &                                          + 0.09500 * RK( 257 ) * 
      PD( xHO2            ,MACR             ) =   0.50000 * RK( 258 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.80200 * RK( 608 ) * 
      PD( xHO2            ,MVK              ) =   0.30000 * RK( 263 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.32200 * RK( 609 ) * 
      PD( xHO2            ,ACROLEIN         ) =   0.25000 * RK( 278 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          + 0.48400 * RK( 607 ) * 
      PD( xHO2            ,ETHENE           ) =             RK( 518 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.29000 * RK( 521 ) * 
     &                                          +           RK( 646 ) * 
      PD( xHO2            ,PROPENE          ) =   0.98400 * RK( 522 ) * 
     &                                          + 0.94900 * RK( 524 ) * 
     &                                          + 0.97100 * RK( 647 ) * 
      PD( xHO2            ,BUTADIENE13      ) =   0.95100 * RK( 526 ) * 
     &                                          + 0.81500 * RK( 528 ) * 
     &                                          + 0.11700 * RK( 529 ) * 
     &                                          + 0.39000 * RK( 648 ) * 
      PD( xHO2            ,ISOPRENE         ) =   0.90700 * RK( 530 ) * 
     &                                          + 0.74900 * RK( 532 ) * 
     &                                          + 0.73800 * RK( 649 ) * 
      PD( xHO2            ,APIN             ) =   0.79900 * RK( 534 ) * 
     &                                          + 0.10200 * RK( 535 ) * 
     &                                          + 0.05600 * RK( 536 ) * 
     &                                          + 0.25200 * RK( 650 ) * 
      PD( xHO2            ,BENZENE          ) =   0.29000 * RK( 540 ) * 
      PD( xHO2            ,TOLUENE          ) =   0.45400 * RK( 541 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
      PD( xHO2            ,MXYL             ) =   0.52000 * RK( 542 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
      PD( xHO2            ,OXYL             ) =   0.55400 * RK( 543 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
      PD( xHO2            ,PXYL             ) =   0.48700 * RK( 544 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
      PD( xHO2            ,TRIMETH_BENZ124  ) =   0.62700 * RK( 545 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
      PD( xHO2            ,ETOH             ) =   0.05000 * RK( 546 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
      PD( xHO2            ,ALK1             ) =             RK( 547 ) * 
     &                                          +           RK( 658 ) * 
      PD( xHO2            ,ALK2             ) =   0.96500 * RK( 548 ) * 
     &                                          + 0.97000 * RK( 659 ) * 
      PD( xHO2            ,ALK3             ) =   0.69500 * RK( 549 ) * 
     &                                          + 0.83500 * RK( 660 ) * 
      PD( xHO2            ,ALK4             ) =   0.83000 * RK( 550 ) * 
     &                                          + 0.82700 * RK( 661 ) * 
      PD( xHO2            ,ALK5             ) =   0.64700 * RK( 551 ) * 
     &                                          + 0.64700 * RK( 662 ) * 
      PD( xHO2            ,OLE1             ) =   0.87100 * RK( 552 ) * 
     &                                          + 0.05700 * RK( 553 ) * 
     &                                          + 0.77200 * RK( 554 ) * 
     &                                          + 0.87300 * RK( 663 ) * 
      PD( xHO2            ,OLE2             ) =   0.91200 * RK( 556 ) * 
     &                                          + 0.04100 * RK( 557 ) * 
     &                                          + 0.40000 * RK( 558 ) * 
     &                                          + 0.45000 * RK( 664 ) * 
      PD( xHO2            ,ARO1             ) =   0.56600 * RK( 560 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
      PD( xHO2            ,ARO2             ) =   0.61700 * RK( 561 ) * 
     &                                          + 0.82800 * RK( 666 ) * 
      PD( xHO2            ,TERP             ) =   0.73400 * RK( 562 ) * 
     &                                          + 0.04600 * RK( 563 ) * 
     &                                          + 0.22700 * RK( 564 ) * 
     &                                          + 0.25200 * RK( 667 ) * 
      PD( xHO2            ,SESQ             ) =   0.73400 * RK( 566 ) * 
     &                                          + 0.04600 * RK( 567 ) * 
     &                                          + 0.22700 * RK( 568 ) * 
     &                                          + 0.25200 * RK( 668 ) * 
      PD( xHO2            ,CL               ) =   0.10000 * RK( 595 ) * 
     &                                          + 0.84000 * RK( 597 ) * 
     &                                          + 0.54700 * RK( 598 ) * 
     &                                          + 0.54100 * RK( 599 ) * 
     &                                          +           RK( 602 ) * 
     &                                          + 0.48200 * RK( 604 ) * 
     &                                          + 0.23700 * RK( 605 ) * 
     &                                          + 0.56500 * RK( 606 ) * 
     &                                          + 0.48400 * RK( 607 ) * 
     &                                          + 0.80200 * RK( 608 ) * 
     &                                          + 0.32200 * RK( 609 ) * 
     &                                          + 0.71200 * RK( 610 ) * 
     &                                          +           RK( 646 ) * 
     &                                          + 0.97100 * RK( 647 ) * 
     &                                          + 0.39000 * RK( 648 ) * 
     &                                          + 0.73800 * RK( 649 ) * 
     &                                          + 0.25200 * RK( 650 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
     &                                          +           RK( 658 ) * 
     &                                          + 0.97000 * RK( 659 ) * 
     &                                          + 0.83500 * RK( 660 ) * 
     &                                          + 0.82700 * RK( 661 ) * 
     &                                          + 0.64700 * RK( 662 ) * 
     &                                          + 0.87300 * RK( 663 ) * 
     &                                          + 0.45000 * RK( 664 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
     &                                          + 0.82800 * RK( 666 ) * 
     &                                          + 0.25200 * RK( 667 ) * 
     &                                          + 0.25200 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  33: yROOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( yROOH           ,NO               ) =             RK( 080 ) * 
     &                                          -           RK( 457 ) * 
      PD( yROOH           ,O3P              ) =   0.29000 * RK( 521 ) * 
     &                                          + 0.25000 * RK( 529 ) * 
      PD( yROOH           ,O3               ) =   0.10000 * RK( 259 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.01100 * RK( 553 ) * 
     &                                          + 0.07300 * RK( 557 ) * 
      PD( yROOH           ,NO3              ) =             RK( 082 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.03300 * RK( 280 ) * 
     &                                          -           RK( 459 ) * 
     &                                          +           RK( 520 ) * 
     &                                          +           RK( 524 ) * 
     &                                          +           RK( 528 ) * 
     &                                          + 0.16900 * RK( 554 ) * 
     &                                          + 0.31900 * RK( 558 ) * 
      PD( yROOH           ,OH               ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 218 ) * 
     &                                          +           RK( 220 ) * 
     &                                          + 0.49100 * RK( 224 ) * 
     &                                          +           RK( 225 ) * 
     &                                          + 0.25600 * RK( 228 ) * 
     &                                          + 0.16000 * RK( 230 ) * 
     &                                          + 0.50000 * RK( 258 ) * 
     &                                          +           RK( 263 ) * 
     &                                          + 0.15700 * RK( 273 ) * 
     &                                          + 0.25000 * RK( 278 ) * 
     &                                          + 0.02000 * RK( 283 ) * 
     &                                          + 0.19400 * RK( 285 ) * 
     &                                          +           RK( 518 ) * 
     &                                          +           RK( 522 ) * 
     &                                          +           RK( 526 ) * 
     &                                          + 0.05000 * RK( 546 ) * 
     &                                          +           RK( 547 ) * 
     &                                          +           RK( 548 ) * 
     &                                          + 0.98300 * RK( 549 ) * 
     &                                          + 0.16900 * RK( 552 ) * 
     &                                          + 0.31900 * RK( 556 ) * 
      PD( yROOH           ,HO2              ) = -           RK( 458 ) * 
      PD( yROOH           ,MEO2             ) =             RK( 083 ) * 
     &                                          -           RK( 460 ) * 
      PD( yROOH           ,RO2C             ) =             RK( 084 ) * 
     &                                          -           RK( 461 ) * 
      PD( yROOH           ,RO2XC            ) =             RK( 085 ) * 
     &                                          -           RK( 462 ) * 
      PD( yROOH           ,MECO3            ) =             RK( 086 ) * 
     &                                          -           RK( 463 ) * 
      PD( yROOH           ,CCOOOH           ) =   0.02000 * RK( 283 ) * 
      PD( yROOH           ,CCOOH            ) =   0.49100 * RK( 224 ) * 
      PD( yROOH           ,RCO3             ) =             RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 084 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 086 ) * 
     &                                          + 4.00000 * RK( 087 ) * 
     &                                          +           RK( 098 ) * 
     &                                          +           RK( 110 ) * 
     &                                          -           RK( 464 ) * 
      PD( yROOH           ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( yROOH           ,yROOH            ) = -           RK( 457 ) * 
     &                                          -           RK( 458 ) * 
     &                                          -           RK( 459 ) * 
     &                                          -           RK( 460 ) * 
     &                                          -           RK( 461 ) * 
     &                                          -           RK( 462 ) * 
     &                                          -           RK( 463 ) * 
     &                                          -           RK( 464 ) * 
     &                                          -           RK( 465 ) * 
     &                                          -           RK( 466 ) * 
      PD( yROOH           ,RCOOOH           ) =   0.19400 * RK( 285 ) * 
     &                                          +           RK( 286 )   
      PD( yROOH           ,RCOOH            ) =             RK( 225 ) * 
      PD( yROOH           ,BZCO3            ) =             RK( 098 ) * 
     &                                          -           RK( 465 ) * 
      PD( yROOH           ,MACO3            ) =             RK( 110 ) * 
     &                                          -           RK( 466 ) * 
      PD( yROOH           ,RNO3             ) =   0.15700 * RK( 273 ) * 
     &                                          + 0.06600 * RK( 274 )   
      PD( yROOH           ,ACETONE          ) =             RK( 218 ) * 
     &                                          +           RK( 596 ) * 
      PD( yROOH           ,RCHO             ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 216 )   
     &                                          + 0.10000 * RK( 595 ) * 
      PD( yROOH           ,MEK              ) =             RK( 220 ) * 
     &                                          +           RK( 221 )   
     &                                          +           RK( 597 ) * 
      PD( yROOH           ,ROOH             ) =   0.25600 * RK( 228 ) * 
     &                                          + 0.58600 * RK( 604 ) * 
      PD( yROOH           ,R6OOH            ) =   0.16000 * RK( 230 ) * 
      PD( yROOH           ,MACR             ) =   0.50000 * RK( 258 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.33000 * RK( 262 )   
     &                                          + 0.83500 * RK( 608 ) * 
      PD( yROOH           ,MVK              ) =             RK( 263 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          +           RK( 609 ) * 
      PD( yROOH           ,ACROLEIN         ) =   0.25000 * RK( 278 ) * 
     &                                          + 0.03300 * RK( 280 ) * 
     &                                          + 0.78400 * RK( 607 ) * 
      PD( yROOH           ,ETHENE           ) =             RK( 518 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.29000 * RK( 521 ) * 
      PD( yROOH           ,PROPENE          ) =             RK( 522 ) * 
     &                                          +           RK( 524 ) * 
     &                                          +           RK( 647 ) * 
      PD( yROOH           ,BUTADIENE13      ) =             RK( 526 ) * 
     &                                          +           RK( 528 ) * 
     &                                          + 0.25000 * RK( 529 ) * 
     &                                          +           RK( 648 ) * 
      PD( yROOH           ,ETOH             ) =   0.05000 * RK( 546 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
      PD( yROOH           ,ALK1             ) =             RK( 547 ) * 
     &                                          +           RK( 658 ) * 
      PD( yROOH           ,ALK2             ) =             RK( 548 ) * 
     &                                          +           RK( 659 ) * 
      PD( yROOH           ,ALK3             ) =   0.98300 * RK( 549 ) * 
     &                                          + 0.98300 * RK( 660 ) * 
      PD( yROOH           ,OLE1             ) =   0.16900 * RK( 552 ) * 
     &                                          + 0.01100 * RK( 553 ) * 
     &                                          + 0.16900 * RK( 554 ) * 
     &                                          + 0.16900 * RK( 663 ) * 
      PD( yROOH           ,OLE2             ) =   0.31900 * RK( 556 ) * 
     &                                          + 0.07300 * RK( 557 ) * 
     &                                          + 0.31900 * RK( 558 ) * 
     &                                          + 0.31900 * RK( 664 ) * 
      PD( yROOH           ,CL               ) =   0.10000 * RK( 595 ) * 
     &                                          +           RK( 596 ) * 
     &                                          +           RK( 597 ) * 
     &                                          + 0.58600 * RK( 604 ) * 
     &                                          + 0.78400 * RK( 607 ) * 
     &                                          + 0.83500 * RK( 608 ) * 
     &                                          +           RK( 609 ) * 
     &                                          +           RK( 647 ) * 
     &                                          +           RK( 648 ) * 
     &                                          + 0.31200 * RK( 657 ) * 
     &                                          +           RK( 658 ) * 
     &                                          +           RK( 659 ) * 
     &                                          + 0.98300 * RK( 660 ) * 
     &                                          + 0.16900 * RK( 663 ) * 
     &                                          + 0.31900 * RK( 664 ) * 
      PD( yROOH           ,CLCCHO           ) =             RK( 611 )   
      PD( yROOH           ,CLACET           ) =             RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  34: xCCHO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xCCHO           ,NO               ) =             RK( 080 ) * 
     &                                          -           RK( 297 ) * 
      PD( xCCHO           ,O3               ) =   0.01100 * RK( 553 ) * 
     &                                          + 0.07100 * RK( 557 ) * 
      PD( xCCHO           ,NO3              ) =             RK( 082 ) * 
     &                                          -           RK( 299 ) * 
     &                                          + 0.01300 * RK( 554 ) * 
     &                                          + 0.57900 * RK( 558 ) * 
     &                                          + 0.00200 * RK( 568 ) * 
      PD( xCCHO           ,OH               ) =   0.03500 * RK( 215 ) * 
     &                                          + 0.50400 * RK( 220 ) * 
     &                                          + 0.14200 * RK( 225 ) * 
     &                                          + 0.01200 * RK( 228 ) * 
     &                                          + 0.02000 * RK( 230 ) * 
     &                                          + 0.08300 * RK( 271 ) * 
     &                                          + 0.42900 * RK( 273 ) * 
     &                                          + 0.16700 * RK( 278 ) * 
     &                                          + 0.11000 * RK( 285 ) * 
     &                                          + 0.98400 * RK( 522 ) * 
     &                                          +           RK( 547 ) * 
     &                                          + 0.44500 * RK( 549 ) * 
     &                                          + 0.43800 * RK( 550 ) * 
     &                                          + 0.10600 * RK( 551 ) * 
     &                                          + 0.01000 * RK( 552 ) * 
     &                                          + 0.83500 * RK( 556 ) * 
      PD( xCCHO           ,HO2              ) = -           RK( 298 ) * 
      PD( xCCHO           ,MEO2             ) =             RK( 083 ) * 
     &                                          -           RK( 300 ) * 
      PD( xCCHO           ,RO2C             ) =             RK( 084 ) * 
     &                                          -           RK( 301 ) * 
      PD( xCCHO           ,RO2XC            ) =             RK( 085 ) * 
     &                                          -           RK( 302 ) * 
      PD( xCCHO           ,MECO3            ) =             RK( 086 ) * 
     &                                          -           RK( 303 ) * 
      PD( xCCHO           ,RCO3             ) =             RK( 080 ) * 
     &                                          +           RK( 082 ) * 
     &                                          +           RK( 083 ) * 
     &                                          +           RK( 084 ) * 
     &                                          +           RK( 085 ) * 
     &                                          +           RK( 086 ) * 
     &                                          + 4.00000 * RK( 087 ) * 
     &                                          +           RK( 098 ) * 
     &                                          +           RK( 110 ) * 
     &                                          -           RK( 304 ) * 
      PD( xCCHO           ,PAN2             ) =   0.40000 * RK( 079 )   
      PD( xCCHO           ,xCCHO            ) = -           RK( 297 ) * 
     &                                          -           RK( 298 ) * 
     &                                          -           RK( 299 ) * 
     &                                          -           RK( 300 ) * 
     &                                          -           RK( 301 ) * 
     &                                          -           RK( 302 ) * 
     &                                          -           RK( 303 ) * 
     &                                          -           RK( 304 ) * 
     &                                          -           RK( 305 ) * 
     &                                          -           RK( 306 ) * 
      PD( xCCHO           ,RCOOOH           ) =   0.11000 * RK( 285 ) * 
     &                                          +           RK( 286 )   
      PD( xCCHO           ,RCOOH            ) =   0.14200 * RK( 225 ) * 
      PD( xCCHO           ,BZCO3            ) =             RK( 098 ) * 
     &                                          -           RK( 305 ) * 
      PD( xCCHO           ,MACO3            ) =             RK( 110 ) * 
     &                                          -           RK( 306 ) * 
      PD( xCCHO           ,RNO3             ) =   0.42900 * RK( 273 ) * 
     &                                          + 0.23000 * RK( 274 )   
     &                                          + 0.30000 * RK( 598 ) * 
      PD( xCCHO           ,RCHO             ) =   0.03500 * RK( 215 ) * 
     &                                          +           RK( 216 )   
     &                                          + 0.10000 * RK( 595 ) * 
      PD( xCCHO           ,MEK              ) =   0.50400 * RK( 220 ) * 
     &                                          +           RK( 221 )   
     &                                          + 0.07000 * RK( 597 ) * 
      PD( xCCHO           ,ROOH             ) =   0.01200 * RK( 228 ) * 
     &                                          + 0.10400 * RK( 604 ) * 
      PD( xCCHO           ,R6OOH            ) =   0.02000 * RK( 230 ) * 
     &                                          + 0.02600 * RK( 231 )   
     &                                          + 0.18600 * RK( 605 ) * 
      PD( xCCHO           ,PRD2             ) =   0.08300 * RK( 271 ) * 
     &                                          + 0.16300 * RK( 272 )   
     &                                          + 0.10900 * RK( 599 ) * 
      PD( xCCHO           ,ACROLEIN         ) =   0.16700 * RK( 278 ) * 
      PD( xCCHO           ,PROPENE          ) =   0.98400 * RK( 522 ) * 
      PD( xCCHO           ,ALK1             ) =             RK( 547 ) * 
     &                                          +           RK( 658 ) * 
      PD( xCCHO           ,ALK3             ) =   0.44500 * RK( 549 ) * 
     &                                          + 0.34000 * RK( 660 ) * 
      PD( xCCHO           ,ALK4             ) =   0.43800 * RK( 550 ) * 
     &                                          + 0.28700 * RK( 661 ) * 
      PD( xCCHO           ,ALK5             ) =   0.10600 * RK( 551 ) * 
     &                                          + 0.08000 * RK( 662 ) * 
      PD( xCCHO           ,OLE1             ) =   0.01000 * RK( 552 ) * 
     &                                          + 0.01100 * RK( 553 ) * 
     &                                          + 0.01300 * RK( 554 ) * 
     &                                          + 0.20600 * RK( 663 ) * 
      PD( xCCHO           ,OLE2             ) =   0.83500 * RK( 556 ) * 
     &                                          + 0.07100 * RK( 557 ) * 
     &                                          + 0.57900 * RK( 558 ) * 
     &                                          + 0.38300 * RK( 664 ) * 
      PD( xCCHO           ,SESQ             ) =   0.00200 * RK( 568 ) * 
      PD( xCCHO           ,CL               ) =   0.10000 * RK( 595 ) * 
     &                                          + 0.07000 * RK( 597 ) * 
     &                                          + 0.30000 * RK( 598 ) * 
     &                                          + 0.10900 * RK( 599 ) * 
     &                                          + 0.10400 * RK( 604 ) * 
     &                                          + 0.18600 * RK( 605 ) * 
     &                                          +           RK( 658 ) * 
     &                                          + 0.34000 * RK( 660 ) * 
     &                                          + 0.28700 * RK( 661 ) * 
     &                                          + 0.08000 * RK( 662 ) * 
     &                                          + 0.20600 * RK( 663 ) * 
     &                                          + 0.38300 * RK( 664 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  35: RCOOOH                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RCOOOH          ,OH               ) = -           RK( 285 ) * 
      PD( RCOOOH          ,HO2              ) =   0.75000 * RK( 081 ) * 
     &                                          + 0.75000 * RK( 092 ) * 
     &                                          + 0.75000 * RK( 104 ) * 
      PD( RCOOOH          ,RCO3             ) =   0.75000 * RK( 081 ) * 
      PD( RCOOOH          ,RCOOOH           ) = -           RK( 285 ) * 
     &                                          -           RK( 286 )   
      PD( RCOOOH          ,BZCO3            ) =   0.75000 * RK( 092 ) * 
      PD( RCOOOH          ,MACO3            ) =   0.75000 * RK( 104 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  36: RCOOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RCOOH           ,O3               ) =   0.37200 * RK( 268 ) * 
     &                                          + 0.15900 * RK( 553 ) * 
     &                                          + 0.23500 * RK( 557 ) * 
     &                                          + 0.06800 * RK( 563 ) * 
     &                                          + 0.06800 * RK( 567 ) * 
      PD( RCOOH           ,OH               ) = -           RK( 225 ) * 
      PD( RCOOH           ,HO2              ) =   0.25000 * RK( 081 ) * 
     &                                          + 0.25000 * RK( 092 ) * 
     &                                          + 0.25000 * RK( 104 ) * 
      PD( RCOOH           ,RCO3             ) =   0.25000 * RK( 081 ) * 
      PD( RCOOH           ,RCOOH            ) = -           RK( 225 ) * 
      PD( RCOOH           ,BZCO3            ) =   0.25000 * RK( 092 ) * 
      PD( RCOOH           ,MACO3            ) =   0.25000 * RK( 104 ) * 
      PD( RCOOH           ,IPRD             ) =   0.37200 * RK( 268 ) * 
      PD( RCOOH           ,OLE1             ) =   0.15900 * RK( 553 ) * 
      PD( RCOOH           ,OLE2             ) =   0.23500 * RK( 557 ) * 
      PD( RCOOH           ,TERP             ) =   0.06800 * RK( 563 ) * 
      PD( RCOOH           ,SESQ             ) =   0.06800 * RK( 567 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  37: BZCO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BZCO3           ,NO2              ) = -           RK( 088 ) * 
      PD( BZCO3           ,NO               ) = -           RK( 091 ) * 
      PD( BZCO3           ,NO3              ) = -           RK( 093 ) * 
     &                                          +           RK( 249 ) * 
      PD( BZCO3           ,OH               ) =             RK( 247 ) * 
      PD( BZCO3           ,HO2              ) = -           RK( 092 ) * 
      PD( BZCO3           ,MEO2             ) = -           RK( 094 ) * 
      PD( BZCO3           ,RO2C             ) = -           RK( 095 ) * 
      PD( BZCO3           ,RO2XC            ) = -           RK( 096 ) * 
      PD( BZCO3           ,MECO3            ) = -           RK( 097 ) * 
      PD( BZCO3           ,RCO3             ) = -           RK( 098 ) * 
      PD( BZCO3           ,BZCO3            ) = -           RK( 088 ) * 
     &                                          -           RK( 091 ) * 
     &                                          -           RK( 092 ) * 
     &                                          -           RK( 093 ) * 
     &                                          -           RK( 094 ) * 
     &                                          -           RK( 095 ) * 
     &                                          -           RK( 096 ) * 
     &                                          -           RK( 097 ) * 
     &                                          -           RK( 098 ) * 
     &                                          - 4.00000 * RK( 099 ) * 
     &                                          -           RK( 111 ) * 
      PD( BZCO3           ,PBZN             ) =             RK( 089 )   
     &                                          + 0.60000 * RK( 090 )   
      PD( BZCO3           ,MACO3            ) = -           RK( 111 ) * 
      PD( BZCO3           ,BALD             ) =             RK( 247 ) * 
     &                                          +           RK( 249 ) * 
     &                                          +           RK( 603 ) * 
      PD( BZCO3           ,CL               ) =             RK( 603 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  38: PBZN                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PBZN            ,NO2              ) =             RK( 088 ) * 
      PD( PBZN            ,BZCO3            ) =             RK( 088 ) * 
      PD( PBZN            ,PBZN             ) = -           RK( 089 )   
     &                                          -           RK( 090 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  39: BZO                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BZO             ,NO2              ) = -           RK( 115 ) * 
      PD( BZO             ,NO               ) =             RK( 091 ) * 
      PD( BZO             ,NO3              ) =             RK( 093 ) * 
     &                                          +           RK( 243 ) * 
      PD( BZO             ,OH               ) =   0.20000 * RK( 242 ) * 
     &                                          +           RK( 244 ) * 
      PD( BZO             ,HO2              ) = -           RK( 116 ) * 
      PD( BZO             ,MEO2             ) =             RK( 094 ) * 
      PD( BZO             ,RO2C             ) =             RK( 095 ) * 
      PD( BZO             ,RO2XC            ) =             RK( 096 ) * 
      PD( BZO             ,MECO3            ) =             RK( 097 ) * 
      PD( BZO             ,RCO3             ) =             RK( 098 ) * 
      PD( BZO             ,BZCO3            ) =             RK( 091 ) * 
     &                                          +           RK( 093 ) * 
     &                                          +           RK( 094 ) * 
     &                                          +           RK( 095 ) * 
     &                                          +           RK( 096 ) * 
     &                                          +           RK( 097 ) * 
     &                                          +           RK( 098 ) * 
     &                                          + 4.00000 * RK( 099 ) * 
     &                                          +           RK( 111 ) * 
      PD( BZO             ,PBZN             ) =   0.40000 * RK( 090 )   
      PD( BZO             ,BZO              ) = -           RK( 115 ) * 
     &                                          -           RK( 116 ) * 
     &                                          -           RK( 117 )   
      PD( BZO             ,MACO3            ) =             RK( 111 ) * 
      PD( BZO             ,NPHE             ) =             RK( 244 ) * 
      PD( BZO             ,CRES             ) =   0.20000 * RK( 242 ) * 
     &                                          +           RK( 243 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  40: MACO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MACO3           ,NO2              ) = -           RK( 100 ) * 
      PD( MACO3           ,NO               ) = -           RK( 103 ) * 
     &                                          +           RK( 178 ) * 
      PD( MACO3           ,NO3              ) = -           RK( 105 ) * 
     &                                          +           RK( 180 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.15000 * RK( 269 ) * 
     &                                          + 0.96700 * RK( 280 ) * 
      PD( MACO3           ,OH               ) =   0.21700 * RK( 250 ) * 
     &                                          + 0.21700 * RK( 253 ) * 
     &                                          + 0.20600 * RK( 256 ) * 
     &                                          + 0.50000 * RK( 258 ) * 
     &                                          + 0.28900 * RK( 267 ) * 
     &                                          + 0.75000 * RK( 278 ) * 
      PD( MACO3           ,HO2              ) = -           RK( 104 ) * 
      PD( MACO3           ,MEO2             ) = -           RK( 106 ) * 
     &                                          + 0.50000 * RK( 181 ) * 
      PD( MACO3           ,RO2C             ) = -           RK( 107 ) * 
     &                                          + 0.50000 * RK( 182 ) * 
      PD( MACO3           ,RO2XC            ) = -           RK( 108 ) * 
     &                                          + 0.50000 * RK( 183 ) * 
      PD( MACO3           ,MECO3            ) = -           RK( 109 ) * 
     &                                          +           RK( 184 ) * 
      PD( MACO3           ,RCO3             ) = -           RK( 110 ) * 
     &                                          +           RK( 185 ) * 
      PD( MACO3           ,BZCO3            ) = -           RK( 111 ) * 
     &                                          +           RK( 186 ) * 
      PD( MACO3           ,MACO3            ) = -           RK( 100 ) * 
     &                                          -           RK( 103 ) * 
     &                                          -           RK( 104 ) * 
     &                                          -           RK( 105 ) * 
     &                                          -           RK( 106 ) * 
     &                                          -           RK( 107 ) * 
     &                                          -           RK( 108 ) * 
     &                                          -           RK( 109 ) * 
     &                                          -           RK( 110 ) * 
     &                                          -           RK( 111 ) * 
     &                                          - 4.00000 * RK( 112 ) * 
      PD( MACO3           ,MAPAN            ) =             RK( 101 )   
     &                                          + 0.60000 * RK( 102 )   
      PD( MACO3           ,xMACO3           ) =             RK( 178 ) * 
     &                                          +           RK( 180 ) * 
     &                                          + 0.50000 * RK( 181 ) * 
     &                                          + 0.50000 * RK( 182 ) * 
     &                                          + 0.50000 * RK( 183 ) * 
     &                                          +           RK( 184 ) * 
     &                                          +           RK( 185 ) * 
     &                                          +           RK( 186 ) * 
      PD( MACO3           ,IPRD             ) =   0.28900 * RK( 267 ) * 
     &                                          + 0.15000 * RK( 269 ) * 
     &                                          + 0.15400 * RK( 610 ) * 
      PD( MACO3           ,AFG1             ) =   0.21700 * RK( 250 ) * 
     &                                          + 0.50000 * RK( 252 )   
      PD( MACO3           ,AFG2             ) =   0.21700 * RK( 253 ) * 
      PD( MACO3           ,AFG3             ) =   0.20600 * RK( 256 ) * 
      PD( MACO3           ,MACR             ) =   0.50000 * RK( 258 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.33000 * RK( 262 )   
     &                                          + 0.16500 * RK( 608 ) * 
      PD( MACO3           ,MVK              ) =   0.40000 * RK( 266 )   
      PD( MACO3           ,ACROLEIN         ) =   0.75000 * RK( 278 ) * 
     &                                          + 0.96700 * RK( 280 ) * 
     &                                          + 0.33000 * RK( 282 )   
     &                                          + 0.21600 * RK( 607 ) * 
      PD( MACO3           ,CL               ) =   0.21600 * RK( 607 ) * 
     &                                          + 0.16500 * RK( 608 ) * 
     &                                          + 0.15400 * RK( 610 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  41: MAPAN                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MAPAN           ,NO2              ) =             RK( 100 ) * 
      PD( MAPAN           ,MACO3            ) =             RK( 100 ) * 
      PD( MAPAN           ,MAPAN            ) = -           RK( 101 )   
     &                                          -           RK( 102 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  42: TBUO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TBUO            ,NO2              ) = -           RK( 113 ) * 
      PD( TBUO            ,NO               ) =             RK( 188 ) * 
      PD( TBUO            ,NO3              ) =             RK( 190 ) * 
      PD( TBUO            ,MEO2             ) =   0.50000 * RK( 191 ) * 
      PD( TBUO            ,RO2C             ) =   0.50000 * RK( 192 ) * 
      PD( TBUO            ,RO2XC            ) =   0.50000 * RK( 193 ) * 
      PD( TBUO            ,MECO3            ) =             RK( 194 ) * 
      PD( TBUO            ,RCO3             ) =             RK( 195 ) * 
      PD( TBUO            ,BZCO3            ) =             RK( 196 ) * 
      PD( TBUO            ,MACO3            ) =             RK( 197 ) * 
      PD( TBUO            ,TBUO             ) = -           RK( 113 ) * 
     &                                          -           RK( 114 )   
      PD( TBUO            ,xTBUO            ) =             RK( 188 ) * 
     &                                          +           RK( 190 ) * 
     &                                          + 0.50000 * RK( 191 ) * 
     &                                          + 0.50000 * RK( 192 ) * 
     &                                          + 0.50000 * RK( 193 ) * 
     &                                          +           RK( 194 ) * 
     &                                          +           RK( 195 ) * 
     &                                          +           RK( 196 ) * 
     &                                          +           RK( 197 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  43: RNO3                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RNO3            ,NO2              ) =             RK( 113 ) * 
      PD( RNO3            ,NO               ) =             RK( 447 ) * 
     &                                          +           RK( 487 ) * 
      PD( RNO3            ,NO3              ) =             RK( 449 ) * 
      PD( RNO3            ,OH               ) = - 0.81100 * RK( 273 ) * 
      PD( RNO3            ,MEO2             ) =   0.50000 * RK( 450 ) * 
      PD( RNO3            ,RO2C             ) =   0.50000 * RK( 451 ) * 
      PD( RNO3            ,RO2XC            ) =   0.50000 * RK( 452 ) * 
      PD( RNO3            ,MECO3            ) =             RK( 453 ) * 
      PD( RNO3            ,RCO3             ) =             RK( 454 ) * 
      PD( RNO3            ,BZCO3            ) =             RK( 455 ) * 
      PD( RNO3            ,MACO3            ) =             RK( 456 ) * 
      PD( RNO3            ,TBUO             ) =             RK( 113 ) * 
      PD( RNO3            ,RNO3             ) = - 0.81100 * RK( 273 ) * 
     &                                          -           RK( 274 )   
     &                                          - 0.94500 * RK( 598 ) * 
      PD( RNO3            ,zRNO3            ) =             RK( 487 ) * 
      PD( RNO3            ,xRNO3            ) =             RK( 447 ) * 
     &                                          +           RK( 449 ) * 
     &                                          + 0.50000 * RK( 450 ) * 
     &                                          + 0.50000 * RK( 451 ) * 
     &                                          + 0.50000 * RK( 452 ) * 
     &                                          +           RK( 453 ) * 
     &                                          +           RK( 454 ) * 
     &                                          +           RK( 455 ) * 
     &                                          +           RK( 456 ) * 
      PD( RNO3            ,CL               ) = - 0.94500 * RK( 598 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  44: ACETONE                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACETONE         ,NO               ) =             RK( 317 ) * 
      PD( ACETONE         ,O3               ) =   0.05100 * RK( 557 ) * 
      PD( ACETONE         ,NO3              ) =             RK( 319 ) * 
      PD( ACETONE         ,OH               ) = -           RK( 218 ) * 
      PD( ACETONE         ,MEO2             ) =   0.50000 * RK( 320 ) * 
      PD( ACETONE         ,RO2C             ) =   0.50000 * RK( 321 ) * 
      PD( ACETONE         ,RO2XC            ) =   0.50000 * RK( 322 ) * 
      PD( ACETONE         ,MECO3            ) =             RK( 323 ) * 
      PD( ACETONE         ,RCO3             ) =             RK( 324 ) * 
      PD( ACETONE         ,BZCO3            ) =             RK( 325 ) * 
      PD( ACETONE         ,MACO3            ) =             RK( 326 ) * 
      PD( ACETONE         ,TBUO             ) =             RK( 114 )   
      PD( ACETONE         ,ACETONE          ) = -           RK( 218 ) * 
     &                                          -           RK( 219 )   
     &                                          -           RK( 596 ) * 
      PD( ACETONE         ,xACETONE         ) =             RK( 317 ) * 
     &                                          +           RK( 319 ) * 
     &                                          + 0.50000 * RK( 320 ) * 
     &                                          + 0.50000 * RK( 321 ) * 
     &                                          + 0.50000 * RK( 322 ) * 
     &                                          +           RK( 323 ) * 
     &                                          +           RK( 324 ) * 
     &                                          +           RK( 325 ) * 
     &                                          +           RK( 326 ) * 
      PD( ACETONE         ,OLE2             ) =   0.05100 * RK( 557 ) * 
      PD( ACETONE         ,CL               ) = -           RK( 596 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  45: NPHE                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NPHE            ,NO2              ) =             RK( 115 ) * 
      PD( NPHE            ,OH               ) = -           RK( 244 ) * 
      PD( NPHE            ,BZO              ) =             RK( 115 ) * 
      PD( NPHE            ,NPHE             ) = -           RK( 244 ) * 
     &                                          -           RK( 245 )   
     &                                          -           RK( 246 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  46: CRES                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRES            ,NO3              ) = -           RK( 243 ) * 
      PD( CRES            ,OH               ) = -           RK( 242 ) * 
     &                                          + 0.57000 * RK( 540 ) * 
     &                                          + 0.18100 * RK( 541 ) * 
     &                                          + 0.15900 * RK( 542 ) * 
     &                                          + 0.16100 * RK( 543 ) * 
     &                                          + 0.15900 * RK( 544 ) * 
     &                                          + 0.02200 * RK( 545 ) * 
     &                                          + 0.12300 * RK( 560 ) * 
     &                                          + 0.07700 * RK( 561 ) * 
      PD( CRES            ,HO2              ) =             RK( 116 ) * 
      PD( CRES            ,BZO              ) =             RK( 116 ) * 
     &                                          +           RK( 117 )   
      PD( CRES            ,CRES             ) = -           RK( 242 ) * 
     &                                          -           RK( 243 ) * 
     &                                          -           RK( 602 ) * 
      PD( CRES            ,BENZENE          ) =   0.57000 * RK( 540 ) * 
      PD( CRES            ,TOLUENE          ) =   0.18100 * RK( 541 ) * 
      PD( CRES            ,MXYL             ) =   0.15900 * RK( 542 ) * 
      PD( CRES            ,OXYL             ) =   0.16100 * RK( 543 ) * 
      PD( CRES            ,PXYL             ) =   0.15900 * RK( 544 ) * 
      PD( CRES            ,TRIMETH_BENZ124  ) =   0.02200 * RK( 545 ) * 
      PD( CRES            ,ARO1             ) =   0.12300 * RK( 560 ) * 
      PD( CRES            ,ARO2             ) =   0.07700 * RK( 561 ) * 
      PD( CRES            ,CL               ) = -           RK( 602 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  47: xOH                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xOH             ,NO               ) = -           RK( 128 ) * 
      PD( xOH             ,NO3              ) = -           RK( 130 ) * 
      PD( xOH             ,OH               ) =   0.01200 * RK( 228 ) * 
     &                                          + 0.04100 * RK( 230 ) * 
     &                                          + 0.02400 * RK( 232 ) * 
     &                                          + 0.02000 * RK( 283 ) * 
     &                                          + 0.11000 * RK( 285 ) * 
      PD( xOH             ,HO2              ) = -           RK( 129 ) * 
      PD( xOH             ,MEO2             ) = -           RK( 131 ) * 
      PD( xOH             ,RO2C             ) = -           RK( 132 ) * 
      PD( xOH             ,RO2XC            ) = -           RK( 133 ) * 
      PD( xOH             ,MECO3            ) = -           RK( 134 ) * 
      PD( xOH             ,CCOOOH           ) =   0.02000 * RK( 283 ) * 
      PD( xOH             ,RCO3             ) = -           RK( 135 ) * 
      PD( xOH             ,RCOOOH           ) =   0.11000 * RK( 285 ) * 
      PD( xOH             ,BZCO3            ) = -           RK( 136 ) * 
      PD( xOH             ,MACO3            ) = -           RK( 137 ) * 
      PD( xOH             ,xOH              ) = -           RK( 128 ) * 
     &                                          -           RK( 129 ) * 
     &                                          -           RK( 130 ) * 
     &                                          -           RK( 131 ) * 
     &                                          -           RK( 132 ) * 
     &                                          -           RK( 133 ) * 
     &                                          -           RK( 134 ) * 
     &                                          -           RK( 135 ) * 
     &                                          -           RK( 136 ) * 
     &                                          -           RK( 137 ) * 
      PD( xOH             ,ROOH             ) =   0.01200 * RK( 228 ) * 
     &                                          + 0.10400 * RK( 604 ) * 
      PD( xOH             ,R6OOH            ) =   0.04100 * RK( 230 ) * 
     &                                          + 0.50200 * RK( 605 ) * 
      PD( xOH             ,RAOOH            ) =   0.02400 * RK( 232 ) * 
     &                                          + 0.02400 * RK( 606 ) * 
      PD( xOH             ,CL               ) =   0.10400 * RK( 604 ) * 
     &                                          + 0.50200 * RK( 605 ) * 
     &                                          + 0.02400 * RK( 606 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  48: xNO2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xNO2            ,NO               ) = -           RK( 138 ) * 
      PD( xNO2            ,NO3              ) = -           RK( 140 ) * 
     &                                          + 0.12000 * RK( 528 ) * 
     &                                          + 0.18700 * RK( 532 ) * 
     &                                          + 0.64300 * RK( 536 ) * 
     &                                          + 0.42600 * RK( 558 ) * 
     &                                          + 0.28700 * RK( 564 ) * 
     &                                          + 0.28700 * RK( 568 ) * 
      PD( xNO2            ,OH               ) =   0.31300 * RK( 273 ) * 
      PD( xNO2            ,HO2              ) = -           RK( 139 ) * 
      PD( xNO2            ,MEO2             ) = -           RK( 141 ) * 
      PD( xNO2            ,RO2C             ) = -           RK( 142 ) * 
      PD( xNO2            ,RO2XC            ) = -           RK( 143 ) * 
      PD( xNO2            ,MECO3            ) = -           RK( 144 ) * 
      PD( xNO2            ,RCO3             ) = -           RK( 145 ) * 
      PD( xNO2            ,BZCO3            ) = -           RK( 146 ) * 
      PD( xNO2            ,MACO3            ) = -           RK( 147 ) * 
      PD( xNO2            ,RNO3             ) =   0.31300 * RK( 273 ) * 
     &                                          + 0.15900 * RK( 598 ) * 
      PD( xNO2            ,xNO2             ) = -           RK( 138 ) * 
     &                                          -           RK( 139 ) * 
     &                                          -           RK( 140 ) * 
     &                                          -           RK( 141 ) * 
     &                                          -           RK( 142 ) * 
     &                                          -           RK( 143 ) * 
     &                                          -           RK( 144 ) * 
     &                                          -           RK( 145 ) * 
     &                                          -           RK( 146 ) * 
     &                                          -           RK( 147 ) * 
      PD( xNO2            ,BUTADIENE13      ) =   0.12000 * RK( 528 ) * 
      PD( xNO2            ,ISOPRENE         ) =   0.18700 * RK( 532 ) * 
      PD( xNO2            ,APIN             ) =   0.64300 * RK( 536 ) * 
      PD( xNO2            ,OLE2             ) =   0.42600 * RK( 558 ) * 
      PD( xNO2            ,TERP             ) =   0.28700 * RK( 564 ) * 
      PD( xNO2            ,SESQ             ) =   0.28700 * RK( 568 ) * 
      PD( xNO2            ,CL               ) =   0.15900 * RK( 598 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  49: xMEO2                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMEO2           ,NO               ) = -           RK( 148 ) * 
      PD( xMEO2           ,NO3              ) = -           RK( 150 ) * 
     &                                          + 0.03500 * RK( 558 ) * 
      PD( xMEO2           ,OH               ) =   0.01000 * RK( 550 ) * 
     &                                          + 0.00100 * RK( 552 ) * 
      PD( xMEO2           ,HO2              ) = -           RK( 149 ) * 
      PD( xMEO2           ,MEO2             ) = -           RK( 151 ) * 
      PD( xMEO2           ,RO2C             ) = -           RK( 152 ) * 
      PD( xMEO2           ,RO2XC            ) = -           RK( 153 ) * 
      PD( xMEO2           ,MECO3            ) = -           RK( 154 ) * 
      PD( xMEO2           ,RCO3             ) = -           RK( 155 ) * 
      PD( xMEO2           ,BZCO3            ) = -           RK( 156 ) * 
      PD( xMEO2           ,MACO3            ) = -           RK( 157 ) * 
      PD( xMEO2           ,xMEO2            ) = -           RK( 148 ) * 
     &                                          -           RK( 149 ) * 
     &                                          -           RK( 150 ) * 
     &                                          -           RK( 151 ) * 
     &                                          -           RK( 152 ) * 
     &                                          -           RK( 153 ) * 
     &                                          -           RK( 154 ) * 
     &                                          -           RK( 155 ) * 
     &                                          -           RK( 156 ) * 
     &                                          -           RK( 157 ) * 
      PD( xMEO2           ,ALK4             ) =   0.01000 * RK( 550 ) * 
     &                                          + 0.00300 * RK( 661 ) * 
      PD( xMEO2           ,OLE1             ) =   0.00100 * RK( 552 ) * 
      PD( xMEO2           ,OLE2             ) =   0.03500 * RK( 558 ) * 
     &                                          + 0.00100 * RK( 664 ) * 
      PD( xMEO2           ,CL               ) =   0.00300 * RK( 661 ) * 
     &                                          + 0.00100 * RK( 664 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  50: xMECO3                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMECO3          ,NO               ) = -           RK( 158 ) * 
      PD( xMECO3          ,O3               ) =   0.00100 * RK( 535 ) * 
     &                                          + 0.15600 * RK( 557 ) * 
     &                                          + 0.20200 * RK( 563 ) * 
     &                                          + 0.20200 * RK( 567 ) * 
      PD( xMECO3          ,NO3              ) = -           RK( 160 ) * 
      PD( xMECO3          ,OH               ) =             RK( 218 ) * 
     &                                          + 0.51000 * RK( 220 ) * 
     &                                          + 0.20100 * RK( 250 ) * 
     &                                          + 0.20100 * RK( 253 ) * 
     &                                          + 0.11700 * RK( 256 ) * 
     &                                          + 0.67500 * RK( 263 ) * 
     &                                          + 0.02900 * RK( 271 ) * 
     &                                          + 0.01100 * RK( 550 ) * 
      PD( xMECO3          ,HO2              ) = -           RK( 159 ) * 
      PD( xMECO3          ,MEO2             ) = -           RK( 161 ) * 
      PD( xMECO3          ,RO2C             ) = -           RK( 162 ) * 
      PD( xMECO3          ,RO2XC            ) = -           RK( 163 ) * 
      PD( xMECO3          ,MECO3            ) = -           RK( 164 ) * 
      PD( xMECO3          ,RCO3             ) = -           RK( 165 ) * 
      PD( xMECO3          ,BZCO3            ) = -           RK( 166 ) * 
      PD( xMECO3          ,MACO3            ) = -           RK( 167 ) * 
      PD( xMECO3          ,ACETONE          ) =             RK( 218 ) * 
     &                                          +           RK( 596 ) * 
      PD( xMECO3          ,xMECO3           ) = -           RK( 158 ) * 
     &                                          -           RK( 159 ) * 
     &                                          -           RK( 160 ) * 
     &                                          -           RK( 161 ) * 
     &                                          -           RK( 162 ) * 
     &                                          -           RK( 163 ) * 
     &                                          -           RK( 164 ) * 
     &                                          -           RK( 165 ) * 
     &                                          -           RK( 166 ) * 
     &                                          -           RK( 167 ) * 
      PD( xMECO3          ,MEK              ) =   0.51000 * RK( 220 ) * 
     &                                          + 0.08500 * RK( 597 ) * 
      PD( xMECO3          ,PRD2             ) =   0.02900 * RK( 271 ) * 
     &                                          + 0.00700 * RK( 599 ) * 
      PD( xMECO3          ,AFG1             ) =   0.20100 * RK( 250 ) * 
      PD( xMECO3          ,AFG2             ) =   0.20100 * RK( 253 ) * 
      PD( xMECO3          ,AFG3             ) =   0.11700 * RK( 256 ) * 
      PD( xMECO3          ,MACR             ) =   0.33000 * RK( 262 )   
      PD( xMECO3          ,MVK              ) =   0.67500 * RK( 263 ) * 
     &                                          + 0.62500 * RK( 609 ) * 
      PD( xMECO3          ,APIN             ) =   0.00100 * RK( 535 ) * 
     &                                          + 0.03400 * RK( 650 ) * 
      PD( xMECO3          ,ALK4             ) =   0.01100 * RK( 550 ) * 
     &                                          + 0.00400 * RK( 661 ) * 
      PD( xMECO3          ,OLE2             ) =   0.15600 * RK( 557 ) * 
      PD( xMECO3          ,TERP             ) =   0.20200 * RK( 563 ) * 
     &                                          + 0.03400 * RK( 667 ) * 
      PD( xMECO3          ,SESQ             ) =   0.20200 * RK( 567 ) * 
     &                                          + 0.03400 * RK( 668 ) * 
      PD( xMECO3          ,CL               ) =             RK( 596 ) * 
     &                                          + 0.08500 * RK( 597 ) * 
     &                                          + 0.00700 * RK( 599 ) * 
     &                                          + 0.62500 * RK( 609 ) * 
     &                                          + 0.03400 * RK( 650 ) * 
     &                                          + 0.00400 * RK( 661 ) * 
     &                                          + 0.03400 * RK( 667 ) * 
     &                                          + 0.03400 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  51: xRCO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xRCO3           ,NO               ) = -           RK( 168 ) * 
      PD( xRCO3           ,O3               ) =   0.65200 * RK( 251 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
     &                                          + 0.16300 * RK( 257 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.04800 * RK( 268 ) * 
     &                                          + 0.29700 * RK( 535 ) * 
     &                                          + 0.00800 * RK( 557 ) * 
     &                                          + 0.05900 * RK( 563 ) * 
     &                                          + 0.05900 * RK( 567 ) * 
      PD( xRCO3           ,NO3              ) = -           RK( 170 ) * 
     &                                          + 0.00700 * RK( 536 ) * 
     &                                          + 0.02600 * RK( 564 ) * 
     &                                          + 0.02600 * RK( 568 ) * 
      PD( xRCO3           ,OH               ) =   0.07400 * RK( 220 ) * 
     &                                          + 0.04900 * RK( 271 ) * 
     &                                          + 0.00400 * RK( 534 ) * 
     &                                          + 0.06400 * RK( 562 ) * 
     &                                          + 0.06400 * RK( 566 ) * 
      PD( xRCO3           ,HO2              ) = -           RK( 169 ) * 
      PD( xRCO3           ,MEO2             ) = -           RK( 171 ) * 
      PD( xRCO3           ,RO2C             ) = -           RK( 172 ) * 
      PD( xRCO3           ,RO2XC            ) = -           RK( 173 ) * 
      PD( xRCO3           ,MECO3            ) = -           RK( 174 ) * 
      PD( xRCO3           ,RCO3             ) = -           RK( 175 ) * 
      PD( xRCO3           ,BZCO3            ) = -           RK( 176 ) * 
      PD( xRCO3           ,MACO3            ) = -           RK( 177 ) * 
      PD( xRCO3           ,xRCO3            ) = -           RK( 168 ) * 
     &                                          -           RK( 169 ) * 
     &                                          -           RK( 170 ) * 
     &                                          -           RK( 171 ) * 
     &                                          -           RK( 172 ) * 
     &                                          -           RK( 173 ) * 
     &                                          -           RK( 174 ) * 
     &                                          -           RK( 175 ) * 
     &                                          -           RK( 176 ) * 
     &                                          -           RK( 177 ) * 
      PD( xRCO3           ,MEK              ) =   0.07400 * RK( 220 ) * 
     &                                          + 0.03600 * RK( 597 ) * 
      PD( xRCO3           ,PRD2             ) =   0.04900 * RK( 271 ) * 
     &                                          + 0.02200 * RK( 599 ) * 
      PD( xRCO3           ,IPRD             ) =   0.04800 * RK( 268 ) * 
      PD( xRCO3           ,AFG1             ) =   0.65200 * RK( 251 ) * 
      PD( xRCO3           ,AFG2             ) =   0.65200 * RK( 254 ) * 
      PD( xRCO3           ,AFG3             ) =   0.16300 * RK( 257 ) * 
      PD( xRCO3           ,MACR             ) =   0.10000 * RK( 259 ) * 
      PD( xRCO3           ,MVK              ) =   0.05000 * RK( 264 ) * 
      PD( xRCO3           ,APIN             ) =   0.00400 * RK( 534 ) * 
     &                                          + 0.29700 * RK( 535 ) * 
     &                                          + 0.00700 * RK( 536 ) * 
     &                                          + 0.05000 * RK( 650 ) * 
      PD( xRCO3           ,OLE2             ) =   0.00800 * RK( 557 ) * 
      PD( xRCO3           ,TERP             ) =   0.06400 * RK( 562 ) * 
     &                                          + 0.05900 * RK( 563 ) * 
     &                                          + 0.02600 * RK( 564 ) * 
     &                                          + 0.05000 * RK( 667 ) * 
      PD( xRCO3           ,SESQ             ) =   0.06400 * RK( 566 ) * 
     &                                          + 0.05900 * RK( 567 ) * 
     &                                          + 0.02600 * RK( 568 ) * 
     &                                          + 0.05000 * RK( 668 ) * 
      PD( xRCO3           ,CL               ) =   0.03600 * RK( 597 ) * 
     &                                          + 0.02200 * RK( 599 ) * 
     &                                          + 0.05000 * RK( 650 ) * 
     &                                          + 0.05000 * RK( 667 ) * 
     &                                          + 0.05000 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  52: xMACO3                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMACO3          ,NO               ) = -           RK( 178 ) * 
      PD( xMACO3          ,O3P              ) =   0.11800 * RK( 529 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
      PD( xMACO3          ,O3               ) =   0.19200 * RK( 531 ) * 
      PD( xMACO3          ,NO3              ) = -           RK( 180 ) * 
      PD( xMACO3          ,HO2              ) = -           RK( 179 ) * 
      PD( xMACO3          ,MEO2             ) = -           RK( 181 ) * 
      PD( xMACO3          ,RO2C             ) = -           RK( 182 ) * 
      PD( xMACO3          ,RO2XC            ) = -           RK( 183 ) * 
      PD( xMACO3          ,MECO3            ) = -           RK( 184 ) * 
      PD( xMACO3          ,RCO3             ) = -           RK( 185 ) * 
      PD( xMACO3          ,BZCO3            ) = -           RK( 186 ) * 
      PD( xMACO3          ,MACO3            ) = -           RK( 187 ) * 
      PD( xMACO3          ,xMACO3           ) = -           RK( 178 ) * 
     &                                          -           RK( 179 ) * 
     &                                          -           RK( 180 ) * 
     &                                          -           RK( 181 ) * 
     &                                          -           RK( 182 ) * 
     &                                          -           RK( 183 ) * 
     &                                          -           RK( 184 ) * 
     &                                          -           RK( 185 ) * 
     &                                          -           RK( 186 ) * 
     &                                          -           RK( 187 ) * 
      PD( xMACO3          ,BUTADIENE13      ) =   0.11800 * RK( 529 ) * 
      PD( xMACO3          ,ISOPRENE         ) =   0.19200 * RK( 531 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
      PD( xMACO3          ,APIN             ) =   0.01600 * RK( 650 ) * 
      PD( xMACO3          ,TERP             ) =   0.01600 * RK( 667 ) * 
      PD( xMACO3          ,SESQ             ) =   0.01600 * RK( 668 ) * 
      PD( xMACO3          ,CL               ) =   0.01600 * RK( 650 ) * 
     &                                          + 0.01600 * RK( 667 ) * 
     &                                          + 0.01600 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  53: xTBUO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xTBUO           ,NO               ) = -           RK( 188 ) * 
      PD( xTBUO           ,NO3              ) = -           RK( 190 ) * 
      PD( xTBUO           ,OH               ) =   0.23600 * RK( 549 ) * 
      PD( xTBUO           ,HO2              ) = -           RK( 189 ) * 
      PD( xTBUO           ,MEO2             ) = -           RK( 191 ) * 
      PD( xTBUO           ,RO2C             ) = -           RK( 192 ) * 
      PD( xTBUO           ,RO2XC            ) = -           RK( 193 ) * 
      PD( xTBUO           ,MECO3            ) = -           RK( 194 ) * 
      PD( xTBUO           ,RCO3             ) = -           RK( 195 ) * 
      PD( xTBUO           ,BZCO3            ) = -           RK( 196 ) * 
      PD( xTBUO           ,MACO3            ) = -           RK( 197 ) * 
      PD( xTBUO           ,xTBUO            ) = -           RK( 188 ) * 
     &                                          -           RK( 189 ) * 
     &                                          -           RK( 190 ) * 
     &                                          -           RK( 191 ) * 
     &                                          -           RK( 192 ) * 
     &                                          -           RK( 193 ) * 
     &                                          -           RK( 194 ) * 
     &                                          -           RK( 195 ) * 
     &                                          -           RK( 196 ) * 
     &                                          -           RK( 197 ) * 
      PD( xTBUO           ,ALK3             ) =   0.23600 * RK( 549 ) * 
     &                                          + 0.09400 * RK( 660 ) * 
      PD( xTBUO           ,CL               ) =   0.09400 * RK( 660 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  54: xCO                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xCO             ,NO               ) = -           RK( 198 ) * 
      PD( xCO             ,O3P              ) =   0.27800 * RK( 521 ) * 
     &                                          + 0.11500 * RK( 529 ) * 
      PD( xCO             ,O3               ) =   0.05100 * RK( 535 ) * 
      PD( xCO             ,NO3              ) = -           RK( 200 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.57200 * RK( 269 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          + 0.00500 * RK( 536 ) * 
     &                                          + 0.01200 * RK( 564 ) * 
     &                                          + 0.01200 * RK( 568 ) * 
      PD( xCO             ,OH               ) =   0.03500 * RK( 215 ) * 
     &                                          + 0.33400 * RK( 250 ) * 
     &                                          + 0.33400 * RK( 253 ) * 
     &                                          + 0.11400 * RK( 256 ) * 
     &                                          + 0.41600 * RK( 258 ) * 
     &                                          + 0.33600 * RK( 267 ) * 
     &                                          + 0.16700 * RK( 278 ) * 
     &                                          + 0.00200 * RK( 534 ) * 
     &                                          + 0.00200 * RK( 550 ) * 
     &                                          + 0.00100 * RK( 562 ) * 
     &                                          + 0.00100 * RK( 566 ) * 
      PD( xCO             ,HO2              ) = -           RK( 199 ) * 
      PD( xCO             ,MEO2             ) = -           RK( 201 ) * 
      PD( xCO             ,RO2C             ) = -           RK( 202 ) * 
      PD( xCO             ,RO2XC            ) = -           RK( 203 ) * 
      PD( xCO             ,MECO3            ) = -           RK( 204 ) * 
      PD( xCO             ,RCO3             ) = -           RK( 205 ) * 
      PD( xCO             ,BZCO3            ) = -           RK( 206 ) * 
      PD( xCO             ,MACO3            ) = -           RK( 207 ) * 
      PD( xCO             ,xCO              ) = -           RK( 198 ) * 
     &                                          -           RK( 199 ) * 
     &                                          -           RK( 200 ) * 
     &                                          -           RK( 201 ) * 
     &                                          -           RK( 202 ) * 
     &                                          -           RK( 203 ) * 
     &                                          -           RK( 204 ) * 
     &                                          -           RK( 205 ) * 
     &                                          -           RK( 206 ) * 
     &                                          -           RK( 207 ) * 
      PD( xCO             ,RCHO             ) =   0.03500 * RK( 215 ) * 
     &                                          + 0.10000 * RK( 595 ) * 
      PD( xCO             ,IPRD             ) =   0.33600 * RK( 267 ) * 
     &                                          + 0.57200 * RK( 269 ) * 
     &                                          + 0.49800 * RK( 610 ) * 
      PD( xCO             ,AFG1             ) =   0.33400 * RK( 250 ) * 
      PD( xCO             ,AFG2             ) =   0.33400 * RK( 253 ) * 
      PD( xCO             ,AFG3             ) =   0.11400 * RK( 256 ) * 
      PD( xCO             ,MACR             ) =   0.41600 * RK( 258 ) * 
     &                                          + 0.50000 * RK( 260 ) * 
     &                                          + 0.54100 * RK( 608 ) * 
      PD( xCO             ,ACROLEIN         ) =   0.16700 * RK( 278 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          + 0.48400 * RK( 607 ) * 
      PD( xCO             ,ETHENE           ) =   0.27800 * RK( 521 ) * 
      PD( xCO             ,BUTADIENE13      ) =   0.11500 * RK( 529 ) * 
      PD( xCO             ,APIN             ) =   0.00200 * RK( 534 ) * 
     &                                          + 0.05100 * RK( 535 ) * 
     &                                          + 0.00500 * RK( 536 ) * 
     &                                          + 0.03500 * RK( 650 ) * 
      PD( xCO             ,ALK4             ) =   0.00200 * RK( 550 ) * 
     &                                          + 0.00300 * RK( 661 ) * 
      PD( xCO             ,TERP             ) =   0.00100 * RK( 562 ) * 
     &                                          + 0.01200 * RK( 564 ) * 
     &                                          + 0.03500 * RK( 667 ) * 
      PD( xCO             ,SESQ             ) =   0.00100 * RK( 566 ) * 
     &                                          + 0.01200 * RK( 568 ) * 
     &                                          + 0.03500 * RK( 668 ) * 
      PD( xCO             ,CL               ) =   0.10000 * RK( 595 ) * 
     &                                          + 0.48400 * RK( 607 ) * 
     &                                          + 0.54100 * RK( 608 ) * 
     &                                          + 0.49800 * RK( 610 ) * 
     &                                          + 0.03500 * RK( 650 ) * 
     &                                          + 0.00300 * RK( 661 ) * 
     &                                          + 0.03500 * RK( 667 ) * 
     &                                          + 0.03500 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  55: CCHO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CCHO            ,NO               ) =             RK( 297 ) * 
      PD( CCHO            ,O3P              ) =   0.10000 * RK( 521 ) * 
      PD( CCHO            ,O3               ) =   0.50000 * RK( 523 ) * 
     &                                          + 0.45300 * RK( 557 ) * 
      PD( CCHO            ,NO3              ) = -           RK( 214 ) * 
     &                                          +           RK( 299 ) * 
      PD( CCHO            ,OH               ) = -           RK( 212 ) * 
     &                                          + 0.00100 * RK( 271 ) * 
     &                                          + 0.95000 * RK( 546 ) * 
      PD( CCHO            ,MEO2             ) =   0.50000 * RK( 300 ) * 
      PD( CCHO            ,RO2C             ) =   0.50000 * RK( 301 ) * 
      PD( CCHO            ,RO2XC            ) =   0.50000 * RK( 302 ) * 
      PD( CCHO            ,MECO3            ) =             RK( 303 ) * 
      PD( CCHO            ,RCO3             ) =             RK( 304 ) * 
      PD( CCHO            ,xCCHO            ) =             RK( 297 ) * 
     &                                          +           RK( 299 ) * 
     &                                          + 0.50000 * RK( 300 ) * 
     &                                          + 0.50000 * RK( 301 ) * 
     &                                          + 0.50000 * RK( 302 ) * 
     &                                          +           RK( 303 ) * 
     &                                          +           RK( 304 ) * 
     &                                          +           RK( 305 ) * 
     &                                          +           RK( 306 ) * 
      PD( CCHO            ,BZCO3            ) =             RK( 305 ) * 
      PD( CCHO            ,MACO3            ) =             RK( 306 ) * 
      PD( CCHO            ,RNO3             ) =   0.21400 * RK( 274 )   
      PD( CCHO            ,CCHO             ) = -           RK( 212 ) * 
     &                                          -           RK( 213 )   
     &                                          -           RK( 214 ) * 
     &                                          -           RK( 593 ) * 
      PD( CCHO            ,PRD2             ) =   0.00100 * RK( 271 ) * 
      PD( CCHO            ,ETHENE           ) =   0.10000 * RK( 521 ) * 
      PD( CCHO            ,PROPENE          ) =   0.50000 * RK( 523 ) * 
      PD( CCHO            ,ETOH             ) =   0.95000 * RK( 546 ) * 
     &                                          + 0.68800 * RK( 657 ) * 
      PD( CCHO            ,OLE2             ) =   0.45300 * RK( 557 ) * 
      PD( CCHO            ,CL               ) = -           RK( 593 ) * 
     &                                          + 0.68800 * RK( 657 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  56: RCHO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RCHO            ,NO               ) =             RK( 307 ) * 
      PD( RCHO            ,O3P              ) =             RK( 261 ) * 
     &                                          + 0.45000 * RK( 265 ) * 
     &                                          +           RK( 281 ) * 
     &                                          + 0.45000 * RK( 525 ) * 
     &                                          + 0.45000 * RK( 555 ) * 
     &                                          + 0.07900 * RK( 559 ) * 
     &                                          + 0.23700 * RK( 565 ) * 
     &                                          + 0.23700 * RK( 569 ) * 
      PD( RCHO            ,O3               ) =   0.50000 * RK( 553 ) * 
     &                                          + 0.33300 * RK( 557 ) * 
      PD( RCHO            ,NO3              ) = -           RK( 217 ) * 
     &                                          +           RK( 309 ) * 
      PD( RCHO            ,OH               ) = -           RK( 215 ) * 
     &                                          + 0.74400 * RK( 228 ) * 
     &                                          + 0.14300 * RK( 271 ) * 
     &                                          + 0.00100 * RK( 273 ) * 
      PD( RCHO            ,MEO2             ) =   0.50000 * RK( 310 ) * 
      PD( RCHO            ,RO2C             ) =   0.50000 * RK( 311 ) * 
      PD( RCHO            ,RO2XC            ) =   0.50000 * RK( 312 ) * 
      PD( RCHO            ,MECO3            ) =             RK( 313 ) * 
      PD( RCHO            ,RCO3             ) =             RK( 314 ) * 
      PD( RCHO            ,BZCO3            ) =             RK( 315 ) * 
      PD( RCHO            ,MACO3            ) =             RK( 316 ) * 
      PD( RCHO            ,RNO3             ) =   0.00100 * RK( 273 ) * 
     &                                          + 0.07400 * RK( 274 )   
     &                                          + 0.00900 * RK( 598 ) * 
      PD( RCHO            ,RCHO             ) = -           RK( 215 ) * 
     &                                          -           RK( 216 )   
     &                                          -           RK( 217 ) * 
     &                                          -           RK( 595 ) * 
      PD( RCHO            ,xRCHO            ) =             RK( 307 ) * 
     &                                          +           RK( 309 ) * 
     &                                          + 0.50000 * RK( 310 ) * 
     &                                          + 0.50000 * RK( 311 ) * 
     &                                          + 0.50000 * RK( 312 ) * 
     &                                          +           RK( 313 ) * 
     &                                          +           RK( 314 ) * 
     &                                          +           RK( 315 ) * 
     &                                          +           RK( 316 ) * 
      PD( RCHO            ,ROOH             ) =   0.74400 * RK( 228 ) * 
     &                                          +           RK( 229 )   
     &                                          + 0.41400 * RK( 604 ) * 
      PD( RCHO            ,R6OOH            ) =   0.08500 * RK( 231 )   
      PD( RCHO            ,PRD2             ) =   0.14300 * RK( 271 ) * 
     &                                          + 0.19800 * RK( 599 ) * 
      PD( RCHO            ,MACR             ) =             RK( 261 ) * 
      PD( RCHO            ,MVK              ) =   0.45000 * RK( 265 ) * 
      PD( RCHO            ,ACROLEIN         ) =             RK( 281 ) * 
      PD( RCHO            ,PROPENE          ) =   0.45000 * RK( 525 ) * 
      PD( RCHO            ,OLE1             ) =   0.50000 * RK( 553 ) * 
     &                                          + 0.45000 * RK( 555 ) * 
      PD( RCHO            ,OLE2             ) =   0.33300 * RK( 557 ) * 
     &                                          + 0.07900 * RK( 559 ) * 
      PD( RCHO            ,TERP             ) =   0.23700 * RK( 565 ) * 
      PD( RCHO            ,SESQ             ) =   0.23700 * RK( 569 ) * 
      PD( RCHO            ,CL               ) = -           RK( 595 ) * 
     &                                          + 0.00900 * RK( 598 ) * 
     &                                          + 0.19800 * RK( 599 ) * 
     &                                          + 0.41400 * RK( 604 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  57: xHCHO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xHCHO           ,NO               ) = -           RK( 287 ) * 
      PD( xHCHO           ,O3P              ) =   0.27800 * RK( 521 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
      PD( xHCHO           ,O3               ) =   0.65200 * RK( 251 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
     &                                          + 0.16300 * RK( 257 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
     &                                          + 0.00100 * RK( 268 ) * 
     &                                          + 0.19200 * RK( 531 ) * 
     &                                          + 0.34400 * RK( 535 ) * 
     &                                          + 0.11400 * RK( 557 ) * 
     &                                          + 0.03300 * RK( 563 ) * 
     &                                          + 0.03300 * RK( 567 ) * 
      PD( xHCHO           ,NO3              ) =   0.22700 * RK( 269 ) * 
     &                                          -           RK( 289 ) * 
     &                                          + 0.11500 * RK( 528 ) * 
     &                                          + 0.00700 * RK( 536 ) * 
     &                                          + 0.07200 * RK( 558 ) * 
     &                                          + 0.02300 * RK( 564 ) * 
     &                                          + 0.02300 * RK( 568 ) * 
      PD( xHCHO           ,OH               ) =             RK( 218 ) * 
     &                                          + 0.08800 * RK( 220 ) * 
     &                                          + 0.01200 * RK( 228 ) * 
     &                                          + 0.08400 * RK( 258 ) * 
     &                                          + 0.30000 * RK( 263 ) * 
     &                                          + 0.05500 * RK( 267 ) * 
     &                                          + 0.21100 * RK( 271 ) * 
     &                                          + 0.01100 * RK( 273 ) * 
     &                                          + 0.08300 * RK( 278 ) * 
     &                                          + 0.02000 * RK( 283 ) * 
     &                                          + 1.61000 * RK( 518 ) * 
     &                                          + 0.98400 * RK( 522 ) * 
     &                                          + 0.70800 * RK( 526 ) * 
     &                                          + 0.62400 * RK( 530 ) * 
     &                                          + 0.02200 * RK( 534 ) * 
     &                                          + 0.08100 * RK( 546 ) * 
     &                                          + 0.02600 * RK( 549 ) * 
     &                                          + 0.02900 * RK( 550 ) * 
     &                                          + 0.04000 * RK( 551 ) * 
     &                                          + 0.58200 * RK( 552 ) * 
     &                                          + 0.17900 * RK( 556 ) * 
     &                                          + 0.41100 * RK( 562 ) * 
     &                                          + 0.41100 * RK( 566 ) * 
      PD( xHCHO           ,HO2              ) = -           RK( 288 ) * 
      PD( xHCHO           ,MEO2             ) = -           RK( 290 ) * 
      PD( xHCHO           ,RO2C             ) = -           RK( 291 ) * 
      PD( xHCHO           ,RO2XC            ) = -           RK( 292 ) * 
      PD( xHCHO           ,MECO3            ) = -           RK( 293 ) * 
      PD( xHCHO           ,CCOOOH           ) =   0.02000 * RK( 283 ) * 
      PD( xHCHO           ,RCO3             ) = -           RK( 294 ) * 
      PD( xHCHO           ,BZCO3            ) = -           RK( 295 ) * 
      PD( xHCHO           ,MACO3            ) = -           RK( 296 ) * 
      PD( xHCHO           ,RNO3             ) =   0.01100 * RK( 273 ) * 
     &                                          + 0.06100 * RK( 274 )   
     &                                          + 0.04500 * RK( 598 ) * 
      PD( xHCHO           ,ACETONE          ) =             RK( 218 ) * 
     &                                          +           RK( 596 ) * 
      PD( xHCHO           ,xHCHO            ) = -           RK( 287 ) * 
     &                                          -           RK( 288 ) * 
     &                                          -           RK( 289 ) * 
     &                                          -           RK( 290 ) * 
     &                                          -           RK( 291 ) * 
     &                                          -           RK( 292 ) * 
     &                                          -           RK( 293 ) * 
     &                                          -           RK( 294 ) * 
     &                                          -           RK( 295 ) * 
     &                                          -           RK( 296 ) * 
      PD( xHCHO           ,MEK              ) =   0.08800 * RK( 220 ) * 
     &                                          + 0.06500 * RK( 597 ) * 
      PD( xHCHO           ,ROOH             ) =   0.01200 * RK( 228 ) * 
     &                                          + 0.10600 * RK( 604 ) * 
      PD( xHCHO           ,PRD2             ) =   0.21100 * RK( 271 ) * 
     &                                          + 0.30300 * RK( 272 )   
     &                                          + 0.23700 * RK( 599 ) * 
      PD( xHCHO           ,IPRD             ) =   0.05500 * RK( 267 ) * 
     &                                          + 0.00100 * RK( 268 ) * 
     &                                          + 0.22700 * RK( 269 ) * 
     &                                          + 0.19500 * RK( 610 ) * 
      PD( xHCHO           ,AFG1             ) =   0.65200 * RK( 251 ) * 
      PD( xHCHO           ,AFG2             ) =   0.65200 * RK( 254 ) * 
      PD( xHCHO           ,AFG3             ) =   0.16300 * RK( 257 ) * 
      PD( xHCHO           ,MACR             ) =   0.08400 * RK( 258 ) * 
     &                                          + 0.10000 * RK( 259 ) * 
     &                                          + 0.33000 * RK( 262 )   
      PD( xHCHO           ,MVK              ) =   0.30000 * RK( 263 ) * 
     &                                          + 0.05000 * RK( 264 ) * 
      PD( xHCHO           ,ACROLEIN         ) =   0.08300 * RK( 278 ) * 
     &                                          + 0.27400 * RK( 607 ) * 
      PD( xHCHO           ,ETHENE           ) =   1.61000 * RK( 518 ) * 
     &                                          + 0.27800 * RK( 521 ) * 
     &                                          +           RK( 646 ) * 
      PD( xHCHO           ,PROPENE          ) =   0.98400 * RK( 522 ) * 
      PD( xHCHO           ,BUTADIENE13      ) =   0.70800 * RK( 526 ) * 
     &                                          + 0.11500 * RK( 528 ) * 
     &                                          + 0.86300 * RK( 648 ) * 
      PD( xHCHO           ,ISOPRENE         ) =   0.62400 * RK( 530 ) * 
     &                                          + 0.19200 * RK( 531 ) * 
     &                                          + 0.24000 * RK( 533 ) * 
     &                                          + 0.27500 * RK( 649 ) * 
      PD( xHCHO           ,APIN             ) =   0.02200 * RK( 534 ) * 
     &                                          + 0.34400 * RK( 535 ) * 
     &                                          + 0.00700 * RK( 536 ) * 
     &                                          + 0.15800 * RK( 650 ) * 
      PD( xHCHO           ,ETOH             ) =   0.08100 * RK( 546 ) * 
     &                                          + 0.50300 * RK( 657 ) * 
      PD( xHCHO           ,ALK3             ) =   0.02600 * RK( 549 ) * 
     &                                          + 0.07800 * RK( 660 ) * 
      PD( xHCHO           ,ALK4             ) =   0.02900 * RK( 550 ) * 
     &                                          + 0.03400 * RK( 661 ) * 
      PD( xHCHO           ,ALK5             ) =   0.04000 * RK( 551 ) * 
     &                                          + 0.02200 * RK( 662 ) * 
      PD( xHCHO           ,OLE1             ) =   0.58200 * RK( 552 ) * 
     &                                          + 0.03600 * RK( 663 ) * 
      PD( xHCHO           ,OLE2             ) =   0.17900 * RK( 556 ) * 
     &                                          + 0.11400 * RK( 557 ) * 
     &                                          + 0.07200 * RK( 558 ) * 
     &                                          + 0.19000 * RK( 664 ) * 
      PD( xHCHO           ,TERP             ) =   0.41100 * RK( 562 ) * 
     &                                          + 0.03300 * RK( 563 ) * 
     &                                          + 0.02300 * RK( 564 ) * 
     &                                          + 0.15800 * RK( 667 ) * 
      PD( xHCHO           ,SESQ             ) =   0.41100 * RK( 566 ) * 
     &                                          + 0.03300 * RK( 567 ) * 
     &                                          + 0.02300 * RK( 568 ) * 
     &                                          + 0.15800 * RK( 668 ) * 
      PD( xHCHO           ,CL               ) =             RK( 596 ) * 
     &                                          + 0.06500 * RK( 597 ) * 
     &                                          + 0.04500 * RK( 598 ) * 
     &                                          + 0.23700 * RK( 599 ) * 
     &                                          + 0.10600 * RK( 604 ) * 
     &                                          + 0.27400 * RK( 607 ) * 
     &                                          + 0.19500 * RK( 610 ) * 
     &                                          +           RK( 646 ) * 
     &                                          + 0.86300 * RK( 648 ) * 
     &                                          + 0.27500 * RK( 649 ) * 
     &                                          + 0.15800 * RK( 650 ) * 
     &                                          + 0.50300 * RK( 657 ) * 
     &                                          + 0.07800 * RK( 660 ) * 
     &                                          + 0.03400 * RK( 661 ) * 
     &                                          + 0.02200 * RK( 662 ) * 
     &                                          + 0.03600 * RK( 663 ) * 
     &                                          + 0.19000 * RK( 664 ) * 
     &                                          + 0.15800 * RK( 667 ) * 
     &                                          + 0.15800 * RK( 668 ) * 
      PD( xHCHO           ,CLCCHO           ) =             RK( 611 )   
      PD( xHCHO           ,CLACET           ) =             RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  58: MEK                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEK             ,NO               ) =             RK( 327 ) * 
      PD( MEK             ,O3P              ) =   0.55000 * RK( 265 ) * 
     &                                          + 0.55000 * RK( 525 ) * 
     &                                          + 0.39000 * RK( 555 ) * 
     &                                          + 0.75100 * RK( 559 ) * 
      PD( MEK             ,O3               ) =   0.21000 * RK( 268 ) * 
     &                                          + 0.00800 * RK( 535 ) * 
     &                                          + 0.00900 * RK( 553 ) * 
     &                                          + 0.03300 * RK( 557 ) * 
     &                                          + 0.00200 * RK( 563 ) * 
     &                                          + 0.00200 * RK( 567 ) * 
      PD( MEK             ,NO3              ) =             RK( 329 ) * 
      PD( MEK             ,OH               ) = -           RK( 220 ) * 
     &                                          + 0.01000 * RK( 273 ) * 
      PD( MEK             ,MEO2             ) =   0.50000 * RK( 330 ) * 
     &                                          + 0.50000 * RK( 460 ) * 
      PD( MEK             ,RO2C             ) =   0.50000 * RK( 331 ) * 
     &                                          + 0.50000 * RK( 461 ) * 
      PD( MEK             ,RO2XC            ) =   0.50000 * RK( 332 ) * 
     &                                          + 0.50000 * RK( 462 ) * 
      PD( MEK             ,MECO3            ) =             RK( 333 ) * 
      PD( MEK             ,RCO3             ) =             RK( 334 ) * 
      PD( MEK             ,yROOH            ) =   0.50000 * RK( 460 ) * 
     &                                          + 0.50000 * RK( 461 ) * 
     &                                          + 0.50000 * RK( 462 ) * 
      PD( MEK             ,BZCO3            ) =             RK( 335 ) * 
      PD( MEK             ,MACO3            ) =             RK( 336 ) * 
      PD( MEK             ,RNO3             ) =   0.01000 * RK( 273 ) * 
     &                                          + 0.12400 * RK( 274 )   
     &                                          + 0.01800 * RK( 598 ) * 
      PD( MEK             ,MEK              ) = -           RK( 220 ) * 
     &                                          -           RK( 221 )   
     &                                          -           RK( 597 ) * 
      PD( MEK             ,IPRD             ) =   0.21000 * RK( 268 ) * 
     &                                          + 0.23300 * RK( 270 )   
      PD( MEK             ,xMEK             ) =             RK( 327 ) * 
     &                                          +           RK( 329 ) * 
     &                                          + 0.50000 * RK( 330 ) * 
     &                                          + 0.50000 * RK( 331 ) * 
     &                                          + 0.50000 * RK( 332 ) * 
     &                                          +           RK( 333 ) * 
     &                                          +           RK( 334 ) * 
     &                                          +           RK( 335 ) * 
     &                                          +           RK( 336 ) * 
      PD( MEK             ,MVK              ) =   0.55000 * RK( 265 ) * 
      PD( MEK             ,PROPENE          ) =   0.55000 * RK( 525 ) * 
      PD( MEK             ,APIN             ) =   0.00800 * RK( 535 ) * 
      PD( MEK             ,OLE1             ) =   0.00900 * RK( 553 ) * 
     &                                          + 0.39000 * RK( 555 ) * 
      PD( MEK             ,OLE2             ) =   0.03300 * RK( 557 ) * 
     &                                          + 0.75100 * RK( 559 ) * 
      PD( MEK             ,TERP             ) =   0.00200 * RK( 563 ) * 
      PD( MEK             ,SESQ             ) =   0.00200 * RK( 567 ) * 
      PD( MEK             ,CL               ) = -           RK( 597 ) * 
     &                                          + 0.01800 * RK( 598 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  59: zRNO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( zRNO3           ,NO               ) = -           RK( 487 ) * 
      PD( zRNO3           ,O3P              ) =   0.01500 * RK( 529 ) * 
     &                                          + 0.01000 * RK( 533 ) * 
      PD( zRNO3           ,O3               ) =   0.00700 * RK( 257 ) * 
     &                                          + 0.00800 * RK( 531 ) * 
     &                                          + 0.33700 * RK( 535 ) * 
     &                                          + 0.00500 * RK( 553 ) * 
     &                                          + 0.00300 * RK( 557 ) * 
     &                                          + 0.12100 * RK( 563 ) * 
     &                                          + 0.12100 * RK( 567 ) * 
      PD( zRNO3           ,NO3              ) =   0.05100 * RK( 269 ) * 
     &                                          + 0.00200 * RK( 280 ) * 
     &                                          -           RK( 489 ) * 
     &                                          + 0.05100 * RK( 524 ) * 
     &                                          + 0.06500 * RK( 528 ) * 
     &                                          + 0.06400 * RK( 532 ) * 
     &                                          + 0.29300 * RK( 536 ) * 
     &                                          + 0.22800 * RK( 554 ) * 
     &                                          + 0.14000 * RK( 558 ) * 
     &                                          + 0.46000 * RK( 564 ) * 
     &                                          + 0.46000 * RK( 568 ) * 
      PD( zRNO3           ,OH               ) =   0.03900 * RK( 220 ) * 
     &                                          + 0.00400 * RK( 228 ) * 
     &                                          + 0.02900 * RK( 230 ) * 
     &                                          + 0.12400 * RK( 232 ) * 
     &                                          + 0.06000 * RK( 250 ) * 
     &                                          + 0.06000 * RK( 253 ) * 
     &                                          + 0.11700 * RK( 256 ) * 
     &                                          + 0.02500 * RK( 263 ) * 
     &                                          + 0.04100 * RK( 267 ) * 
     &                                          + 0.07100 * RK( 271 ) * 
     &                                          + 0.17500 * RK( 273 ) * 
     &                                          + 0.01600 * RK( 522 ) * 
     &                                          + 0.04900 * RK( 526 ) * 
     &                                          + 0.09300 * RK( 530 ) * 
     &                                          + 0.19700 * RK( 534 ) * 
     &                                          + 0.02400 * RK( 540 ) * 
     &                                          + 0.05400 * RK( 541 ) * 
     &                                          + 0.08200 * RK( 542 ) * 
     &                                          + 0.08700 * RK( 543 ) * 
     &                                          + 0.07600 * RK( 544 ) * 
     &                                          + 0.12100 * RK( 545 ) * 
     &                                          + 0.03500 * RK( 548 ) * 
     &                                          + 0.07000 * RK( 549 ) * 
     &                                          + 0.14900 * RK( 550 ) * 
     &                                          + 0.35300 * RK( 551 ) * 
     &                                          + 0.12800 * RK( 552 ) * 
     &                                          + 0.08800 * RK( 556 ) * 
     &                                          + 0.11000 * RK( 560 ) * 
     &                                          + 0.12800 * RK( 561 ) * 
     &                                          + 0.20100 * RK( 562 ) * 
     &                                          + 0.20100 * RK( 566 ) * 
      PD( zRNO3           ,HO2              ) = -           RK( 488 ) * 
      PD( zRNO3           ,MEO2             ) = -           RK( 490 ) * 
      PD( zRNO3           ,RO2C             ) = -           RK( 491 ) * 
      PD( zRNO3           ,RO2XC            ) = -           RK( 492 ) * 
      PD( zRNO3           ,MECO3            ) = -           RK( 493 ) * 
      PD( zRNO3           ,RCO3             ) = -           RK( 494 ) * 
      PD( zRNO3           ,BZCO3            ) = -           RK( 495 ) * 
      PD( zRNO3           ,MACO3            ) = -           RK( 496 ) * 
      PD( zRNO3           ,RNO3             ) =   0.17500 * RK( 273 ) * 
     &                                          + 0.10200 * RK( 274 )   
     &                                          + 0.20200 * RK( 598 ) * 
      PD( zRNO3           ,MEK              ) =   0.03900 * RK( 220 ) * 
     &                                          + 0.03900 * RK( 597 ) * 
      PD( zRNO3           ,zRNO3            ) = -           RK( 487 ) * 
     &                                          -           RK( 488 ) * 
     &                                          -           RK( 489 ) * 
     &                                          -           RK( 490 ) * 
     &                                          -           RK( 491 ) * 
     &                                          -           RK( 492 ) * 
     &                                          -           RK( 493 ) * 
     &                                          -           RK( 494 ) * 
     &                                          -           RK( 495 ) * 
     &                                          -           RK( 496 ) * 
      PD( zRNO3           ,ROOH             ) =   0.00400 * RK( 228 ) * 
      PD( zRNO3           ,R6OOH            ) =   0.02900 * RK( 230 ) * 
     &                                          + 0.07700 * RK( 231 )   
     &                                          + 0.11700 * RK( 605 ) * 
      PD( zRNO3           ,PRD2             ) =   0.07100 * RK( 271 ) * 
     &                                          + 0.08700 * RK( 272 )   
     &                                          + 0.11600 * RK( 599 ) * 
      PD( zRNO3           ,RAOOH            ) =   0.12400 * RK( 232 ) * 
     &                                          + 0.12400 * RK( 606 ) * 
      PD( zRNO3           ,IPRD             ) =   0.04100 * RK( 267 ) * 
     &                                          + 0.05100 * RK( 269 ) * 
     &                                          + 0.05100 * RK( 610 ) * 
      PD( zRNO3           ,AFG1             ) =   0.06000 * RK( 250 ) * 
      PD( zRNO3           ,AFG2             ) =   0.06000 * RK( 253 ) * 
      PD( zRNO3           ,AFG3             ) =   0.11700 * RK( 256 ) * 
     &                                          + 0.00700 * RK( 257 ) * 
      PD( zRNO3           ,MACR             ) =   0.03300 * RK( 608 ) * 
      PD( zRNO3           ,MVK              ) =   0.02500 * RK( 263 ) * 
     &                                          + 0.05300 * RK( 609 ) * 
      PD( zRNO3           ,ACROLEIN         ) =   0.00200 * RK( 280 ) * 
     &                                          + 0.02600 * RK( 607 ) * 
      PD( zRNO3           ,PROPENE          ) =   0.01600 * RK( 522 ) * 
     &                                          + 0.05100 * RK( 524 ) * 
     &                                          + 0.02900 * RK( 647 ) * 
      PD( zRNO3           ,BUTADIENE13      ) =   0.04900 * RK( 526 ) * 
     &                                          + 0.06500 * RK( 528 ) * 
     &                                          + 0.01500 * RK( 529 ) * 
     &                                          + 0.06900 * RK( 648 ) * 
      PD( zRNO3           ,ISOPRENE         ) =   0.09300 * RK( 530 ) * 
     &                                          + 0.00800 * RK( 531 ) * 
     &                                          + 0.06400 * RK( 532 ) * 
     &                                          + 0.01000 * RK( 533 ) * 
     &                                          + 0.08500 * RK( 649 ) * 
      PD( zRNO3           ,APIN             ) =   0.19700 * RK( 534 ) * 
     &                                          + 0.33700 * RK( 535 ) * 
     &                                          + 0.29300 * RK( 536 ) * 
     &                                          + 0.58200 * RK( 650 ) * 
      PD( zRNO3           ,BENZENE          ) =   0.02400 * RK( 540 ) * 
      PD( zRNO3           ,TOLUENE          ) =   0.05400 * RK( 541 ) * 
     &                                          + 0.10600 * RK( 652 ) * 
      PD( zRNO3           ,MXYL             ) =   0.08200 * RK( 542 ) * 
     &                                          + 0.13600 * RK( 653 ) * 
      PD( zRNO3           ,OXYL             ) =   0.08700 * RK( 543 ) * 
     &                                          + 0.13600 * RK( 654 ) * 
      PD( zRNO3           ,PXYL             ) =   0.07600 * RK( 544 ) * 
     &                                          + 0.13600 * RK( 655 ) * 
      PD( zRNO3           ,TRIMETH_BENZ124  ) =   0.12100 * RK( 545 ) * 
     &                                          + 0.16200 * RK( 656 ) * 
      PD( zRNO3           ,ALK2             ) =   0.03500 * RK( 548 ) * 
     &                                          + 0.03000 * RK( 659 ) * 
      PD( zRNO3           ,ALK3             ) =   0.07000 * RK( 549 ) * 
     &                                          + 0.07000 * RK( 660 ) * 
      PD( zRNO3           ,ALK4             ) =   0.14900 * RK( 550 ) * 
     &                                          + 0.16500 * RK( 661 ) * 
      PD( zRNO3           ,ALK5             ) =   0.35300 * RK( 551 ) * 
     &                                          + 0.35200 * RK( 662 ) * 
      PD( zRNO3           ,OLE1             ) =   0.12800 * RK( 552 ) * 
     &                                          + 0.00500 * RK( 553 ) * 
     &                                          + 0.22800 * RK( 554 ) * 
     &                                          + 0.12700 * RK( 663 ) * 
      PD( zRNO3           ,OLE2             ) =   0.08800 * RK( 556 ) * 
     &                                          + 0.00300 * RK( 557 ) * 
     &                                          + 0.14000 * RK( 558 ) * 
     &                                          + 0.10600 * RK( 664 ) * 
      PD( zRNO3           ,ARO1             ) =   0.11000 * RK( 560 ) * 
     &                                          + 0.16000 * RK( 665 ) * 
      PD( zRNO3           ,ARO2             ) =   0.12800 * RK( 561 ) * 
     &                                          + 0.17200 * RK( 666 ) * 
      PD( zRNO3           ,TERP             ) =   0.20100 * RK( 562 ) * 
     &                                          + 0.12100 * RK( 563 ) * 
     &                                          + 0.46000 * RK( 564 ) * 
     &                                          + 0.58200 * RK( 667 ) * 
      PD( zRNO3           ,SESQ             ) =   0.20100 * RK( 566 ) * 
     &                                          + 0.12100 * RK( 567 ) * 
     &                                          + 0.46000 * RK( 568 ) * 
     &                                          + 0.58200 * RK( 668 ) * 
      PD( zRNO3           ,CL               ) =   0.03900 * RK( 597 ) * 
     &                                          + 0.20200 * RK( 598 ) * 
     &                                          + 0.11600 * RK( 599 ) * 
     &                                          + 0.11700 * RK( 605 ) * 
     &                                          + 0.12400 * RK( 606 ) * 
     &                                          + 0.02600 * RK( 607 ) * 
     &                                          + 0.03300 * RK( 608 ) * 
     &                                          + 0.05300 * RK( 609 ) * 
     &                                          + 0.05100 * RK( 610 ) * 
     &                                          + 0.02900 * RK( 647 ) * 
     &                                          + 0.06900 * RK( 648 ) * 
     &                                          + 0.08500 * RK( 649 ) * 
     &                                          + 0.58200 * RK( 650 ) * 
     &                                          + 0.10600 * RK( 652 ) * 
     &                                          + 0.13600 * RK( 653 ) * 
     &                                          + 0.13600 * RK( 654 ) * 
     &                                          + 0.13600 * RK( 655 ) * 
     &                                          + 0.16200 * RK( 656 ) * 
     &                                          + 0.03000 * RK( 659 ) * 
     &                                          + 0.07000 * RK( 660 ) * 
     &                                          + 0.16500 * RK( 661 ) * 
     &                                          + 0.35200 * RK( 662 ) * 
     &                                          + 0.12700 * RK( 663 ) * 
     &                                          + 0.10600 * RK( 664 ) * 
     &                                          + 0.16000 * RK( 665 ) * 
     &                                          + 0.17200 * RK( 666 ) * 
     &                                          + 0.58200 * RK( 667 ) * 
     &                                          + 0.58200 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  60: xRCHO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xRCHO           ,NO               ) = -           RK( 307 ) * 
      PD( xRCHO           ,O3               ) =   0.24000 * RK( 535 ) * 
     &                                          + 0.04400 * RK( 553 ) * 
     &                                          + 0.01900 * RK( 557 ) * 
     &                                          + 0.20800 * RK( 563 ) * 
     &                                          + 0.20800 * RK( 567 ) * 
      PD( xRCHO           ,NO3              ) =   0.21800 * RK( 269 ) * 
     &                                          -           RK( 309 ) * 
     &                                          +           RK( 520 ) * 
     &                                          + 0.68400 * RK( 536 ) * 
     &                                          + 0.00300 * RK( 554 ) * 
     &                                          + 0.16300 * RK( 558 ) * 
     &                                          + 0.40300 * RK( 564 ) * 
     &                                          + 0.40300 * RK( 568 ) * 
      PD( xRCHO           ,OH               ) =   0.37600 * RK( 220 ) * 
     &                                          + 0.40000 * RK( 225 ) * 
     &                                          + 0.20500 * RK( 228 ) * 
     &                                          + 0.07500 * RK( 230 ) * 
     &                                          + 0.44800 * RK( 232 ) * 
     &                                          + 0.40700 * RK( 250 ) * 
     &                                          + 0.40700 * RK( 253 ) * 
     &                                          + 0.01300 * RK( 267 ) * 
     &                                          + 0.40200 * RK( 271 ) * 
     &                                          + 0.03600 * RK( 273 ) * 
     &                                          + 0.08400 * RK( 285 ) * 
     &                                          + 0.77600 * RK( 534 ) * 
     &                                          + 0.26100 * RK( 548 ) * 
     &                                          + 0.12200 * RK( 549 ) * 
     &                                          + 0.23600 * RK( 550 ) * 
     &                                          + 0.20900 * RK( 551 ) * 
     &                                          + 0.66600 * RK( 552 ) * 
     &                                          + 0.51000 * RK( 556 ) * 
     &                                          + 0.38500 * RK( 562 ) * 
     &                                          + 0.38500 * RK( 566 ) * 
      PD( xRCHO           ,HO2              ) = -           RK( 308 ) * 
      PD( xRCHO           ,MEO2             ) = -           RK( 310 ) * 
      PD( xRCHO           ,RO2C             ) = -           RK( 311 ) * 
      PD( xRCHO           ,RO2XC            ) = -           RK( 312 ) * 
      PD( xRCHO           ,MECO3            ) = -           RK( 313 ) * 
      PD( xRCHO           ,RCO3             ) = -           RK( 314 ) * 
      PD( xRCHO           ,RCOOOH           ) =   0.08400 * RK( 285 ) * 
      PD( xRCHO           ,RCOOH            ) =   0.40000 * RK( 225 ) * 
      PD( xRCHO           ,BZCO3            ) = -           RK( 315 ) * 
      PD( xRCHO           ,MACO3            ) = -           RK( 316 ) * 
      PD( xRCHO           ,RNO3             ) =   0.03600 * RK( 273 ) * 
     &                                          + 0.06300 * RK( 274 )   
     &                                          + 0.02000 * RK( 598 ) * 
      PD( xRCHO           ,MEK              ) =   0.37600 * RK( 220 ) * 
     &                                          + 0.84000 * RK( 597 ) * 
      PD( xRCHO           ,xRCHO            ) = -           RK( 307 ) * 
     &                                          -           RK( 308 ) * 
     &                                          -           RK( 309 ) * 
     &                                          -           RK( 310 ) * 
     &                                          -           RK( 311 ) * 
     &                                          -           RK( 312 ) * 
     &                                          -           RK( 313 ) * 
     &                                          -           RK( 314 ) * 
     &                                          -           RK( 315 ) * 
     &                                          -           RK( 316 ) * 
      PD( xRCHO           ,ROOH             ) =   0.20500 * RK( 228 ) * 
     &                                          + 0.19700 * RK( 604 ) * 
      PD( xRCHO           ,R6OOH            ) =   0.07500 * RK( 230 ) * 
     &                                          + 0.05800 * RK( 231 )   
     &                                          + 0.67600 * RK( 605 ) * 
      PD( xRCHO           ,PRD2             ) =   0.40200 * RK( 271 ) * 
     &                                          + 0.78000 * RK( 272 )   
     &                                          + 0.59100 * RK( 599 ) * 
      PD( xRCHO           ,RAOOH            ) =   0.44800 * RK( 232 ) * 
     &                                          + 0.44800 * RK( 606 ) * 
      PD( xRCHO           ,IPRD             ) =   0.01300 * RK( 267 ) * 
     &                                          + 0.21800 * RK( 269 ) * 
      PD( xRCHO           ,AFG1             ) =   0.40700 * RK( 250 ) * 
      PD( xRCHO           ,AFG2             ) =   0.40700 * RK( 253 ) * 
      PD( xRCHO           ,ETHENE           ) =             RK( 520 ) * 
      PD( xRCHO           ,APIN             ) =   0.77600 * RK( 534 ) * 
     &                                          + 0.24000 * RK( 535 ) * 
     &                                          + 0.68400 * RK( 536 ) * 
     &                                          + 0.18500 * RK( 650 ) * 
      PD( xRCHO           ,ALK2             ) =   0.26100 * RK( 548 ) * 
     &                                          + 0.48200 * RK( 659 ) * 
      PD( xRCHO           ,ALK3             ) =   0.12200 * RK( 549 ) * 
     &                                          + 0.34300 * RK( 660 ) * 
      PD( xRCHO           ,ALK4             ) =   0.23600 * RK( 550 ) * 
     &                                          + 0.41200 * RK( 661 ) * 
      PD( xRCHO           ,ALK5             ) =   0.20900 * RK( 551 ) * 
     &                                          + 0.25800 * RK( 662 ) * 
      PD( xRCHO           ,OLE1             ) =   0.66600 * RK( 552 ) * 
     &                                          + 0.04400 * RK( 553 ) * 
     &                                          + 0.00300 * RK( 554 ) * 
     &                                          + 0.07200 * RK( 663 ) * 
      PD( xRCHO           ,OLE2             ) =   0.51000 * RK( 556 ) * 
     &                                          + 0.01900 * RK( 557 ) * 
     &                                          + 0.16300 * RK( 558 ) * 
     &                                          + 0.31700 * RK( 664 ) * 
      PD( xRCHO           ,TERP             ) =   0.38500 * RK( 562 ) * 
     &                                          + 0.20800 * RK( 563 ) * 
     &                                          + 0.40300 * RK( 564 ) * 
     &                                          + 0.18500 * RK( 667 ) * 
      PD( xRCHO           ,SESQ             ) =   0.38500 * RK( 566 ) * 
     &                                          + 0.20800 * RK( 567 ) * 
     &                                          + 0.40300 * RK( 568 ) * 
     &                                          + 0.18500 * RK( 668 ) * 
      PD( xRCHO           ,CL               ) =   0.84000 * RK( 597 ) * 
     &                                          + 0.02000 * RK( 598 ) * 
     &                                          + 0.59100 * RK( 599 ) * 
     &                                          + 0.19700 * RK( 604 ) * 
     &                                          + 0.67600 * RK( 605 ) * 
     &                                          + 0.44800 * RK( 606 ) * 
     &                                          + 0.18500 * RK( 650 ) * 
     &                                          + 0.48200 * RK( 659 ) * 
     &                                          + 0.34300 * RK( 660 ) * 
     &                                          + 0.41200 * RK( 661 ) * 
     &                                          + 0.25800 * RK( 662 ) * 
     &                                          + 0.07200 * RK( 663 ) * 
     &                                          + 0.31700 * RK( 664 ) * 
     &                                          + 0.18500 * RK( 667 ) * 
     &                                          + 0.18500 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  61: HCOOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCOOH           ,O3               ) =   0.33300 * RK( 259 ) * 
     &                                          + 0.35100 * RK( 264 ) * 
     &                                          + 0.10000 * RK( 268 ) * 
     &                                          + 0.18500 * RK( 279 ) * 
     &                                          + 0.37000 * RK( 519 ) * 
     &                                          + 0.18500 * RK( 523 ) * 
     &                                          + 0.18500 * RK( 527 ) * 
     &                                          + 0.20400 * RK( 531 ) * 
     &                                          + 0.18500 * RK( 553 ) * 
     &                                          + 0.02400 * RK( 557 ) * 
     &                                          + 0.17200 * RK( 563 ) * 
     &                                          + 0.17200 * RK( 567 ) * 
      PD( HCOOH           ,OH               ) = -           RK( 223 ) * 
     &                                          + 0.30000 * RK( 538 ) * 
      PD( HCOOH           ,HCOOH            ) = -           RK( 223 ) * 
      PD( HCOOH           ,IPRD             ) =   0.10000 * RK( 268 ) * 
      PD( HCOOH           ,MACR             ) =   0.33300 * RK( 259 ) * 
      PD( HCOOH           ,MVK              ) =   0.35100 * RK( 264 ) * 
      PD( HCOOH           ,ACROLEIN         ) =   0.18500 * RK( 279 ) * 
      PD( HCOOH           ,ETHENE           ) =   0.37000 * RK( 519 ) * 
      PD( HCOOH           ,PROPENE          ) =   0.18500 * RK( 523 ) * 
      PD( HCOOH           ,BUTADIENE13      ) =   0.18500 * RK( 527 ) * 
      PD( HCOOH           ,ISOPRENE         ) =   0.20400 * RK( 531 ) * 
      PD( HCOOH           ,ACETYLENE        ) =   0.30000 * RK( 538 ) * 
      PD( HCOOH           ,OLE1             ) =   0.18500 * RK( 553 ) * 
      PD( HCOOH           ,OLE2             ) =   0.02400 * RK( 557 ) * 
      PD( HCOOH           ,TERP             ) =   0.17200 * RK( 563 ) * 
      PD( HCOOH           ,SESQ             ) =   0.17200 * RK( 567 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  62: xMGLY                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMGLY           ,NO               ) = -           RK( 357 ) * 
      PD( xMGLY           ,O3               ) =   0.09500 * RK( 257 ) * 
     &                                          + 0.00300 * RK( 563 ) * 
     &                                          + 0.00300 * RK( 567 ) * 
      PD( xMGLY           ,NO3              ) =   0.00800 * RK( 269 ) * 
     &                                          -           RK( 359 ) * 
     &                                          + 0.00200 * RK( 536 ) * 
      PD( xMGLY           ,OH               ) =   0.49100 * RK( 224 ) * 
     &                                          + 0.25200 * RK( 232 ) * 
     &                                          + 0.25000 * RK( 242 ) * 
     &                                          + 0.26700 * RK( 250 ) * 
     &                                          + 0.26700 * RK( 253 ) * 
     &                                          + 0.15300 * RK( 256 ) * 
     &                                          + 0.08400 * RK( 258 ) * 
     &                                          + 0.30000 * RK( 263 ) * 
     &                                          + 0.17400 * RK( 267 ) * 
     &                                          + 0.02000 * RK( 534 ) * 
     &                                          + 0.15100 * RK( 541 ) * 
     &                                          + 0.38000 * RK( 542 ) * 
     &                                          + 0.23800 * RK( 543 ) * 
     &                                          + 0.11200 * RK( 544 ) * 
     &                                          + 0.40500 * RK( 545 ) * 
     &                                          + 0.10000 * RK( 560 ) * 
     &                                          + 0.31200 * RK( 561 ) * 
     &                                          + 0.00300 * RK( 562 ) * 
     &                                          + 0.00300 * RK( 566 ) * 
      PD( xMGLY           ,HO2              ) = -           RK( 358 ) * 
      PD( xMGLY           ,MEO2             ) = -           RK( 360 ) * 
      PD( xMGLY           ,RO2C             ) = -           RK( 361 ) * 
      PD( xMGLY           ,RO2XC            ) = -           RK( 362 ) * 
      PD( xMGLY           ,MECO3            ) = -           RK( 363 ) * 
      PD( xMGLY           ,CCOOH            ) =   0.49100 * RK( 224 ) * 
      PD( xMGLY           ,RCO3             ) = -           RK( 364 ) * 
      PD( xMGLY           ,BZCO3            ) = -           RK( 365 ) * 
      PD( xMGLY           ,MACO3            ) = -           RK( 366 ) * 
      PD( xMGLY           ,CRES             ) =   0.25000 * RK( 242 ) * 
      PD( xMGLY           ,xMGLY            ) = -           RK( 357 ) * 
     &                                          -           RK( 358 ) * 
     &                                          -           RK( 359 ) * 
     &                                          -           RK( 360 ) * 
     &                                          -           RK( 361 ) * 
     &                                          -           RK( 362 ) * 
     &                                          -           RK( 363 ) * 
     &                                          -           RK( 364 ) * 
     &                                          -           RK( 365 ) * 
     &                                          -           RK( 366 ) * 
      PD( xMGLY           ,RAOOH            ) =   0.25200 * RK( 232 ) * 
     &                                          + 0.25200 * RK( 606 ) * 
      PD( xMGLY           ,IPRD             ) =   0.17400 * RK( 267 ) * 
     &                                          + 0.00800 * RK( 269 ) * 
     &                                          + 0.01700 * RK( 610 ) * 
      PD( xMGLY           ,AFG1             ) =   0.26700 * RK( 250 ) * 
      PD( xMGLY           ,AFG2             ) =   0.26700 * RK( 253 ) * 
      PD( xMGLY           ,AFG3             ) =   0.15300 * RK( 256 ) * 
     &                                          + 0.09500 * RK( 257 ) * 
      PD( xMGLY           ,MACR             ) =   0.08400 * RK( 258 ) * 
      PD( xMGLY           ,MVK              ) =   0.30000 * RK( 263 ) * 
      PD( xMGLY           ,APIN             ) =   0.02000 * RK( 534 ) * 
     &                                          + 0.00200 * RK( 536 ) * 
      PD( xMGLY           ,TOLUENE          ) =   0.15100 * RK( 541 ) * 
      PD( xMGLY           ,MXYL             ) =   0.38000 * RK( 542 ) * 
      PD( xMGLY           ,OXYL             ) =   0.23800 * RK( 543 ) * 
      PD( xMGLY           ,PXYL             ) =   0.11200 * RK( 544 ) * 
      PD( xMGLY           ,TRIMETH_BENZ124  ) =   0.40500 * RK( 545 ) * 
      PD( xMGLY           ,ARO1             ) =   0.10000 * RK( 560 ) * 
      PD( xMGLY           ,ARO2             ) =   0.31200 * RK( 561 ) * 
      PD( xMGLY           ,TERP             ) =   0.00300 * RK( 562 ) * 
     &                                          + 0.00300 * RK( 563 ) * 
      PD( xMGLY           ,SESQ             ) =   0.00300 * RK( 566 ) * 
     &                                          + 0.00300 * RK( 567 ) * 
      PD( xMGLY           ,CL               ) =   0.25200 * RK( 606 ) * 
     &                                          + 0.01700 * RK( 610 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  63: xBACL                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xBACL           ,NO               ) = -           RK( 367 ) * 
      PD( xBACL           ,O3               ) =   0.08100 * RK( 535 ) * 
     &                                          + 0.03900 * RK( 563 ) * 
     &                                          + 0.03900 * RK( 567 ) * 
      PD( xBACL           ,NO3              ) = -           RK( 369 ) * 
      PD( xBACL           ,OH               ) =   0.45700 * RK( 225 ) * 
     &                                          + 0.01900 * RK( 256 ) * 
     &                                          + 0.02300 * RK( 534 ) * 
     &                                          + 0.18500 * RK( 543 ) * 
     &                                          + 0.11200 * RK( 545 ) * 
     &                                          + 0.13400 * RK( 561 ) * 
     &                                          + 0.00900 * RK( 562 ) * 
     &                                          + 0.00900 * RK( 566 ) * 
      PD( xBACL           ,HO2              ) = -           RK( 368 ) * 
      PD( xBACL           ,MEO2             ) = -           RK( 370 ) * 
      PD( xBACL           ,RO2C             ) = -           RK( 371 ) * 
      PD( xBACL           ,RO2XC            ) = -           RK( 372 ) * 
      PD( xBACL           ,MECO3            ) = -           RK( 373 ) * 
      PD( xBACL           ,RCO3             ) = -           RK( 374 ) * 
      PD( xBACL           ,RCOOH            ) =   0.45700 * RK( 225 ) * 
      PD( xBACL           ,BZCO3            ) = -           RK( 375 ) * 
      PD( xBACL           ,MACO3            ) = -           RK( 376 ) * 
      PD( xBACL           ,xBACL            ) = -           RK( 367 ) * 
     &                                          -           RK( 368 ) * 
     &                                          -           RK( 369 ) * 
     &                                          -           RK( 370 ) * 
     &                                          -           RK( 371 ) * 
     &                                          -           RK( 372 ) * 
     &                                          -           RK( 373 ) * 
     &                                          -           RK( 374 ) * 
     &                                          -           RK( 375 ) * 
     &                                          -           RK( 376 ) * 
      PD( xBACL           ,AFG3             ) =   0.01900 * RK( 256 ) * 
      PD( xBACL           ,APIN             ) =   0.02300 * RK( 534 ) * 
     &                                          + 0.08100 * RK( 535 ) * 
     &                                          + 0.00300 * RK( 650 ) * 
      PD( xBACL           ,OXYL             ) =   0.18500 * RK( 543 ) * 
      PD( xBACL           ,TRIMETH_BENZ124  ) =   0.11200 * RK( 545 ) * 
      PD( xBACL           ,ARO2             ) =   0.13400 * RK( 561 ) * 
      PD( xBACL           ,TERP             ) =   0.00900 * RK( 562 ) * 
     &                                          + 0.03900 * RK( 563 ) * 
     &                                          + 0.00300 * RK( 667 ) * 
      PD( xBACL           ,SESQ             ) =   0.00900 * RK( 566 ) * 
     &                                          + 0.03900 * RK( 567 ) * 
     &                                          + 0.00300 * RK( 668 ) * 
      PD( xBACL           ,CL               ) =   0.00300 * RK( 650 ) * 
     &                                          + 0.00300 * RK( 667 ) * 
     &                                          + 0.00300 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  64: ROOH                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROOH            ,OH               ) = -           RK( 228 ) * 
      PD( ROOH            ,HO2              ) =             RK( 458 ) * 
      PD( ROOH            ,yROOH            ) =             RK( 458 ) * 
      PD( ROOH            ,ROOH             ) = -           RK( 228 ) * 
     &                                          -           RK( 229 )   
     &                                          -           RK( 604 ) * 
      PD( ROOH            ,CL               ) = -           RK( 604 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  65: xPROD2                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xPROD2          ,NO               ) = -           RK( 337 ) * 
      PD( xPROD2          ,NO3              ) = -           RK( 339 ) * 
      PD( xPROD2          ,OH               ) =   0.03400 * RK( 228 ) * 
     &                                          + 0.08400 * RK( 230 ) * 
     &                                          + 0.33200 * RK( 267 ) * 
     &                                          + 0.00700 * RK( 271 ) * 
     &                                          + 0.03100 * RK( 273 ) * 
     &                                          + 0.14600 * RK( 550 ) * 
     &                                          + 0.40700 * RK( 551 ) * 
     &                                          + 0.16800 * RK( 552 ) * 
     &                                          + 0.02300 * RK( 556 ) * 
     &                                          + 0.30900 * RK( 560 ) * 
     &                                          + 0.05700 * RK( 561 ) * 
     &                                          + 0.40900 * RK( 562 ) * 
     &                                          + 0.40900 * RK( 566 ) * 
      PD( xPROD2          ,HO2              ) = -           RK( 338 ) * 
      PD( xPROD2          ,MEO2             ) = -           RK( 340 ) * 
      PD( xPROD2          ,RO2C             ) = -           RK( 341 ) * 
      PD( xPROD2          ,RO2XC            ) = -           RK( 342 ) * 
      PD( xPROD2          ,MECO3            ) = -           RK( 343 ) * 
      PD( xPROD2          ,RCO3             ) = -           RK( 344 ) * 
      PD( xPROD2          ,BZCO3            ) = -           RK( 345 ) * 
      PD( xPROD2          ,MACO3            ) = -           RK( 346 ) * 
      PD( xPROD2          ,RNO3             ) =   0.03100 * RK( 273 ) * 
     &                                          + 0.26100 * RK( 274 )   
     &                                          + 0.04600 * RK( 598 ) * 
      PD( xPROD2          ,ROOH             ) =   0.03400 * RK( 228 ) * 
      PD( xPROD2          ,xPROD2           ) = -           RK( 337 ) * 
     &                                          -           RK( 338 ) * 
     &                                          -           RK( 339 ) * 
     &                                          -           RK( 340 ) * 
     &                                          -           RK( 341 ) * 
     &                                          -           RK( 342 ) * 
     &                                          -           RK( 343 ) * 
     &                                          -           RK( 344 ) * 
     &                                          -           RK( 345 ) * 
     &                                          -           RK( 346 ) * 
      PD( xPROD2          ,R6OOH            ) =   0.08400 * RK( 230 ) * 
     &                                          + 0.69800 * RK( 231 )   
     &                                          + 0.28000 * RK( 605 ) * 
      PD( xPROD2          ,PRD2             ) =   0.00700 * RK( 271 ) * 
     &                                          + 0.04000 * RK( 599 ) * 
      PD( xPROD2          ,IPRD             ) =   0.33200 * RK( 267 ) * 
      PD( xPROD2          ,ALK4             ) =   0.14600 * RK( 550 ) * 
     &                                          + 0.13000 * RK( 661 ) * 
      PD( xPROD2          ,ALK5             ) =   0.40700 * RK( 551 ) * 
     &                                          + 0.37800 * RK( 662 ) * 
      PD( xPROD2          ,OLE1             ) =   0.16800 * RK( 552 ) * 
      PD( xPROD2          ,OLE2             ) =   0.02300 * RK( 556 ) * 
      PD( xPROD2          ,ARO1             ) =   0.30900 * RK( 560 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
      PD( xPROD2          ,ARO2             ) =   0.05700 * RK( 561 ) * 
     &                                          + 0.35900 * RK( 666 ) * 
      PD( xPROD2          ,TERP             ) =   0.40900 * RK( 562 ) * 
      PD( xPROD2          ,SESQ             ) =   0.40900 * RK( 566 ) * 
      PD( xPROD2          ,CL               ) =   0.04600 * RK( 598 ) * 
     &                                          + 0.04000 * RK( 599 ) * 
     &                                          + 0.28000 * RK( 605 ) * 
     &                                          + 0.13000 * RK( 661 ) * 
     &                                          + 0.37800 * RK( 662 ) * 
     &                                          + 0.84000 * RK( 665 ) * 
     &                                          + 0.35900 * RK( 666 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  66: R6OOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( R6OOH           ,OH               ) = -           RK( 230 ) * 
      PD( R6OOH           ,HO2              ) =             RK( 468 ) * 
      PD( R6OOH           ,R6OOH            ) = -           RK( 230 ) * 
     &                                          -           RK( 231 )   
     &                                          -           RK( 605 ) * 
      PD( R6OOH           ,yR6OOH           ) =             RK( 468 ) * 
      PD( R6OOH           ,CL               ) = -           RK( 605 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  67: PRD2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PRD2            ,NO               ) =             RK( 337 ) * 
      PD( PRD2            ,O3P              ) =   0.75000 * RK( 529 ) * 
     &                                          + 0.75000 * RK( 533 ) * 
     &                                          +           RK( 537 ) * 
     &                                          + 0.16000 * RK( 555 ) * 
     &                                          + 0.17000 * RK( 559 ) * 
     &                                          + 0.76300 * RK( 565 ) * 
     &                                          + 0.76300 * RK( 569 ) * 
      PD( PRD2            ,O3               ) =   0.12500 * RK( 527 ) * 
     &                                          + 0.10000 * RK( 531 ) * 
     &                                          + 0.25500 * RK( 535 ) * 
     &                                          + 0.26800 * RK( 553 ) * 
     &                                          + 0.03700 * RK( 557 ) * 
     &                                          + 0.50200 * RK( 563 ) * 
     &                                          + 0.50200 * RK( 567 ) * 
      PD( PRD2            ,NO3              ) =             RK( 339 ) * 
     &                                          +           RK( 489 ) * 
      PD( PRD2            ,OH               ) =   0.84000 * RK( 230 ) * 
     &                                          + 0.07400 * RK( 232 ) * 
     &                                          - 0.67100 * RK( 271 ) * 
     &                                          + 0.00800 * RK( 273 ) * 
      PD( PRD2            ,MEO2             ) =   0.50000 * RK( 340 ) * 
     &                                          + 0.50000 * RK( 470 ) * 
     &                                          + 0.50000 * RK( 480 ) * 
     &                                          + 0.50000 * RK( 490 ) * 
      PD( PRD2            ,RO2C             ) =   0.50000 * RK( 341 ) * 
     &                                          + 0.50000 * RK( 471 ) * 
     &                                          + 0.50000 * RK( 481 ) * 
     &                                          + 0.50000 * RK( 491 ) * 
      PD( PRD2            ,RO2XC            ) =   0.50000 * RK( 342 ) * 
     &                                          + 0.50000 * RK( 472 ) * 
     &                                          + 0.50000 * RK( 482 ) * 
     &                                          + 0.50000 * RK( 492 ) * 
      PD( PRD2            ,MECO3            ) =             RK( 343 ) * 
     &                                          +           RK( 493 ) * 
      PD( PRD2            ,RCO3             ) =             RK( 344 ) * 
     &                                          +           RK( 494 ) * 
      PD( PRD2            ,BZCO3            ) =             RK( 345 ) * 
     &                                          +           RK( 495 ) * 
      PD( PRD2            ,MACO3            ) =             RK( 346 ) * 
     &                                          +           RK( 496 ) * 
      PD( PRD2            ,RNO3             ) =   0.00800 * RK( 273 ) * 
     &                                          + 0.19000 * RK( 274 )   
     &                                          + 0.01200 * RK( 598 ) * 
      PD( PRD2            ,zRNO3            ) =             RK( 489 ) * 
     &                                          + 0.50000 * RK( 490 ) * 
     &                                          + 0.50000 * RK( 491 ) * 
     &                                          + 0.50000 * RK( 492 ) * 
     &                                          +           RK( 493 ) * 
     &                                          +           RK( 494 ) * 
     &                                          +           RK( 495 ) * 
     &                                          +           RK( 496 ) * 
      PD( PRD2            ,xPROD2           ) =             RK( 337 ) * 
     &                                          +           RK( 339 ) * 
     &                                          + 0.50000 * RK( 340 ) * 
     &                                          + 0.50000 * RK( 341 ) * 
     &                                          + 0.50000 * RK( 342 ) * 
     &                                          +           RK( 343 ) * 
     &                                          +           RK( 344 ) * 
     &                                          +           RK( 345 ) * 
     &                                          +           RK( 346 ) * 
      PD( PRD2            ,R6OOH            ) =   0.84000 * RK( 230 ) * 
     &                                          + 0.14200 * RK( 231 )   
     &                                          + 0.14500 * RK( 605 ) * 
      PD( PRD2            ,PRD2             ) = - 0.67100 * RK( 271 ) * 
     &                                          -           RK( 272 )   
     &                                          - 0.88400 * RK( 599 ) * 
      PD( PRD2            ,yR6OOH           ) =   0.50000 * RK( 470 ) * 
     &                                          + 0.50000 * RK( 471 ) * 
     &                                          + 0.50000 * RK( 472 ) * 
      PD( PRD2            ,RAOOH            ) =   0.07400 * RK( 232 ) * 
     &                                          + 0.07400 * RK( 606 ) * 
      PD( PRD2            ,AFG2             ) =             RK( 255 )   
      PD( PRD2            ,MVK              ) =   0.60000 * RK( 266 )   
      PD( PRD2            ,yRAOOH           ) =   0.50000 * RK( 480 ) * 
     &                                          + 0.50000 * RK( 481 ) * 
     &                                          + 0.50000 * RK( 482 ) * 
      PD( PRD2            ,BUTADIENE13      ) =   0.12500 * RK( 527 ) * 
     &                                          + 0.75000 * RK( 529 ) * 
      PD( PRD2            ,ISOPRENE         ) =   0.10000 * RK( 531 ) * 
     &                                          + 0.75000 * RK( 533 ) * 
      PD( PRD2            ,APIN             ) =   0.25500 * RK( 535 ) * 
     &                                          +           RK( 537 ) * 
      PD( PRD2            ,OLE1             ) =   0.26800 * RK( 553 ) * 
     &                                          + 0.16000 * RK( 555 ) * 
      PD( PRD2            ,OLE2             ) =   0.03700 * RK( 557 ) * 
     &                                          + 0.17000 * RK( 559 ) * 
      PD( PRD2            ,TERP             ) =   0.50200 * RK( 563 ) * 
     &                                          + 0.76300 * RK( 565 ) * 
      PD( PRD2            ,SESQ             ) =   0.50200 * RK( 567 ) * 
     &                                          + 0.76300 * RK( 569 ) * 
      PD( PRD2            ,CL               ) =   0.01200 * RK( 598 ) * 
     &                                          - 0.88400 * RK( 599 ) * 
     &                                          + 0.14500 * RK( 605 ) * 
     &                                          + 0.07400 * RK( 606 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  68: yR6OOH                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( yR6OOH          ,NO               ) = -           RK( 467 ) * 
      PD( yR6OOH          ,O3P              ) =   0.25000 * RK( 533 ) * 
      PD( yR6OOH          ,O3               ) =   0.65200 * RK( 251 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
     &                                          + 0.26400 * RK( 257 ) * 
     &                                          + 0.04800 * RK( 268 ) * 
     &                                          + 0.20000 * RK( 531 ) * 
     &                                          + 0.73700 * RK( 535 ) * 
     &                                          + 0.05200 * RK( 553 ) * 
     &                                          + 0.13600 * RK( 557 ) * 
     &                                          + 0.42800 * RK( 563 ) * 
     &                                          + 0.42800 * RK( 567 ) * 
      PD( yR6OOH          ,NO3              ) =   0.85000 * RK( 269 ) * 
     &                                          -           RK( 469 ) * 
     &                                          +           RK( 532 ) * 
     &                                          +           RK( 536 ) * 
     &                                          + 0.83100 * RK( 554 ) * 
     &                                          + 0.68100 * RK( 558 ) * 
     &                                          +           RK( 564 ) * 
     &                                          +           RK( 568 ) * 
      PD( yR6OOH          ,OH               ) =   0.71300 * RK( 232 ) * 
     &                                          + 0.80000 * RK( 242 ) * 
     &                                          + 0.78300 * RK( 250 ) * 
     &                                          + 0.78300 * RK( 253 ) * 
     &                                          + 0.79400 * RK( 256 ) * 
     &                                          + 0.71100 * RK( 267 ) * 
     &                                          + 0.52800 * RK( 271 ) * 
     &                                          + 0.63600 * RK( 273 ) * 
     &                                          +           RK( 530 ) * 
     &                                          +           RK( 534 ) * 
     &                                          + 0.07300 * RK( 541 ) * 
     &                                          + 0.04700 * RK( 542 ) * 
     &                                          + 0.05500 * RK( 543 ) * 
     &                                          + 0.10200 * RK( 544 ) * 
     &                                          + 0.04300 * RK( 545 ) * 
     &                                          + 0.01700 * RK( 549 ) * 
     &                                          +           RK( 550 ) * 
     &                                          +           RK( 551 ) * 
     &                                          + 0.83100 * RK( 552 ) * 
     &                                          + 0.68100 * RK( 556 ) * 
     &                                          + 0.36900 * RK( 560 ) * 
     &                                          + 0.10100 * RK( 561 ) * 
     &                                          +           RK( 562 ) * 
     &                                          +           RK( 566 ) * 
      PD( yR6OOH          ,HO2              ) = -           RK( 468 ) * 
      PD( yR6OOH          ,MEO2             ) = -           RK( 470 ) * 
      PD( yR6OOH          ,RO2C             ) = -           RK( 471 ) * 
      PD( yR6OOH          ,RO2XC            ) = -           RK( 472 ) * 
      PD( yR6OOH          ,MECO3            ) = -           RK( 473 ) * 
      PD( yR6OOH          ,RCO3             ) = -           RK( 474 ) * 
      PD( yR6OOH          ,BZCO3            ) = -           RK( 475 ) * 
      PD( yR6OOH          ,MACO3            ) = -           RK( 476 ) * 
      PD( yR6OOH          ,RNO3             ) =   0.63600 * RK( 273 ) * 
     &                                          + 0.59100 * RK( 274 )   
     &                                          + 0.90800 * RK( 598 ) * 
      PD( yR6OOH          ,CRES             ) =   0.80000 * RK( 242 ) * 
     &                                          +           RK( 602 ) * 
      PD( yR6OOH          ,R6OOH            ) =   0.85800 * RK( 231 )   
     &                                          + 0.85500 * RK( 605 ) * 
      PD( yR6OOH          ,PRD2             ) =   0.52800 * RK( 271 ) * 
     &                                          +           RK( 272 )   
     &                                          + 0.68600 * RK( 599 ) * 
      PD( yR6OOH          ,yR6OOH           ) = -           RK( 467 ) * 
     &                                          -           RK( 468 ) * 
     &                                          -           RK( 469 ) * 
     &                                          -           RK( 470 ) * 
     &                                          -           RK( 471 ) * 
     &                                          -           RK( 472 ) * 
     &                                          -           RK( 473 ) * 
     &                                          -           RK( 474 ) * 
     &                                          -           RK( 475 ) * 
     &                                          -           RK( 476 ) * 
      PD( yR6OOH          ,RAOOH            ) =   0.71300 * RK( 232 ) * 
     &                                          + 0.71300 * RK( 606 ) * 
      PD( yR6OOH          ,IPRD             ) =   0.71100 * RK( 267 ) * 
     &                                          + 0.04800 * RK( 268 ) * 
     &                                          + 0.85000 * RK( 269 ) * 
     &                                          + 0.76200 * RK( 610 ) * 
      PD( yR6OOH          ,AFG1             ) =   0.78300 * RK( 250 ) * 
     &                                          + 0.65200 * RK( 251 ) * 
      PD( yR6OOH          ,AFG2             ) =   0.78300 * RK( 253 ) * 
     &                                          + 0.65200 * RK( 254 ) * 
      PD( yR6OOH          ,AFG3             ) =   0.79400 * RK( 256 ) * 
     &                                          + 0.26400 * RK( 257 ) * 
      PD( yR6OOH          ,ISOPRENE         ) =             RK( 530 ) * 
     &                                          + 0.20000 * RK( 531 ) * 
     &                                          +           RK( 532 ) * 
     &                                          + 0.25000 * RK( 533 ) * 
     &                                          +           RK( 649 ) * 
      PD( yR6OOH          ,APIN             ) =             RK( 534 ) * 
     &                                          + 0.73700 * RK( 535 ) * 
     &                                          +           RK( 536 ) * 
     &                                          +           RK( 650 ) * 
      PD( yR6OOH          ,TOLUENE          ) =   0.07300 * RK( 541 ) * 
      PD( yR6OOH          ,MXYL             ) =   0.04700 * RK( 542 ) * 
      PD( yR6OOH          ,OXYL             ) =   0.05500 * RK( 543 ) * 
      PD( yR6OOH          ,PXYL             ) =   0.10200 * RK( 544 ) * 
      PD( yR6OOH          ,TRIMETH_BENZ124  ) =   0.04300 * RK( 545 ) * 
      PD( yR6OOH          ,ALK3             ) =   0.01700 * RK( 549 ) * 
     &                                          + 0.01700 * RK( 660 ) * 
      PD( yR6OOH          ,ALK4             ) =             RK( 550 ) * 
     &                                          +           RK( 661 ) * 
      PD( yR6OOH          ,ALK5             ) =             RK( 551 ) * 
     &                                          +           RK( 662 ) * 
      PD( yR6OOH          ,OLE1             ) =   0.83100 * RK( 552 ) * 
     &                                          + 0.05200 * RK( 553 ) * 
     &                                          + 0.83100 * RK( 554 ) * 
     &                                          + 0.83100 * RK( 663 ) * 
      PD( yR6OOH          ,OLE2             ) =   0.68100 * RK( 556 ) * 
     &                                          + 0.13600 * RK( 557 ) * 
     &                                          + 0.68100 * RK( 558 ) * 
     &                                          + 0.68100 * RK( 664 ) * 
      PD( yR6OOH          ,ARO1             ) =   0.36900 * RK( 560 ) * 
      PD( yR6OOH          ,ARO2             ) =   0.10100 * RK( 561 ) * 
      PD( yR6OOH          ,TERP             ) =             RK( 562 ) * 
     &                                          + 0.42800 * RK( 563 ) * 
     &                                          +           RK( 564 ) * 
     &                                          +           RK( 667 ) * 
      PD( yR6OOH          ,SESQ             ) =             RK( 566 ) * 
     &                                          + 0.42800 * RK( 567 ) * 
     &                                          +           RK( 568 ) * 
     &                                          +           RK( 668 ) * 
      PD( yR6OOH          ,CL               ) =   0.90800 * RK( 598 ) * 
     &                                          + 0.68600 * RK( 599 ) * 
     &                                          +           RK( 602 ) * 
     &                                          + 0.85500 * RK( 605 ) * 
     &                                          + 0.71300 * RK( 606 ) * 
     &                                          + 0.76200 * RK( 610 ) * 
     &                                          +           RK( 649 ) * 
     &                                          +           RK( 650 ) * 
     &                                          + 0.01700 * RK( 660 ) * 
     &                                          +           RK( 661 ) * 
     &                                          +           RK( 662 ) * 
     &                                          + 0.83100 * RK( 663 ) * 
     &                                          + 0.68100 * RK( 664 ) * 
     &                                          +           RK( 667 ) * 
     &                                          +           RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  69: RAOOH                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RAOOH           ,OH               ) = -           RK( 232 ) * 
      PD( RAOOH           ,HO2              ) =             RK( 478 ) * 
      PD( RAOOH           ,RAOOH            ) = -           RK( 232 ) * 
     &                                          -           RK( 233 )   
     &                                          -           RK( 606 ) * 
      PD( RAOOH           ,yRAOOH           ) =             RK( 478 ) * 
      PD( RAOOH           ,CL               ) = -           RK( 606 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  70: MGLY                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MGLY            ,NO               ) =             RK( 357 ) * 
      PD( MGLY            ,O3               ) =   0.56800 * RK( 251 ) * 
     &                                          + 0.56800 * RK( 254 ) * 
     &                                          + 0.18400 * RK( 257 ) * 
     &                                          + 0.90000 * RK( 259 ) * 
     &                                          + 0.95000 * RK( 264 ) * 
     &                                          + 0.74200 * RK( 268 ) * 
      PD( MGLY            ,NO3              ) = -           RK( 240 ) * 
     &                                          +           RK( 359 ) * 
      PD( MGLY            ,OH               ) =   0.14700 * RK( 232 ) * 
     &                                          -           RK( 239 ) * 
      PD( MGLY            ,MEO2             ) =   0.50000 * RK( 360 ) * 
      PD( MGLY            ,RO2C             ) =   0.50000 * RK( 361 ) * 
      PD( MGLY            ,RO2XC            ) =   0.50000 * RK( 362 ) * 
      PD( MGLY            ,MECO3            ) =             RK( 363 ) * 
      PD( MGLY            ,RCO3             ) =             RK( 364 ) * 
      PD( MGLY            ,BZCO3            ) =             RK( 365 ) * 
      PD( MGLY            ,MACO3            ) =             RK( 366 ) * 
      PD( MGLY            ,xMGLY            ) =             RK( 357 ) * 
     &                                          +           RK( 359 ) * 
     &                                          + 0.50000 * RK( 360 ) * 
     &                                          + 0.50000 * RK( 361 ) * 
     &                                          + 0.50000 * RK( 362 ) * 
     &                                          +           RK( 363 ) * 
     &                                          +           RK( 364 ) * 
     &                                          +           RK( 365 ) * 
     &                                          +           RK( 366 ) * 
      PD( MGLY            ,RAOOH            ) =   0.14700 * RK( 232 ) * 
     &                                          + 0.50000 * RK( 233 )   
     &                                          + 0.14700 * RK( 606 ) * 
      PD( MGLY            ,MGLY             ) = -           RK( 238 )   
     &                                          -           RK( 239 ) * 
     &                                          -           RK( 240 ) * 
     &                                          -           RK( 601 ) * 
      PD( MGLY            ,IPRD             ) =   0.74200 * RK( 268 ) * 
      PD( MGLY            ,AFG1             ) =   0.56800 * RK( 251 ) * 
     &                                          + 0.30500 * RK( 252 )   
      PD( MGLY            ,AFG2             ) =   0.56800 * RK( 254 ) * 
      PD( MGLY            ,AFG3             ) =   0.18400 * RK( 257 ) * 
      PD( MGLY            ,MACR             ) =   0.90000 * RK( 259 ) * 
      PD( MGLY            ,MVK              ) =   0.95000 * RK( 264 ) * 
      PD( MGLY            ,CL               ) = -           RK( 601 ) * 
     &                                          + 0.14700 * RK( 606 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  71: IPRD                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IPRD            ,NO               ) =             RK( 437 ) * 
      PD( IPRD            ,O3               ) =   0.00300 * RK( 257 ) * 
     &                                          -           RK( 268 ) * 
     &                                          + 0.15000 * RK( 531 ) * 
      PD( IPRD            ,NO3              ) = -           RK( 269 ) * 
     &                                          +           RK( 439 ) * 
      PD( IPRD            ,OH               ) =   0.13900 * RK( 232 ) * 
     &                                          -           RK( 267 ) * 
      PD( IPRD            ,MEO2             ) =   0.50000 * RK( 440 ) * 
      PD( IPRD            ,RO2C             ) =   0.50000 * RK( 441 ) * 
      PD( IPRD            ,RO2XC            ) =   0.50000 * RK( 442 ) * 
      PD( IPRD            ,MECO3            ) =             RK( 443 ) * 
      PD( IPRD            ,RCO3             ) =             RK( 444 ) * 
      PD( IPRD            ,BZCO3            ) =             RK( 445 ) * 
      PD( IPRD            ,MACO3            ) =             RK( 446 ) * 
      PD( IPRD            ,RAOOH            ) =   0.13900 * RK( 232 ) * 
     &                                          + 0.13900 * RK( 606 ) * 
      PD( IPRD            ,IPRD             ) = -           RK( 267 ) * 
     &                                          -           RK( 268 ) * 
     &                                          -           RK( 269 ) * 
     &                                          -           RK( 270 )   
     &                                          -           RK( 610 ) * 
      PD( IPRD            ,AFG3             ) =   0.00300 * RK( 257 ) * 
      PD( IPRD            ,xIPRD            ) =             RK( 437 ) * 
     &                                          +           RK( 439 ) * 
     &                                          + 0.50000 * RK( 440 ) * 
     &                                          + 0.50000 * RK( 441 ) * 
     &                                          + 0.50000 * RK( 442 ) * 
     &                                          +           RK( 443 ) * 
     &                                          +           RK( 444 ) * 
     &                                          +           RK( 445 ) * 
     &                                          +           RK( 446 ) * 
      PD( IPRD            ,ISOPRENE         ) =   0.15000 * RK( 531 ) * 
      PD( IPRD            ,CL               ) =   0.13900 * RK( 606 ) * 
     &                                          -           RK( 610 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  72: xGLY                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xGLY            ,NO               ) = -           RK( 347 ) * 
      PD( xGLY            ,O3P              ) =   0.01200 * RK( 521 ) * 
      PD( xGLY            ,O3               ) =   0.00200 * RK( 535 ) * 
      PD( xGLY            ,NO3              ) = -           RK( 349 ) * 
      PD( xGLY            ,OH               ) =   0.02600 * RK( 232 ) * 
     &                                          + 0.10700 * RK( 250 ) * 
     &                                          + 0.10700 * RK( 253 ) * 
     &                                          + 0.27400 * RK( 256 ) * 
     &                                          + 0.15000 * RK( 267 ) * 
     &                                          + 0.08300 * RK( 278 ) * 
     &                                          + 0.29000 * RK( 540 ) * 
     &                                          + 0.23800 * RK( 541 ) * 
     &                                          + 0.10000 * RK( 542 ) * 
     &                                          + 0.08400 * RK( 543 ) * 
     &                                          + 0.28600 * RK( 544 ) * 
     &                                          + 0.07400 * RK( 545 ) * 
     &                                          + 0.15800 * RK( 560 ) * 
     &                                          + 0.08800 * RK( 561 ) * 
      PD( xGLY            ,HO2              ) = -           RK( 348 ) * 
      PD( xGLY            ,MEO2             ) = -           RK( 350 ) * 
      PD( xGLY            ,RO2C             ) = -           RK( 351 ) * 
      PD( xGLY            ,RO2XC            ) = -           RK( 352 ) * 
      PD( xGLY            ,MECO3            ) = -           RK( 353 ) * 
      PD( xGLY            ,RCO3             ) = -           RK( 354 ) * 
      PD( xGLY            ,BZCO3            ) = -           RK( 355 ) * 
      PD( xGLY            ,MACO3            ) = -           RK( 356 ) * 
      PD( xGLY            ,RAOOH            ) =   0.02600 * RK( 232 ) * 
     &                                          + 0.02600 * RK( 606 ) * 
      PD( xGLY            ,IPRD             ) =   0.15000 * RK( 267 ) * 
      PD( xGLY            ,xGLY             ) = -           RK( 347 ) * 
     &                                          -           RK( 348 ) * 
     &                                          -           RK( 349 ) * 
     &                                          -           RK( 350 ) * 
     &                                          -           RK( 351 ) * 
     &                                          -           RK( 352 ) * 
     &                                          -           RK( 353 ) * 
     &                                          -           RK( 354 ) * 
     &                                          -           RK( 355 ) * 
     &                                          -           RK( 356 ) * 
      PD( xGLY            ,AFG1             ) =   0.10700 * RK( 250 ) * 
      PD( xGLY            ,AFG2             ) =   0.10700 * RK( 253 ) * 
      PD( xGLY            ,AFG3             ) =   0.27400 * RK( 256 ) * 
      PD( xGLY            ,ACROLEIN         ) =   0.08300 * RK( 278 ) * 
     &                                          + 0.27400 * RK( 607 ) * 
      PD( xGLY            ,ETHENE           ) =   0.01200 * RK( 521 ) * 
      PD( xGLY            ,APIN             ) =   0.00200 * RK( 535 ) * 
     &                                          + 0.00700 * RK( 650 ) * 
      PD( xGLY            ,BENZENE          ) =   0.29000 * RK( 540 ) * 
      PD( xGLY            ,TOLUENE          ) =   0.23800 * RK( 541 ) * 
      PD( xGLY            ,MXYL             ) =   0.10000 * RK( 542 ) * 
      PD( xGLY            ,OXYL             ) =   0.08400 * RK( 543 ) * 
      PD( xGLY            ,PXYL             ) =   0.28600 * RK( 544 ) * 
      PD( xGLY            ,TRIMETH_BENZ124  ) =   0.07400 * RK( 545 ) * 
      PD( xGLY            ,ARO1             ) =   0.15800 * RK( 560 ) * 
      PD( xGLY            ,ARO2             ) =   0.08800 * RK( 561 ) * 
      PD( xGLY            ,TERP             ) =   0.00700 * RK( 667 ) * 
      PD( xGLY            ,SESQ             ) =   0.00700 * RK( 668 ) * 
      PD( xGLY            ,CL               ) =   0.02600 * RK( 606 ) * 
     &                                          + 0.27400 * RK( 607 ) * 
     &                                          + 0.00700 * RK( 650 ) * 
     &                                          + 0.00700 * RK( 667 ) * 
     &                                          + 0.00700 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  73: xMEK                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMEK            ,NO               ) = -           RK( 327 ) * 
      PD( xMEK            ,O3               ) =   0.00100 * RK( 557 ) * 
      PD( xMEK            ,NO3              ) = -           RK( 329 ) * 
     &                                          + 0.00200 * RK( 558 ) * 
      PD( xMEK            ,OH               ) =   0.03000 * RK( 232 ) * 
     &                                          + 0.12900 * RK( 250 ) * 
     &                                          + 0.12900 * RK( 253 ) * 
     &                                          + 0.41600 * RK( 258 ) * 
     &                                          + 0.15000 * RK( 267 ) * 
     &                                          + 0.11500 * RK( 271 ) * 
     &                                          + 0.17000 * RK( 273 ) * 
     &                                          + 0.33200 * RK( 549 ) * 
     &                                          + 0.10600 * RK( 550 ) * 
     &                                          + 0.08600 * RK( 551 ) * 
     &                                          + 0.08000 * RK( 556 ) * 
     &                                          + 0.00700 * RK( 562 ) * 
     &                                          + 0.00700 * RK( 566 ) * 
      PD( xMEK            ,HO2              ) = -           RK( 328 ) * 
      PD( xMEK            ,MEO2             ) = -           RK( 330 ) * 
      PD( xMEK            ,RO2C             ) = -           RK( 331 ) * 
      PD( xMEK            ,RO2XC            ) = -           RK( 332 ) * 
      PD( xMEK            ,MECO3            ) = -           RK( 333 ) * 
      PD( xMEK            ,RCO3             ) = -           RK( 334 ) * 
      PD( xMEK            ,BZCO3            ) = -           RK( 335 ) * 
      PD( xMEK            ,MACO3            ) = -           RK( 336 ) * 
      PD( xMEK            ,RNO3             ) =   0.17000 * RK( 273 ) * 
     &                                          + 0.08300 * RK( 274 )   
     &                                          + 0.04100 * RK( 598 ) * 
      PD( xMEK            ,ROOH             ) =   0.28500 * RK( 604 ) * 
      PD( xMEK            ,PRD2             ) =   0.11500 * RK( 271 ) * 
     &                                          + 0.05100 * RK( 599 ) * 
      PD( xMEK            ,RAOOH            ) =   0.03000 * RK( 232 ) * 
     &                                          + 0.03000 * RK( 606 ) * 
      PD( xMEK            ,IPRD             ) =   0.15000 * RK( 267 ) * 
      PD( xMEK            ,xMEK             ) = -           RK( 327 ) * 
     &                                          -           RK( 328 ) * 
     &                                          -           RK( 329 ) * 
     &                                          -           RK( 330 ) * 
     &                                          -           RK( 331 ) * 
     &                                          -           RK( 332 ) * 
     &                                          -           RK( 333 ) * 
     &                                          -           RK( 334 ) * 
     &                                          -           RK( 335 ) * 
     &                                          -           RK( 336 ) * 
      PD( xMEK            ,AFG1             ) =   0.12900 * RK( 250 ) * 
      PD( xMEK            ,AFG2             ) =   0.12900 * RK( 253 ) * 
      PD( xMEK            ,MACR             ) =   0.41600 * RK( 258 ) * 
      PD( xMEK            ,ALK3             ) =   0.33200 * RK( 549 ) * 
     &                                          + 0.25300 * RK( 660 ) * 
      PD( xMEK            ,ALK4             ) =   0.10600 * RK( 550 ) * 
     &                                          + 0.07600 * RK( 661 ) * 
      PD( xMEK            ,ALK5             ) =   0.08600 * RK( 551 ) * 
     &                                          + 0.04100 * RK( 662 ) * 
      PD( xMEK            ,OLE2             ) =   0.08000 * RK( 556 ) * 
     &                                          + 0.00100 * RK( 557 ) * 
     &                                          + 0.00200 * RK( 558 ) * 
     &                                          + 0.04200 * RK( 664 ) * 
      PD( xMEK            ,TERP             ) =   0.00700 * RK( 562 ) * 
      PD( xMEK            ,SESQ             ) =   0.00700 * RK( 566 ) * 
      PD( xMEK            ,CL               ) =   0.04100 * RK( 598 ) * 
     &                                          + 0.05100 * RK( 599 ) * 
     &                                          + 0.28500 * RK( 604 ) * 
     &                                          + 0.03000 * RK( 606 ) * 
     &                                          + 0.25300 * RK( 660 ) * 
     &                                          + 0.07600 * RK( 661 ) * 
     &                                          + 0.04100 * RK( 662 ) * 
     &                                          + 0.04200 * RK( 664 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  74: xAFG1                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xAFG1           ,NO               ) = -           RK( 387 ) * 
      PD( xAFG1           ,O3P              ) =   0.00100 * RK( 529 ) * 
      PD( xAFG1           ,NO3              ) = -           RK( 389 ) * 
      PD( xAFG1           ,OH               ) =   0.07300 * RK( 232 ) * 
     &                                          + 0.19500 * RK( 256 ) * 
     &                                          + 0.02900 * RK( 540 ) * 
     &                                          + 0.19500 * RK( 541 ) * 
     &                                          + 0.33600 * RK( 542 ) * 
     &                                          + 0.25300 * RK( 543 ) * 
     &                                          + 0.04500 * RK( 544 ) * 
     &                                          + 0.08800 * RK( 545 ) * 
     &                                          + 0.07200 * RK( 560 ) * 
     &                                          + 0.22100 * RK( 561 ) * 
      PD( xAFG1           ,HO2              ) = -           RK( 388 ) * 
      PD( xAFG1           ,MEO2             ) = -           RK( 390 ) * 
      PD( xAFG1           ,RO2C             ) = -           RK( 391 ) * 
      PD( xAFG1           ,RO2XC            ) = -           RK( 392 ) * 
      PD( xAFG1           ,MECO3            ) = -           RK( 393 ) * 
      PD( xAFG1           ,RCO3             ) = -           RK( 394 ) * 
      PD( xAFG1           ,BZCO3            ) = -           RK( 395 ) * 
      PD( xAFG1           ,MACO3            ) = -           RK( 396 ) * 
      PD( xAFG1           ,RAOOH            ) =   0.07300 * RK( 232 ) * 
     &                                          + 0.07300 * RK( 606 ) * 
      PD( xAFG1           ,IPRD             ) =   0.00900 * RK( 610 ) * 
      PD( xAFG1           ,xAFG1            ) = -           RK( 387 ) * 
     &                                          -           RK( 388 ) * 
     &                                          -           RK( 389 ) * 
     &                                          -           RK( 390 ) * 
     &                                          -           RK( 391 ) * 
     &                                          -           RK( 392 ) * 
     &                                          -           RK( 393 ) * 
     &                                          -           RK( 394 ) * 
     &                                          -           RK( 395 ) * 
     &                                          -           RK( 396 ) * 
      PD( xAFG1           ,AFG3             ) =   0.19500 * RK( 256 ) * 
      PD( xAFG1           ,BUTADIENE13      ) =   0.00100 * RK( 529 ) * 
      PD( xAFG1           ,APIN             ) =   0.00600 * RK( 650 ) * 
      PD( xAFG1           ,BENZENE          ) =   0.02900 * RK( 540 ) * 
      PD( xAFG1           ,TOLUENE          ) =   0.19500 * RK( 541 ) * 
      PD( xAFG1           ,MXYL             ) =   0.33600 * RK( 542 ) * 
      PD( xAFG1           ,OXYL             ) =   0.25300 * RK( 543 ) * 
      PD( xAFG1           ,PXYL             ) =   0.04500 * RK( 544 ) * 
      PD( xAFG1           ,TRIMETH_BENZ124  ) =   0.08800 * RK( 545 ) * 
      PD( xAFG1           ,ARO1             ) =   0.07200 * RK( 560 ) * 
      PD( xAFG1           ,ARO2             ) =   0.22100 * RK( 561 ) * 
      PD( xAFG1           ,TERP             ) =   0.00600 * RK( 667 ) * 
      PD( xAFG1           ,SESQ             ) =   0.00600 * RK( 668 ) * 
      PD( xAFG1           ,CL               ) =   0.07300 * RK( 606 ) * 
     &                                          + 0.00900 * RK( 610 ) * 
     &                                          + 0.00600 * RK( 650 ) * 
     &                                          + 0.00600 * RK( 667 ) * 
     &                                          + 0.00600 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  75: xAFG2                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xAFG2           ,NO               ) = -           RK( 397 ) * 
      PD( xAFG2           ,O3P              ) =   0.00100 * RK( 529 ) * 
      PD( xAFG2           ,NO3              ) = -           RK( 399 ) * 
      PD( xAFG2           ,OH               ) =   0.07300 * RK( 232 ) * 
     &                                          + 0.19500 * RK( 256 ) * 
     &                                          + 0.26100 * RK( 540 ) * 
     &                                          + 0.19500 * RK( 541 ) * 
     &                                          + 0.14400 * RK( 542 ) * 
     &                                          + 0.25300 * RK( 543 ) * 
     &                                          + 0.06700 * RK( 544 ) * 
     &                                          + 0.35200 * RK( 545 ) * 
     &                                          + 0.18500 * RK( 560 ) * 
     &                                          + 0.24700 * RK( 561 ) * 
      PD( xAFG2           ,HO2              ) = -           RK( 398 ) * 
      PD( xAFG2           ,MEO2             ) = -           RK( 400 ) * 
      PD( xAFG2           ,RO2C             ) = -           RK( 401 ) * 
      PD( xAFG2           ,RO2XC            ) = -           RK( 402 ) * 
      PD( xAFG2           ,MECO3            ) = -           RK( 403 ) * 
      PD( xAFG2           ,RCO3             ) = -           RK( 404 ) * 
      PD( xAFG2           ,BZCO3            ) = -           RK( 405 ) * 
      PD( xAFG2           ,MACO3            ) = -           RK( 406 ) * 
      PD( xAFG2           ,RAOOH            ) =   0.07300 * RK( 232 ) * 
     &                                          + 0.07300 * RK( 606 ) * 
      PD( xAFG2           ,IPRD             ) =   0.00900 * RK( 610 ) * 
      PD( xAFG2           ,xAFG2            ) = -           RK( 397 ) * 
     &                                          -           RK( 398 ) * 
     &                                          -           RK( 399 ) * 
     &                                          -           RK( 400 ) * 
     &                                          -           RK( 401 ) * 
     &                                          -           RK( 402 ) * 
     &                                          -           RK( 403 ) * 
     &                                          -           RK( 404 ) * 
     &                                          -           RK( 405 ) * 
     &                                          -           RK( 406 ) * 
      PD( xAFG2           ,AFG3             ) =   0.19500 * RK( 256 ) * 
      PD( xAFG2           ,BUTADIENE13      ) =   0.00100 * RK( 529 ) * 
      PD( xAFG2           ,APIN             ) =   0.00600 * RK( 650 ) * 
      PD( xAFG2           ,BENZENE          ) =   0.26100 * RK( 540 ) * 
      PD( xAFG2           ,TOLUENE          ) =   0.19500 * RK( 541 ) * 
      PD( xAFG2           ,MXYL             ) =   0.14400 * RK( 542 ) * 
      PD( xAFG2           ,OXYL             ) =   0.25300 * RK( 543 ) * 
      PD( xAFG2           ,PXYL             ) =   0.06700 * RK( 544 ) * 
      PD( xAFG2           ,TRIMETH_BENZ124  ) =   0.35200 * RK( 545 ) * 
      PD( xAFG2           ,ARO1             ) =   0.18500 * RK( 560 ) * 
      PD( xAFG2           ,ARO2             ) =   0.24700 * RK( 561 ) * 
      PD( xAFG2           ,TERP             ) =   0.00600 * RK( 667 ) * 
      PD( xAFG2           ,SESQ             ) =   0.00600 * RK( 668 ) * 
      PD( xAFG2           ,CL               ) =   0.07300 * RK( 606 ) * 
     &                                          + 0.00900 * RK( 610 ) * 
     &                                          + 0.00600 * RK( 650 ) * 
     &                                          + 0.00600 * RK( 667 ) * 
     &                                          + 0.00600 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  76: GLY                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( GLY             ,NO               ) =             RK( 347 ) * 
      PD( GLY             ,O3               ) =   0.43200 * RK( 251 ) * 
     &                                          + 0.43200 * RK( 254 ) * 
     &                                          + 0.36600 * RK( 257 ) * 
     &                                          + 0.02300 * RK( 268 ) * 
     &                                          + 0.50000 * RK( 279 ) * 
      PD( GLY             ,NO3              ) = -           RK( 237 ) * 
     &                                          +           RK( 349 ) * 
      PD( GLY             ,OH               ) = -           RK( 236 ) * 
     &                                          + 0.70000 * RK( 538 ) * 
      PD( GLY             ,MEO2             ) =   0.50000 * RK( 350 ) * 
      PD( GLY             ,RO2C             ) =   0.50000 * RK( 351 ) * 
      PD( GLY             ,RO2XC            ) =   0.50000 * RK( 352 ) * 
      PD( GLY             ,MECO3            ) =             RK( 353 ) * 
      PD( GLY             ,RCO3             ) =             RK( 354 ) * 
      PD( GLY             ,BZCO3            ) =             RK( 355 ) * 
      PD( GLY             ,MACO3            ) =             RK( 356 ) * 
      PD( GLY             ,RAOOH            ) =   0.50000 * RK( 233 )   
      PD( GLY             ,IPRD             ) =   0.02300 * RK( 268 ) * 
      PD( GLY             ,xGLY             ) =             RK( 347 ) * 
     &                                          +           RK( 349 ) * 
     &                                          + 0.50000 * RK( 350 ) * 
     &                                          + 0.50000 * RK( 351 ) * 
     &                                          + 0.50000 * RK( 352 ) * 
     &                                          +           RK( 353 ) * 
     &                                          +           RK( 354 ) * 
     &                                          +           RK( 355 ) * 
     &                                          +           RK( 356 ) * 
      PD( GLY             ,GLY              ) = -           RK( 234 )   
     &                                          -           RK( 235 )   
     &                                          -           RK( 236 ) * 
     &                                          -           RK( 237 ) * 
     &                                          -           RK( 600 ) * 
      PD( GLY             ,AFG1             ) =   0.43200 * RK( 251 ) * 
     &                                          + 0.19500 * RK( 252 )   
      PD( GLY             ,AFG2             ) =   0.43200 * RK( 254 ) * 
      PD( GLY             ,AFG3             ) =   0.36600 * RK( 257 ) * 
      PD( GLY             ,ACROLEIN         ) =   0.50000 * RK( 279 ) * 
      PD( GLY             ,ACETYLENE        ) =   0.70000 * RK( 538 ) * 
      PD( GLY             ,CL               ) = -           RK( 600 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  77: AFG1                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AFG1            ,NO               ) =             RK( 387 ) * 
      PD( AFG1            ,O3               ) = -           RK( 251 ) * 
     &                                          + 0.35000 * RK( 257 ) * 
      PD( AFG1            ,NO3              ) =             RK( 389 ) * 
      PD( AFG1            ,OH               ) = -           RK( 250 ) * 
      PD( AFG1            ,MEO2             ) =   0.50000 * RK( 390 ) * 
      PD( AFG1            ,RO2C             ) =   0.50000 * RK( 391 ) * 
      PD( AFG1            ,RO2XC            ) =   0.50000 * RK( 392 ) * 
      PD( AFG1            ,MECO3            ) =             RK( 393 ) * 
      PD( AFG1            ,RCO3             ) =             RK( 394 ) * 
      PD( AFG1            ,BZCO3            ) =             RK( 395 ) * 
      PD( AFG1            ,MACO3            ) =             RK( 396 ) * 
      PD( AFG1            ,RAOOH            ) =   0.50000 * RK( 233 )   
      PD( AFG1            ,IPRD             ) =   0.04200 * RK( 610 ) * 
      PD( AFG1            ,xAFG1            ) =             RK( 387 ) * 
     &                                          +           RK( 389 ) * 
     &                                          + 0.50000 * RK( 390 ) * 
     &                                          + 0.50000 * RK( 391 ) * 
     &                                          + 0.50000 * RK( 392 ) * 
     &                                          +           RK( 393 ) * 
     &                                          +           RK( 394 ) * 
     &                                          +           RK( 395 ) * 
     &                                          +           RK( 396 ) * 
      PD( AFG1            ,AFG1             ) = -           RK( 250 ) * 
     &                                          -           RK( 251 ) * 
     &                                          -           RK( 252 )   
      PD( AFG1            ,AFG3             ) =   0.35000 * RK( 257 ) * 
      PD( AFG1            ,CL               ) =   0.04200 * RK( 610 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  78: AFG2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AFG2            ,NO               ) =             RK( 397 ) * 
      PD( AFG2            ,O3               ) = -           RK( 254 ) * 
     &                                          + 0.35000 * RK( 257 ) * 
      PD( AFG2            ,NO3              ) =             RK( 399 ) * 
      PD( AFG2            ,OH               ) = -           RK( 253 ) * 
      PD( AFG2            ,MEO2             ) =   0.50000 * RK( 400 ) * 
      PD( AFG2            ,RO2C             ) =   0.50000 * RK( 401 ) * 
      PD( AFG2            ,RO2XC            ) =   0.50000 * RK( 402 ) * 
      PD( AFG2            ,MECO3            ) =             RK( 403 ) * 
      PD( AFG2            ,RCO3             ) =             RK( 404 ) * 
      PD( AFG2            ,BZCO3            ) =             RK( 405 ) * 
      PD( AFG2            ,MACO3            ) =             RK( 406 ) * 
      PD( AFG2            ,RAOOH            ) =   0.50000 * RK( 233 )   
      PD( AFG2            ,IPRD             ) =   0.04200 * RK( 610 ) * 
      PD( AFG2            ,xAFG2            ) =             RK( 397 ) * 
     &                                          +           RK( 399 ) * 
     &                                          + 0.50000 * RK( 400 ) * 
     &                                          + 0.50000 * RK( 401 ) * 
     &                                          + 0.50000 * RK( 402 ) * 
     &                                          +           RK( 403 ) * 
     &                                          +           RK( 404 ) * 
     &                                          +           RK( 405 ) * 
     &                                          +           RK( 406 ) * 
      PD( AFG2            ,AFG2             ) = -           RK( 253 ) * 
     &                                          -           RK( 254 ) * 
     &                                          -           RK( 255 )   
      PD( AFG2            ,AFG3             ) =   0.35000 * RK( 257 ) * 
      PD( AFG2            ,CL               ) =   0.04200 * RK( 610 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  79: BACL                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BACL            ,NO               ) =             RK( 367 ) * 
      PD( BACL            ,NO3              ) =             RK( 369 ) * 
      PD( BACL            ,MEO2             ) =   0.50000 * RK( 370 ) * 
      PD( BACL            ,RO2C             ) =   0.50000 * RK( 371 ) * 
      PD( BACL            ,RO2XC            ) =   0.50000 * RK( 372 ) * 
      PD( BACL            ,MECO3            ) =             RK( 373 ) * 
      PD( BACL            ,RCO3             ) =             RK( 374 ) * 
      PD( BACL            ,BZCO3            ) =             RK( 375 ) * 
      PD( BACL            ,MACO3            ) =             RK( 376 ) * 
      PD( BACL            ,xBACL            ) =             RK( 367 ) * 
     &                                          +           RK( 369 ) * 
     &                                          + 0.50000 * RK( 370 ) * 
     &                                          + 0.50000 * RK( 371 ) * 
     &                                          + 0.50000 * RK( 372 ) * 
     &                                          +           RK( 373 ) * 
     &                                          +           RK( 374 ) * 
     &                                          +           RK( 375 ) * 
     &                                          +           RK( 376 ) * 
      PD( BACL            ,BACL             ) = -           RK( 241 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  80: BALD                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BALD            ,NO               ) =             RK( 377 ) * 
      PD( BALD            ,NO3              ) = -           RK( 249 ) * 
     &                                          +           RK( 379 ) * 
      PD( BALD            ,OH               ) = -           RK( 247 ) * 
      PD( BALD            ,MEO2             ) =   0.50000 * RK( 380 ) * 
      PD( BALD            ,RO2C             ) =   0.50000 * RK( 381 ) * 
      PD( BALD            ,RO2XC            ) =   0.50000 * RK( 382 ) * 
      PD( BALD            ,MECO3            ) =             RK( 383 ) * 
      PD( BALD            ,RCO3             ) =             RK( 384 ) * 
      PD( BALD            ,BZCO3            ) =             RK( 385 ) * 
      PD( BALD            ,MACO3            ) =             RK( 386 ) * 
      PD( BALD            ,BALD             ) = -           RK( 247 ) * 
     &                                          -           RK( 248 )   
     &                                          -           RK( 249 ) * 
     &                                          -           RK( 603 ) * 
      PD( BALD            ,xBALD            ) =             RK( 377 ) * 
     &                                          +           RK( 379 ) * 
     &                                          + 0.50000 * RK( 380 ) * 
     &                                          + 0.50000 * RK( 381 ) * 
     &                                          + 0.50000 * RK( 382 ) * 
     &                                          +           RK( 383 ) * 
     &                                          +           RK( 384 ) * 
     &                                          +           RK( 385 ) * 
     &                                          +           RK( 386 ) * 
      PD( BALD            ,CL               ) = -           RK( 603 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  81: AFG3                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AFG3            ,NO               ) =             RK( 407 ) * 
      PD( AFG3            ,O3               ) = - 0.86100 * RK( 257 ) * 
      PD( AFG3            ,NO3              ) =             RK( 409 ) * 
      PD( AFG3            ,OH               ) = -           RK( 256 ) * 
     &                                          + 0.11600 * RK( 540 ) * 
     &                                          + 0.31200 * RK( 541 ) * 
     &                                          + 0.23900 * RK( 542 ) * 
     &                                          + 0.19800 * RK( 543 ) * 
     &                                          + 0.27800 * RK( 544 ) * 
     &                                          + 0.23000 * RK( 545 ) * 
     &                                          + 0.20200 * RK( 560 ) * 
     &                                          + 0.17800 * RK( 561 ) * 
      PD( AFG3            ,MEO2             ) =   0.50000 * RK( 410 ) * 
      PD( AFG3            ,RO2C             ) =   0.50000 * RK( 411 ) * 
      PD( AFG3            ,RO2XC            ) =   0.50000 * RK( 412 ) * 
      PD( AFG3            ,MECO3            ) =             RK( 413 ) * 
      PD( AFG3            ,RCO3             ) =             RK( 414 ) * 
      PD( AFG3            ,BZCO3            ) =             RK( 415 ) * 
      PD( AFG3            ,MACO3            ) =             RK( 416 ) * 
      PD( AFG3            ,AFG3             ) = -           RK( 256 ) * 
     &                                          - 0.86100 * RK( 257 ) * 
      PD( AFG3            ,xAFG3            ) =             RK( 407 ) * 
     &                                          +           RK( 409 ) * 
     &                                          + 0.50000 * RK( 410 ) * 
     &                                          + 0.50000 * RK( 411 ) * 
     &                                          + 0.50000 * RK( 412 ) * 
     &                                          +           RK( 413 ) * 
     &                                          +           RK( 414 ) * 
     &                                          +           RK( 415 ) * 
     &                                          +           RK( 416 ) * 
      PD( AFG3            ,BENZENE          ) =   0.11600 * RK( 540 ) * 
      PD( AFG3            ,TOLUENE          ) =   0.31200 * RK( 541 ) * 
      PD( AFG3            ,MXYL             ) =   0.23900 * RK( 542 ) * 
      PD( AFG3            ,OXYL             ) =   0.19800 * RK( 543 ) * 
      PD( AFG3            ,PXYL             ) =   0.27800 * RK( 544 ) * 
      PD( AFG3            ,TRIMETH_BENZ124  ) =   0.23000 * RK( 545 ) * 
      PD( AFG3            ,ARO1             ) =   0.20200 * RK( 560 ) * 
      PD( AFG3            ,ARO2             ) =   0.17800 * RK( 561 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  82: xIPRD                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xIPRD           ,NO               ) = -           RK( 437 ) * 
      PD( xIPRD           ,O3               ) =   0.00100 * RK( 563 ) * 
     &                                          + 0.00100 * RK( 567 ) * 
      PD( xIPRD           ,NO3              ) = -           RK( 439 ) * 
     &                                          + 0.12000 * RK( 528 ) * 
     &                                          + 0.93600 * RK( 532 ) * 
     &                                          + 0.00400 * RK( 564 ) * 
     &                                          + 0.00400 * RK( 568 ) * 
      PD( xIPRD           ,OH               ) =   0.23100 * RK( 256 ) * 
     &                                          + 0.47100 * RK( 526 ) * 
     &                                          + 0.35700 * RK( 530 ) * 
     &                                          + 0.00900 * RK( 552 ) * 
     &                                          + 0.01200 * RK( 556 ) * 
     &                                          + 0.00200 * RK( 562 ) * 
     &                                          + 0.00200 * RK( 566 ) * 
      PD( xIPRD           ,HO2              ) = -           RK( 438 ) * 
      PD( xIPRD           ,MEO2             ) = -           RK( 440 ) * 
      PD( xIPRD           ,RO2C             ) = -           RK( 441 ) * 
      PD( xIPRD           ,RO2XC            ) = -           RK( 442 ) * 
      PD( xIPRD           ,MECO3            ) = -           RK( 443 ) * 
      PD( xIPRD           ,RCO3             ) = -           RK( 444 ) * 
      PD( xIPRD           ,BZCO3            ) = -           RK( 445 ) * 
      PD( xIPRD           ,MACO3            ) = -           RK( 446 ) * 
      PD( xIPRD           ,IPRD             ) =   0.11500 * RK( 610 ) * 
      PD( xIPRD           ,AFG3             ) =   0.23100 * RK( 256 ) * 
      PD( xIPRD           ,xIPRD            ) = -           RK( 437 ) * 
     &                                          -           RK( 438 ) * 
     &                                          -           RK( 439 ) * 
     &                                          -           RK( 440 ) * 
     &                                          -           RK( 441 ) * 
     &                                          -           RK( 442 ) * 
     &                                          -           RK( 443 ) * 
     &                                          -           RK( 444 ) * 
     &                                          -           RK( 445 ) * 
     &                                          -           RK( 446 ) * 
      PD( xIPRD           ,MACR             ) =   0.08200 * RK( 608 ) * 
      PD( xIPRD           ,BUTADIENE13      ) =   0.47100 * RK( 526 ) * 
     &                                          + 0.12000 * RK( 528 ) * 
     &                                          + 0.47300 * RK( 648 ) * 
      PD( xIPRD           ,ISOPRENE         ) =   0.35700 * RK( 530 ) * 
     &                                          + 0.93600 * RK( 532 ) * 
     &                                          + 0.67100 * RK( 649 ) * 
      PD( xIPRD           ,APIN             ) =   0.15800 * RK( 650 ) * 
      PD( xIPRD           ,OLE1             ) =   0.00900 * RK( 552 ) * 
     &                                          + 0.03800 * RK( 663 ) * 
      PD( xIPRD           ,OLE2             ) =   0.01200 * RK( 556 ) * 
     &                                          + 0.16100 * RK( 664 ) * 
      PD( xIPRD           ,TERP             ) =   0.00200 * RK( 562 ) * 
     &                                          + 0.00100 * RK( 563 ) * 
     &                                          + 0.00400 * RK( 564 ) * 
     &                                          + 0.15800 * RK( 667 ) * 
      PD( xIPRD           ,SESQ             ) =   0.00200 * RK( 566 ) * 
     &                                          + 0.00100 * RK( 567 ) * 
     &                                          + 0.00400 * RK( 568 ) * 
     &                                          + 0.15800 * RK( 668 ) * 
      PD( xIPRD           ,CL               ) =   0.08200 * RK( 608 ) * 
     &                                          + 0.11500 * RK( 610 ) * 
     &                                          + 0.47300 * RK( 648 ) * 
     &                                          + 0.67100 * RK( 649 ) * 
     &                                          + 0.15800 * RK( 650 ) * 
     &                                          + 0.03800 * RK( 663 ) * 
     &                                          + 0.16100 * RK( 664 ) * 
     &                                          + 0.15800 * RK( 667 ) * 
     &                                          + 0.15800 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  83: MACR                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MACR            ,NO               ) =             RK( 417 ) * 
      PD( MACR            ,O3P              ) = -           RK( 261 ) * 
      PD( MACR            ,O3               ) =   0.00300 * RK( 257 ) * 
     &                                          -           RK( 259 ) * 
     &                                          + 0.39000 * RK( 531 ) * 
      PD( MACR            ,NO3              ) = -           RK( 260 ) * 
     &                                          +           RK( 419 ) * 
      PD( MACR            ,OH               ) = -           RK( 258 ) * 
      PD( MACR            ,MEO2             ) =   0.50000 * RK( 420 ) * 
      PD( MACR            ,RO2C             ) =   0.50000 * RK( 421 ) * 
      PD( MACR            ,RO2XC            ) =   0.50000 * RK( 422 ) * 
      PD( MACR            ,MECO3            ) =             RK( 423 ) * 
      PD( MACR            ,RCO3             ) =             RK( 424 ) * 
      PD( MACR            ,BZCO3            ) =             RK( 425 ) * 
      PD( MACR            ,MACO3            ) =             RK( 426 ) * 
      PD( MACR            ,AFG3             ) =   0.00300 * RK( 257 ) * 
      PD( MACR            ,MACR             ) = -           RK( 258 ) * 
     &                                          -           RK( 259 ) * 
     &                                          -           RK( 260 ) * 
     &                                          -           RK( 261 ) * 
     &                                          -           RK( 262 )   
     &                                          -           RK( 608 ) * 
      PD( MACR            ,xMACR            ) =             RK( 417 ) * 
     &                                          +           RK( 419 ) * 
     &                                          + 0.50000 * RK( 420 ) * 
     &                                          + 0.50000 * RK( 421 ) * 
     &                                          + 0.50000 * RK( 422 ) * 
     &                                          +           RK( 423 ) * 
     &                                          +           RK( 424 ) * 
     &                                          +           RK( 425 ) * 
     &                                          +           RK( 426 ) * 
      PD( MACR            ,ISOPRENE         ) =   0.39000 * RK( 531 ) * 
      PD( MACR            ,CL               ) = -           RK( 608 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  84: MVK                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MVK             ,NO               ) =             RK( 427 ) * 
      PD( MVK             ,O3P              ) = -           RK( 265 ) * 
      PD( MVK             ,O3               ) =   0.00400 * RK( 257 ) * 
     &                                          -           RK( 264 ) * 
     &                                          + 0.37500 * RK( 527 ) * 
     &                                          + 0.16000 * RK( 531 ) * 
      PD( MVK             ,NO3              ) =             RK( 429 ) * 
      PD( MVK             ,OH               ) = -           RK( 263 ) * 
      PD( MVK             ,MEO2             ) =   0.50000 * RK( 430 ) * 
      PD( MVK             ,RO2C             ) =   0.50000 * RK( 431 ) * 
      PD( MVK             ,RO2XC            ) =   0.50000 * RK( 432 ) * 
      PD( MVK             ,MECO3            ) =             RK( 433 ) * 
      PD( MVK             ,RCO3             ) =             RK( 434 ) * 
      PD( MVK             ,BZCO3            ) =             RK( 435 ) * 
      PD( MVK             ,MACO3            ) =             RK( 436 ) * 
      PD( MVK             ,AFG3             ) =   0.00400 * RK( 257 ) * 
      PD( MVK             ,MVK              ) = -           RK( 263 ) * 
     &                                          -           RK( 264 ) * 
     &                                          -           RK( 265 ) * 
     &                                          -           RK( 266 )   
     &                                          -           RK( 609 ) * 
      PD( MVK             ,xMVK             ) =             RK( 427 ) * 
     &                                          +           RK( 429 ) * 
     &                                          + 0.50000 * RK( 430 ) * 
     &                                          + 0.50000 * RK( 431 ) * 
     &                                          + 0.50000 * RK( 432 ) * 
     &                                          +           RK( 433 ) * 
     &                                          +           RK( 434 ) * 
     &                                          +           RK( 435 ) * 
     &                                          +           RK( 436 ) * 
      PD( MVK             ,BUTADIENE13      ) =   0.37500 * RK( 527 ) * 
      PD( MVK             ,ISOPRENE         ) =   0.16000 * RK( 531 ) * 
      PD( MVK             ,CL               ) = -           RK( 609 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  85: xHOCCHO                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xHOCCHO         ,NO               ) = -           RK( 497 ) * 
      PD( xHOCCHO         ,O3               ) =   0.04700 * RK( 268 ) * 
      PD( xHOCCHO         ,NO3              ) = -           RK( 499 ) * 
     &                                          + 0.00200 * RK( 564 ) * 
      PD( xHOCCHO         ,OH               ) =   0.67500 * RK( 263 ) * 
     &                                          + 0.12900 * RK( 267 ) * 
     &                                          + 0.19500 * RK( 518 ) * 
     &                                          + 0.01000 * RK( 546 ) * 
     &                                          + 0.00700 * RK( 552 ) * 
      PD( xHOCCHO         ,HO2              ) = -           RK( 498 ) * 
      PD( xHOCCHO         ,MEO2             ) = -           RK( 500 ) * 
      PD( xHOCCHO         ,RO2C             ) = -           RK( 501 ) * 
      PD( xHOCCHO         ,RO2XC            ) = -           RK( 502 ) * 
      PD( xHOCCHO         ,MECO3            ) = -           RK( 503 ) * 
      PD( xHOCCHO         ,RCO3             ) = -           RK( 504 ) * 
      PD( xHOCCHO         ,BZCO3            ) = -           RK( 505 ) * 
      PD( xHOCCHO         ,MACO3            ) = -           RK( 506 ) * 
      PD( xHOCCHO         ,IPRD             ) =   0.12900 * RK( 267 ) * 
     &                                          + 0.04700 * RK( 268 ) * 
      PD( xHOCCHO         ,MVK              ) =   0.67500 * RK( 263 ) * 
      PD( xHOCCHO         ,xHOCCHO          ) = -           RK( 497 ) * 
     &                                          -           RK( 498 ) * 
     &                                          -           RK( 499 ) * 
     &                                          -           RK( 500 ) * 
     &                                          -           RK( 501 ) * 
     &                                          -           RK( 502 ) * 
     &                                          -           RK( 503 ) * 
     &                                          -           RK( 504 ) * 
     &                                          -           RK( 505 ) * 
     &                                          -           RK( 506 ) * 
      PD( xHOCCHO         ,ETHENE           ) =   0.19500 * RK( 518 ) * 
      PD( xHOCCHO         ,ETOH             ) =   0.01000 * RK( 546 ) * 
     &                                          + 0.06100 * RK( 657 ) * 
      PD( xHOCCHO         ,OLE1             ) =   0.00700 * RK( 552 ) * 
      PD( xHOCCHO         ,TERP             ) =   0.00200 * RK( 564 ) * 
      PD( xHOCCHO         ,CL               ) =   0.06100 * RK( 657 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  86: xRNO3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xRNO3           ,NO               ) = -           RK( 447 ) * 
      PD( xRNO3           ,NO3              ) =   0.57200 * RK( 269 ) * 
     &                                          + 0.03100 * RK( 280 ) * 
     &                                          -           RK( 449 ) * 
     &                                          + 0.35500 * RK( 528 ) * 
     &                                          + 0.05600 * RK( 536 ) * 
     &                                          + 0.77400 * RK( 554 ) * 
     &                                          + 0.32000 * RK( 558 ) * 
     &                                          + 0.22800 * RK( 564 ) * 
     &                                          + 0.22800 * RK( 568 ) * 
      PD( xRNO3           ,OH               ) =   0.30500 * RK( 273 ) * 
      PD( xRNO3           ,HO2              ) = -           RK( 448 ) * 
      PD( xRNO3           ,MEO2             ) = -           RK( 450 ) * 
      PD( xRNO3           ,RO2C             ) = -           RK( 451 ) * 
      PD( xRNO3           ,RO2XC            ) = -           RK( 452 ) * 
      PD( xRNO3           ,MECO3            ) = -           RK( 453 ) * 
      PD( xRNO3           ,RCO3             ) = -           RK( 454 ) * 
      PD( xRNO3           ,BZCO3            ) = -           RK( 455 ) * 
      PD( xRNO3           ,MACO3            ) = -           RK( 456 ) * 
      PD( xRNO3           ,RNO3             ) =   0.30500 * RK( 273 ) * 
     &                                          + 0.54700 * RK( 598 ) * 
      PD( xRNO3           ,IPRD             ) =   0.57200 * RK( 269 ) * 
      PD( xRNO3           ,xRNO3            ) = -           RK( 447 ) * 
     &                                          -           RK( 448 ) * 
     &                                          -           RK( 449 ) * 
     &                                          -           RK( 450 ) * 
     &                                          -           RK( 451 ) * 
     &                                          -           RK( 452 ) * 
     &                                          -           RK( 453 ) * 
     &                                          -           RK( 454 ) * 
     &                                          -           RK( 455 ) * 
     &                                          -           RK( 456 ) * 
      PD( xRNO3           ,ACROLEIN         ) =   0.03100 * RK( 280 ) * 
      PD( xRNO3           ,BUTADIENE13      ) =   0.35500 * RK( 528 ) * 
      PD( xRNO3           ,APIN             ) =   0.05600 * RK( 536 ) * 
      PD( xRNO3           ,OLE1             ) =   0.77400 * RK( 554 ) * 
      PD( xRNO3           ,OLE2             ) =   0.32000 * RK( 558 ) * 
      PD( xRNO3           ,TERP             ) =   0.22800 * RK( 564 ) * 
      PD( xRNO3           ,SESQ             ) =   0.22800 * RK( 568 ) * 
      PD( xRNO3           ,CL               ) =   0.54700 * RK( 598 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  87: HOCCHO                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HOCCHO          ,NO               ) =             RK( 497 ) * 
      PD( HOCCHO          ,NO3              ) = -           RK( 277 ) * 
     &                                          +           RK( 499 ) * 
      PD( HOCCHO          ,OH               ) = -           RK( 275 ) * 
      PD( HOCCHO          ,MEO2             ) =   0.50000 * RK( 500 ) * 
      PD( HOCCHO          ,RO2C             ) =   0.50000 * RK( 501 ) * 
      PD( HOCCHO          ,RO2XC            ) =   0.50000 * RK( 502 ) * 
      PD( HOCCHO          ,MECO3            ) =             RK( 503 ) * 
      PD( HOCCHO          ,RCO3             ) =             RK( 504 ) * 
      PD( HOCCHO          ,BZCO3            ) =             RK( 505 ) * 
      PD( HOCCHO          ,MACO3            ) =             RK( 506 ) * 
      PD( HOCCHO          ,IPRD             ) =   0.46700 * RK( 270 )   
      PD( HOCCHO          ,xHOCCHO          ) =             RK( 497 ) * 
     &                                          +           RK( 499 ) * 
     &                                          + 0.50000 * RK( 500 ) * 
     &                                          + 0.50000 * RK( 501 ) * 
     &                                          + 0.50000 * RK( 502 ) * 
     &                                          +           RK( 503 ) * 
     &                                          +           RK( 504 ) * 
     &                                          +           RK( 505 ) * 
     &                                          +           RK( 506 ) * 
      PD( HOCCHO          ,HOCCHO           ) = -           RK( 275 ) * 
     &                                          -           RK( 276 )   
     &                                          -           RK( 277 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  88: xACETONE                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xACETONE        ,NO               ) = -           RK( 317 ) * 
      PD( xACETONE        ,O3               ) =   0.34500 * RK( 535 ) * 
     &                                          + 0.00300 * RK( 553 ) * 
     &                                          + 0.05700 * RK( 563 ) * 
     &                                          + 0.05700 * RK( 567 ) * 
      PD( xACETONE        ,NO3              ) = -           RK( 319 ) * 
     &                                          + 0.06900 * RK( 536 ) * 
     &                                          + 0.03400 * RK( 554 ) * 
     &                                          + 0.11600 * RK( 558 ) * 
     &                                          + 0.23900 * RK( 564 ) * 
     &                                          + 0.23900 * RK( 568 ) * 
      PD( xACETONE        ,OH               ) =   0.00400 * RK( 273 ) * 
     &                                          + 0.03400 * RK( 534 ) * 
     &                                          + 0.70400 * RK( 548 ) * 
     &                                          + 0.02400 * RK( 549 ) * 
     &                                          + 0.42600 * RK( 550 ) * 
     &                                          + 0.07100 * RK( 551 ) * 
     &                                          + 0.00700 * RK( 552 ) * 
     &                                          + 0.14400 * RK( 556 ) * 
     &                                          + 0.03700 * RK( 562 ) * 
     &                                          + 0.03700 * RK( 566 ) * 
      PD( xACETONE        ,HO2              ) = -           RK( 318 ) * 
      PD( xACETONE        ,MEO2             ) = -           RK( 320 ) * 
      PD( xACETONE        ,RO2C             ) = -           RK( 321 ) * 
      PD( xACETONE        ,RO2XC            ) = -           RK( 322 ) * 
      PD( xACETONE        ,MECO3            ) = -           RK( 323 ) * 
      PD( xACETONE        ,RCO3             ) = -           RK( 324 ) * 
      PD( xACETONE        ,BZCO3            ) = -           RK( 325 ) * 
      PD( xACETONE        ,MACO3            ) = -           RK( 326 ) * 
      PD( xACETONE        ,RNO3             ) =   0.00400 * RK( 273 ) * 
     &                                          + 0.00800 * RK( 274 )   
     &                                          + 0.00300 * RK( 598 ) * 
      PD( xACETONE        ,xACETONE         ) = -           RK( 317 ) * 
     &                                          -           RK( 318 ) * 
     &                                          -           RK( 319 ) * 
     &                                          -           RK( 320 ) * 
     &                                          -           RK( 321 ) * 
     &                                          -           RK( 322 ) * 
     &                                          -           RK( 323 ) * 
     &                                          -           RK( 324 ) * 
     &                                          -           RK( 325 ) * 
     &                                          -           RK( 326 ) * 
      PD( xACETONE        ,APIN             ) =   0.03400 * RK( 534 ) * 
     &                                          + 0.34500 * RK( 535 ) * 
     &                                          + 0.06900 * RK( 536 ) * 
     &                                          + 0.27400 * RK( 650 ) * 
      PD( xACETONE        ,ALK2             ) =   0.70400 * RK( 548 ) * 
     &                                          + 0.48800 * RK( 659 ) * 
      PD( xACETONE        ,ALK3             ) =   0.02400 * RK( 549 ) * 
     &                                          + 0.07500 * RK( 660 ) * 
      PD( xACETONE        ,ALK4             ) =   0.42600 * RK( 550 ) * 
     &                                          + 0.24700 * RK( 661 ) * 
      PD( xACETONE        ,ALK5             ) =   0.07100 * RK( 551 ) * 
     &                                          + 0.04400 * RK( 662 ) * 
      PD( xACETONE        ,OLE1             ) =   0.00700 * RK( 552 ) * 
     &                                          + 0.00300 * RK( 553 ) * 
     &                                          + 0.03400 * RK( 554 ) * 
      PD( xACETONE        ,OLE2             ) =   0.14400 * RK( 556 ) * 
     &                                          + 0.11600 * RK( 558 ) * 
     &                                          + 0.08600 * RK( 664 ) * 
      PD( xACETONE        ,TERP             ) =   0.03700 * RK( 562 ) * 
     &                                          + 0.05700 * RK( 563 ) * 
     &                                          + 0.23900 * RK( 564 ) * 
     &                                          + 0.27400 * RK( 667 ) * 
      PD( xACETONE        ,SESQ             ) =   0.03700 * RK( 566 ) * 
     &                                          + 0.05700 * RK( 567 ) * 
     &                                          + 0.23900 * RK( 568 ) * 
     &                                          + 0.27400 * RK( 668 ) * 
      PD( xACETONE        ,CL               ) =   0.00300 * RK( 598 ) * 
     &                                          + 0.27400 * RK( 650 ) * 
     &                                          + 0.48800 * RK( 659 ) * 
     &                                          + 0.07500 * RK( 660 ) * 
     &                                          + 0.24700 * RK( 661 ) * 
     &                                          + 0.04400 * RK( 662 ) * 
     &                                          + 0.08600 * RK( 664 ) * 
     &                                          + 0.27400 * RK( 667 ) * 
     &                                          + 0.27400 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  89: ACROLEIN                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACROLEIN        ,NO               ) =             RK( 507 ) * 
      PD( ACROLEIN        ,O3P              ) = -           RK( 281 ) * 
      PD( ACROLEIN        ,O3               ) = -           RK( 279 ) * 
     &                                          + 0.50000 * RK( 527 ) * 
      PD( ACROLEIN        ,NO3              ) = -           RK( 280 ) * 
     &                                          +           RK( 509 ) * 
      PD( ACROLEIN        ,OH               ) = -           RK( 278 ) * 
      PD( ACROLEIN        ,MEO2             ) =   0.50000 * RK( 510 ) * 
      PD( ACROLEIN        ,RO2C             ) =   0.50000 * RK( 511 ) * 
      PD( ACROLEIN        ,RO2XC            ) =   0.50000 * RK( 512 ) * 
      PD( ACROLEIN        ,MECO3            ) =             RK( 513 ) * 
      PD( ACROLEIN        ,RCO3             ) =             RK( 514 ) * 
      PD( ACROLEIN        ,BZCO3            ) =             RK( 515 ) * 
      PD( ACROLEIN        ,MACO3            ) =             RK( 516 ) * 
      PD( ACROLEIN        ,ACROLEIN         ) = -           RK( 278 ) * 
     &                                          -           RK( 279 ) * 
     &                                          -           RK( 280 ) * 
     &                                          -           RK( 281 ) * 
     &                                          -           RK( 282 )   
     &                                          -           RK( 607 ) * 
      PD( ACROLEIN        ,xACROLEIN        ) =             RK( 507 ) * 
     &                                          +           RK( 509 ) * 
     &                                          + 0.50000 * RK( 510 ) * 
     &                                          + 0.50000 * RK( 511 ) * 
     &                                          + 0.50000 * RK( 512 ) * 
     &                                          +           RK( 513 ) * 
     &                                          +           RK( 514 ) * 
     &                                          +           RK( 515 ) * 
     &                                          +           RK( 516 ) * 
      PD( ACROLEIN        ,BUTADIENE13      ) =   0.50000 * RK( 527 ) * 
      PD( ACROLEIN        ,CL               ) = -           RK( 607 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  90: xBALD                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xBALD           ,NO               ) = -           RK( 377 ) * 
      PD( xBALD           ,NO3              ) = -           RK( 379 ) * 
      PD( xBALD           ,OH               ) =   0.06500 * RK( 541 ) * 
     &                                          + 0.04100 * RK( 542 ) * 
     &                                          + 0.04700 * RK( 543 ) * 
     &                                          + 0.08800 * RK( 544 ) * 
     &                                          + 0.03600 * RK( 545 ) * 
     &                                          + 0.02600 * RK( 561 ) * 
      PD( xBALD           ,HO2              ) = -           RK( 378 ) * 
      PD( xBALD           ,MEO2             ) = -           RK( 380 ) * 
      PD( xBALD           ,RO2C             ) = -           RK( 381 ) * 
      PD( xBALD           ,RO2XC            ) = -           RK( 382 ) * 
      PD( xBALD           ,MECO3            ) = -           RK( 383 ) * 
      PD( xBALD           ,RCO3             ) = -           RK( 384 ) * 
      PD( xBALD           ,BZCO3            ) = -           RK( 385 ) * 
      PD( xBALD           ,MACO3            ) = -           RK( 386 ) * 
      PD( xBALD           ,CRES             ) =             RK( 602 ) * 
      PD( xBALD           ,xBALD            ) = -           RK( 377 ) * 
     &                                          -           RK( 378 ) * 
     &                                          -           RK( 379 ) * 
     &                                          -           RK( 380 ) * 
     &                                          -           RK( 381 ) * 
     &                                          -           RK( 382 ) * 
     &                                          -           RK( 383 ) * 
     &                                          -           RK( 384 ) * 
     &                                          -           RK( 385 ) * 
     &                                          -           RK( 386 ) * 
      PD( xBALD           ,TOLUENE          ) =   0.06500 * RK( 541 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
      PD( xBALD           ,MXYL             ) =   0.04100 * RK( 542 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
      PD( xBALD           ,OXYL             ) =   0.04700 * RK( 543 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
      PD( xBALD           ,PXYL             ) =   0.08800 * RK( 544 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
      PD( xBALD           ,TRIMETH_BENZ124  ) =   0.03600 * RK( 545 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
      PD( xBALD           ,ARO2             ) =   0.02600 * RK( 561 ) * 
     &                                          + 0.46900 * RK( 666 ) * 
      PD( xBALD           ,CL               ) =             RK( 602 ) * 
     &                                          + 0.89400 * RK( 652 ) * 
     &                                          + 0.86400 * RK( 653 ) * 
     &                                          + 0.86400 * RK( 654 ) * 
     &                                          + 0.86400 * RK( 655 ) * 
     &                                          + 0.83800 * RK( 656 ) * 
     &                                          + 0.46900 * RK( 666 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  91: xAFG3                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xAFG3           ,NO               ) = -           RK( 407 ) * 
      PD( xAFG3           ,NO3              ) = -           RK( 409 ) * 
      PD( xAFG3           ,OH               ) =   0.28600 * RK( 544 ) * 
     &                                          + 0.15100 * RK( 545 ) * 
     &                                          + 0.06800 * RK( 561 ) * 
      PD( xAFG3           ,HO2              ) = -           RK( 408 ) * 
      PD( xAFG3           ,MEO2             ) = -           RK( 410 ) * 
      PD( xAFG3           ,RO2C             ) = -           RK( 411 ) * 
      PD( xAFG3           ,RO2XC            ) = -           RK( 412 ) * 
      PD( xAFG3           ,MECO3            ) = -           RK( 413 ) * 
      PD( xAFG3           ,RCO3             ) = -           RK( 414 ) * 
      PD( xAFG3           ,BZCO3            ) = -           RK( 415 ) * 
      PD( xAFG3           ,MACO3            ) = -           RK( 416 ) * 
      PD( xAFG3           ,xAFG3            ) = -           RK( 407 ) * 
     &                                          -           RK( 408 ) * 
     &                                          -           RK( 409 ) * 
     &                                          -           RK( 410 ) * 
     &                                          -           RK( 411 ) * 
     &                                          -           RK( 412 ) * 
     &                                          -           RK( 413 ) * 
     &                                          -           RK( 414 ) * 
     &                                          -           RK( 415 ) * 
     &                                          -           RK( 416 ) * 
      PD( xAFG3           ,APIN             ) =   0.00100 * RK( 650 ) * 
      PD( xAFG3           ,PXYL             ) =   0.28600 * RK( 544 ) * 
      PD( xAFG3           ,TRIMETH_BENZ124  ) =   0.15100 * RK( 545 ) * 
      PD( xAFG3           ,ARO2             ) =   0.06800 * RK( 561 ) * 
      PD( xAFG3           ,TERP             ) =   0.00100 * RK( 667 ) * 
      PD( xAFG3           ,SESQ             ) =   0.00100 * RK( 668 ) * 
      PD( xAFG3           ,CL               ) =   0.00100 * RK( 650 ) * 
     &                                          + 0.00100 * RK( 667 ) * 
     &                                          + 0.00100 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  92: xMACR                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMACR           ,NO               ) = -           RK( 417 ) * 
      PD( xMACR           ,O3               ) =   0.00200 * RK( 563 ) * 
     &                                          + 0.00200 * RK( 567 ) * 
      PD( xMACR           ,NO3              ) = -           RK( 419 ) * 
     &                                          + 0.00500 * RK( 564 ) * 
     &                                          + 0.00500 * RK( 568 ) * 
      PD( xMACR           ,OH               ) =   0.23000 * RK( 530 ) * 
     &                                          + 0.00100 * RK( 552 ) * 
      PD( xMACR           ,HO2              ) = -           RK( 418 ) * 
      PD( xMACR           ,MEO2             ) = -           RK( 420 ) * 
      PD( xMACR           ,RO2C             ) = -           RK( 421 ) * 
      PD( xMACR           ,RO2XC            ) = -           RK( 422 ) * 
      PD( xMACR           ,MECO3            ) = -           RK( 423 ) * 
      PD( xMACR           ,RCO3             ) = -           RK( 424 ) * 
      PD( xMACR           ,BZCO3            ) = -           RK( 425 ) * 
      PD( xMACR           ,MACO3            ) = -           RK( 426 ) * 
      PD( xMACR           ,xMACR            ) = -           RK( 417 ) * 
     &                                          -           RK( 418 ) * 
     &                                          -           RK( 419 ) * 
     &                                          -           RK( 420 ) * 
     &                                          -           RK( 421 ) * 
     &                                          -           RK( 422 ) * 
     &                                          -           RK( 423 ) * 
     &                                          -           RK( 424 ) * 
     &                                          -           RK( 425 ) * 
     &                                          -           RK( 426 ) * 
      PD( xMACR           ,ISOPRENE         ) =   0.23000 * RK( 530 ) * 
      PD( xMACR           ,OLE1             ) =   0.00100 * RK( 552 ) * 
      PD( xMACR           ,OLE2             ) =   0.02500 * RK( 664 ) * 
      PD( xMACR           ,TERP             ) =   0.00200 * RK( 563 ) * 
     &                                          + 0.00500 * RK( 564 ) * 
      PD( xMACR           ,SESQ             ) =   0.00200 * RK( 567 ) * 
     &                                          + 0.00500 * RK( 568 ) * 
      PD( xMACR           ,CL               ) =   0.02500 * RK( 664 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  93: xMVK                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xMVK            ,NO               ) = -           RK( 427 ) * 
      PD( xMVK            ,NO3              ) = -           RK( 429 ) * 
     &                                          + 0.46000 * RK( 528 ) * 
     &                                          + 0.00100 * RK( 564 ) * 
     &                                          + 0.00100 * RK( 568 ) * 
      PD( xMVK            ,OH               ) =   0.32000 * RK( 530 ) * 
     &                                          + 0.01200 * RK( 552 ) * 
     &                                          + 0.00200 * RK( 556 ) * 
     &                                          + 0.00300 * RK( 562 ) * 
     &                                          + 0.00300 * RK( 566 ) * 
      PD( xMVK            ,HO2              ) = -           RK( 428 ) * 
      PD( xMVK            ,MEO2             ) = -           RK( 430 ) * 
      PD( xMVK            ,RO2C             ) = -           RK( 431 ) * 
      PD( xMVK            ,RO2XC            ) = -           RK( 432 ) * 
      PD( xMVK            ,MECO3            ) = -           RK( 433 ) * 
      PD( xMVK            ,RCO3             ) = -           RK( 434 ) * 
      PD( xMVK            ,BZCO3            ) = -           RK( 435 ) * 
      PD( xMVK            ,MACO3            ) = -           RK( 436 ) * 
      PD( xMVK            ,xMVK             ) = -           RK( 427 ) * 
     &                                          -           RK( 428 ) * 
     &                                          -           RK( 429 ) * 
     &                                          -           RK( 430 ) * 
     &                                          -           RK( 431 ) * 
     &                                          -           RK( 432 ) * 
     &                                          -           RK( 433 ) * 
     &                                          -           RK( 434 ) * 
     &                                          -           RK( 435 ) * 
     &                                          -           RK( 436 ) * 
      PD( xMVK            ,BUTADIENE13      ) =   0.46000 * RK( 528 ) * 
      PD( xMVK            ,ISOPRENE         ) =   0.32000 * RK( 530 ) * 
     &                                          + 0.17700 * RK( 649 ) * 
      PD( xMVK            ,APIN             ) =   0.00300 * RK( 650 ) * 
      PD( xMVK            ,OLE1             ) =   0.01200 * RK( 552 ) * 
     &                                          + 0.01900 * RK( 663 ) * 
      PD( xMVK            ,OLE2             ) =   0.00200 * RK( 556 ) * 
     &                                          + 0.05800 * RK( 664 ) * 
      PD( xMVK            ,TERP             ) =   0.00300 * RK( 562 ) * 
     &                                          + 0.00100 * RK( 564 ) * 
     &                                          + 0.00300 * RK( 667 ) * 
      PD( xMVK            ,SESQ             ) =   0.00300 * RK( 566 ) * 
     &                                          + 0.00100 * RK( 568 ) * 
     &                                          + 0.00300 * RK( 668 ) * 
      PD( xMVK            ,CL               ) =   0.17700 * RK( 649 ) * 
     &                                          + 0.00300 * RK( 650 ) * 
     &                                          + 0.01900 * RK( 663 ) * 
     &                                          + 0.05800 * RK( 664 ) * 
     &                                          + 0.00300 * RK( 667 ) * 
     &                                          + 0.00300 * RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  94: yRAOOH                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( yRAOOH          ,NO               ) = -           RK( 477 ) * 
      PD( yRAOOH          ,NO3              ) = -           RK( 479 ) * 
      PD( yRAOOH          ,OH               ) =   0.31400 * RK( 540 ) * 
     &                                          + 0.43500 * RK( 541 ) * 
     &                                          + 0.55500 * RK( 542 ) * 
     &                                          + 0.58600 * RK( 543 ) * 
     &                                          + 0.46100 * RK( 544 ) * 
     &                                          + 0.70500 * RK( 545 ) * 
      PD( yRAOOH          ,HO2              ) = -           RK( 478 ) * 
      PD( yRAOOH          ,MEO2             ) = -           RK( 480 ) * 
      PD( yRAOOH          ,RO2C             ) = -           RK( 481 ) * 
      PD( yRAOOH          ,RO2XC            ) = -           RK( 482 ) * 
      PD( yRAOOH          ,MECO3            ) = -           RK( 483 ) * 
      PD( yRAOOH          ,RCO3             ) = -           RK( 484 ) * 
      PD( yRAOOH          ,BZCO3            ) = -           RK( 485 ) * 
      PD( yRAOOH          ,MACO3            ) = -           RK( 486 ) * 
      PD( yRAOOH          ,yRAOOH           ) = -           RK( 477 ) * 
     &                                          -           RK( 478 ) * 
     &                                          -           RK( 479 ) * 
     &                                          -           RK( 480 ) * 
     &                                          -           RK( 481 ) * 
     &                                          -           RK( 482 ) * 
     &                                          -           RK( 483 ) * 
     &                                          -           RK( 484 ) * 
     &                                          -           RK( 485 ) * 
     &                                          -           RK( 486 ) * 
      PD( yRAOOH          ,BENZENE          ) =   0.31400 * RK( 540 ) * 
      PD( yRAOOH          ,TOLUENE          ) =   0.43500 * RK( 541 ) * 
      PD( yRAOOH          ,MXYL             ) =   0.55500 * RK( 542 ) * 
      PD( yRAOOH          ,OXYL             ) =   0.58600 * RK( 543 ) * 
      PD( yRAOOH          ,PXYL             ) =   0.46100 * RK( 544 ) * 
      PD( yRAOOH          ,TRIMETH_BENZ124  ) =   0.70500 * RK( 545 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  95: xACROLEIN                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xACROLEIN       ,NO               ) = -           RK( 507 ) * 
      PD( xACROLEIN       ,O3P              ) =   0.11500 * RK( 529 ) * 
      PD( xACROLEIN       ,NO3              ) = -           RK( 509 ) * 
      PD( xACROLEIN       ,OH               ) =   0.48000 * RK( 526 ) * 
     &                                          + 0.03600 * RK( 552 ) * 
      PD( xACROLEIN       ,HO2              ) = -           RK( 508 ) * 
      PD( xACROLEIN       ,MEO2             ) = -           RK( 510 ) * 
      PD( xACROLEIN       ,RO2C             ) = -           RK( 511 ) * 
      PD( xACROLEIN       ,RO2XC            ) = -           RK( 512 ) * 
      PD( xACROLEIN       ,MECO3            ) = -           RK( 513 ) * 
      PD( xACROLEIN       ,RCO3             ) = -           RK( 514 ) * 
      PD( xACROLEIN       ,BZCO3            ) = -           RK( 515 ) * 
      PD( xACROLEIN       ,MACO3            ) = -           RK( 516 ) * 
      PD( xACROLEIN       ,xACROLEIN        ) = -           RK( 507 ) * 
     &                                          -           RK( 508 ) * 
     &                                          -           RK( 509 ) * 
     &                                          -           RK( 510 ) * 
     &                                          -           RK( 511 ) * 
     &                                          -           RK( 512 ) * 
     &                                          -           RK( 513 ) * 
     &                                          -           RK( 514 ) * 
     &                                          -           RK( 515 ) * 
     &                                          -           RK( 516 ) * 
      PD( xACROLEIN       ,PROPENE          ) =   0.12400 * RK( 647 ) * 
      PD( xACROLEIN       ,BUTADIENE13      ) =   0.48000 * RK( 526 ) * 
     &                                          + 0.11500 * RK( 529 ) * 
     &                                          + 0.45700 * RK( 648 ) * 
      PD( xACROLEIN       ,OLE1             ) =   0.03600 * RK( 552 ) * 
     &                                          + 0.21500 * RK( 663 ) * 
      PD( xACROLEIN       ,CL               ) =   0.12400 * RK( 647 ) * 
     &                                          + 0.45700 * RK( 648 ) * 
     &                                          + 0.21500 * RK( 663 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  96: ETHENE                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETHENE          ,O3P              ) = -           RK( 521 ) * 
      PD( ETHENE          ,O3               ) = -           RK( 519 ) * 
      PD( ETHENE          ,NO3              ) = -           RK( 520 ) * 
      PD( ETHENE          ,OH               ) = -           RK( 518 ) * 
      PD( ETHENE          ,ETHENE           ) = -           RK( 518 ) * 
     &                                          -           RK( 519 ) * 
     &                                          -           RK( 520 ) * 
     &                                          -           RK( 521 ) * 
     &                                          -           RK( 646 ) * 
      PD( ETHENE          ,CL               ) = -           RK( 646 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  97: PROPENE                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PROPENE         ,O3P              ) = -           RK( 525 ) * 
      PD( PROPENE         ,O3               ) = -           RK( 523 ) * 
      PD( PROPENE         ,NO3              ) = -           RK( 524 ) * 
      PD( PROPENE         ,OH               ) = -           RK( 522 ) * 
      PD( PROPENE         ,PROPENE          ) = -           RK( 522 ) * 
     &                                          -           RK( 523 ) * 
     &                                          -           RK( 524 ) * 
     &                                          -           RK( 525 ) * 
     &                                          -           RK( 647 ) * 
      PD( PROPENE         ,CL               ) = -           RK( 647 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  98: BUTADIENE13                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BUTADIENE13     ,O3P              ) = -           RK( 529 ) * 
      PD( BUTADIENE13     ,O3               ) = -           RK( 527 ) * 
      PD( BUTADIENE13     ,NO3              ) = -           RK( 528 ) * 
      PD( BUTADIENE13     ,OH               ) = -           RK( 526 ) * 
      PD( BUTADIENE13     ,BUTADIENE13      ) = -           RK( 526 ) * 
     &                                          -           RK( 527 ) * 
     &                                          -           RK( 528 ) * 
     &                                          -           RK( 529 ) * 
     &                                          -           RK( 648 ) * 
      PD( BUTADIENE13     ,CL               ) = -           RK( 648 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  99: ISOPRENE                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPRENE        ,O3P              ) = -           RK( 533 ) * 
      PD( ISOPRENE        ,O3               ) = -           RK( 531 ) * 
      PD( ISOPRENE        ,NO3              ) = -           RK( 532 ) * 
      PD( ISOPRENE        ,OH               ) = -           RK( 530 ) * 
      PD( ISOPRENE        ,ISOPRENE         ) = -           RK( 530 ) * 
     &                                          -           RK( 531 ) * 
     &                                          -           RK( 532 ) * 
     &                                          -           RK( 533 ) * 
     &                                          -           RK( 649 ) * 
      PD( ISOPRENE        ,CL               ) = -           RK( 649 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 100: ISOPRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPRXN         ,OH               ) =             RK( 530 ) * 
      PD( ISOPRXN         ,ISOPRENE         ) =             RK( 530 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 101: APIN                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( APIN            ,O3P              ) = -           RK( 537 ) * 
      PD( APIN            ,O3               ) = -           RK( 535 ) * 
      PD( APIN            ,NO3              ) = -           RK( 536 ) * 
      PD( APIN            ,OH               ) = -           RK( 534 ) * 
      PD( APIN            ,APIN             ) = -           RK( 534 ) * 
     &                                          -           RK( 535 ) * 
     &                                          -           RK( 536 ) * 
     &                                          -           RK( 537 ) * 
     &                                          -           RK( 650 ) * 
      PD( APIN            ,CL               ) = -           RK( 650 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 102: TRPRXN                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TRPRXN          ,O3P              ) =             RK( 537 ) * 
     &                                          +           RK( 565 ) * 
      PD( TRPRXN          ,O3               ) =             RK( 535 ) * 
     &                                          +           RK( 563 ) * 
      PD( TRPRXN          ,NO3              ) =             RK( 536 ) * 
     &                                          +           RK( 564 ) * 
      PD( TRPRXN          ,OH               ) =             RK( 534 ) * 
     &                                          +           RK( 562 ) * 
      PD( TRPRXN          ,APIN             ) =             RK( 534 ) * 
     &                                          +           RK( 535 ) * 
     &                                          +           RK( 536 ) * 
     &                                          +           RK( 537 ) * 
      PD( TRPRXN          ,TERP             ) =             RK( 562 ) * 
     &                                          +           RK( 563 ) * 
     &                                          +           RK( 564 ) * 
     &                                          +           RK( 565 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 103: ACETYLENE                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACETYLENE       ,O3               ) = -           RK( 539 ) * 
      PD( ACETYLENE       ,OH               ) = -           RK( 538 ) * 
      PD( ACETYLENE       ,ACETYLENE        ) = -           RK( 538 ) * 
     &                                          -           RK( 539 ) * 
     &                                          -           RK( 651 ) * 
      PD( ACETYLENE       ,CL               ) = -           RK( 651 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 104: BENZENE                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZENE         ,OH               ) = -           RK( 540 ) * 
      PD( BENZENE         ,BENZENE          ) = -           RK( 540 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 105: BENZRO2                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZRO2         ,NO               ) = -           RK( 669 ) * 
      PD( BENZRO2         ,OH               ) =             RK( 540 ) * 
      PD( BENZRO2         ,HO2              ) = -           RK( 670 ) * 
      PD( BENZRO2         ,BENZENE          ) =             RK( 540 ) * 
      PD( BENZRO2         ,BENZRO2          ) = -           RK( 669 ) * 
     &                                          -           RK( 670 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 106: TOLUENE                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLUENE         ,OH               ) = -           RK( 541 ) * 
      PD( TOLUENE         ,TOLUENE          ) = -           RK( 541 ) * 
     &                                          -           RK( 652 ) * 
      PD( TOLUENE         ,CL               ) = -           RK( 652 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 107: TOLRO2                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLRO2          ,NO               ) = -           RK( 673 ) * 
      PD( TOLRO2          ,OH               ) =             RK( 541 ) * 
     &                                          +           RK( 560 ) * 
      PD( TOLRO2          ,HO2              ) = -           RK( 674 ) * 
      PD( TOLRO2          ,TOLUENE          ) =             RK( 541 ) * 
      PD( TOLRO2          ,TOLRO2           ) = -           RK( 673 ) * 
     &                                          -           RK( 674 ) * 
      PD( TOLRO2          ,ARO1             ) =             RK( 560 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 108: MXYL                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MXYL            ,OH               ) = -           RK( 542 ) * 
      PD( MXYL            ,MXYL             ) = -           RK( 542 ) * 
     &                                          -           RK( 653 ) * 
      PD( MXYL            ,CL               ) = -           RK( 653 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 109: XYLRO2                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLRO2          ,NO               ) = -           RK( 671 ) * 
      PD( XYLRO2          ,OH               ) =             RK( 542 ) * 
     &                                          +           RK( 543 ) * 
     &                                          +           RK( 544 ) * 
     &                                          +           RK( 545 ) * 
     &                                          +           RK( 561 ) * 
      PD( XYLRO2          ,HO2              ) = -           RK( 672 ) * 
      PD( XYLRO2          ,MXYL             ) =             RK( 542 ) * 
      PD( XYLRO2          ,XYLRO2           ) = -           RK( 671 ) * 
     &                                          -           RK( 672 ) * 
      PD( XYLRO2          ,OXYL             ) =             RK( 543 ) * 
      PD( XYLRO2          ,PXYL             ) =             RK( 544 ) * 
      PD( XYLRO2          ,TRIMETH_BENZ124  ) =             RK( 545 ) * 
      PD( XYLRO2          ,ARO2             ) =             RK( 561 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 110: OXYL                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OXYL            ,OH               ) = -           RK( 543 ) * 
      PD( OXYL            ,OXYL             ) = -           RK( 543 ) * 
     &                                          -           RK( 654 ) * 
      PD( OXYL            ,CL               ) = -           RK( 654 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 111: PXYL                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PXYL            ,OH               ) = -           RK( 544 ) * 
      PD( PXYL            ,PXYL             ) = -           RK( 544 ) * 
     &                                          -           RK( 655 ) * 
      PD( PXYL            ,CL               ) = -           RK( 655 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 112: TRIMETH_BENZ124            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TRIMETH_BENZ124 ,OH               ) = -           RK( 545 ) * 
      PD( TRIMETH_BENZ124 ,TRIMETH_BENZ124  ) = -           RK( 545 ) * 
     &                                          -           RK( 656 ) * 
      PD( TRIMETH_BENZ124 ,CL               ) = -           RK( 656 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 113: ETOH                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETOH            ,OH               ) = -           RK( 546 ) * 
      PD( ETOH            ,ETOH             ) = -           RK( 546 ) * 
     &                                          -           RK( 657 ) * 
      PD( ETOH            ,CL               ) = -           RK( 657 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 114: ALK1                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK1            ,OH               ) = -           RK( 547 ) * 
      PD( ALK1            ,ALK1             ) = -           RK( 547 ) * 
     &                                          -           RK( 658 ) * 
      PD( ALK1            ,CL               ) = -           RK( 658 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 115: ALK2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK2            ,OH               ) = -           RK( 548 ) * 
      PD( ALK2            ,ALK2             ) = -           RK( 548 ) * 
     &                                          -           RK( 659 ) * 
      PD( ALK2            ,CL               ) = -           RK( 659 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 116: ALK3                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK3            ,OH               ) = -           RK( 549 ) * 
      PD( ALK3            ,ALK3             ) = -           RK( 549 ) * 
     &                                          -           RK( 660 ) * 
      PD( ALK3            ,CL               ) = -           RK( 660 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 117: ALK4                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK4            ,OH               ) = -           RK( 550 ) * 
      PD( ALK4            ,ALK4             ) = -           RK( 550 ) * 
     &                                          -           RK( 661 ) * 
      PD( ALK4            ,CL               ) = -           RK( 661 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 118: ALK5                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK5            ,OH               ) = -           RK( 551 ) * 
      PD( ALK5            ,ALK5             ) = -           RK( 551 ) * 
     &                                          -           RK( 662 ) * 
      PD( ALK5            ,CL               ) = -           RK( 662 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 119: ALK5RXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALK5RXN         ,OH               ) =             RK( 551 ) * 
      PD( ALK5RXN         ,ALK5             ) =             RK( 551 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 120: OLE1                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OLE1            ,O3P              ) = -           RK( 555 ) * 
      PD( OLE1            ,O3               ) = -           RK( 553 ) * 
      PD( OLE1            ,NO3              ) = -           RK( 554 ) * 
      PD( OLE1            ,OH               ) = -           RK( 552 ) * 
      PD( OLE1            ,OLE1             ) = -           RK( 552 ) * 
     &                                          -           RK( 553 ) * 
     &                                          -           RK( 554 ) * 
     &                                          -           RK( 555 ) * 
     &                                          -           RK( 663 ) * 
      PD( OLE1            ,CL               ) = -           RK( 663 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 121: OLE2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OLE2            ,O3P              ) = -           RK( 559 ) * 
      PD( OLE2            ,O3               ) = -           RK( 557 ) * 
      PD( OLE2            ,NO3              ) = -           RK( 558 ) * 
      PD( OLE2            ,OH               ) = -           RK( 556 ) * 
      PD( OLE2            ,OLE2             ) = -           RK( 556 ) * 
     &                                          -           RK( 557 ) * 
     &                                          -           RK( 558 ) * 
     &                                          -           RK( 559 ) * 
     &                                          -           RK( 664 ) * 
      PD( OLE2            ,CL               ) = -           RK( 664 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 122: ARO1                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ARO1            ,OH               ) = -           RK( 560 ) * 
      PD( ARO1            ,ARO1             ) = -           RK( 560 ) * 
     &                                          -           RK( 665 ) * 
      PD( ARO1            ,CL               ) = -           RK( 665 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 123: ARO2                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ARO2            ,OH               ) = -           RK( 561 ) * 
      PD( ARO2            ,ARO2             ) = -           RK( 561 ) * 
     &                                          -           RK( 666 ) * 
      PD( ARO2            ,CL               ) = -           RK( 666 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 124: TERP                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TERP            ,O3P              ) = -           RK( 565 ) * 
      PD( TERP            ,O3               ) = -           RK( 563 ) * 
      PD( TERP            ,NO3              ) = -           RK( 564 ) * 
      PD( TERP            ,OH               ) = -           RK( 562 ) * 
      PD( TERP            ,TERP             ) = -           RK( 562 ) * 
     &                                          -           RK( 563 ) * 
     &                                          -           RK( 564 ) * 
     &                                          -           RK( 565 ) * 
     &                                          -           RK( 667 ) * 
      PD( TERP            ,CL               ) = -           RK( 667 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 125: SESQ                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQ            ,O3P              ) = -           RK( 569 ) * 
      PD( SESQ            ,O3               ) = -           RK( 567 ) * 
      PD( SESQ            ,NO3              ) = -           RK( 568 ) * 
      PD( SESQ            ,OH               ) = -           RK( 566 ) * 
      PD( SESQ            ,SESQ             ) = -           RK( 566 ) * 
     &                                          -           RK( 567 ) * 
     &                                          -           RK( 568 ) * 
     &                                          -           RK( 569 ) * 
     &                                          -           RK( 668 ) * 
      PD( SESQ            ,CL               ) = -           RK( 668 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 126: SESQRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQRXN         ,O3P              ) =             RK( 569 ) * 
      PD( SESQRXN         ,O3               ) =             RK( 567 ) * 
      PD( SESQRXN         ,NO3              ) =             RK( 568 ) * 
      PD( SESQRXN         ,OH               ) =             RK( 566 ) * 
      PD( SESQRXN         ,SESQ             ) =             RK( 566 ) * 
     &                                          +           RK( 567 ) * 
     &                                          +           RK( 568 ) * 
     &                                          +           RK( 569 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 127: CL2                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL2             ,CL2              ) = -           RK( 570 )   
      PD( CL2             ,CL               ) =             RK( 586 ) * 
      PD( CL2             ,CLO              ) =   4.00000 * RK( 589 ) * 
      PD( CL2             ,CLONO2           ) =             RK( 586 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 128: CL                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL              ,NO2              ) = -           RK( 573 ) * 
     &                                          -           RK( 574 ) * 
      PD( CL              ,NO               ) = -           RK( 571 ) * 
     &                                          +           RK( 581 ) * 
     &                                          +           RK( 615 ) * 
      PD( CL              ,O3               ) = -           RK( 579 ) * 
      PD( CL              ,NO3              ) = -           RK( 580 ) * 
     &                                          +           RK( 617 ) * 
      PD( CL              ,OH               ) =             RK( 590 ) * 
      PD( CL              ,HO2              ) = -           RK( 577 ) * 
     &                                          -           RK( 578 ) * 
      PD( CL              ,MEO2             ) =   0.50000 * RK( 618 ) * 
      PD( CL              ,HCHO             ) = -           RK( 592 ) * 
      PD( CL              ,MEOH             ) = -           RK( 594 ) * 
      PD( CL              ,RO2C             ) =   0.50000 * RK( 619 ) * 
      PD( CL              ,RO2XC            ) =   0.50000 * RK( 620 ) * 
      PD( CL              ,MECO3            ) =             RK( 621 ) * 
      PD( CL              ,RCO3             ) =             RK( 622 ) * 
      PD( CL              ,BZCO3            ) =             RK( 623 ) * 
      PD( CL              ,MACO3            ) =             RK( 624 ) * 
      PD( CL              ,RNO3             ) = -           RK( 598 ) * 
      PD( CL              ,ACETONE          ) = -           RK( 596 ) * 
      PD( CL              ,CRES             ) = -           RK( 602 ) * 
      PD( CL              ,CCHO             ) = -           RK( 593 ) * 
      PD( CL              ,RCHO             ) = -           RK( 595 ) * 
      PD( CL              ,MEK              ) = -           RK( 597 ) * 
      PD( CL              ,ROOH             ) = -           RK( 604 ) * 
      PD( CL              ,R6OOH            ) = -           RK( 605 ) * 
      PD( CL              ,PRD2             ) = -           RK( 599 ) * 
      PD( CL              ,RAOOH            ) = -           RK( 606 ) * 
      PD( CL              ,MGLY             ) = -           RK( 601 ) * 
      PD( CL              ,IPRD             ) = -           RK( 610 ) * 
      PD( CL              ,GLY              ) = -           RK( 600 ) * 
      PD( CL              ,BALD             ) = -           RK( 603 ) * 
      PD( CL              ,MACR             ) = -           RK( 608 ) * 
      PD( CL              ,MVK              ) = -           RK( 609 ) * 
      PD( CL              ,ACROLEIN         ) = -           RK( 607 ) * 
      PD( CL              ,ETHENE           ) = -           RK( 646 ) * 
      PD( CL              ,PROPENE          ) = -           RK( 647 ) * 
      PD( CL              ,BUTADIENE13      ) = -           RK( 648 ) * 
      PD( CL              ,ISOPRENE         ) = -           RK( 649 ) * 
      PD( CL              ,APIN             ) = -           RK( 650 ) * 
      PD( CL              ,ACETYLENE        ) = -           RK( 651 ) * 
      PD( CL              ,TOLUENE          ) = -           RK( 652 ) * 
      PD( CL              ,MXYL             ) = -           RK( 653 ) * 
      PD( CL              ,OXYL             ) = -           RK( 654 ) * 
      PD( CL              ,PXYL             ) = -           RK( 655 ) * 
      PD( CL              ,TRIMETH_BENZ124  ) = -           RK( 656 ) * 
      PD( CL              ,ETOH             ) = -           RK( 657 ) * 
      PD( CL              ,ALK1             ) = -           RK( 658 ) * 
      PD( CL              ,ALK2             ) = -           RK( 659 ) * 
      PD( CL              ,ALK3             ) = -           RK( 660 ) * 
      PD( CL              ,ALK4             ) = -           RK( 661 ) * 
      PD( CL              ,ALK5             ) = -           RK( 662 ) * 
      PD( CL              ,OLE1             ) = -           RK( 663 ) * 
      PD( CL              ,OLE2             ) = -           RK( 664 ) * 
      PD( CL              ,ARO1             ) = -           RK( 665 ) * 
      PD( CL              ,ARO2             ) = -           RK( 666 ) * 
      PD( CL              ,TERP             ) = -           RK( 667 ) * 
      PD( CL              ,SESQ             ) = -           RK( 668 ) * 
      PD( CL              ,CL2              ) =   2.00000 * RK( 570 )   
      PD( CL              ,CL               ) = -           RK( 571 ) * 
     &                                          -           RK( 573 ) * 
     &                                          -           RK( 574 ) * 
     &                                          -           RK( 577 ) * 
     &                                          -           RK( 578 ) * 
     &                                          -           RK( 579 ) * 
     &                                          -           RK( 580 ) * 
     &                                          -           RK( 586 ) * 
     &                                          -           RK( 591 )   
     &                                          -           RK( 592 ) * 
     &                                          -           RK( 593 ) * 
     &                                          -           RK( 594 ) * 
     &                                          -           RK( 595 ) * 
     &                                          -           RK( 596 ) * 
     &                                          -           RK( 597 ) * 
     &                                          -           RK( 598 ) * 
     &                                          -           RK( 599 ) * 
     &                                          -           RK( 600 ) * 
     &                                          -           RK( 601 ) * 
     &                                          -           RK( 602 ) * 
     &                                          -           RK( 603 ) * 
     &                                          -           RK( 604 ) * 
     &                                          -           RK( 605 ) * 
     &                                          -           RK( 606 ) * 
     &                                          -           RK( 607 ) * 
     &                                          -           RK( 608 ) * 
     &                                          -           RK( 609 ) * 
     &                                          -           RK( 610 ) * 
     &                                          -           RK( 613 ) * 
     &                                          -           RK( 645 )   
     &                                          -           RK( 646 ) * 
     &                                          -           RK( 647 ) * 
     &                                          -           RK( 648 ) * 
     &                                          -           RK( 649 ) * 
     &                                          -           RK( 650 ) * 
     &                                          -           RK( 651 ) * 
     &                                          -           RK( 652 ) * 
     &                                          -           RK( 653 ) * 
     &                                          -           RK( 654 ) * 
     &                                          -           RK( 655 ) * 
     &                                          -           RK( 656 ) * 
     &                                          -           RK( 657 ) * 
     &                                          -           RK( 658 ) * 
     &                                          -           RK( 659 ) * 
     &                                          -           RK( 660 ) * 
     &                                          -           RK( 661 ) * 
     &                                          -           RK( 662 ) * 
     &                                          -           RK( 663 ) * 
     &                                          -           RK( 664 ) * 
     &                                          -           RK( 665 ) * 
     &                                          -           RK( 666 ) * 
     &                                          -           RK( 667 ) * 
     &                                          -           RK( 668 ) * 
      PD( CL              ,CLNO             ) =             RK( 572 )   
      PD( CL              ,CLONO            ) =             RK( 575 )   
      PD( CL              ,CLNO2            ) =             RK( 576 )   
      PD( CL              ,HCL              ) =             RK( 590 ) * 
      PD( CL              ,CLO              ) =             RK( 581 ) * 
     &                                          + 4.00000 * RK( 589 ) * 
      PD( CL              ,CLONO2           ) =             RK( 584 )   
     &                                          -           RK( 586 ) * 
      PD( CL              ,HOCL             ) =             RK( 588 )   
      PD( CL              ,xCL              ) =             RK( 615 ) * 
     &                                          +           RK( 617 ) * 
     &                                          + 0.50000 * RK( 618 ) * 
     &                                          + 0.50000 * RK( 619 ) * 
     &                                          + 0.50000 * RK( 620 ) * 
     &                                          +           RK( 621 ) * 
     &                                          +           RK( 622 ) * 
     &                                          +           RK( 623 ) * 
     &                                          +           RK( 624 ) * 
      PD( CL              ,CLCCHO           ) = -           RK( 613 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 129: CLNO                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO            ,NO               ) =             RK( 571 ) * 
      PD( CLNO            ,CL               ) =             RK( 571 ) * 
      PD( CLNO            ,CLNO             ) = -           RK( 572 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 130: CLONO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLONO           ,NO2              ) =             RK( 573 ) * 
      PD( CLONO           ,CL               ) =             RK( 573 ) * 
      PD( CLONO           ,CLONO            ) = -           RK( 575 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 131: CLNO2                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO2           ,NO2              ) =             RK( 574 ) * 
      PD( CLNO2           ,CL               ) =             RK( 574 ) * 
      PD( CLNO2           ,CLNO2            ) = -           RK( 576 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 132: HCL                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCL             ,OH               ) = -           RK( 590 ) * 
      PD( HCL             ,HO2              ) =             RK( 577 ) * 
      PD( HCL             ,HCHO             ) =             RK( 592 ) * 
      PD( HCL             ,MEOH             ) =             RK( 594 ) * 
      PD( HCL             ,RNO3             ) =             RK( 598 ) * 
      PD( HCL             ,ACETONE          ) =             RK( 596 ) * 
      PD( HCL             ,CRES             ) =             RK( 602 ) * 
      PD( HCL             ,CCHO             ) =             RK( 593 ) * 
      PD( HCL             ,RCHO             ) =             RK( 595 ) * 
      PD( HCL             ,MEK              ) =             RK( 597 ) * 
      PD( HCL             ,ROOH             ) =             RK( 604 ) * 
      PD( HCL             ,R6OOH            ) =             RK( 605 ) * 
      PD( HCL             ,PRD2             ) =             RK( 599 ) * 
      PD( HCL             ,RAOOH            ) =   0.40400 * RK( 606 ) * 
      PD( HCL             ,MGLY             ) =             RK( 601 ) * 
      PD( HCL             ,IPRD             ) =   0.40100 * RK( 610 ) * 
      PD( HCL             ,GLY              ) =             RK( 600 ) * 
      PD( HCL             ,BALD             ) =             RK( 603 ) * 
      PD( HCL             ,MACR             ) =   0.25000 * RK( 608 ) * 
      PD( HCL             ,ACROLEIN         ) =   0.21600 * RK( 607 ) * 
      PD( HCL             ,PROPENE          ) =   0.12400 * RK( 647 ) * 
      PD( HCL             ,ISOPRENE         ) =   0.15000 * RK( 649 ) * 
      PD( HCL             ,APIN             ) =   0.54800 * RK( 650 ) * 
      PD( HCL             ,ETOH             ) =             RK( 657 ) * 
      PD( HCL             ,ALK1             ) =             RK( 658 ) * 
      PD( HCL             ,ALK2             ) =             RK( 659 ) * 
      PD( HCL             ,ALK3             ) =             RK( 660 ) * 
      PD( HCL             ,ALK4             ) =             RK( 661 ) * 
      PD( HCL             ,ALK5             ) =             RK( 662 ) * 
      PD( HCL             ,OLE1             ) =   0.38400 * RK( 663 ) * 
      PD( HCL             ,OLE2             ) =   0.27900 * RK( 664 ) * 
      PD( HCL             ,TERP             ) =   0.54800 * RK( 667 ) * 
      PD( HCL             ,SESQ             ) =   0.54800 * RK( 668 ) * 
      PD( HCL             ,CL               ) =             RK( 577 ) * 
     &                                          +           RK( 591 )   
     &                                          +           RK( 592 ) * 
     &                                          +           RK( 593 ) * 
     &                                          +           RK( 594 ) * 
     &                                          +           RK( 595 ) * 
     &                                          +           RK( 596 ) * 
     &                                          +           RK( 597 ) * 
     &                                          +           RK( 598 ) * 
     &                                          +           RK( 599 ) * 
     &                                          +           RK( 600 ) * 
     &                                          +           RK( 601 ) * 
     &                                          +           RK( 602 ) * 
     &                                          +           RK( 603 ) * 
     &                                          +           RK( 604 ) * 
     &                                          +           RK( 605 ) * 
     &                                          + 0.40400 * RK( 606 ) * 
     &                                          + 0.21600 * RK( 607 ) * 
     &                                          + 0.25000 * RK( 608 ) * 
     &                                          + 0.40100 * RK( 610 ) * 
     &                                          +           RK( 613 ) * 
     &                                          +           RK( 645 )   
     &                                          + 0.12400 * RK( 647 ) * 
     &                                          + 0.15000 * RK( 649 ) * 
     &                                          + 0.54800 * RK( 650 ) * 
     &                                          +           RK( 657 ) * 
     &                                          +           RK( 658 ) * 
     &                                          +           RK( 659 ) * 
     &                                          +           RK( 660 ) * 
     &                                          +           RK( 661 ) * 
     &                                          +           RK( 662 ) * 
     &                                          + 0.38400 * RK( 663 ) * 
     &                                          + 0.27900 * RK( 664 ) * 
     &                                          + 0.54800 * RK( 667 ) * 
     &                                          + 0.54800 * RK( 668 ) * 
      PD( HCL             ,HCL              ) = -           RK( 590 ) * 
      PD( HCL             ,CLCCHO           ) =             RK( 613 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 133: CLO                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLO             ,NO2              ) = -           RK( 582 ) * 
      PD( CLO             ,NO               ) = -           RK( 581 ) * 
      PD( CLO             ,O3               ) =             RK( 579 ) * 
      PD( CLO             ,NO3              ) =             RK( 580 ) * 
      PD( CLO             ,HO2              ) =             RK( 578 ) * 
     &                                          -           RK( 587 ) * 
      PD( CLO             ,CL               ) =             RK( 578 ) * 
     &                                          +           RK( 579 ) * 
     &                                          +           RK( 580 ) * 
      PD( CLO             ,CLO              ) = -           RK( 581 ) * 
     &                                          -           RK( 582 ) * 
     &                                          -           RK( 587 ) * 
     &                                          - 4.00000 * RK( 589 ) * 
      PD( CLO             ,CLONO2           ) =             RK( 583 )   
     &                                          +           RK( 585 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 134: CLONO2                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLONO2          ,NO2              ) =             RK( 582 ) * 
      PD( CLONO2          ,CL               ) = -           RK( 586 ) * 
      PD( CLONO2          ,CLO              ) =             RK( 582 ) * 
      PD( CLONO2          ,CLONO2           ) = -           RK( 583 )   
     &                                          -           RK( 584 )   
     &                                          -           RK( 585 )   
     &                                          -           RK( 586 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 135: HOCL                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HOCL            ,HO2              ) =             RK( 587 ) * 
      PD( HOCL            ,CLO              ) =             RK( 587 ) * 
      PD( HOCL            ,HOCL             ) = -           RK( 588 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 136: xCL                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xCL             ,NO               ) = -           RK( 615 ) * 
      PD( xCL             ,NO3              ) = -           RK( 617 ) * 
      PD( xCL             ,HO2              ) = -           RK( 616 ) * 
      PD( xCL             ,MEO2             ) = -           RK( 618 ) * 
      PD( xCL             ,RO2C             ) = -           RK( 619 ) * 
      PD( xCL             ,RO2XC            ) = -           RK( 620 ) * 
      PD( xCL             ,MECO3            ) = -           RK( 621 ) * 
      PD( xCL             ,RCO3             ) = -           RK( 622 ) * 
      PD( xCL             ,BZCO3            ) = -           RK( 623 ) * 
      PD( xCL             ,MACO3            ) = -           RK( 624 ) * 
      PD( xCL             ,ACROLEIN         ) =   0.27400 * RK( 607 ) * 
      PD( xCL             ,BUTADIENE13      ) =   0.54100 * RK( 648 ) * 
      PD( xCL             ,ISOPRENE         ) =   0.17700 * RK( 649 ) * 
      PD( xCL             ,APIN             ) =   0.06800 * RK( 650 ) * 
      PD( xCL             ,OLE2             ) =   0.44200 * RK( 664 ) * 
      PD( xCL             ,TERP             ) =   0.06800 * RK( 667 ) * 
      PD( xCL             ,SESQ             ) =   0.06800 * RK( 668 ) * 
      PD( xCL             ,CL               ) =   0.27400 * RK( 607 ) * 
     &                                          + 0.54100 * RK( 648 ) * 
     &                                          + 0.17700 * RK( 649 ) * 
     &                                          + 0.06800 * RK( 650 ) * 
     &                                          + 0.44200 * RK( 664 ) * 
     &                                          + 0.06800 * RK( 667 ) * 
     &                                          + 0.06800 * RK( 668 ) * 
      PD( xCL             ,xCL              ) = -           RK( 615 ) * 
     &                                          -           RK( 616 ) * 
     &                                          -           RK( 617 ) * 
     &                                          -           RK( 618 ) * 
     &                                          -           RK( 619 ) * 
     &                                          -           RK( 620 ) * 
     &                                          -           RK( 621 ) * 
     &                                          -           RK( 622 ) * 
     &                                          -           RK( 623 ) * 
     &                                          -           RK( 624 ) * 
      PD( xCL             ,CLCCHO           ) =             RK( 611 )   
      PD( xCL             ,CLACET           ) =             RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 137: xCLCCHO                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xCLCCHO         ,NO               ) = -           RK( 625 ) * 
      PD( xCLCCHO         ,NO3              ) = -           RK( 627 ) * 
      PD( xCLCCHO         ,HO2              ) = -           RK( 626 ) * 
      PD( xCLCCHO         ,MEO2             ) = -           RK( 628 ) * 
      PD( xCLCCHO         ,RO2C             ) = -           RK( 629 ) * 
      PD( xCLCCHO         ,RO2XC            ) = -           RK( 630 ) * 
      PD( xCLCCHO         ,MECO3            ) = -           RK( 631 ) * 
      PD( xCLCCHO         ,RCO3             ) = -           RK( 632 ) * 
      PD( xCLCCHO         ,BZCO3            ) = -           RK( 633 ) * 
      PD( xCLCCHO         ,MACO3            ) = -           RK( 634 ) * 
      PD( xCLCCHO         ,IPRD             ) =   0.14000 * RK( 610 ) * 
      PD( xCLCCHO         ,MACR             ) =   0.18000 * RK( 608 ) * 
      PD( xCLCCHO         ,MVK              ) =   0.94700 * RK( 609 ) * 
      PD( xCLCCHO         ,ACROLEIN         ) =   0.48400 * RK( 607 ) * 
      PD( xCLCCHO         ,PROPENE          ) =   0.30600 * RK( 647 ) * 
      PD( xCLCCHO         ,ISOPRENE         ) =   0.06700 * RK( 649 ) * 
      PD( xCLCCHO         ,APIN             ) =   0.10900 * RK( 650 ) * 
      PD( xCLCCHO         ,OLE1             ) =   0.19200 * RK( 663 ) * 
      PD( xCLCCHO         ,OLE2             ) =   0.01300 * RK( 664 ) * 
      PD( xCLCCHO         ,TERP             ) =   0.10900 * RK( 667 ) * 
      PD( xCLCCHO         ,SESQ             ) =   0.10900 * RK( 668 ) * 
      PD( xCLCCHO         ,CL               ) =   0.48400 * RK( 607 ) * 
     &                                          + 0.18000 * RK( 608 ) * 
     &                                          + 0.94700 * RK( 609 ) * 
     &                                          + 0.14000 * RK( 610 ) * 
     &                                          + 0.30600 * RK( 647 ) * 
     &                                          + 0.06700 * RK( 649 ) * 
     &                                          + 0.10900 * RK( 650 ) * 
     &                                          + 0.19200 * RK( 663 ) * 
     &                                          + 0.01300 * RK( 664 ) * 
     &                                          + 0.10900 * RK( 667 ) * 
     &                                          + 0.10900 * RK( 668 ) * 
      PD( xCLCCHO         ,xCLCCHO          ) = -           RK( 625 ) * 
     &                                          -           RK( 626 ) * 
     &                                          -           RK( 627 ) * 
     &                                          -           RK( 628 ) * 
     &                                          -           RK( 629 ) * 
     &                                          -           RK( 630 ) * 
     &                                          -           RK( 631 ) * 
     &                                          -           RK( 632 ) * 
     &                                          -           RK( 633 ) * 
     &                                          -           RK( 634 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 138: xCLACET                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( xCLACET         ,NO               ) = -           RK( 635 ) * 
      PD( xCLACET         ,NO3              ) = -           RK( 637 ) * 
      PD( xCLACET         ,HO2              ) = -           RK( 636 ) * 
      PD( xCLACET         ,MEO2             ) = -           RK( 638 ) * 
      PD( xCLACET         ,RO2C             ) = -           RK( 639 ) * 
      PD( xCLACET         ,RO2XC            ) = -           RK( 640 ) * 
      PD( xCLACET         ,MECO3            ) = -           RK( 641 ) * 
      PD( xCLACET         ,RCO3             ) = -           RK( 642 ) * 
      PD( xCLACET         ,BZCO3            ) = -           RK( 643 ) * 
      PD( xCLACET         ,MACO3            ) = -           RK( 644 ) * 
      PD( xCLACET         ,IPRD             ) =   0.42000 * RK( 610 ) * 
      PD( xCLACET         ,MACR             ) =   0.54100 * RK( 608 ) * 
      PD( xCLACET         ,PROPENE          ) =   0.54000 * RK( 647 ) * 
      PD( xCLACET         ,OLE1             ) =   0.33700 * RK( 663 ) * 
      PD( xCLACET         ,OLE2             ) =   0.19100 * RK( 664 ) * 
      PD( xCLACET         ,CL               ) =   0.54100 * RK( 608 ) * 
     &                                          + 0.42000 * RK( 610 ) * 
     &                                          + 0.54000 * RK( 647 ) * 
     &                                          + 0.33700 * RK( 663 ) * 
     &                                          + 0.19100 * RK( 664 ) * 
      PD( xCLACET         ,xCLACET          ) = -           RK( 635 ) * 
     &                                          -           RK( 636 ) * 
     &                                          -           RK( 637 ) * 
     &                                          -           RK( 638 ) * 
     &                                          -           RK( 639 ) * 
     &                                          -           RK( 640 ) * 
     &                                          -           RK( 641 ) * 
     &                                          -           RK( 642 ) * 
     &                                          -           RK( 643 ) * 
     &                                          -           RK( 644 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 139: CLCCHO                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLCCHO          ,NO               ) =             RK( 625 ) * 
      PD( CLCCHO          ,NO3              ) =             RK( 627 ) * 
      PD( CLCCHO          ,OH               ) = -           RK( 612 ) * 
      PD( CLCCHO          ,MEO2             ) =   0.50000 * RK( 628 ) * 
      PD( CLCCHO          ,RO2C             ) =   0.50000 * RK( 629 ) * 
      PD( CLCCHO          ,RO2XC            ) =   0.50000 * RK( 630 ) * 
      PD( CLCCHO          ,MECO3            ) =             RK( 631 ) * 
      PD( CLCCHO          ,RCO3             ) =             RK( 632 ) * 
      PD( CLCCHO          ,BZCO3            ) =             RK( 633 ) * 
      PD( CLCCHO          ,MACO3            ) =             RK( 634 ) * 
      PD( CLCCHO          ,CL               ) = -           RK( 613 ) * 
      PD( CLCCHO          ,xCLCCHO          ) =             RK( 625 ) * 
     &                                          +           RK( 627 ) * 
     &                                          + 0.50000 * RK( 628 ) * 
     &                                          + 0.50000 * RK( 629 ) * 
     &                                          + 0.50000 * RK( 630 ) * 
     &                                          +           RK( 631 ) * 
     &                                          +           RK( 632 ) * 
     &                                          +           RK( 633 ) * 
     &                                          +           RK( 634 ) * 
      PD( CLCCHO          ,CLCCHO           ) = -           RK( 611 )   
     &                                          -           RK( 612 ) * 
     &                                          -           RK( 613 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 140: CLACET                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLACET          ,NO               ) =             RK( 635 ) * 
      PD( CLACET          ,NO3              ) =             RK( 637 ) * 
      PD( CLACET          ,MEO2             ) =   0.50000 * RK( 638 ) * 
      PD( CLACET          ,RO2C             ) =   0.50000 * RK( 639 ) * 
      PD( CLACET          ,RO2XC            ) =   0.50000 * RK( 640 ) * 
      PD( CLACET          ,MECO3            ) =             RK( 641 ) * 
      PD( CLACET          ,RCO3             ) =             RK( 642 ) * 
      PD( CLACET          ,BZCO3            ) =             RK( 643 ) * 
      PD( CLACET          ,MACO3            ) =             RK( 644 ) * 
      PD( CLACET          ,xCLACET          ) =             RK( 635 ) * 
     &                                          +           RK( 637 ) * 
     &                                          + 0.50000 * RK( 638 ) * 
     &                                          + 0.50000 * RK( 639 ) * 
     &                                          + 0.50000 * RK( 640 ) * 
     &                                          +           RK( 641 ) * 
     &                                          +           RK( 642 ) * 
     &                                          +           RK( 643 ) * 
     &                                          +           RK( 644 ) * 
      PD( CLACET          ,CLACET           ) = -           RK( 614 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 141: CLCHO                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLCHO           ,ETHENE           ) =             RK( 646 ) * 
      PD( CLCHO           ,CL               ) =             RK( 646 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 142: BNZNRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZNRXN         ,NO               ) =             RK( 669 ) * 
      PD( BNZNRXN         ,BENZRO2          ) =             RK( 669 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 143: BNZHRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZHRXN         ,HO2              ) =             RK( 670 ) * 
      PD( BNZHRXN         ,BENZRO2          ) =             RK( 670 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 144: XYLNRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLNRXN         ,NO               ) =             RK( 671 ) * 
      PD( XYLNRXN         ,XYLRO2           ) =             RK( 671 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 145: XYLHRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLHRXN         ,HO2              ) =             RK( 672 ) * 
      PD( XYLHRXN         ,XYLRO2           ) =             RK( 672 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 146: TOLNRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLNRXN         ,NO               ) =             RK( 673 ) * 
      PD( TOLNRXN         ,TOLRO2           ) =             RK( 673 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 147: TOLHRXN                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLHRXN         ,HO2              ) =             RK( 674 ) * 
      PD( TOLHRXN         ,TOLRO2           ) =             RK( 674 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 148: HCHO_PRIMARY               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCHO_PRIMARY    ,NO3              ) = -           RK( 678 ) * 
      PD( HCHO_PRIMARY    ,OH               ) = -           RK( 677 ) * 
      PD( HCHO_PRIMARY    ,CL               ) = -           RK( 679 ) * 
      PD( HCHO_PRIMARY    ,HCHO_PRIMARY     ) = -           RK( 675 )   
     &                                          -           RK( 676 )   
     &                                          -           RK( 677 ) * 
     &                                          -           RK( 678 ) * 
     &                                          -           RK( 679 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 149: CCHO_PRIMARY               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CCHO_PRIMARY    ,NO3              ) = -           RK( 682 ) * 
      PD( CCHO_PRIMARY    ,OH               ) = -           RK( 680 ) * 
      PD( CCHO_PRIMARY    ,CL               ) = -           RK( 683 ) * 
      PD( CCHO_PRIMARY    ,CCHO_PRIMARY     ) = -           RK( 680 ) * 
     &                                          -           RK( 681 )   
     &                                          -           RK( 682 ) * 
     &                                          -           RK( 683 ) * 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 150: ACROLEIN_PRIMARY           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACROLEIN_PRIMARY,O3P              ) = -           RK( 687 ) * 
      PD( ACROLEIN_PRIMARY,O3               ) = -           RK( 685 ) * 
      PD( ACROLEIN_PRIMARY,NO3              ) = -           RK( 686 ) * 
      PD( ACROLEIN_PRIMARY,OH               ) = -           RK( 684 ) * 
      PD( ACROLEIN_PRIMARY,CL               ) = -           RK( 689 ) * 
      PD( ACROLEIN_PRIMARY,ACROLEIN_PRIMARY ) = -           RK( 684 ) * 
     &                                          -           RK( 685 ) * 
     &                                          -           RK( 686 ) * 
     &                                          -           RK( 687 ) * 
     &                                          -           RK( 688 )   
     &                                          -           RK( 689 ) * 


      RETURN                                                            
      END                                                               
