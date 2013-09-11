
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      SUBROUTINE SA_FIL_OTHRTAG( ISAMEM, JDATE, JTIME ) 

C*************************************************************
Ckrt  Dec 23, 2010  R. Kwok
Ckrt   In each grid cell, subtract total tagged emission, 
Ckrt   ISAMEM(non-othrtag), 
Ckrt   from total emission, VDEMIS.
Ckrt   Then fill up ISAMEM(othrtag) with this difference.
Ckrt
Ckrt  Dec 7, 2011 : get rid of n_spctag, s_spctag, t_spctag, vnam_spctag
Ckrt                use spc_name instead
Ckrt
Ckrt  Jul 12, 2012 : transcribed to cmaq501
C*************************************************************

      USE HGRD_DEFN        ! 20120712 just for diagnosis with single mype
      USE EMIS_DEFN        ! fini_gc, strt_gc, etc, vdemis defined      
      USE SA_DEFN          ! Mc06
      USE CGRID_SPCS       ! 20120712 in place of gc_emis, ae_emis, nr_emis, tr_emis, etc
      USE UTILIO_DEFN      ! 20120712 in place of ioparms, iodecl
      USE SUBST_MODULES    ! stenex
      USE OZVOC_DEFN       ! 20120918  for VOC

      IMPLICIT NONE

Ckrt....Arguments
      
      REAL, POINTER :: ISAMEM( :,:,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE, JTIME

Ckrt....Local variables
      INTEGER  L, R, C, N, V, S
      INTEGER  JSPC, ITAG, JSPCTAG, IBGN
      INTEGER  NVARI
      REAL     KNTAG

Ckrt...Dealing with NOX...
      INTEGER  JNO, JNO2, JHONO

Ckrt...20120918 dealing with VOC
      INTEGER JV
      REAL    VDEMIS_VOC

Ckrt...Error handling
      CHARACTER( 120 ) :: XMSG
      CHARACTER( 16 ) :: PNAME = 'SA_FIL_OTHRTAG  '

C==========================================================


      DO L = 1, EMLAYS
       DO R = 1, MY_NROWS
        DO C = 1, MY_NCOLS
         DO JSPC = 1, NSPC_SA
          KNTAG = 0.0
          DO ITAG = 1, NTAG_SA - 3
           KNTAG = KNTAG + ISAMEM( C,R,L,JSPC,ITAG )
          ENDDO ! itag
!KRT20120925          if ( MYPE .eq. 0 ) then
!KRT20120925            if ( C .eq. 13 .and. R .eq. 10 .and. L .eq. 1 .and. JSPC .eq. 1 )   ! Mc05
!KRT20120925     &      print*,'KNTAG=isamem(13,10,1,1,1)=',KNTAG                          ! Mc05
!KRT20120925          endif ! mype0

Ckrt...Search from Gas emis species list
          NVARI = LAST_GC - FRST_GC + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,GC_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               XMSG = 'Total tagging GC emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
!KRT20120920          ELSEIF ( SPC_NAME(JSPC,OTHRTAG) .EQ. 'O3N' .OR.
!KRT20120920     &            SPC_NAME(JSPC,OTHRTAG) .EQ. 'O3V' ) THEN
!KRT20120920           V = INDEX1( 'O3',NVARI,GC_EMIS )
!KRT20120920           ISAMEM(C,R,L,JSPC,OTHRTAG) =
!KRT20120920     &       VDEMIS( V,L,C,R ) - KNTAG
!KRT20120920           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
!KRT20120920             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
!KRT20120920               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
!KRT20120920             ELSE
!KRT20120920               XMSG = 'Total tagging GC emiss exceeds basic emiss !'
!KRT20120920               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
!KRT20120920             ENDIF ! negative number very small ?
!KRT20120920           ENDIF ! indeed a negative number ?
          ENDIF ! species matched

Ckrt...Search from PM emis species list
          NVARI = LAST_AE - FRST_AE + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,AE_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N + FRST_AE - 1
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
!KRT20120925           if ( MYPE .eq. 0 ) then
!KRT20120925             if ( C .eq. 13 .and. R .eq. 10 .and. L .eq. 1 .and. JSPC .eq. 1 ) then  ! Mc05
!KRT20120925               print*,'SPC_NAME,C,R,L,VDEMIS,KNTAG'
!KRT20120925               print*, SPC_NAME(JSPC,OTHRTAG),C,R,L,VDEMIS(V,L,C,R),KNTAG 
!KRT20120925             endif  ! Mc05
!KRT20120925           endif ! mype0
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               print*,'SPC_NAME,C,R,L,VDEMIS,KNTAG'
               print*, SPC_NAME(JSPC,OTHRTAG),C,R,L,VDEMIS(V,L,C,R),KNTAG 
               XMSG = 'Total tagging AE emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
          ENDIF ! species matched

Ckrt...Search from NR emis species list
          NVARI = LAST_NR - FRST_NR + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,NR_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N + FRST_NR - 1
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0 ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               XMSG = 'Total tagging NR emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
          ENDIF ! species matched

Ckrt....20120918 Now deal with VOC.....
          IF ( SPC_NAME( JSPC, OTHRTAG ) .EQ. 'VOC' ) THEN
            VDEMIS_VOC = 0.0
            NVARI = LAST_GC - FRST_GC + 1
            DO JV = 1, N_EVOC
              N = INDEX1( NAM_VOC(JV), NVARI, GC_EMIS )
              IF ( N .GT. 0 ) THEN
                V = N
                VDEMIS_VOC = VDEMIS_VOC + VDEMIS( V,L,C,R ) * NCARBON( JV )
!KRT20120921    if ( mype .eq. 0 .and. C .eq. 5 .and. R .eq. 5 .and. L .eq. 1 ) then
!KRT20120921      print*,'JV, NAM_VOC, NCARBON:',JV, NAM_VOC(JV), NCARBON(JV)
!KRT20120921    endif ! mype0
              ENDIF ! n > 0 ?
            ENDDO ! jv
            ISAMEM( C,R,L,JSPC,OTHRTAG ) = VDEMIS_VOC - KNTAG
            IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .GT. 1.E-10 ) THEN ! 20120924
              WCH_VOC( :, OTHRTAG ) = 1.0
            ELSEIF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0 ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               XMSG = 'Total tagging VOC emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
            ENDIF ! indeed a negative number ?
          ENDIF  ! spc_name = voc?

         ENDDO ! jspc
        ENDDO ! C
       ENDDO ! R
      ENDDO ! L

      END SUBROUTINE SA_FIL_OTHRTAG
