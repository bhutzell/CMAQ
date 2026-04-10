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

C***********************************************************************
C
C  MODULE:  site ids and location        
C             
C***********************************************************************
      MODULE SITES     

      Logical ALLCELLS
      Integer nsites
      Character*(10) siteid(2000)
      Real   longitude(2000)
      Real   latitude(2000)
      Integer siteCol(2000)
      Integer siteRow(2000)

      contains

C  subroutine to open site file and load arrays     
      Subroutine loadSites()

      USE M3UTILIO

      IMPLICIT NONE

      ! functions
      Integer getNumberOfFields

      ! local variables
      Integer status
      Integer lfn
      Integer nflds
      Character*(512) sitefile
      Character*(512) record   
      Character*(16) field    
      Character*(1)  delimiter
      Logical        useColRow
      Real value
      Real x, y 

      lfn = 20
      ALLCELLS = .false.

      CALL ENVSTR( 'SITE_FILE', 'Name of site definition file', 'ALL', sitefile, status)
      if( sitefile.eq.'ALL') then
        ALLCELLS = .true.
        return
        endif

      CALL ENVSTR( 'DELIMITER', 'Delimiter character in site file', char(9), delimiter, status)

      useColRow = ENVYN( 'USECOLROW', 'Column/row in site file', .false., status)

      OPEN(unit=lfn, file=sitefile, status='old', iostat=status)
      IF( status .ne. 0 ) then
        write(*,'(''**ERROR** Cannot open SITE_FILE:'',a)') TRIM(sitefile)
        Stop
        endif

      ! read until site record found
      do
        read(lfn,'(a)',iostat=status) record
        if(status.ne.0) then
          write(*,'(''**ERROR** Cannot find site records in SITE_FILE:'',a)') TRIM(sitefile) 
          Stop 
          endif 

        nflds = getNumberOfFields( record, delimiter ) 
        if( nflds.lt.3 ) CYCLE

        Call getField( record, delimiter, 2, field ) 
        read(field,'(f16.0)',iostat=status) value
        if( status.ne.0 ) CYCLE
    
        Call getField( record, delimiter, 3, field ) 
        read(field,'(f16.0)',iostat=status) value
        if( status.ne.0 ) CYCLE

        !! site record found, backspace and exit loop   
        Backspace(lfn)
        EXIT 

        enddo

      ! read and load sites
      nsites = 0
      do 
        read(lfn,'(a)',iostat=status) record                                    
        if(status.ne.0) EXIT 

        nflds = getNumberOfFields( record, delimiter ) 
        if( nflds.lt.3 ) EXIT

        nsites = nsites + 1
        if( nsites.gt.SIZE(siteid) ) then
          write(*,'(''**ERROR** The number of sites exceed maximum'')')
          Stop                                                                  
          endif

        Call getField( record, delimiter, 1, siteid(nsites) ) 

        if( useColRow ) then    ! read col/row and compute lon/lat

          Call getField( record, delimiter, 2, field ) 
          read(field,'(i16)') siteCol(nsites)

          Call getField( record, delimiter, 3, field ) 
          read(field,'(i16)') siteRow(nsites)

          ! compute longitude and latitude for site
          x = XORIG3D + (siteCol(nsites)-0.5) * XCELL3D
          y = YORIG3D + (siteRow(nsites)-0.5) * YCELL3D

          Call toLL( GDTYP3D, x, y, longitude(nsites), latitude(nsites) )

        else   ! read lon/lat and compute col/row

          Call getField( record, delimiter, 2, field ) 
          read(field,'(f16.0)') longitude(nsites)
 
          Call getField( record, delimiter, 3, field ) 
          read(field,'(f16.0)') latitude(nsites)

          ! compute row and column for site
          Call toProj( GDTYP3D, longitude(nsites), latitude(nsites), x, y )

          siteCol(nsites) = INT( (x-XORIG3D)/XCELL3D ) + 1
          siteRow(nsites) = INT( (y-YORIG3D)/YCELL3D ) + 1
          endif

        enddo

      write(*,'(i5,'' sites loaded'')') nsites
      close(unit=lfn)

      return
      end Subroutine loadSites
! ------------------------------------------------------------------------------
          real function DecimalDate(julian_date, time)

            Implicit None
! convert date and time which uses yyyyddd and  hhmmss format, to yyyy.fraction

            integer, intent(in) :: julian_date ! in yyyyddd format
            integer, intent(in) :: time        ! time of day in hhmmss format
            integer :: neg_time
            integer :: time2sec, hr, mins, sec
            real    :: year, day
            real    :: time2FracDay
            real    :: SecondsInYear

            Real, Parameter :: Day2Secs    = 8.64e+4
            Real, Parameter :: Sec2FracDay = 1.0 / Day2Secs
            
            year = real( int( julian_date/1000 ) )
            day  = real( mod(julian_date,1000) )

            if (time .gt. 0) then
               if ( time .gt. 240000 ) then
                 write(6,'("Function DecimalDate input TIME = ",I8)')time
                 write(6,'("Time of day greater then 24 hours not accepted input value")')
                 STOP
               end if                    
               hr   = int( time/10000 )
               mins = max( int(time/100)-100*hr,0 )
               sec  = mod(time, 100)
               time2FracDay = Sec2FracDay*real(hr*3600 + mins*60 + sec)
               DecimalDate = year + ( day+time2FracDay )*yr2day( int( year ) )
            else if ( time .lt. 0 ) then
               write(6,'("Function DecimalDate input TIME = ",I8)')time
               write(6,'("Negative time of day not accepted input value")')
               STOP
            else
               time2FracDay = 0.0
               DecimalDate  = year + day*yr2day( int( year ) )
            end if
            
          end function DecimalDate
          real function Time2Secs(time)
            Implicit None
            
            integer, intent(in) :: time        ! in hhmmss format
            
            Real, Parameter :: Day2Secs = 8.64e+4
            
            integer :: hr, mins, sec

            if ( abs(time) .gt. 0 ) then
               hr   = int( time/10000 )
               mins = max( int(time/100)-100*hr,0 )
               sec  = mod(time, 100)
               Time2Secs = real(hr*3600 + mins*60 + sec)
            else
               Time2Secs = 0.0 
            end if
                 
          end function Time2Secs
! ------------------------------------------------------------------------------
          real function yr2day (year)

! compute the reciprocal of number of days in a given year
! TO DO: add optional argument for climatological 365-day calendar
            integer, intent(in) :: year

            if (leap_year(year)) then
               yr2day = 1.0/366.0
            else
               yr2day = 1.0/365.0
            end if

          end function yr2day
! -----------------------------------------------------------
          logical function leap_year (year)

! determine whether a given year is a leap year or not

            integer, intent(in) :: year

            if (mod(year, 4) .ne. 0) then
               leap_year = .false.
            else if (mod(year, 400) .eq. 0) then
               leap_year = .true.
            else if (mod(year, 100) .eq. 0) then
               leap_year = .false.
            else
               leap_year = .true.
            endif
 
          end function leap_year
      END MODULE SITES     
