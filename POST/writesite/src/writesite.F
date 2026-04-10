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

C****************************************************************************************
      PROGRAM WRITESITE
c
C*****************************************************************************************
      USE SITES
      USE SPECIES

      USE M3UTILIO

      IMPLICIT NONE

C external functions
      integer getTZ
      character*(16) int2Str
      character*(16) real2Str
      character*(16) date2Str
      character*(16) time2Str

C local variables
      integer status
      logical lstatus
      logical prtXY
      logical uselocal
      logical prthead
      logical pivot_table
      integer timeShift
      integer layer     
      integer startDate, endDate
      integer startTime, endTime
      integer logdev
      integer cdate, ctime
      integer ldate, ltime, tzoff
      integer runlen
      character*(4096)  header1
      character*(4096)  header2
      character*(4096)  record
      character*(16)   field
      character*(656)  infile
      character*(656)  outfile
      character*(656)  pivot_file
      real, allocatable :: data(:,:,:) 
      integer, allocatable :: tzoffset(:,:) 
      integer lfn
      integer lpfn
      integer i, s, t, idx
      integer column, row
      real x, y, lon, lat


      lfn  = 10
      lpfn = 20

C... start program
      logdev = init3 ()

C... get print headings switch
      prthead = ENVYN('PRTHEAD',"Print heading switch", .TRUE., status)

C... get print XY switch
      prtXY = ENVYN('PRT_XY',"Print Map Projected XY switch", .FALSE., status)

C... get print XY switch
      pivot_table = ENVYN('PIVOT_TABLE',"Create output file that table pivots axes", .FALSE., status)

C... get USELOCAL switch
      uselocal = ENVYN('USELOCAL',"Adjust to local time", .FALSE., status)

C... get for Time shift
      timeShift = ENVINT('TIME_SHIFT','Hours to shift', 0, status)

C... get layer to process
      layer = ENVINT('LAYER','Layer to process', 1, status)

C... open input file
      if( .not. open3('INFILE',fsread3, 'writeSite')) then
        Call m3err('writeSite', 0, 0, 'Could not open INFILE', .TRUE.)
        endif

C... load file description from INFILE
      if( .not. desc3('INFILE')) then
        Call m3err ('writeSite', 0, 0, 'Could not load file description from IN_FILE', .TRUE.)
        endif

C...compute end date and time of input file              
      i = 1
      endDate = SDATE3D
      endTime = STIME3D 
      do t = 2,MXREC3D
         CALL NEXTIME(endDate,endTime,TSTEP3D)
         i = i+ 1
      enddo

C... check for invalid layer
      if( layer.lt.1 .or. layer.gt.NLAYS3D ) then
        Call m3err ('writeSite', 0, 0, 'Invalid Layer number to process', .TRUE.)
        endif

C... check for start and end dates
      startDate = ENVINT('STARTDATE','Starting Date', SDATE3D, status)
      endDate = ENVINT('ENDDATE','Ending Date', endDate, status)

C... check for start and end times
      startTime = ENVINT('START_TIME','Starting Time', STIME3D, status)
      endTime   = ENVINT('END_TIME','Ending Time', endTime, status)

C... set map projection
      Call SETPROJ( GDTYP3D, Real(P_ALP3D),Real(P_BET3D),Real(P_GAM3D),Real(XCENT3D),Real(YCENT3D) )

C... load sites
      Call loadSites()

C... get species definitions from system variables
      Call loadSpecies()
      write(*,'(i5,'' species defined'')') NSPECVAR

C... get name of output file and try to open
      CALL ENVSTR( 'OUTFILE', 'Name of output file', 'out.csv', outfile, status)
      OPEN(unit=lfn, file=outfile, iostat=status)
      IF( status .ne. 0 ) then
        write(*,'(''**ERROR** Cannot open OUTFILE:'',a)') TRIM(outfile)
        Stop
        endif

C... get name of pivot output file and try to open
      IF( pivot_table .And. .Not. ALLCELLS )then
         CALL ENVSTR( 'PIVOT_FILE', 'Name of output file', 'pivot_out.csv', pivot_file, status)
         OPEN(unit=lpfn, file=pivot_file, iostat=status)
         IF( status .ne. 0 ) then
           write(*,'(''**ERROR** Cannot open PIVOT_FILE:'',a)') TRIM(outfile)
           Stop
         endif
      endif

C... create header record
      header1 = 'column,row,longitude,latitude'
      header2 = ',,degrees,degrees'
     
      ! add lambert XY headers
      if(prtXY) then
        header1 = TRIM(header1) // ',Lambert_X,LAMBERT_Y'
        header2 = TRIM(header2) // ',meters,meters'
        endif

      header1 = TRIM(header1) // ',date' 
      header2 = TRIM(header2) // ',YYYY-MM-DD'

      ! add time field if time step < 24 hours
      if( TSTEP3D .lt. 240000 ) then
        header1 = TRIM(header1) // ',Time'
        header2 = TRIM(header2) // ',hh:mm:ss'
        endif

      ! add siteid if using site file
      if( .not.ALLCELLS ) then
        header1 = 'siteid,' // header1
        header2 = ',' // header2
        endif

      do i=1,NSPECVAR
        idx = index1(TRIM(SPECVARS(i)), NVARS3D, VNAME3D)
        if( idx.le.0 ) then
          write(*,'(''**ERROR** Invalid Species Variable:'',a)') TRIM(SPECVARS(i))
          Stop
          endif
        header1 = TRIM(header1) // ',' // SPECVARS(i)
        header2 = TRIM(header2) // ',' // UNITS3D(idx)
        enddo

      if( prthead ) then
        CALL ENVSTR( 'INFILE', 'Name of input file', 'INFILE', infile, status)
        write(lfn,'(''! Data read from file:'',a,'' (layer'',i3,'')'',/)') TRIM(infile),layer

        if( TSTEP3D .lt. 240000 ) then 
          if( USELOCAL ) then
            write(lfn,'(''!   Note: Times have been converted to local standard'')')
          else
            write(lfn,'(''!   Note: All Times are in GMT'')')
            endif

          if( timeShift.ne.0 ) write(lfn,'(9x,''Data shifted'',i2,'' (hours)'')') timeShift

          Write(lfn,'(/)')
          endif


        write(lfn,'(a)') TRIM(header1)
        write(lfn,'(a)') TRIM(header2)
        endif

C... start process loops for sites
      if( .not.ALLCELLS ) then

        ! allocate data array for 1 species
        allocate( data(NCOLS3D, NROWS3D, 1) )


        do s = 1,NSITES
          write(*,'(''0) Processing site:'',a)') TRIM(siteid(s))

          ! set first date and time
          cdate = SDATE3D
          ctime = STIME3D

          ! compute x,y for cell
          x = (siteCol(s)-0.5)*XCELL3D + XORIG3D
          y = (siteRow(s)-0.5)*YCELL3D + YORIG3D

          tzoff = 0
          if( USELOCAL .and. TSTEP3D.lt.240000 ) then
            tzoff = getTZ(longitude(s), latitude(s))
            endif


          do t = 1,MXREC3D
            if( ( cdate.ge.startDate .and. cdate.le.endDate )
     &           .and. ( ctime.ge.startTime .and. ctime.le.endTime ) ) then
              record = TRIM(siteid(s))
              record = TRIM(record) // ',' // int2Str(siteCol(s), '(i5)')
              record = TRIM(record) // ',' // int2Str(siteRow(s), '(i5)')
              record = TRIM(record) // ',' // real2Str(longitude(s), '(f16.5)')
              record = TRIM(record) // ',' // real2Str(latitude(s), '(f16.5)')
              if( prtXY ) then
                record = TRIM(record) // ',' // real2Str(x, '(f16.1)')
                record = TRIM(record) // ',' // real2Str(y, '(f16.1)')
                endif

              ldate = cdate
              ltime = ctime
              Call NEXTIME(ldate, ltime, -tzoff * 10000)
        
              if( TSTEP3D .lt. 240000 ) then
                Call NEXTIME(ldate, ltime, timeShift * 10000)
                endif
    
              record = TRIM(record) // ',' // date2Str(ldate)

              if( TSTEP3D .lt. 240000 ) then
                record = TRIM(record) // ',' // time2Str(ltime)
                endif


              do i = 1, NSPECVAR
                if(.not.READ3( 'INFILE', SPECVARS(i), layer, cdate, ctime, data )) then
                  call M3ERR( 'writesite', cdate, ctime, 'Read Error', .TRUE. )
                  endif

                field = 'm'
                if( siteCol(s).gt.0 .and. siteCol(s).le.NCOLS3D .and.
     &              siteRow(s).gt.0 .and. siteRow(s).le.NROWS3D ) then
                  field = real2Str( data(siteCol(s),siteRow(s),1), '(g16.6)' ) 
                  endif

                record = TRIM(record) // ',' // field
                enddo   ! end species loop

              write(lfn,'(a)') TRIM(record)
              endif  ! time window 

            call NEXTIME( cdate, ctime, TSTEP3D )
            enddo   ! end time record loop
          enddo   ! end site loop 
      endif


C... start process loops for all cells
      if( ALLCELLS ) then

        ! allocate data array for NSPECVAR species
        allocate( data(NSPECVAR, NCOLS3D, NROWS3D) )

        ! allocate data array for timezone offsets 
        allocate( tzoffset(NCOLS3D, NROWS3D) )
        tzoffset = 0
        if( USELOCAL .and. TSTEP3D.lt.240000 ) then
          write(*,'(/'' Computing timezone offset for each cell''/)')

          do column = 1,NCOLS3D
            do row = 1,NROWS3D
              ! compute lon and lat for cell
              x = (column-0.5)*XCELL3D + XORIG3D
              y = (row-0.5)*YCELL3D + YORIG3D

              Call ToLL( GDTYP3D, x, y, lon, lat )
    
              tzoffset(column,row) = getTZ(lon,lat)
              enddo
            enddo
          endif   ! loop for computing tz offsets

        ! set first date and time
        cdate = SDATE3D
        ctime = STIME3D

        do t = 1,MXREC3D

          if( cdate.ge.startDate .and. cdate.le.endDate 
     &           .and. ( ctime.ge.startTime .and. ctime.le.endTime ) ) then
            write(*,'(''processing data for '',2i8)') cdate,ctime

            ! read data for record
            do i = 1, NSPECVAR
              if(.not.READ3( 'INFILE', SPECVARS(i), layer, cdate, ctime, data(i,:,:) )) then
                call M3ERR( 'writesite', cdate, ctime, 'Read Error', .TRUE. )
                endif
              enddo

            ! print record for each cell
            do column = 1,NCOLS3D
              do row = 1,NROWS3D

                ! compute lon and lat for cell
                x = (column-0.5)*XCELL3D + XORIG3D
                y = (row-0.5)*YCELL3D + YORIG3D

                Call ToLL( GDTYP3D, x, y, lon, lat )

                ! build output record
                record = int2Str(column, '(i5)')
                record = TRIM(record) // ',' // int2Str(row, '(i5)')
                record = TRIM(record) // ',' // real2Str(lon, '(f16.5)')
                record = TRIM(record) // ',' // real2Str(lat, '(f16.5)')
                if( prtXY ) then
                  record = TRIM(record) // ',' // real2Str(x, '(f16.1)')
                  record = TRIM(record) // ',' // real2Str(y, '(f16.1)')
                  endif

                ! adjust time
                ldate = cdate
                ltime = ctime
                Call NEXTIME(ldate, ltime, -tzoffset(column,row) * 10000)
                call NEXTIME(ldate, ltime, timeShift*10000)
                record = TRIM(record) // ',' // date2Str(ldate)

                if( TSTEP3D .lt. 240000 ) then
                  record = TRIM(record) // ',' // time2Str(ltime)
                  endif

                do i = 1, NSPECVAR
                  field = real2Str( data(i,column,row), '(g16.6)' ) 
                  record = TRIM(record) // ',' // field
                  enddo  

                write(lfn,'(a)') TRIM(record)
                enddo   ! end row loop 
              enddo   ! end column loop 
            endif   ! time window
          call NEXTIME( cdate, ctime, TSTEP3D ) 
          enddo   ! end time record loop

        close(lfn)
        endif  ! all cells condition

      ! add siteid if using site file
      if( pivot_table .And. .not.ALLCELLS ) then


! pivot table so all times of variables on single line per site
      if( prthead ) then
        CALL ENVSTR( 'INFILE', 'Name of input file', 'INFILE', infile, status)
        write(lpfn,'(''! Data read from file:'',a,'' (layer'',i3,'')'')') TRIM(infile),layer

        if( TSTEP3D .lt. 240000 ) then 
          if( USELOCAL ) then
            write(lpfn,'(''!   Note: Times have been converted to local standard'')')
          else
            write(lpfn,'(''!   Note: All Times are in GMT'')')
            endif
       endif

C... create header record
      header1 = ""
      header2 = ""
      write(header1,'(A)')
     &  "!       Variable"
      write(header2,'(A)')
     & "!       Variable"
     
        cdate = SDATE3D
        ctime = STIME3D
        do t = 1,MXREC3D
         if( cdate.ge.startDate .and. cdate.le.endDate 
     &       .and. ( ctime.ge.startTime .and. ctime.le.endTime ) ) then

            write(header1,'(A,7X,84(A16))')TRIM(header1),date2Str(cdate)

            ! add time field if time step < 24 hours
            if( TSTEP3D .lt. 240000 ) then
              write(header2,'(A,6X,84(3X,A16))')TRIM(header2),time2Str(ctime)
            else
              write(header2,'(A,6X,84(3X,A16))')TRIM(header2),"             "
            endif
            endif
         call NEXTIME( cdate, ctime, TSTEP3D )
       end do

       write(header1,'(A,6X,A)')TRIM(header1),
     & "       units           siteid  col           row              longitude         latitude "
       write(header2,'(A,6X,A)')TRIM(header2),
     & "       units           siteid  col           row                degrees          degrees  "
       write(lpfn,'(a)') TRIM(header1)
       write(lpfn,'(a)') TRIM(header2)

C... create header record
      header1 = ""
      header2 = ""
      write(header1,'(A16,1X)')"JDAY"
      write(header2,'(A16,1X)')"JTIME"

        cdate = SDATE3D
        ctime = STIME3D
        do t = 1,MXREC3D
         if( cdate.ge.startDate .and. cdate.le.endDate
     &       .and. ( ctime.ge.startTime .and. ctime.le.endTime ) ) then
            write(header1,'(A,1X,F16.6)')TRIM(header1),DecimalDate(cdate,0)
            write(header2,'(A,1X,F16.6)')TRIM(header2),Time2Secs(ctime)

         endif
         call NEXTIME( cdate, ctime, TSTEP3D )
       end do
       write(header1,'(A,1X,A16)')TRIM(header1),"!YYYY.fraction"
       write(header2,'(A,1X,A16)')TRIM(header2),"!secs"
       write(lpfn,'(a)') TRIM(header1)
       write(lpfn,'(a)') TRIM(header2)

        cdate = SDATE3D
        ctime = STIME3D
        do s = 1,NSITES
          write(*,'(''2) Processing site:'',a)') TRIM(siteid(s))

          ! reset first date and time for new site
          cdate = SDATE3D
          ctime = STIME3D

          ! compute x,y for cell
          x = (siteCol(s)-0.5)*XCELL3D + XORIG3D
          y = (siteRow(s)-0.5)*YCELL3D + YORIG3D

          tzoff = 0

         do i = 1, NSPECVAR
              ! reset first date and time for new variable
              cdate = SDATE3D
              ctime = STIME3D
              record = ""
              write(record,'(84(A16,1X))')
     &        TRIM(SPECVARS(i)) 


            do t = 1,MXREC3D
            
               if( cdate.ge.startDate .and. cdate.le.endDate 
     &           .and. ( ctime.ge.startTime .and. ctime.le.endTime ) ) then

                if(.not.READ3( 'INFILE', SPECVARS(i), layer, cdate, ctime, data )) then
                  call M3ERR( 'writesite', cdate, ctime, 'Read Error', .TRUE. )
                endif

                field = 'm'
                if( siteCol(s).gt.0 .and. siteCol(s).le.NCOLS3D .and.
     &              siteRow(s).gt.0 .and. siteRow(s).le.NROWS3D ) then
                  field = real2Str( data(siteCol(s),siteRow(s),1), '(ES16.6)' ) 
                end if

                write(record,'(A,1X,84(A16,1X))')TRIM(record),TRIM(field)
                if( t .ge. MXREC3D )then
                   write(record,'(A,1X,84(A16,1X))')TRIM(record),
     &             "!" // TRIM(UNITS3D(i)),
     &             TRIM(siteid(s)),
     &             int2Str(siteCol(s), '(i16)'),
     &             int2Str(siteRow(s), '(i16)'),
     &             real2Str(longitude(s), '(f16.5)'),
     &             real2Str(latitude(s), '(f16.5)')
                end if
              end if  ! time window 

              call NEXTIME( cdate, ctime, TSTEP3D )
            enddo   ! end time record loop
            write(lpfn,'(a)') TRIM(record)
           enddo   ! end species loop
          enddo   ! end site loop 
         end if
 
         close(lpfn)
        endif  ! pivot_table and sites condition

        close(lfn)
        lstatus = SHUT3 ()
        stop

      end PROGRAM WRITESITE
C****************************************************************************
C  routine to set map projection
C****************************************************************************
      Subroutine SetProj(gdtype, alpha, beta, gamma, xcent, ycent) 

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real alpha, beta, gamma, xcent, ycent

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if( .NOT. SETLAM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('writeSite', 0, 0, 'Lambert projection setup error', xstat2)
          endif
        return
        endif

      !  check for polar stereographic projection
      if( gdtype .eq. 6 ) then
        if( .NOT. SETPOL( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('writeSite', 0, 0, 'Polar stereographic projection setup error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if( .NOT. SETEQM( alpha, beta, gamma, xcent, ycent) ) then
          Call m3exit ('writeSite', 0, 0, 'Equatorial mercator projection setup error', xstat2)
          endif
        return
        endif

      Call m3exit ('writeSite', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine SetProj


C****************************************************************************
C  routine to compute map projection from LAT/LON
C****************************************************************************
      Subroutine ToProj(gdtype, longitude, latitude, x, y) 

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        x = longitude
        y = latitude
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LL2LAM(longitude, latitude, x, y) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polar stereographic projection
      if( gdtype .eq. 6 ) then
        if(.NOT.LL2POL(longitude, latitude, x, y) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to polar stereographic error', xstat2)
          endif
        return
        endif

      !  check for polar stereographic projection
      if( gdtype .eq. 7 ) then
        if(.NOT.LL2EQM(longitude, latitude, x, y) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('writeSite', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine ToProj


C****************************************************************************
C  routine to compute LAT/LON from map projection
C****************************************************************************
      Subroutine ToLL(gdtype, x, y, longitude, latitude)

      USE M3UTILIO

      IMPLICIT NONE

      ! arguments
      Integer gdtype
      Real longitude, latitude, x, y

      !  check for LAT/LON projection
      if( gdtype .eq. 1 ) then
        longitude = x
        latitude = y
        return
        endif

      !  check for lambert projection
      if( gdtype .eq. 2 ) then
        if(.NOT.LAM2LL(x, y, longitude, latitude) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to Lambert error', xstat2)
          endif
        return
        endif

      !  check for polar stereographic projection
      if( gdtype .eq. 6 ) then
        if(.NOT.POL2LL(x, y, longitude, latitude) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to polar stereographic error', xstat2)
          endif
        return
        endif

      !  check for equatorial mercator projection
      if( gdtype .eq. 7 ) then
        if(.NOT.EQM2LL(x, y, longitude, latitude) ) then
          Call m3exit('writesite', 0, 0, 'Lat/Lon to equatorial mercator error', xstat2)
          endif
        return
        endif

      Call m3exit ('writeSite', 0, 0, 'Unsupported map projection', xstat2)

      end Subroutine ToLL   



C****************************************************************************
C  routine to convert integer to string
C****************************************************************************
      Character*16 Function int2Str( value, fmt ) result(intStr)

      IMPLICIT NONE

      ! argument
      Integer value
      Character*(*) fmt

      Write(intStr,fmt) value
      Call LeftTrim(intStr)
      return
      End Function int2Str   


C****************************************************************************
C  routine to convert real to string
C****************************************************************************
      Character*16 Function real2Str( value, fmt ) result(realStr)

      IMPLICIT NONE

      ! argument
      Real value
      Character*(*) fmt

      Write(realStr,fmt) value
      Call LeftTrim(realStr)
      return
      End Function real2Str    
 

C****************************************************************************
C  routine to convert date and time to string as "yyyy-mm-dd"
C****************************************************************************
      Character*16 Function date2Str( date ) result(dateStr)
 
      Implicit None

      Integer date
 
C..  local variables
      Integer month, day, year
 
      call DayMon( date, month, day )
      year = date/1000
 
      write(dateStr,'(i4.4,''-'',i2.2,''-'',i2.2)') year, month, day

      return
      End Function date2Str


C****************************************************************************
C  routine to convert time to string as "HH:MM:SS"         
C****************************************************************************
      Character*16 Function time2Str( time ) result(timeStr)
                                                              
      Implicit None

      Integer time                                      
                                                              
C..  local variables                                          
      Integer hour, minutes, secs
                                                              
      hour = time/10000                                       
      minutes = (time - 10000*hour)/100                       
      secs = mod(time,100)                       
                                                              
      write(timeStr,'(i2.2,'':'',i2.2,'':'',i2.2)')   
     &      hour,minutes,secs
      return                                                  
      End Function time2Str                                   
