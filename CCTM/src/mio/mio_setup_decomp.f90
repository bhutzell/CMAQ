!  Purpose: To construct a spatial decomposition map based on a given
!           processor configuration for all netCDF formats supported.
!           For CMAQ IOAPI3 type file, this will set up for both 
!           cross and dot grids. For dot grid, it follows subhfile 
!           setting.

      subroutine mio_setup_decomp (nprocs, npcol, nprow, ncols, nrows, &
                                   op_type, ncols_pe, nrows_pe,        &
                                   colde_pe, rowde_pe)

        use mio_global_data_module, only : mio_mype,              &
                                           mio_mype_p1,           &
                                           mio_npcol,             &
                                           mio_nprocs,            &
                                           mio_mpas_dmap_file,    &
                                           mio_logdev,            &
                                           mio_mpas_dmap
        use mio_get_env_module
        use mio_parameter_module, only : mio_ioapi3_format,       &
                                         mio_wrf_format,          &
                                         mio_mpas_format,         &
                                         mio_epa_format

        implicit  none

        integer, intent(in)  :: nprocs           ! number of processors
        integer, intent(in)  :: npcol            ! number of processors arcross grid cols
        integer, intent(in)  :: nprow            ! number of processors arcross grid rows
        integer, intent(in)  :: ncols            ! total number of columns in grid
        integer, intent(in)  :: nrows            ! total number of rows in grid
        integer, intent(in)  :: op_type          ! CMAQ grid = 1 or WRF grid = 2
        integer, intent(out) :: ncols_pe(:,:)    ! number of columns for each PE
        integer, intent(out) :: nrows_pe(:,:)    ! number of columns for each PE
        integer, intent(out) :: colde_pe(:,:,:)  ! column index range for each PE
        integer, intent(out) :: rowde_pe(:,:,:)  ! column index range for each PE

        integer, parameter :: iunit = 11

        integer :: i, j           ! loop counter
        integer :: t1, t2, pe     ! temporary variable
        integer :: quotient
        integer :: remainder
        integer :: ncolx, nrowx, ndx
        integer :: stat
        integer, allocatable       :: adjustment(:)
        integer, allocatable, save :: count(:)

        integer, allocatable :: ncols_we(:)
        integer, allocatable :: nrows_sn(:)

        if ((op_type == mio_ioapi3_format) .or.  &
            (op_type == mio_epa_format)) then       ! both are CMAQ grid

           allocate ( ncols_we( npcol ), nrows_sn( nprow ), stat=stat )

           ncolx = ncols / npcol
           nrowx = nrows / nprow

           do i = 1 , npcol
              ncols_we(i) = ncolx
           end do

           do i = 1 , nprow
              nrows_sn(i) = nrowx
           end do

           do i = 1, ncols - npcol*ncolx
              ncols_we(i) = ncols_we(i) + 1
           end do

           do i = 1, nrows - nprow*nrowx
              nrows_sn(i) = nrows_sn(i) + 1
           end do

           do i = 1, nprocs

              ndx = mod (i, npcol)
              if ( ndx .eq. 0 ) ndx = npcol

              ncols_pe(i, 1) = ncols_we(ndx)

              if ( ndx .eq. 1 ) then
                 colde_pe(1, i, 1) = 1
                 colde_pe(2, i, 1) = ncols_pe(i, 1)
              else
                 colde_pe(1, i, 1) = colde_pe(2, i-1, 1) + 1
                 colde_pe(2, i, 1) = colde_pe(2, i-1, 1) + ncols_pe(i, 1)
              end if

              ndx = (i - 1) / npcol + 1

              nrows_pe(i,1) = nrows_sn(ndx)

              if ( i .le. npcol ) then
                 rowde_pe(1, i, 1) = 1
                 rowde_pe(2, i, 1) = nrows_pe(i,1)
              else
                 rowde_pe(1, i, 1) = rowde_pe(2, i-npcol, 1) + 1
                 rowde_pe(2, i, 1) = rowde_pe(2, i-npcol, 1) + nrows_pe(i,1)
              end if

           end do

           deallocate ( ncols_we, nrows_sn)

! setup corresponding dot file map 

           do i = 1, nprocs
              colde_pe(1, i, 2) = colde_pe(1, i, 1)
              colde_pe(2, i, 2) = colde_pe(2, i, 1) + 1
              rowde_pe(1, i, 2) = rowde_pe(1, i, 1)
              rowde_pe(2, i, 2) = rowde_pe(2, i, 1) + 1
           end do

           ncols_pe(:, 2) = colde_pe(2, :, 2) - colde_pe(1, :, 2) + 1
           nrows_pe(:, 2) = rowde_pe(2, :, 2) - rowde_pe(1, :, 2) + 1

!           write( mio_logdev, '(A8,2x,i3)' ) ' MYPE = ', mio_mype
!           write( mio_logdev, '(A)' ) ' PE  NCOLS  Col_Range  NROWS Row_Range  NCOLSD  Col_RangeD NROWSD  Row_RangeD'
!           do i = 1, nprocs
!              write( mio_logdev, '(i3, 4(3x, i3, 2x, i3, ":", 2x, i3))') i-1, &
!                     ncols_pe(i,1), colde_pe(1,i,1), colde_pe(2,i,1), &
!                     nrows_pe(i,1), rowde_pe(1,i,1), rowde_pe(2,i,1), &
!                     ncols_pe(i,2), colde_pe(1,i,2), colde_pe(2,i,2), &
!                     nrows_pe(i,2), rowde_pe(1,i,2), colde_pe(2,i,2)
!           end do
!           write( mio_logdev, * )

        else if (op_type == mio_wrf_format) then  ! WRF grid
! for the column dimension
           quotient = ncols / npcol
           remainder = mod(ncols, npcol)
           allocate (adjustment(npcol), stat=stat)
           adjustment = 0
           t1 = remainder / 2
           adjustment(1:t1) = 1
           t2 = remainder - t1
           adjustment(npcol-t2+1:npcol) = 1

           t1 = 1
           do i = 1, npcol
              ncols_pe(i,1)   = quotient + adjustment(i)
              colde_pe(1,i,1) = t1
              colde_pe(2,i,1) = colde_pe(1,i,1) + ncols_pe(i,1) - 1
! propogate vertically
              pe = i
              do j = 2, nprow
                 pe = pe + npcol
                 ncols_pe(pe,1) = ncols_pe(i,1)
                 colde_pe(1:2,pe,1) = colde_pe(1:2,i,1)
              end do
              t1 = colde_pe(2,i,1) + 1
           end do
           deallocate (adjustment)

! for the row dimension
           quotient = nrows / nprow
           remainder = mod(nrows, nprow)
           allocate (adjustment(nprow), stat=stat)
           adjustment = 0
           t1 = remainder / 2
           adjustment(1:t1) = 1
           t2 = remainder - t1
           adjustment(nprow-t2+1:nprow) = 1

           pe = 0
           t1 = 1
           do j = 1, nprow
              pe = pe + 1
              nrows_pe(pe,1) = quotient + adjustment(j)
              rowde_pe (1,pe,1) = t1
              rowde_pe (2,pe,1) = rowde_pe (1,pe,1) + nrows_pe(pe,1) - 1
              t2 = pe
! propogate horizontally
              do i = 2, npcol
                 pe = pe + 1
                 nrows_pe(pe,1) = nrows_pe(t2,1)
                 rowde_pe (1:2,pe,1) = rowde_pe (1:2,t2,1)
              end do
              t1 = rowde_pe (2,t2,1) + 1
           end do
           deallocate (adjustment)

           ! for WRF stag grid, TBD
           colde_pe(:,:,2) = -1
           rowde_pe(:,:,2) = -1
           ncols_pe(:,2) = -1
           nrows_pe(:,2) = -1

        else if (op_type == mio_mpas_format) then  ! MPAS grid

           call mio_get_env (mio_mpas_dmap_file, 'mpas_dmap_file', ' ')

           ncols_pe = 0
           nrows_pe = 0

           if (mio_mpas_dmap_file .ne. ' ') then

              ! compute number of mesh points in a process where 51 is
              ! to account for uneven distribution of mesh points
              t1 = (ncols - 1) / (npcol * nprow) + 51
              if (.not. allocated (mio_mpas_dmap)) then
                 allocate (mio_mpas_dmap(0:t1, 0:nprocs-1),   &
                           count(0:nprocs-1),                 &
                           stat=stat)
                 open (unit = iunit, file=mio_mpas_dmap_file, status='old')
                 count = 0

                 do i = 1, ncols
                    read (iunit, *) t2
                    count(t2) = count(t2) + 1
                    mio_mpas_dmap(count(t2), t2) = i
                 end do
                 mio_mpas_dmap(0,:) = count
                 close (iunit)
              end if
              ncols_pe( 1,mio_mype_p1 ) = count(mio_mype)
              nrows_pe( 1,mio_mype_p1 ) = 1
           else if ((npcol == 1) .and. (nprow == 1)) then
              if (.not. allocated (mio_mpas_dmap)) then
                 allocate (mio_mpas_dmap(0:1, 0:ncols),     &
                           count(0:1),                      &
                           stat=stat)
                 do i = 1, ncols
                    mio_mpas_dmap(i, 0) = i
                 end do
              end if
           else
              write (mio_logdev, *) ' Warning: ncols_pe and nrows_pe are set to 0 in subroutine'
              write (mio_logdev, *) '          mio_setup_decomp due to missing mpas_dmap_file'
           end if

        else
           write (mio_logdev, *) ' Abort in subroutine mio_setup_decomp routine'
           write (mio_logdev, *) '       due to invalid grid type '
           stop
        end if

      end subroutine mio_setup_decomp
