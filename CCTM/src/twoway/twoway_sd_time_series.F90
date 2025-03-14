!===============================================================================
! Purpose:  To capture the variation of CGRID in a pre-defined sub domain
!
! Revised:  May 2010  Original version.  David Wong
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           01 Aug 2019  (David Wong)
!              -- removed interface block for get_envlist
!===============================================================================

module sd_time_series_module

  implicit none

  integer :: n_sd_spcs, sd_scol, sd_ecol, sd_srow, sd_erow
  character (len = 16), allocatable :: sd_spcs(:)
  integer, allocatable :: sd_spcs_index(:)
  real, allocatable :: sd_ts_data(:,:,:,:)

  contains

! --------------------------------------------------------------------------------
  subroutine sd_time_series_init (in_logdev, tstep)

    use hgrd_defn, only   : mype
    use get_env_module
    use logdev_mod, only  : m3exit
    use mio_ascii
    use mio_module
    use runtime_vars, only: CONC_BLEV, CONC_ELEV
    use std_conc, only    : C_NLAYS, N_CSPCS 
    use utilio_defn
 
    include SUBST_FILES_ID    ! Filenames

    integer, intent(in) :: in_logdev, tstep

    character (len = 16), parameter :: pname = 'sd_time_series_i'
    character ( 200 ) xmsg
    integer :: stat, n, i, j, sd_ncols, sd_nrows
    logical :: found = .false.

    allocate (sd_spcs(N_CSPCS), stat=stat)

    call get_envlist ('SD_CONC_SPCS', n_sd_spcs, sd_spcs)

    allocate (sd_spcs_index(n_sd_spcs), stat=stat)

! search for CTM_CONC_1 in mio_txt_file_data
    i = 0
    do while (i .lt. n_mio_out .and. .not. found)
      i = i+1
      if ( mio_txt_file_data(i)%file_logical == ctm_conc_1 ) found = .true.       
    end do
    if ( i .le. 0 ) then
       xmsg = 'Could not find CTM_CONC_1 in mio_txt_file_data'
       call m3exit(pname, 0, 0, xmsg, 1)
    end if

    do n = 1, n_sd_spcs
       j = index1 (sd_spcs(n), mio_txt_file_data(i)%NVARS, mio_txt_file_data(i)%VARNAMES)
       if ( j .le. 0 ) then
          xmsg = 'Could not find requested species ' // sd_spcs(n) // &
                 ' on file ' // mio_txt_file_data(i)%file_logical
          call m3exit(pname, 0, 0, xmsg, 1)
       end if
       sd_spcs_index(n) = j
       VTYPE3D(n)       = M3REAL
       VNAME3D(n)(1:16) = mio_txt_file_data(i)%VARNAMES(j)
       UNITS3D(n)(1:16) = mio_txt_file_data(i)%VARUNITS(j)
       VDESC3D(n)       = mio_txt_file_data(i)%VARDESC(j)
    end do

    nvars3d = n_sd_spcs
    sd_ncols = sd_ecol - sd_scol + 1
    sd_nrows = sd_erow - sd_srow + 1

    ! Store MIO Metadata
    NDIMS3D( 1:NVARS3D ) = 4
    L_TSTEP( 1:NVARS3D ) = .True.
    L_LAY  (1 :NVARS3D ) = .True. ! even if 2D
    L_COL  (1 :NVARS3D ) = .True. 
    L_ROW  (1 :NVARS3D ) = .True. 
    L_VEXT (1 :NVARS3D ) = .False.

    CALL LOAD_MIO_FILE ( CTM_SD_TS, C_NLAYS, CONC_BLEV, CONC_ELEV,     &
           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D), &
           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D), &
           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D),   L_ROW(1:NVARS3D),   &
           L_VEXT(1:NVARS3D) )

    allocate (sd_ts_data(sd_ncols, sd_nrows, c_nlays, n_sd_spcs), stat=stat)

  end subroutine sd_time_series_init

! --------------------------------------------------------------------------------
  subroutine sd_ts_data_ext (cgrid, sd_ts_data, send_to, send_index,        &
                             recv_from, recv_index, n_recv, loc_n_sd_spcs,  &  
                             var_index, jtime, mype)

  include 'mpif.h'

  real, pointer, intent(in)  :: cgrid(:,:,:,:)
  real, intent(out) :: sd_ts_data(:,:,:,:)
  integer, intent(in) :: send_to, send_index(:,:), recv_from(:),     &
                         recv_index(:,:,:), n_recv, loc_n_sd_spcs,   &
                         var_index(:), jtime, mype

  real, allocatable, save :: sdata(:,:,:,:)
  integer :: stat, n, data_size, tag, status(MPI_STATUS_SIZE), s_index, e_index

  if (.not. allocated(sdata)) then
     allocate(sdata(send_index(2,1)-send_index(1,1)+1,   &
                    send_index(2,2)-send_index(1,2)+1,   &
                    size(sd_ts_data,3), loc_n_sd_spcs),      &
              stat=stat)
  end if

  if (send_to >= 0) then
     do n = 1, loc_n_sd_spcs
        sdata(:,:,:,n) = cgrid(send_index(1,1):send_index(2,1), send_index(1,2):send_index(2,2),:,var_index(n))
     end do
  end if

  if (mype .eq. 0) then
 
     if (send_to >= 0) then
        s_index = 2
        sd_ts_data(recv_index(1,1,1):recv_index(2,1,1), recv_index(1,2,1):recv_index(2,2,1),:,:) = sdata
     else
        s_index = 1
     end if
     e_index = n_recv

     do n = s_index, e_index
        tag = jtime * 1000 + recv_from(n)
        data_size = (recv_index(2,1,n) - recv_index(1,1,n) + 1) * &
                    (recv_index(2,2,n) - recv_index(1,2,n) + 1) * &
                    size(sd_ts_data,3) * loc_n_sd_spcs


        call mpi_recv(sd_ts_data(recv_index(1,1,n):recv_index(2,1,n),      &
                                 recv_index(1,2,n):recv_index(2,2,n),:,:), &
                      data_size, mpi_real, recv_from(n), tag,              &
                      mpi_comm_world, status, stat)
     end do
  else
     if (send_to >= 0) then
        data_size = size(sdata)
        tag = jtime * 1000 + mype
        call mpi_send (sdata, data_size, mpi_real, send_to, tag, mpi_comm_world, stat)
     end if
  end if

  end subroutine sd_ts_data_ext

! ------------------------------------------------------------------------------
  subroutine output_sd_time_series (cgrid, jdate, jtime)

    use HGRD_DEFN
    use mio_module
    use runtime_vars, only: CONC_BLEV, CONC_ELEV
    use std_conc, only: CONC_SUBSET_LAYER, C_NLAYS
    use utilio_defn

    include SUBST_FILES_ID    ! filenames

    real, pointer :: cgrid(:,:,:,:)
    integer, intent(in) :: jdate, jtime

    character (len = 16), parameter :: pname = 'output_sd_time_s'
    character (len = 80) :: xmsg
    character (len = 20) :: timestamp

    integer :: stat, n
    integer, save :: send_to, n_recv, send_index(2,2)
    logical, save :: firstime = .true.
    integer, allocatable, save :: recv_from(:), recv_index(:,:,:)
    logical :: x_intercepted, y_intercepted 

    if (firstime) then
       allocate (recv_from(nprow*npcol), recv_index(2,2,nprow*npcol), stat=stat)

       send_to = -1
       recv_from = -1
       n_recv = 0
       do n = 1, NPCOL*NPROW

           x_intercepted = (( ((sd_scol <= colsx_pe(1,n)) .and. (colsx_pe(1,n) <= sd_ecol)) .or.          &
                              ((sd_scol <= colsx_pe(2,n)) .and. (colsx_pe(2,n) <= sd_ecol))      ) .or.   &
                            ( ((colsx_pe(1,n) <= sd_scol) .and. (sd_scol <= colsx_pe(2,n))) .or.          &
                              ((colsx_pe(1,n) <= sd_ecol) .and. (sd_ecol <= colsx_pe(2,n)))      ))
           y_intercepted = (( ((sd_srow <= rowsx_pe(1,n)) .and. (rowsx_pe(1,n) <= sd_erow)) .or.          &
                              ((sd_srow <= rowsx_pe(2,n)) .and. (rowsx_pe(2,n) <= sd_erow))      ) .or.   &
                            ( ((rowsx_pe(1,n) <= sd_srow) .and. (sd_srow <= rowsx_pe(2,n))) .or.          &
                              ((rowsx_pe(1,n) <= sd_erow) .and. (sd_erow <= rowsx_pe(2,n)))      ))

          if (x_intercepted .and. y_intercepted) then

              n_recv = n_recv + 1
              recv_from(n_recv) = n - 1
              if (n .eq. mype + 1) then
                 send_to = 0
                 send_index(1,1) = max(sd_scol, colsx_pe(1,n)) - colsx_pe(1,n) + 1
                 send_index(2,1) = min(sd_ecol, colsx_pe(2,n)) - colsx_pe(1,n) + 1
                 send_index(1,2) = max(sd_srow, rowsx_pe(1,n)) - rowsx_pe(1,n) + 1
                 send_index(2,2) = min(sd_erow, rowsx_pe(2,n)) - rowsx_pe(1,n) + 1
              end if

              recv_index(1,1,n_recv) = max(sd_scol, colsx_pe(1,n)) - sd_scol + 1
              recv_index(2,1,n_recv) = min(sd_ecol, colsx_pe(2,n)) - sd_scol + 1
              recv_index(1,2,n_recv) = max(sd_srow, rowsx_pe(1,n)) - sd_srow + 1
              recv_index(2,2,n_recv) = min(sd_erow, rowsx_pe(2,n)) - sd_srow + 1
          end if
       end do

       call mio_setfile( met_cro_3d )
       if (CONC_SUBSET_LAYER) mio_nlays = c_nlays

       call mio_fcreate( ctm_sd_ts, mio_new_file )
       if (conc_subset_layer) then
          if (mype .eq. 0) then
             call mio_set_global_attr ( CTM_SD_TS, 'NLAYS', C_NLAYS )
             call mio_set_global_attr ( CTM_SD_TS, 'VGLVLS', MET_CRO_3D, CONC_BLEV, CONC_ELEV+1 )
          end if
       end if

       firstime = .false.

    end if

    call sd_ts_data_ext (cgrid, sd_ts_data, send_to, send_index,    &
                         recv_from, recv_index, n_recv, n_sd_spcs,  &
                         sd_spcs_index, jtime, mype)

    call mio_time_format_conversion( jdate, jtime, timestamp )

    do n = 1, n_sd_spcs
       call mio_fwrite( ctm_sd_ts, sd_spcs(n), pname, sd_ts_data(:,:,:,n), timestamp )
    end do

  end subroutine output_sd_time_series

end module sd_time_series_module
