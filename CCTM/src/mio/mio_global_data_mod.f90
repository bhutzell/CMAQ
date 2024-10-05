! Purpose: Define variables which are sharable among mio routines.

      module mio_global_data_module

        use mio_type_def_module

        implicit none

        integer :: mio_nfiles           ! total number of files
        integer :: mio_n_infiles        ! total number of input files
        integer :: mio_n_outfiles       ! total number of output files
        integer :: mio_cfile            ! current file pointer

        integer :: mio_mype             ! mype
        integer :: mio_mype_p1          ! mype + 1
        logical :: mio_io_pe_inclusive  ! indicator whether this PE handles output

        integer :: mio_logdev           ! log device number

        integer :: mio_parallelism      ! indicator for I/O parallelism implementation

        integer :: mio_domain_ncols   
        integer :: mio_domain_nrows     ! # of columns and rows in simulation domain

        integer :: mio_nprocs           ! total # allocated processors
        integer :: mio_npcol            ! # allocated processor along column dimension
        integer :: mio_nprow            ! # allocated processor along row dimension

! mio_file_data will be allocated based on pre-defined number of
! input and output files. It also assumes that once a file is opened, it
! won't be closed until the very end.
        type(mio_file_record), target, allocatable :: mio_file_data0(:)
        type(mio_file_record), target, allocatable :: mio_file_data1(:)

        type(mio_file_record), pointer :: mio_file_data(:)    !  file_data (fd)
!       type(mio_file_record), allocatable :: mio_file_data(:)    !  file_data (fd)

        integer :: mio_fd_circular = 0  ! indicates which mio_file_data* (fd) storage to use
                                        !   0 -- infor_record0
                                        !   1 -- infor_record1

! to store output file variable information defined in mio_file_input
        type(mio_outfile_def_record) :: mio_outfile_def_info

! for cmaq
        real*8 :: mio_domain_alp, mio_domain_bet, mio_domain_gam,  &
                  mio_domain_xcent, mio_domain_ycent,              &
                  mio_domain_xorig, mio_domain_yorig,              &
                  mio_domain_xcell, mio_domain_ycell

        integer :: mio_domain_gdtyp, mio_domain_nthik

        integer, allocatable :: mio_domain_ncols_pe(:,:)     ! # of columns for cross or dot type
        integer, allocatable :: mio_domain_nrows_pe(:,:)     ! # of rows    for cross or dot type
        integer, allocatable :: mio_domain_colde_pe(:,:,:)   ! start and end column for cross or dot type
        integer, allocatable :: mio_domain_rowde_pe(:,:,:)   ! start and end row    for cross or dot type

! for mpas
        character (len = 1000) :: mio_mpas_dmap_file

        integer, allocatable :: mio_mpas_dmap(:,:)

! Once mio_setfile is called the following 8 variables w.r.t. the file are available 
        integer :: mio_gl_ncols, mio_gl_nrows   ! # of columns and rows in global domain
        integer :: mio_ncols, mio_nrows         ! # of columns and rows in each processor

        integer :: mio_nbase_vars               ! # of base variables, which includes
                                                ! time variable and variable in MPAS that 
                                                ! defines a mesh
        integer :: mio_nlays                    ! # of layers
        integer :: mio_nvars                    ! # of variables excluding time and basic MPAS variables 
        integer :: mio_time_var_ind             ! time dimension variable index
        integer :: mio_bndy_var_ind             ! boundary dimension variable index

      end module mio_global_data_module
