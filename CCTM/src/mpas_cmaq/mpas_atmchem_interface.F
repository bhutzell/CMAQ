 module mpas_atmchem_interface

   use mpas_kind_types
   use mpas_derived_types
   use mpas_pool_routines
 
   use mpas_atmphys_vars

   use get_env_module
   use RUNTIME_VARS, only : logdev

   use mpi

   implicit none

   private :: convert_time

   contains

   subroutine mpas_cmaq_coupler (config_dt, domain, mesh, state,        &
                                 diag, diag_physics, sfc_input, gfso3)

      use mpas_timekeeping
!     use mpas_kind_types
      use coupler_module
      use util_module, only : time2sec
      use RXNS_DATA, only : MECHNAME

! ppmv = MR * 1e6 * 28.97 / 47.9982 = MR * 603564.300327929
     real, parameter :: o3_convert_factor = 603564.300327929
     real, parameter :: rad_to_deg_factor = 57.295779513

     real(kind=RKIND), pointer           :: config_dt
     type(domain_type),    intent(in)    :: domain
     type(mpas_pool_type), intent(in)    :: mesh
     type(mpas_pool_type), intent(inout) :: state
     type(mpas_pool_type), intent(in)    :: diag
     type(mpas_pool_type), intent(in)    :: diag_physics
     type(mpas_pool_type), intent(in)    :: sfc_input
     type(mpas_pool_type), intent(in)    :: gfso3

! local pointers:
     integer, pointer :: index_qv, index_qc, index_qr, index_qi, index_qs, index_qg

     real(kind=RKIND), dimension(:),   pointer :: latCell, lonCell, xland, xice, snowc, &
                                                  t2m, skintemp, ter, prsfc, rs, ra,    &
                                                  rmol, hpbl, ustar, hfx, lh, canwat,   &
                                                  lai, vegpx, znt, q2, rainc, rainnc,   &
                                                  cldfract, cldfracwcut, areaCell,      &
                                                  gfs_ptrop, u10, v10, rgrnd,           &
                                                  laim, laip
                                                  
     real(kind=RKIND), dimension(:,:), pointer :: zgrid, cldfrac, rho, theta, &
                                                  landusef, smois, qv, qc, qr, qi, qs,  &
                                                  qg, eddy, pressure_p, pressure_base,  &
                                                  cldfracwcu, qc_cu, qi_cu, gfs_ozone,  &
                                                  uReconstructMeridional,               &
                                                  uReconstructZonal,tslb,               &
                                                  lai12

     real(kind=RKIND), allocatable, save :: pressure(:,:)

     real(kind=RKIND), dimension(:,:,:), pointer :: scalars

     real(kind=RKIND), allocatable :: ta(:,:)

     integer, dimension(:), pointer :: ivgtyp, lw, sltyp

     character(len=StrKIND), pointer :: mminlu

     integer, pointer :: num_scalars
 
     character(len=StrKIND)          :: timeStamp
     type (MPAS_Time_Type)           :: currTime
     type (MPAS_TimeInterval_Type)   :: xtimeTime
     type (MPAS_Clock_type), pointer :: clock

     integer :: ierr, i, v, k 
     integer, save :: mpas_date, mpas_time, model_tstep, nsteps, jdate, jtime, runlen, couple_tstep
     logical, save :: firstime = .true.
     logical, save :: initialized = .false.
     logical, save :: called_convert_time = .false.
     integer, save :: counter = -1
     integer, save :: mpas_cmaq_freq
     logical, save :: run_cmaq_driver, mpas_restart
     integer, save :: total_step = 0
     logical       :: cmaq_step, all_zeros
     character(96) :: MSG = ' '

     integer :: my_n_gc
     integer :: my_n_ae
     integer :: my_n_nr

     integer :: min_loc(3), lwater, lice
     real, save, allocatable :: temp_1dr_data(:), temp_3dr_data(:,:,:), loc_zgrid(:,:), loc_pressure(:,:)
     integer, save, allocatable :: temp_1di_data(:)

     counter = counter + 1

     if (firstime) then
        call coupler_init (ite, 1, kte)

        call get_env (mpas_cmaq_freq, 'mpas_cmaq_freq', 1)

        call get_env (mpas_restart, 'mpas_restart', .false.)

        call get_env (run_cmaq_driver, 'run_cmaq_driver', .false.)

        call get_env (model_tstep, 'CTM_TSTEP', 10000)
        call get_env (runlen, 'CTM_RUNLEN', 240000)

        allocate (loc_pressure(kte, ite),                             &
                  temp_1dr_data(ite),                                 &
                  temp_3dr_data(ite, 1, kte),                         &
                  temp_1di_data(ite),                                 &
                  coupler_ocean(ite, 1),                              &
                  coupler_szone(ite, 1),                              &
                  ta(kte, ite),                                       &
                  loc_zgrid(kte+1, ite),                              &
                  cell_area(ite, 1),                                  &
                  cell_vol(ite, 1, kte),                              &
                  inv_cell_vol(ite, 1, kte),                          &
                  cell_ht(ite, 1, kte),                               &
                  inv_cell_ht(ite, 1, kte),                           &
                  inv_mlvl_cell_ht(ite, 1, kte),                      &
                  smois_data(ite, 1, 2),                              &
                  pressure(kte, ite),                                 &
                  stat=ierr)
        if (ierr .ne. 0) then
           print *, ' Error: Cannot allocate memory in mpas_to_chem_var_mapping'
           stop
        end if 

        couple_tstep = int(config_dt * real(mpas_cmaq_freq))

        total_step = (time2sec( runlen ) / int(config_dt * mpas_cmaq_freq) - 1) * mpas_cmaq_freq
     end if

     call mpas_pool_get_dimension (state, 'num_scalars', num_scalars)

     if ( .not. mpas_restart .and. .not. initialized) then
!       coupler_m2a_dr_3d_data = 1.0e-20

        ! Initialize Ozone
        coupler_m2a_dr_3d_data(:, 1, 1:18  , 4, 1) =   20.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 19:40 , 4, 1) =   50.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 41    , 4, 1) =   70.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 42    , 4, 1) =  100.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 43    , 4, 1) =  200.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 44    , 4, 1) =  500.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 45    , 4, 1) = 1000.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 46    , 4, 1) = 2000.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 47    , 4, 1) = 3000.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 48    , 4, 1) = 4000.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 49    , 4, 1) = 6000.0 * 1.0e-3
        coupler_m2a_dr_3d_data(:, 1, 50    , 4, 1) = 8000.0 * 1.0e-3

        ! Initialize Aerosols Modes with a negligible amount of SO4
        do v = 1, n_so4
           coupler_m2a_dr_3d_data(:, 1, :, so4_ind(v), 1) = 1.0e-5
        end do

     end if

     if (run_cmaq_driver) then

        cmaq_step = (mod(counter, mpas_cmaq_freq) .eq. 0)

        if (cmaq_step) then

           call mpas_pool_get_array (mesh, 'latCell', latCell)
           call mpas_pool_get_array (mesh, 'lonCell', lonCell)
           call mpas_pool_get_array (mesh, 'zgrid', zgrid)
           call mpas_pool_get_array (mesh, 'areaCell', areaCell)

           call mpas_pool_get_array (diag, 'uReconstructMeridional', uReconstructMeridional)
           call mpas_pool_get_array (diag, 'uReconstructZonal', uReconstructZonal)

! compute wind speed using orthogonal wind components, uReconstructMeridional and uReconstructZonal
           do k = 1, kte
              do i = 1, ite
                 coupler_m2a_dr_3d_data(i, 1, k, wspd_loc, 1) = sqrt(    uReconstructMeridional(k,i)     &
                                                                       * uReconstructMeridional(k,i)     &
                                                                     +   uReconstructZonal(k,i)          &
                                                                       * uReconstructZonal(k,i))
              end do
           end do

           call mpas_pool_get_array (sfc_input, 'xland', xland)
           call mpas_pool_get_array (sfc_input, 'landmask', lw)
           call mpas_pool_get_array (sfc_input, 'xice', xice)
           call mpas_pool_get_array (sfc_input, 'snowc', snowc)
           call mpas_pool_get_array (sfc_input, 'skintemp', skintemp)
           call mpas_pool_get_array (sfc_input, 'ter', ter)
           call mpas_pool_get_array (sfc_input, 'isltyp', sltyp)

           call mpas_pool_get_array (sfc_input, 'ivgtyp', ivgtyp)
           call mpas_pool_get_array (sfc_input, 'mminlu', mminlu)
           call mpas_pool_get_array (sfc_input, 'landusef', landusef)
           call mpas_pool_get_array (sfc_input, 'smois', smois)
           call mpas_pool_get_array (sfc_input, 'tslb', tslb)

           call mpas_pool_get_array (gfso3, 'ozone', gfs_ozone)
!          call mpas_pool_get_array (gfso3, 'gfs_ozone', gfs_ozone)
           call mpas_pool_get_array (gfso3, 'ptrop', gfs_ptrop)

           call mpas_pool_get_array (diag_physics, 't2m', t2m)
           call mpas_pool_get_array (diag_physics, 'rs', rs)
           call mpas_pool_get_array (diag_physics, 'ra', ra)
           call mpas_pool_get_array (diag_physics, 'rmol', rmol)
           call mpas_pool_get_array (diag_physics, 'hpbl', hpbl)
           call mpas_pool_get_array (diag_physics, 'ust', ustar)
           call mpas_pool_get_array (diag_physics, 'hfx', hfx)
!          call mpas_pool_get_array (diag_physics, 'lai', lai)
           call mpas_pool_get_array (diag_physics, 'lai_px', laip)

!          call mpas_pool_get_array (sfc_input, 'lai_modis', laim)
!          call mpas_pool_get_array (sfc_input, 'lai12m', lai12)

           call mpas_pool_get_array (diag_physics, 'u10', u10)
           call mpas_pool_get_array (diag_physics, 'v10', v10)
           call mpas_pool_get_array (diag_physics, 'swdnb', rgrnd)

! compute 10m wind speed using orthogonal wind components, u10 and v10
           do i = 1, ite
              temp_1dr_data(i) = sqrt(  u10(i) * u10(i)   &
                                      + v10(i) * v10(i))
           end do
           call coupler_data_storing ('WSPD10', temp_1dr_data)

           call mpas_pool_get_array (diag_physics, 'vegpx', vegpx)
           call mpas_pool_get_array (diag_physics, 'znt', znt)
           call mpas_pool_get_array (diag_physics, 'q2', q2)
           call mpas_pool_get_array (diag_physics, 'lh', lh)
           call mpas_pool_get_array (diag_physics, 'canwat', canwat)
           call mpas_pool_get_array (diag_physics, 'raincv', rainc)
           call mpas_pool_get_array (diag_physics, 'rainncv', rainnc)
           call mpas_pool_get_array (diag_physics, 'cldfract', cldfract)
           call mpas_pool_get_array (diag_physics, 'cldfracwcut', cldfracwcut)

           call mpas_pool_get_array (diag_physics, 'cldfrac', cldfrac)
           call mpas_pool_get_array (diag_physics, 'cldfracwcu', cldfracwcu)
           call mpas_pool_get_array (diag_physics, 'qc_cu', qc_cu)
           call mpas_pool_get_array (diag_physics, 'qi_cu', qi_cu)
!          call mpas_pool_get_array (diag, 'pressure', pressure)
           call mpas_pool_get_array (diag, 'pressure_p', pressure_p)
           call mpas_pool_get_array (diag, 'pressure_base', pressure_base)
           call mpas_pool_get_array (diag, 'rho', rho)
           call mpas_pool_get_array (diag, 'theta', theta)
           call mpas_pool_get_array (diag, 'surface_pressure', prsfc)
           call mpas_pool_get_array (diag_physics, 'exch_h', eddy)

           pressure = pressure_p(:,1:ite) + pressure_base(:,1:ite) + 1.0

           allocate (ta(kte, ite), stat=ierr)

           call mpas_pool_get_array (state, 'scalars', scalars, 2)

           call mpas_pool_get_dimension (state, 'index_qv', index_qv )
           call mpas_pool_get_dimension (state, 'index_qc', index_qc )
           call mpas_pool_get_dimension (state, 'index_qr', index_qr )
           call mpas_pool_get_dimension (state, 'index_qi', index_qi )
           call mpas_pool_get_dimension (state, 'index_qs', index_qs )
           call mpas_pool_get_dimension (state, 'index_qg', index_qg )

           qv => scalars(index_qv , :,: )
           qc => scalars(index_qc , :,: )
           qr => scalars(index_qr , :,: )
           qi => scalars(index_qi , :,: )
           qs => scalars(index_qs , :,: )
           qg => scalars(index_qg , :,: )

           ! convert potential temperature to temperature which CMAQ requires
           ta = theta(:,1:ite) / ( ( 1.0 + 0.608 * qv(:,1:ite) ) * ( ( 100000.0 / pressure ) ** 0.286 ))

           clock => domain % clock

           currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)

           call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)

!          if (.not. called_convert_time) then
              call convert_time (timeStamp, mpas_date, mpas_time)
!             called_convert_time = .true.
!          end if

           mminlu_mpas = mminlu

           my_gc_adj = num_scalars - coupler_a2m_nvars

           if (.not. allocated(lufrac_data)) then
              allocate (lufrac_data(size(landusef,1), size(landusef,2)), stat=ierr)
              if (ierr .ne. 0) then
                 print *, ' Error: Cannot allocate lufrac_data'
                 stop
              end if

              lufrac_data = landusef
           end if

           smois_data(:,1,1) = smois(1,1:ite)
! jwilliso adding second level for MEG_DEFN (MEGVSA needs it)
           smois_data(:,1,2) = smois(2,1:ite) ! gamsm deep soil

           temp_1dr_data = latCell(1:ite) * rad_to_deg_factor       ! convert to degree which CMAQ requires
           call coupler_data_storing ('LAT', temp_1dr_data)

           temp_1dr_data = lonCell(1:ite) * rad_to_deg_factor       ! convert to degree which CMAQ requires
           ! in MPAS lonCell is 0 - 360 counter clockwise and
           ! in CMAQ longitude is 0 - 180 counter clockwise and 0 - -180 clockwise
           where (temp_1dr_data > 180.0) temp_1dr_data = temp_1dr_data - 360.0
           call coupler_data_storing ('LON', temp_1dr_data)

!          g2ddata(:, 1, lwmask_ind) = xland(1:ite)
           call coupler_data_storing ('LWMASK', real(xland))

! in case OCEAN file does not exist, setup ocean bases on ground level height and xland paramenter,
! and surf zone is set to 0 for now
           do i = 1, ite
              if ((zgrid(1, i) .le. 20.0) .and. (xland(i) .ge. 1.5)) then
                 coupler_ocean(i,1)      = 1
              else
                 coupler_ocean(i,1)      = 0
              end if
           end do
           coupler_szone(:,1)        = 0

           call coupler_data_storing ('SEAICE', real(xice))
           call coupler_data_storing ('PRSFC', real(prsfc))
           call coupler_data_storing ('SNOCOV', real(snowc))
           call coupler_data_storing ('TEMP2', real(t2m))
           call coupler_data_storing ('TEMPG', real(skintemp))
           call coupler_data_storing ('HT', real(ter))
           call coupler_data_storing ('rmol', 1.0/real(rmol))
           call coupler_data_storing ('RS', real(rs))
           call coupler_data_storing ('RA', real(ra))
           call coupler_data_storing ('PBL', real(hpbl))
           call coupler_data_storing ('USTAR', real(ustar))
           call coupler_data_storing ('HFX', real(hfx))
           call coupler_data_storing ('LH', real(lh))
           temp_1dr_data = canwat(1:ite) * 0.001
           call coupler_data_storing ('WR', temp_1dr_data)
!          g2ddata(:, 1, lai_ind)    = lai(1:ite)
           call coupler_data_storing ('LAI', real(laip))
           call coupler_data_storing ('VEG', real(vegpx))
           call coupler_data_storing ('SLTYP', real(sltyp))
           call coupler_data_storing ('RGRND', real(rgrnd))
           call coupler_data_storing ('SOIT1', real(tslb(1,:)))
           call coupler_data_storing ('SOIT2', real(tslb(2,:)))   ! has confirmed with dzs that second soil level is 1 m

           call coupler_data_storing ('ZRUF', real(znt))
           call coupler_data_storing ('Q2', real(q2))

           if ((mminlu == 'MODIS') .or. (mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
              lwater = 17
              lice   = 15
           else if (mminlu == 'NLCD40') then
              lwater = 17
              lice   = 15
           end if

! to compute PURB
           do i = 1, ite
              if (landusef(lwater,i) == 1.0) then
                 temp_1dr_data(i) = 0.0
              else 
                 if ((mminlu == 'MODIS') .or. (mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
                    temp_1dr_data(i) = ( landusef(13,i) /  (1.0 - landusef(lwater,i)) ) * 100.0
                 else if (mminlu == 'NLCD40') then
                    temp_1dr_data(i) = ( ( landusef(23,i) * 0.10 +    &
                                           landusef(24,i) * 0.35 +    &
                                           landusef(25,i) * 0.65 +    &
                                           landusef(26,i) * 0.90 +    &
                                           landusef(13,i)        ) /  &
                                         (1.0 - landusef(lwater,i)) ) * 100.0
                 else
                    print *, ' Warning:: Unknow Land Use type'
                    temp_1dr_data = 0.0
                 end if
              end if
           end do

           call coupler_data_storing ('PURB', temp_1dr_data)

           do i = 1, ite
              if (rainc(i) >= 0.0) then
                 temp_1dr_data(i) = rainc(i) / 10.0        ! convert mm->cm
              else
                 temp_1dr_data(i) = 0.0
              end if
           end do
           call coupler_data_storing ('RC', temp_1dr_data)

           temp_1dr_data(1:ite) = rainnc(1:ite) / 10.0     ! convert mm->cm
           call coupler_data_storing ('RN', temp_1dr_data)

           call coupler_data_storing ('CFRAC', real(cldfract(1:ite)))
           call coupler_data_storing ('cfrac2dt', real(cldfracwcut(1:ite)))

           if (firstime) then
              cell_area(:,1) = areaCell(1:ite)

              do k = 1, kte
                 do i = 1, ite
                    coupler_m2a_dr_3d_data(i, 1, k, zf_loc, 1) = (zgrid(k+1, i) - zgrid(1, i))

                    if (k .eq. 1) then
                       coupler_m2a_dr_3d_data(i, 1, k, zh_loc, 1) = coupler_m2a_dr_3d_data(i, 1, 1, zf_loc, 1) * 0.5
                       cell_ht(i, 1, k)  = coupler_m2a_dr_3d_data(i, 1, k, zf_loc, 1)
                    else
                       coupler_m2a_dr_3d_data(i, 1, k, zh_loc, 1) = (  coupler_m2a_dr_3d_data(i, 1, k, zf_loc, 1)      &
                                                                     + coupler_m2a_dr_3d_data(i, 1, k-1, zf_loc, 1)) * 0.5
                       cell_ht(i, 1, k)  =   coupler_m2a_dr_3d_data(i, 1, k, zf_loc, 1)                                &
                                           - coupler_m2a_dr_3d_data(i, 1, k-1, zf_loc, 1)
                       inv_mlvl_cell_ht(i, 1, k-1) = 1.0 / (  coupler_m2a_dr_3d_data(i, 1, k, zh_loc, 1)               &
                                                            - coupler_m2a_dr_3d_data(i, 1, k-1, zh_loc, 1))
                    end if

                    cell_vol(i, 1, k)     = cell_ht(i, 1, k) * cell_area(i,1)
                    inv_cell_ht(i, 1, k)  = 1.0 / cell_ht(i, 1, k)
                    inv_cell_vol(i, 1, k) = inv_cell_ht(i, 1, k) / cell_area(i,1)

                 end do
              end do
              inv_mlvl_cell_ht(:, 1, kte) = inv_mlvl_cell_ht(:, 1, kte-1)
           end if

           do k = 1, kte
              do i = 1, ite
                 coupler_m2a_dr_3d_data(i, 1, k, cfrac3d_loc, 1) = cldfrac(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, pres_loc, 1)    = pressure(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, dens_loc, 1)    = rho(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, temp_loc, 1)    = ta(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qv_loc, 1)      = qv(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qc_loc, 1)      = qc(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qr_loc, 1)      = qr(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qg_loc, 1)      = qg(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qi_loc, 1)      = qi(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qs_loc, 1)      = qs(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qc_cu_loc, 1)   = qc_cu(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, qi_cu_loc, 1)   = qi_cu(k, i)
!                coupler_m2a_dr_3d_data(i, 1, k, densa_j_loc, 1) = 1.0 / rho(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, densa_j_loc, 1) = rho(k, i)
                 coupler_m2a_dr_3d_data(i, 1, k, eddy_loc, 1)    = eddy(k, i)
              end do
           end do

           deallocate (ta)

           if (mpas_restart .or. initialized) then
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, coupler_a2m_nvars
                       coupler_m2a_dr_3d_data(i, 1, k, v, 1) = scalars(v+my_gc_adj, k, i)
                    end do
                 end do
              end do
           end if

           if ( .not. mpas_restart .and. .not. initialized) then
              initialized = .true.
! use initial value of ozone from another global model which has been incorporated in the first time step of the GFS file
              do k = 1, kte
                 do i = 1, ite
                    coupler_m2a_dr_3d_data(i, 1, k, 4, 1) = gfs_ozone(k,i) * o3_convert_factor
                 end do
              end do
           else
! replace ozone value in cgrid when pressure < 10000.0 
!              if (mod(mpas_time, 60000) .eq. 0) then
!                 all_zeros = .false.
!                 k = kte
!                 do while ((.not. all_zeros) .and. (k .ge. 1))
!                    all_zeros = .true.
                do k = 1, kte
                    do i = 1, ite
                      if ((pressure(k,i) .le. 30000.0) .and. (gfs_ozone(k,i)*o3_convert_factor .gt. 0.2)) then
                         coupler_m2a_dr_3d_data(i, 1, k, 4, 1) = gfs_ozone(k,i) * o3_convert_factor
                      end if
                    end do
!                   k = k - 1
                 end do
!              end if
           end if

           if (counter >= 1) then

              call cmaq_driver (mpas_date, mpas_time, model_tstep, jdate, jtime,    &
                                (counter .eq. total_step), couple_tstep, ite, kte)

              ! transfer species information back to MPAS
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, coupler_a2m_nvars
                       scalars(v+my_gc_adj, k, i) = coupler_m2a_dr_3d_data(i, 1, k, v, 1)
                    end do
                 end do
              end do
           end if

        end if  ! end of cmaq_step
     end if  ! end of run_cmaq_driver

     if (firstime) then
        firstime = .false.

        ! this initialize CMAQ data in scalar array at the very first step
        if (( run_cmaq_driver ) .and. ( .not. mpas_restart )) then
           do k = 1, kte
              do i = 1, ite
                 do v = 1, coupler_a2m_nvars
                    scalars(v+my_gc_adj, k, i) = coupler_m2a_dr_3d_data(i, 1, k, v, 1)
                 end do
              end do
           end do
        end if
     end if

   end subroutine mpas_cmaq_coupler

! -------------------------------------------------------------------

   subroutine convert_time (timeStamp, mpas_date, mpas_time)

     use util_module, only : leap_year

     character(len=StrKIND), intent(in) :: timeStamp
     integer, intent(out) :: mpas_date, mpas_time

     integer, parameter :: ly_days(12) = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /)
     integer, parameter :: ry_days(12) = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)

     integer :: year, month, day, hour, min, sec, tday

     read (timeStamp, '(i4, 5(1x, i2))') year, month, day, hour, min, sec

     if (leap_year(year)) then
        tday = ly_days(month) + day
     else
        tday = ry_days(month) + day
     end if

     mpas_date = year * 1000 + tday
     mpas_time = hour * 10000 + min * 100 + sec

   end subroutine convert_time

 end module mpas_atmchem_interface
