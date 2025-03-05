module datm_datamode_era5_mod

  use ESMF             , only : ESMF_State, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use shr_precip_mod   , only : shr_precip_partition_rain_snow_ramp
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_era5_advertise
  public  :: datm_datamode_era5_init_pointers
  public  :: datm_datamode_era5_advance
  private :: datm_eSat  ! determine saturation vapor pressure

  ! export state data
  real(r8), pointer :: Sa_z(:)              => null()
  real(r8), pointer :: Sa_u10m(:)           => null()
  real(r8), pointer :: Sa_v10m(:)           => null()
  real(r8), pointer :: Sa_u(:)              => null()
  real(r8), pointer :: Sa_v(:)              => null()
  real(r8), pointer :: Sa_wspd10m(:)        => null()
  real(r8), pointer :: Sa_wspd(:)           => null()
  real(r8), pointer :: Sa_t2m(:)            => null()
  real(r8), pointer :: Sa_tbot(:)           => null()
  real(r8), pointer :: Sa_tskn(:)           => null()
  real(r8), pointer :: Sa_q2m(:)            => null()
  real(r8), pointer :: Sa_shum(:)           => null()
  real(r8), pointer :: Sa_pslv(:)           => null()
  real(r8), pointer :: Sa_pbot(:)           => null()
  real(r8), pointer :: Faxa_rain(:)         => null()
  real(r8), pointer :: Faxa_rainc(:)        => null()
  real(r8), pointer :: Faxa_rainl(:)        => null()
  real(r8), pointer :: Faxa_snowc(:)        => null()
  real(r8), pointer :: Faxa_snowl(:)        => null()
  real(r8), pointer :: Faxa_swndr(:)        => null()
  real(r8), pointer :: Faxa_swndf(:)        => null()
  real(r8), pointer :: Faxa_swvdr(:)        => null()
  real(r8), pointer :: Faxa_swvdf(:)        => null()
  real(r8), pointer :: Faxa_swdn(:)         => null()
  real(r8), pointer :: Faxa_swnet(:)        => null()
  real(r8), pointer :: Faxa_lwdn(:)         => null()
  real(r8), pointer :: Faxa_lwnet(:)        => null()
  real(r8), pointer :: Faxa_sen(:)          => null()
  real(r8), pointer :: Faxa_lat(:)          => null()
  real(r8), pointer :: Faxa_taux(:)         => null()
  real(r8), pointer :: Faxa_tauy(:)         => null()
!
!  real(r8), pointer :: Faxa_ndep(:,:)       => null()

  ! stream data
  real(r8), pointer :: strm_z(:)            => null()
  real(r8), pointer :: strm_tdew(:)         => null()
  real(r8), pointer :: strm_wind(:)         => null()
  real(r8), pointer :: strm_wind10m(:)      => null()
  real(r8), pointer :: strm_u(:)            => null()
  real(r8), pointer :: strm_v(:)            => null()
  real(r8), pointer :: strm_u10m(:)         => null()
  real(r8), pointer :: strm_v10m(:)         => null()

  real(r8) :: t2max   ! units detector
  real(r8) :: td2max  ! units detector
  real(r8) :: lwmax ! units detector
  real(r8) :: precmax ! units detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3

  character(*), parameter :: nullstr = 'undefined'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_era5_advertise(exportState, fldsexport, &
       flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_z'       )
    call dshr_fldList_add(fldsExport, 'Sa_u10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_v10m'    )
    call dshr_fldList_add(fldsExport, 'Sa_u'       )
    call dshr_fldList_add(fldsExport, 'Sa_v'       )
    call dshr_fldList_add(fldsExport, 'Sa_wspd10m' )
    call dshr_fldList_add(fldsExport, 'Sa_wspd'    )
    call dshr_fldList_add(fldsExport, 'Sa_t2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_tbot'    )
    call dshr_fldList_add(fldsExport, 'Sa_tskn'    )
    call dshr_fldList_add(fldsExport, 'Sa_q2m'     )
    call dshr_fldList_add(fldsExport, 'Sa_shum'    )
    call dshr_fldList_add(fldsExport, 'Sa_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sa_pbot'    )
    call dshr_fldList_add(fldsExport, 'Faxa_rain'  )
    call dshr_fldList_add(fldsExport, 'Faxa_rainc' )
    call dshr_fldList_add(fldsExport, 'Faxa_rainl' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowc' )
    call dshr_fldList_add(fldsExport, 'Faxa_snowl' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdr' )
    call dshr_fldList_add(fldsExport, 'Faxa_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swvdf' )
    call dshr_fldList_add(fldsExport, 'Faxa_swdn'  )
    call dshr_fldList_add(fldsExport, 'Faxa_swnet' )
    call dshr_fldList_add(fldsExport, 'Faxa_lwdn'  )
    call dshr_fldList_add(fldsExport, 'Faxa_lwnet' )
    call dshr_fldList_add(fldsExport, 'Faxa_sen'   )
    call dshr_fldList_add(fldsExport, 'Faxa_lat'   )
    call dshr_fldList_add(fldsExport, 'Faxa_taux'  )
    call dshr_fldList_add(fldsExport, 'Faxa_tauy'  )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_era5_advertise

  !===============================================================================
  subroutine datm_datamode_era5_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers for module level stream arrays
    call shr_strdata_get_stream_pointer(sdat, 'Sa_z'      , strm_z   , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_tdew'   , strm_tdew, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_wspd'   , strm_wind, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_wspd10m', strm_wind10m, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u'      , strm_u, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v'      , strm_v, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_u10m'   , strm_u10m, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Sa_v10m'   , strm_v10m, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sa_z'       , fldptr1=Sa_z       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u10m'    , fldptr1=Sa_u10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m'    , fldptr1=Sa_v10m    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u'       , fldptr1=Sa_u       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v'       , fldptr1=Sa_v       , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_wspd10m' , fldptr1=Sa_wspd10m , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_wspd'    , fldptr1=Sa_wspd    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_t2m'     , fldptr1=Sa_t2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tbot'    , fldptr1=Sa_tbot    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_tskn'    , fldptr1=Sa_tskn    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_q2m'     , fldptr1=Sa_q2m     , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_shum'    , fldptr1=Sa_shum    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pslv'    , fldptr1=Sa_pslv    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_pbot'    , fldptr1=Sa_pbot    , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rain'  , fldptr1=Faxa_rain  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainc' , fldptr1=Faxa_rainc , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_rainl' , fldptr1=Faxa_rainl , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowc' , fldptr1=Faxa_snowc , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_snowl' , fldptr1=Faxa_snowl , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdr' , fldptr1=Faxa_swvdr , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swvdf' , fldptr1=Faxa_swvdf , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndr' , fldptr1=Faxa_swndr , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swndf' , fldptr1=Faxa_swndf , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swdn'  , fldptr1=Faxa_swdn  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_swnet' , fldptr1=Faxa_swnet , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwdn'  , fldptr1=Faxa_lwdn  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lwnet' , fldptr1=Faxa_lwnet , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_sen'   , fldptr1=Faxa_sen   , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_lat'   , fldptr1=Faxa_lat   , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_taux'  , fldptr1=Faxa_taux  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_tauy'  , fldptr1=Faxa_tauy  , allowNullReturn=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine datm_datamode_era5_init_pointers

  !===============================================================================  
  subroutine datm_datamode_era5_advance(exportstate, mainproc, logunit, mpicom, target_ymd, target_tod, model_calendar, rc)
    use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MAX, ESMF_VM

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: mpicom
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize = 0           ! size of attr vect
    real(r8) :: rtmp(2)
    real(r8) :: tbot, pbot
    real(r8) :: e, qsat
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_era5_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! one of the following needs to be in the stream
    if (associated(Sa_z)) lsize = size(Sa_z)
    if (associated(strm_tdew)) lsize = size(strm_tdew)
    if (mainproc .and. lsize == 0) then
       write(logunit,*) trim(subname),' Sa_z or Sa_tdew need to be in the stream! Exiting ...'
    end if

    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       ! determine t2max (see below for use)
       if (associated(Sa_t2m)) then
          rtmp(1) = maxval(Sa_t2m(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          t2max = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' t2max = ',t2max
       end if

       ! determine tdewmax (see below for use)
       if (associated(strm_tdew)) then
          rtmp(1) = maxval(strm_tdew(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          td2max = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' td2max = ',td2max
       end if

       ! determine lwmax / lwmax
       if (associated(Faxa_lwdn)) then
          rtmp(1) = maxval(Faxa_lwdn(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          lwmax = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' lwmax = ',lwmax
       else
          ! try with other variable since Faxa_lwdn is not available
          if (associated(Faxa_lwnet)) then
            rtmp(1) = maxval(Faxa_lwnet(:))
            call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
            lwmax = rtmp(2)
            if (mainproc) write(logunit,*) trim(subname),' lwmax = ',lwmax
          else
            lwmax = 0.0_r8
          end if
       end if

       ! determine precmax
       if (associated(Faxa_rain)) then
          rtmp(1) = maxval(Faxa_rain(:))
          call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
          precmax = rtmp(2)
          if (mainproc) write(logunit,*) trim(subname),' precmax = ', precmax
       else
          ! try with other variable since Faxa_rain is not available
          if (associated(Faxa_rainl)) then
            rtmp(1) = maxval(Faxa_rainl(:))
            call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MAX, rc=rc)
            precmax = rtmp(2)
            if (mainproc) write(logunit,*) trim(subname),' precmax = ', precmax
          else
            precmax = 0.0_r8
          end if
       end if

       ! reset first_time
       first_time = .false.
    end if

    do n = 1, lsize
       !--- bottom layer height ---
       if (.not. associated(strm_z) .and. associated(Sa_z)) then
          Sa_z(n) = 10.0_r8
       end if

       !--- calculate wind components if wind speed is provided ---
       if (associated(strm_wind)) then
          Sa_u(n) = strm_wind(n)/sqrt(2.0_r8)
          Sa_v(n) = Sa_u(n)
       end if
       if (associated(strm_wind10m)) then
          Sa_u10m(n) = strm_wind10m(n)/sqrt(2.0_r8)
          Sa_v10m(n) = Sa_u10m(n)
       end if

       !--- calculate wind speed if wind components are provided ---
       if (associated(strm_u10m) .and. associated(strm_v10m) .and. associated(Sa_wspd10m)) then
          Sa_wspd10m(n) = sqrt(Sa_u10m(n)*Sa_u10m(n)+Sa_v10m(n)*Sa_v10m(n))
       end if
       if (associated(strm_u) .and. associated(strm_v) .and. associated(Sa_wspd)) then
          Sa_wspd(n) = sqrt(Sa_u(n)*Sa_u(n)+Sa_v(n)*Sa_v(n))
       end if

       !--- calculate specific humidity from dew point temperature ---
       if (associated(strm_tdew)) then
          if (associated(Sa_t2m)) then
             tbot = Sa_t2m(n)
          else if (associated(Sa_tbot)) then
             tbot = Sa_tbot(n)
          end if

          if (associated(Sa_pslv)) then
             pbot = Sa_pslv(n)
          else if (associated(Sa_pbot)) then
             pbot = Sa_pbot(n)
          end if

          if (td2max < 50.0_r8) strm_tdew(n) = strm_tdew(n) + tkFrz
          e = datm_eSat(strm_tdew(n), tbot)
          qsat = (0.622_r8 * e)/(pbot - 0.378_r8 * e)
          if (associated(Sa_q2m)) Sa_q2m(n) = qsat
          if (associated(Sa_shum)) Sa_shum(n) = qsat
       end if
    end do

    !----------------------------------------------------------
    ! shortwave bands
    !----------------------------------------------------------

    !--- shortwave radiation (Faxa_* basically holds albedo) ---
    !--- see comments for Faxa_swnet
    if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swdn(:)*Faxa_swvdr(:)
    if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swdn(:)*Faxa_swndr(:)
    if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swdn(:)*Faxa_swvdf(:)
    if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swdn(:)*Faxa_swndf(:)

    !--- TODO: need to understand relationship between shortwave bands and
    !--- net shortwave rad. currently it is provided directly from ERA5
    !--- and the total of the bands are not consistent with the swnet
    !--- swnet: a diagnostic quantity ---
    !if (associated(Faxa_swnet)) then
    !  if (associated(Faxa_swndr) .and. associated(Faxa_swvdr) .and. &
    !      associated(Faxa_swndf) .and. associated(Faxa_swvdf)) then
    !    Faxa_swnet(:) = Faxa_swndr(:) + Faxa_swvdr(:) + Faxa_swndf(:) + Faxa_swvdf(:)
    !  end if
    !end if

    !----------------------------------------------------------
    ! unit conversions (temporal resolution is hourly)
    !----------------------------------------------------------

    ! convert J/m^2 to W/m^2
    if (lwmax < 1.0e4_r8) then
       if (mainproc) write(logunit,*) trim(subname),' flux related variables are already in W/m^2 unit!'
    else
       if (associated(Faxa_lwdn))  Faxa_lwdn(:)  = Faxa_lwdn(:)/3600.0_r8
       if (associated(Faxa_lwnet)) Faxa_lwnet(:) = Faxa_lwnet(:)/3600.0_r8
       if (associated(Faxa_swvdr)) Faxa_swvdr(:) = Faxa_swvdr(:)/3600.0_r8
       if (associated(Faxa_swndr)) Faxa_swndr(:) = Faxa_swndr(:)/3600.0_r8
       if (associated(Faxa_swvdf)) Faxa_swvdf(:) = Faxa_swvdf(:)/3600.0_r8
       if (associated(Faxa_swndf)) Faxa_swndf(:) = Faxa_swndf(:)/3600.0_r8
       if (associated(Faxa_swdn))  Faxa_swdn(:)  = Faxa_swdn(:)/3600.0_r8
       if (associated(Faxa_swnet)) Faxa_swnet(:) = Faxa_swnet(:)/3600.0_r8
       if (associated(Faxa_sen))   Faxa_sen(:)   = Faxa_sen(:)/3600.0_r8
       if (associated(Faxa_lat))   Faxa_lat(:)   = Faxa_lat(:)/3600.0_r8
    end if

    ! convert m to kg/m^2/s
    if (precmax < 0.01_r8) then
       if (mainproc) write(logunit,*) trim(subname),' precipitation related variables are already in kg/m^2/s unit!'
    else
       if (associated(Faxa_rain))  Faxa_rain(:)  = Faxa_rain(:)/3600.0_r8*rhofw
       if (associated(Faxa_rainc)) Faxa_rainc(:) = Faxa_rainc(:)/3600.0_r8*rhofw
       if (associated(Faxa_rainl)) Faxa_rainl(:) = Faxa_rainl(:)/3600.0_r8*rhofw
       if (associated(Faxa_snowc)) Faxa_snowc(:) = Faxa_snowc(:)/3600.0_r8*rhofw
       if (associated(Faxa_snowl)) Faxa_snowl(:) = Faxa_snowl(:)/3600.0_r8*rhofw
    end if

    ! convert N/m^2 s to N/m^2
    if (associated(Faxa_taux))  Faxa_taux(:)  = Faxa_taux(:)/3600.0_r8
    if (associated(Faxa_tauy))  Faxa_tauy(:)  = Faxa_tauy(:)/3600.0_r8

  end subroutine datm_datamode_era5_advance

  !===============================================================================  
  real(r8) function datm_eSat(tK,tKbot)

    !----------------------------------------------------------------------------
    ! use polynomials to calculate saturation vapor pressure and derivative with
    ! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
    ! required to convert relative humidity to specific humidity
    !----------------------------------------------------------------------------

    ! input/output variables
    real(r8),intent(in) :: tK    ! temp used in polynomial calculation
    real(r8),intent(in) :: tKbot ! bottom atm temp

    ! local variables
    real(r8)           :: t     ! tK converted to Celcius
    real(r8),parameter :: tkFrz = shr_const_tkfrz  ! freezing T of fresh water ~ K

    !--- coefficients for esat over water ---
    real(r8),parameter :: a0=6.107799961_r8
    real(r8),parameter :: a1=4.436518521e-01_r8
    real(r8),parameter :: a2=1.428945805e-02_r8
    real(r8),parameter :: a3=2.650648471e-04_r8
    real(r8),parameter :: a4=3.031240396e-06_r8
    real(r8),parameter :: a5=2.034080948e-08_r8
    real(r8),parameter :: a6=6.136820929e-11_r8

    !--- coefficients for esat over ice ---
    real(r8),parameter :: b0=6.109177956_r8
    real(r8),parameter :: b1=5.034698970e-01_r8
    real(r8),parameter :: b2=1.886013408e-02_r8
    real(r8),parameter :: b3=4.176223716e-04_r8
    real(r8),parameter :: b4=5.824720280e-06_r8
    real(r8),parameter :: b5=4.838803174e-08_r8
    real(r8),parameter :: b6=1.838826904e-10_r8

    t = min( 50.0_r8, max(-50.0_r8,(tK-tKfrz)) )
    if ( tKbot < tKfrz) then
       datm_eSat = 100.0_r8*(b0+t*(b1+t*(b2+t*(b3+t*(b4+t*(b5+t*b6))))))
    else
       datm_eSat = 100.0_r8*(a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+t*a6))))))
    end if

  end function datm_eSat

end module datm_datamode_era5_mod
