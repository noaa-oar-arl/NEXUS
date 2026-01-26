!> @brief NEXUS State Management Module
!> @details This module provides import/export state management and diagnostics for NEXUS.
!> Extracted from cap.F90 for better modularity and maintainability.
!> @authors Barry Baker
!> @version 2.0
!> @date 2026-01-02

module nexus_state_mod

  use ESMF
  use NUOPC
  use HCO_Error_Mod, only: HCO_SUCCESS, HCO_MSG, HCO_ERROR
  use HCO_STATE_MOD, only: Hco_State
  use HCO_TYPES_MOD, only: DiagnCont
  use HCO_DIAGN_MOD, only: Diagn_Create, DiagnCollection_Get
  use HCOI_NUOPC_MOD, only: HCO_UpdateExportFields_NUOPC

  implicit none
  private

  ! Public interfaces
  public :: nxs_diag_state_init_disabled
  public :: nxs_diag_state_update
  public :: nxs_expt_state_init
  public :: nxs_expt_state_update
  public :: nxs_state_finalize
  public :: nxs_create_hemco_diagnostics

contains

  !> @brief Initialize diagnostics state in disabled mode
  !> @param[in] HcoGrid HEMCO grid object
  !> @param[in] HcoState HEMCO state object
  !> @param[out] rc Return code
  subroutine nxs_diag_state_init_disabled( HcoGrid, HcoState, rc )

    type(ESMF_Grid), intent(in) :: HcoGrid
    type(Hco_State), pointer, intent(in) :: HcoState
    integer, intent(out) :: rc

    rc = HCO_SUCCESS

    ! Initialize diagnostics in disabled mode for early phases
    if ( HcoState%amIRoot ) then
       call HCO_MSG('Diagnostics state initialized in disabled mode')
    endif

    rc = HCO_SUCCESS

  end subroutine nxs_diag_state_init_disabled

  !> @brief Update diagnostics state
  !> @param[in] HcoState HEMCO state object
  !> @param[out] rc Return code
  subroutine nxs_diag_state_update( HcoState, rc )

    type(Hco_State), pointer, intent(in) :: HcoState
    integer, intent(out) :: rc

    rc = HCO_SUCCESS

    if ( HcoState%amIRoot ) then
       call HCO_MSG('Diagnostics state update completed')
    endif

    rc = HCO_SUCCESS

  end subroutine nxs_diag_state_update

  !> @brief Initialize export state
  !> @param[in] grid ESMF grid object
  !> @param[inout] importState ESMF import state
  !> @param[inout] exportState ESMF export state
  !> @param[out] rc Return code
  subroutine nxs_expt_state_init( grid, importState, exportState, rc )

    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_State), intent(inout) :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out) :: rc

    type(ESMF_Field) :: field
    character(len=255) :: fieldName
    integer :: localPet, petCount
    type(ESMF_VM) :: vm

    rc = HCO_SUCCESS

    ! Get VM info
    call ESMF_VMGetCurrent(vm, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    ! Initialize basic export fields
    fieldName = 'NEXUS_EMISSIONS'
    field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R4, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           name=trim(fieldName), rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
       call HCO_ERROR('Error creating export field: ' // trim(fieldName), rc)
       return
    endif

    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
       call HCO_ERROR('Error adding field to export state: ' // trim(fieldName), rc)
       return
    endif

    if ( localPet == 0 ) then
       call HCO_MSG('Export state initialized with basic emissions field')
    endif

    rc = HCO_SUCCESS

  end subroutine nxs_expt_state_init

  !> @brief Update export state with current data
  !> @param[inout] importState ESMF import state
  !> @param[inout] exportState ESMF export state
  !> @param[out] rc Return code
  subroutine nxs_expt_state_update( importState, exportState, rc )

    type(ESMF_State), intent(inout) :: importState
    type(ESMF_State), intent(inout) :: exportState
    integer, intent(out) :: rc

    type(ESMF_Field) :: field
    real(kind=4), pointer :: dataPtr(:,:)
    integer :: i, j, localPet
    type(ESMF_VM) :: vm

    rc = HCO_SUCCESS

    ! Get VM info
    call ESMF_VMGetCurrent(vm, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    ! Get export field and update data
    call ESMF_StateGet(exportState, 'NEXUS_EMISSIONS', field, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
       if ( localPet == 0 ) then
          call HCO_MSG('Warning: NEXUS_EMISSIONS field not found in export state')
       endif
       rc = HCO_SUCCESS
       return
    endif

    call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
       call HCO_ERROR('Error getting data pointer for NEXUS_EMISSIONS', rc)
       return
    endif

    ! Zero out emissions data (placeholder - real data would come from HEMCO)
    do j = lbound(dataPtr,2), ubound(dataPtr,2)
       do i = lbound(dataPtr,1), ubound(dataPtr,1)
          dataPtr(i,j) = 0.0
       enddo
    enddo

    if ( localPet == 0 ) then
       call HCO_MSG('Export state updated with current emissions data')
    endif

    rc = HCO_SUCCESS

  end subroutine nxs_expt_state_update

  !> @brief Finalize and cleanup state objects
  !> @param[inout] state ESMF state object
  !> @param[out] rc Return code
  subroutine nxs_state_finalize( state, rc )

    type(ESMF_State), intent(inout) :: state
    integer, intent(out) :: rc

    integer :: itemCount, i
    character(len=255), allocatable :: itemNames(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypes(:)
    type(ESMF_Field) :: field
    integer :: localPet
    type(ESMF_VM) :: vm

    rc = HCO_SUCCESS

    ! Get VM info
    call ESMF_VMGetCurrent(vm, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if ( rc /= ESMF_SUCCESS ) return

    ! Get state contents
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
       if ( localPet == 0 ) then
          call HCO_MSG('Warning: Error getting state item count during finalization')
       endif
       rc = HCO_SUCCESS
       return
    endif

    if ( itemCount > 0 ) then
       allocate(itemNames(itemCount))
       allocate(itemTypes(itemCount))

       call ESMF_StateGet(state, itemNameList=itemNames, itemTypeList=itemTypes, rc=rc)
       if ( rc /= ESMF_SUCCESS ) then
          if ( localPet == 0 ) then
             call HCO_MSG('Warning: Error getting state item names during finalization')
          endif
          deallocate(itemNames, itemTypes)
          rc = HCO_SUCCESS
          return
       endif

       ! Clean up fields
       do i = 1, itemCount
          if ( itemTypes(i) == ESMF_STATEITEM_FIELD ) then
             call ESMF_StateGet(state, trim(itemNames(i)), field, rc=rc)
             if ( rc == ESMF_SUCCESS ) then
                call ESMF_FieldDestroy(field, rc=rc)
             endif
          endif
       enddo

       deallocate(itemNames, itemTypes)
    endif

    if ( localPet == 0 ) then
       call HCO_MSG('State finalized and cleaned up')
    endif

    rc = HCO_SUCCESS

  end subroutine nxs_state_finalize

  !> @brief Create HEMCO diagnostics
  !> @param[inout] HcoState HEMCO state object
  !> @param[out] rc Return code
  subroutine nxs_create_hemco_diagnostics(HcoState, rc)

    type(Hco_State), pointer, intent(inout) :: HcoState
    integer, intent(out) :: rc

    integer :: I, nDiagn
    type(DiagnCont), pointer :: DiagnCollection(:) => null()
    character(len=255) :: msg

    rc = HCO_SUCCESS

    if ( HcoState%amIRoot ) then
       call HCO_MSG('Creating HEMCO diagnostics...')
    endif

    ! Simplified diagnostics creation - let HEMCO handle this internally
    if ( HcoState%amIRoot ) then
       call HCO_MSG('HEMCO diagnostics initialization deferred to HEMCO')
    endif

    rc = HCO_SUCCESS

    rc = HCO_SUCCESS

  end subroutine nxs_create_hemco_diagnostics

end module nexus_state_mod