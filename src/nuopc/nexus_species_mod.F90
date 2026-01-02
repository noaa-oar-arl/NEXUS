!> @file nexus_species_mod.F90
!> @brief NEXUS species registration and configuration utilities for HEMCO coupling.
!> @author AI Assistant
!> @date 2024-12-19

!> Module for registering NEXUS model species with HEMCO
!> 
!> This module handles the proper registration of model species with HEMCO
!> as required by the HEMCO 3.0 coupling interface. It reads species definitions
!> from HEMCO_sa_Spec.rc and registers them with both HcoConfig and HcoState
!> structures to enable proper species mapping.
module nexus_species_mod

  use ESMF
  use HCO_ERROR_MOD
  use HCO_TYPES_MOD, only: ConfigObj
  use HCO_STATE_MOD, only: HCO_State


  implicit none
  private

  !> Maximum number of model species supported
  integer, parameter :: MAXSPC = 100

  !> Model species data structure
  type :: ModelSpecies_t
    integer :: specID                    !< Species ID from spec file
    character(len=31) :: specName        !< Species name
    real(kind=8) :: MW_g                 !< Molecular weight [g/mol]
    real(kind=8) :: MW_emis              !< Emitted species molecular weight [g/mol]
    real(kind=8) :: molec_ratio          !< Molecule ratio
    real(kind=8) :: Henry_K0             !< Henry constant [M/atm]
    real(kind=8) :: Henry_CR             !< Henry temperature dependency [K]
    real(kind=8) :: Henry_pKA            !< Acid dissociation constant
  end type ModelSpecies_t

  public :: NEXUS_RegisterSpecies
  public :: NEXUS_ReadSpeciesFile

contains

  !> @brief Read species definitions from HEMCO_sa_Spec.rc file
  !> 
  !> This subroutine reads the species definition file (typically HEMCO_sa_Spec.rc)
  !> and parses the species properties needed for HEMCO registration.
  !>
  !> @param[in]  specFile     Path to species definition file
  !> @param[in]  am_I_Root    True if this is the root processor
  !> @param[out] nSpc         Number of species read
  !> @param[out] species      Array of species data structures
  !> @param[out] rc           Return code
  subroutine NEXUS_ReadSpeciesFile(specFile, am_I_Root, nSpc, species, rc)
    character(len=*), intent(in) :: specFile
    logical, intent(in) :: am_I_Root
    integer, intent(out) :: nSpc
    type(ModelSpecies_t), intent(out) :: species(MAXSPC)
    integer, intent(out) :: rc

    ! Local variables
    integer :: ios, lun, lineNum
    character(len=255) :: line, msg
    character(len=255), parameter :: thisProcedure = 'NEXUS_ReadSpeciesFile'

    rc = HCO_SUCCESS
    nSpc = 0

    if (am_I_Root) then
      print *, 'NEXUS: Reading species file: ', trim(specFile)
    endif

    ! Find free logical unit
    lun = 10
    do while (lun < 100)
      inquire(unit=lun, opened=ios)
      if (.not. ios) exit
      lun = lun + 1
    end do

    ! Open species file
    open(unit=lun, file=trim(specFile), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(msg, '(a,a)') 'Cannot open species file: ', trim(specFile)
      call HCO_ERROR(msg, rc, thisloc=thisProcedure)
      return
    endif

    if (am_I_Root) then
      print *, 'NEXUS: Successfully opened species file'
    endif

    ! Read species definitions
    lineNum = 0
    do
      read(lun, '(a)', iostat=ios) line
      if (ios /= 0) exit  ! End of file or error

      lineNum = lineNum + 1

      ! Skip comment lines and empty lines
      line = adjustl(line)
      if (len_trim(line) == 0 .or. line(1:1) == '#') cycle

      ! Parse species line: ID NAME MW MWEMIS MOLECRATIO K0 CR PKA
      if (nSpc >= MAXSPC) then
        write(msg, '(a,i0)') 'Too many species! Maximum supported: ', MAXSPC
        call HCO_ERROR(msg, rc, thisloc=thisProcedure)
        close(lun)
        return
      endif

      nSpc = nSpc + 1
      read(line, *, iostat=ios) &
        species(nSpc)%specID, &
        species(nSpc)%specName, &
        species(nSpc)%MW_g, &
        species(nSpc)%MW_emis, &
        species(nSpc)%molec_ratio, &
        species(nSpc)%Henry_K0, &
        species(nSpc)%Henry_CR, &
        species(nSpc)%Henry_pKA

      if (ios /= 0) then
        write(msg, '(a,i0,a,a)') 'Error parsing line ', lineNum, ': ', trim(line)
        call HCO_ERROR(msg, rc, thisloc=thisProcedure)
        close(lun)
        return
      endif

      if (am_I_Root) then
        print '(a,i0,a,a,a,f8.2)', 'NEXUS: Species ', species(nSpc)%specID, &
          ' = ', trim(species(nSpc)%specName), ' MW = ', species(nSpc)%MW_g
      endif

    end do

    close(lun)

    if (am_I_Root) then
      print *, 'NEXUS: Read ', nSpc, ' species from file'
    endif

  end subroutine NEXUS_ReadSpeciesFile

  !> @brief Register model species with HEMCO configuration and state objects
  !>
  !> This subroutine registers the model species with HEMCO following the
  !> HEMCO 3.0 coupling interface requirements. It properly configures both
  !> HcoConfig and HcoState structures with species information.
  !>
  !> @param[in]    specFile    Path to species definition file  
  !> @param[in]    am_I_Root   True if this is the root processor
  !> @param[inout] HcoConfig   HEMCO configuration object
  !> @param[inout] HcoState    HEMCO state object
  !> @param[out]   rc          Return code
  subroutine NEXUS_RegisterSpecies(specFile, am_I_Root, HcoConfig, HcoState, rc)
    character(len=*), intent(in) :: specFile
    logical, intent(in) :: am_I_Root
    type(ConfigObj), pointer :: HcoConfig
    type(HCO_State), pointer :: HcoState
    integer, intent(out) :: rc

    ! Local variables
    type(ModelSpecies_t) :: species(MAXSPC)
    integer :: nSpc, i
    character(len=255) :: msg
    character(len=255), parameter :: thisProcedure = 'NEXUS_RegisterSpecies'

    rc = HCO_SUCCESS

    if (am_I_Root) then
      print *, 'NEXUS: Registering model species with HEMCO'
    endif

    ! Read species from file
    call NEXUS_ReadSpeciesFile(specFile, am_I_Root, nSpc, species, rc)
    if (rc /= HCO_SUCCESS) then
      call HCO_ERROR('Error reading species file', rc, thisloc=thisProcedure)
      return
    endif

    if (nSpc == 0) then
      call HCO_ERROR('No species found in species file', rc, thisloc=thisProcedure)
      return
    endif

    ! Initialize HcoConfig with correct number of model species
    if (associated(HcoConfig)) then
      ! HcoConfig already exists, update it
      if (am_I_Root) then
        print *, 'NEXUS: HcoConfig already exists, updating with ', nSpc, ' species'
      endif
      
      ! Set number of model species
      HcoConfig%nModelSpc = nSpc
      
      ! Allocate ModelSpc array if not already associated
      if (.not. associated(HcoConfig%ModelSpc)) then
        allocate(HcoConfig%ModelSpc(nSpc))
      else
        ! Reallocate if size is different
        if (size(HcoConfig%ModelSpc) /= nSpc) then
          deallocate(HcoConfig%ModelSpc)
          allocate(HcoConfig%ModelSpc(nSpc))
        endif
      endif
    else
      call HCO_ERROR('HcoConfig is not associated', rc, thisloc=thisProcedure)
      return
    endif

    ! Register species in HcoConfig
    do i = 1, nSpc
      HcoConfig%ModelSpc(i)%HcoID   = i
      HcoConfig%ModelSpc(i)%ModID   = species(i)%specID
      HcoConfig%ModelSpc(i)%SpcName = trim(species(i)%specName)

      if (am_I_Root) then
        print '(a,i0,a,i0,a,a)', 'NEXUS: HcoConfig species ', i, &
          ' (ModID=', species(i)%specID, ') = ', trim(species(i)%specName)
      endif
    end do

    ! Register species in HcoState
    if (associated(HcoState)) then
      ! Set number of species
      HcoState%nSpc = nSpc
      
      ! Allocate Spc array if not already associated
      if (.not. associated(HcoState%Spc)) then
        allocate(HcoState%Spc(nSpc))
      endif

      ! Fill species information
      do i = 1, nSpc
        HcoState%Spc(i)%HcoID      = i
        HcoState%Spc(i)%ModID      = species(i)%specID  
        HcoState%Spc(i)%SpcName    = trim(species(i)%specName)
        HcoState%Spc(i)%MW_g       = species(i)%MW_g
        HcoState%Spc(i)%HenryK0    = species(i)%Henry_K0
        HcoState%Spc(i)%HenryCR    = species(i)%Henry_CR
        HcoState%Spc(i)%HenryPKA   = species(i)%Henry_pKA

        if (am_I_Root) then
          print '(a,i0,a,a,a,f8.2)', 'NEXUS: HcoState species ', i, &
            ' = ', trim(species(i)%specName), ' MW = ', species(i)%MW_g
        endif
      end do

      if (am_I_Root) then
        print *, 'NEXUS: Successfully registered ', nSpc, ' species with HcoState'
      endif
    else
      call HCO_ERROR('HcoState is not associated', rc, thisloc=thisProcedure)
      return
    endif

    if (am_I_Root) then
      print *, 'NEXUS: Species registration complete'
    endif

  end subroutine NEXUS_RegisterSpecies

end module nexus_species_mod