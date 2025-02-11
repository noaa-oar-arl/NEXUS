!BOC
#if defined ( ESMF_ ) || defined( MODEL_UFS )
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_read_esmf_mod.F90
!
! !DESCRIPTION: Module HCOIO\_Read\_mod is the HEMCO interface for
!  data reading using ESMF/NUOPC.
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_Read_Mod
!
! !USES:
!
  USE HCO_Types_Mod
  USE HCO_Error_Mod
  USE HCO_State_Mod,       ONLY : Hco_State
  USE HCO_FILEDATA_MOD,    ONLY : FileData_ArrInit
  USE ESMF

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: HCOIO_Read
  PUBLIC  :: HCOIO_CloseAll

CONTAINS

  SUBROUTINE HCOIO_Read( HcoState, Lct, RC )
    TYPE(HCO_State),  POINTER        :: HcoState
    TYPE(ListCont),   POINTER        :: Lct
    INTEGER,          INTENT(INOUT)  :: RC

    ! Local variables
    INTEGER                    :: II, JJ, LL, TT
    REAL,             POINTER  :: Ptr3D(:,:,:)
    REAL,             POINTER  :: Ptr2D(:,:)
    TYPE(ESMF_State), POINTER  :: IMPORT
    TYPE(ESMF_VM)             :: vm
    INTEGER                    :: localPet
    CHARACTER(LEN=255)         :: MSG
    CHARACTER(LEN=255)         :: LOC = 'HCOIO_READ (hcoio_read_esmf_mod.F90)'
    TYPE(ESMF_Field)          :: field
    INTEGER                    :: itemCount
    TYPE(ESMF_StateItem_Flag) :: itemType
    LOGICAL                    :: isPresent

    !=================================================================
    ! HCOIO_READ begins here
    !=================================================================
    CALL HCO_ENTER( HcoState%Config%Err, LOC, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Point to ESMF IMPORT state
    IMPORT => HcoState%IMPORT
    IF (.NOT. ASSOCIATED(IMPORT)) THEN
       CALL HCO_ERROR('IMPORT state not associated', RC )
       RETURN
    ENDIF

    ! Init pointers
    Ptr3D => NULL()
    Ptr2D => NULL()

    ! Check if field exists in import state
    CALL ESMF_StateGet(IMPORT, TRIM(Lct%Dct%Dta%ncFile), field, RC=RC)
    IF (RC /= ESMF_SUCCESS) THEN
       MSG = 'Field not found in IMPORT state: ' // TRIM(Lct%Dct%Dta%ncFile)
       CALL HCO_ERROR(MSG, RC)
       RETURN
    ENDIF

    !-----------------------------------------------------------------
    ! Read 3D data from ESMF
    !-----------------------------------------------------------------
    IF ( Lct%Dct%Dta%SpaceDim == 3 ) THEN
       CALL ESMF_FieldGet(field, farrayPtr=Ptr3D, RC=RC)
       IF (RC /= ESMF_SUCCESS) THEN
          MSG = 'Cannot get 3D array pointer: ' // TRIM(Lct%Dct%Dta%ncFile)
          CALL HCO_ERROR(MSG, RC)
          RETURN
       ENDIF

       ! Get array dimensions
       II = SIZE(Ptr3D,1)
       JJ = SIZE(Ptr3D,2)
       LL = SIZE(Ptr3D,3)
       TT = 1

       ! Define HEMCO array if not yet defined
       IF ( .NOT. ASSOCIATED(Lct%Dct%Dta%V3) ) THEN
          CALL FileData_ArrInit( Lct%Dct%Dta, TT, 0, 0, 0, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
              CALL HCO_ERROR( 'ERROR 1 | Did not allocate array for 3d data', RC, THISLOC=LOC )
              RETURN
          ENDIF
       ENDIF

       ! Pointer to data with vertical flip
       Lct%Dct%Dta%V3(1)%Val => Ptr3D(:,:,LL:1:-1)

       ! Debug message on root PE only
       CALL ESMF_VMGetCurrent(vm, rc=RC)
       CALL ESMF_VMGet(vm, localPet=localPet, rc=RC)
       IF (localPet == 0) THEN
          print *, "HEMCO: array pointer vertically flipped relative to ESMF Import ", trim(Lct%Dct%Dta%ncFile)
       ENDIF

    !-----------------------------------------------------------------
    ! Read 2D data from ESMF
    !-----------------------------------------------------------------
    ELSEIF ( Lct%Dct%Dta%SpaceDim == 2 ) THEN

       ! Get Data
       CALL ESMF_FieldGet(field, farrayPtr=Ptr2D, RC=RC)
       IF (RC /= ESMF_SUCCESS) THEN
          MSG = 'Cannot get 2D array pointer: ' // TRIM(Lct%Dct%Dta%ncFile)
          CALL HCO_ERROR(MSG, RC)
          RETURN
       ENDIF

       ! Get array dimensions
       II = SIZE(Ptr2D,1)
       JJ = SIZE(Ptr2D,2)
       LL = 1
       TT = 1

       ! Define HEMCO array if not yet defined
       IF ( .NOT. ASSOCIATED(Lct%Dct%Dta%V2) ) THEN
          CALL FileData_ArrInit( Lct%Dct%Dta, TT, 0, 0, RC )
          IF ( RC /= HCO_SUCCESS ) THEN
              CALL HCO_ERROR( 'ERROR 2 | Did not allocate array for 2D data', RC ), THISLOC=LOC )
              RETURN
          ENDIF
       ENDIF

       ! Pointer to data
       Lct%Dct%Dta%V2(1)%Val => Ptr2D

    ENDIF

    !-----------------------------------------------------------------
    ! Cleanup and leave
    !-----------------------------------------------------------------
    Ptr3D  => NULL()
    Ptr2D  => NULL()
    IMPORT => NULL()

    ! Return w/ success
    CALL HCO_LEAVE ( HcoState%Config%Err, RC )

  END SUBROUTINE HCOIO_Read

  SUBROUTINE HCOIO_CloseAll( HcoState, RC )
    TYPE(HCO_State), POINTER          :: HcoState
    INTEGER,          INTENT(INOUT)   :: RC

    ! This is a stub as ESMF handles file closing
    RC = HCO_SUCCESS

  END SUBROUTINE HCOIO_CloseAll

END MODULE HCOIO_Read_Mod
#endif
