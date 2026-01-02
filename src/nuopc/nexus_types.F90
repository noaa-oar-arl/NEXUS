!> @file nexus_types.F90
!> @brief Derived types for NEXUS NUOPC component
!> @details This module contains all derived type definitions used by NEXUS
!>          components, including field data containers and I/O structures.
!>
!> @author NEXUS Development Team
!> @date 2025
!> @copyright Public Domain

module nexus_types

  use ESMF

  implicit none

  private

  !----------------------------------------------------------------------------
  ! Public types
  !----------------------------------------------------------------------------
  public :: FieldDataEntry, FieldDataContainer, HistoryStream, cdeps_stream_wrapper

  !----------------------------------------------------------------------------
  ! Field data container types
  !----------------------------------------------------------------------------

  !> @brief Container for individual field data entry
  !> @details Stores field metadata and data for both NUOPC and file-based fields
  type :: FieldDataEntry
    character(len=256) :: name          !< Field name (e.g., "STREAM:VARIABLE")
    character(len=64)  :: units         !< Units string (e.g., "kg m-2 s-1")
    character(len=256) :: standard_name !< CF standard name
    character(len=64)  :: source_type   !< "NUOPC" or "FILE"
    integer :: nx, ny                   !< Grid dimensions
    real(kind=ESMF_KIND_R8), allocatable :: data(:,:) !< Field data array
    logical :: is_valid                 !< Whether the entry contains valid data
  end type FieldDataEntry

  !> @brief Container for multiple field data entries
  !> @details Registry-style container for managing multiple field data entries
  type :: FieldDataContainer
    type(FieldDataEntry), allocatable :: entries(:) !< Array of field entries
    integer :: num_entries                           !< Current number of entries
    integer :: max_entries                           !< Maximum allocated entries
  end type FieldDataContainer

  !----------------------------------------------------------------------------
  ! I/O and history types
  !----------------------------------------------------------------------------

  !> @brief Wrapper for CDEPS stream data with field bundle
  !> @details Container for CDEPS input streams with associated field bundles
  type :: cdeps_stream_wrapper
    type(ESMF_FieldBundle) :: fieldBundle !< ESMF field bundle for the stream
    character(len=255) :: name             !< Stream name
  end type cdeps_stream_wrapper

  !> @brief Type for managing history streams (output)
  !> @details Configuration for output file streams including timing and variables
  type :: HistoryStream
    character(len=255) :: name           !< Stream name
    character(len=255) :: fileName       !< Output filename template
    type(ESMF_TimeInterval) :: frequency !< Output frequency
    character(len=10) :: mode            !< "append" or "overwrite"
    character(len=255), allocatable :: variables(:) !< Variables to output
    type(ESMF_Clock) :: clock           !< Timing control
    logical :: initialized = .false.    !< Initialization status
  end type HistoryStream

end module nexus_types