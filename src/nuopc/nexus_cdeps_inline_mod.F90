!> @file nexus_cdeps_inline_mod.F90
!> @brief Wrapper for real CDEPS modules
!> @details Provides access to real dshr/cdeps modules from src/external/CDEPS build

module nexus_cdeps_inline_mod

  use dshr_strdata_mod, only: shr_strdata_type, shr_strdata_init_from_inline, shr_strdata_advance
  use dshr_methods_mod, only: dshr_fldbun_getfldptr

  implicit none

  private

  public :: shr_strdata_type
  public :: shr_strdata_init_from_inline
  public :: shr_strdata_advance
  public :: dshr_fldbun_getfldptr

end module nexus_cdeps_inline_mod
