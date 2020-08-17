module NEXUS_Error_Mod

  use ESMF
  use HCO_Error_Mod, only: HCO_SUCCESS

  implicit none

  private

  public :: NEXUS_Error_Log

contains

  logical function NEXUS_Error_Log(rcToCheck, msg, line, file, rcToReturn)
    integer,                    intent(in)  :: rcToCheck
    character(len=*), optional, intent(in)  :: msg
    integer,          optional, intent(in)  :: line
    character(len=*), optional, intent(in)  :: file
    integer,          optional, intent(out) :: rcToReturn

    ! -- local variables
    ! -- begin
    NEXUS_Error_Log = (rcToCheck /= HCO_SUCCESS)

    if (NEXUS_Error_Log) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg=msg, &
        line=line, file=file, rcToReturn=rcToReturn)
    else
      if (present(rcToReturn)) rcToReturn = ESMF_SUCCESS
    end if

  end function NEXUS_Error_Log

end module NEXUS_Error_Mod
