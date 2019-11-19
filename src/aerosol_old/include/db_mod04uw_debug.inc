c---------------------------------------------------------------------
C!F77-INC
c
c!Description:
c Debug include file to carry file handle to separate subroutines
c through the Direct Broadcast Version of the MODIS Aerosol
c software (MOD04).
c
c!Input parameters: None.
c
c!Output Parameters: None.
c
c!Revision History:
c
c!Team-unique Header:
c
c!References and Credits:
c
c!END
c---------------------------------------------------------------------

      integer debug, h_output
      common / mod04_debug / debug, h_output
