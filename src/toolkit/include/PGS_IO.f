!-------------------------------------------------------------------------!
!                                                                         !
!  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  !
!  and suppliers.  ALL RIGHTS RESERVED.                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!******************************************************************************
! BEGIN_FILE_PROLOG:
!     
! FILENAME:
!     
!     PGS_IO.f
!     
! DESCRIPTION:
!     
!     This file contains declarations needed by the FORTRAN IO tools.
!     
! AUTHOR:
!     Mike Sucher / Applied Research Corp.
!     
! HISTORY:
!     01-Aug-1994 MES  Initial version
!     05-Jan-1995 TWA  Added APPEND support for F90 OPEN
!     
! END_FILE_PROLOG:
!******************************************************************************

!     
! valid PGS temporary file creation duration modes
!     

!     Force creation of temporary, non-static file 
      integer PGSd_IO_Gen_NoEndurance
      parameter (PGSd_IO_Gen_NoEndurance = 0)

!     Allow creation of intermediate, static file
      integer PGSd_IO_Gen_Endurance
      parameter (PGSd_IO_Gen_Endurance = 1)


!     
! valid PGS temporary file access modes
!     

!     PGS                 
!     C  File Access Mode          Description             
!     ===================          ========================
!     
!     PGSd_IO_Gen_Write           open for writing
!     PGSd_IO_Gen_Read            open for reading, must exist
!     PGSd_IO_Gen_Append          open for append
!     PGSd_IO_Gen_Update          open read/write, all existing data is saved
!     PGSd_IO_Gen_Trunc           truncate to zero and open file for write
!     PGSd_IO_Gen_AppendUpdate    read any place, but write to end-of-file


      integer PGSd_IO_Gen_Write
      parameter (PGSd_IO_Gen_Write = 1)
      integer PGSd_IO_Gen_Read
      parameter (PGSd_IO_Gen_Read = 2)
      integer PGSd_IO_Gen_Append
      parameter (PGSd_IO_Gen_Append = 3)
      integer PGSd_IO_Gen_Update
      parameter (PGSd_IO_Gen_Update = 4)
      integer PGSd_IO_Gen_Trunc
      parameter (PGSd_IO_Gen_Trunc = 5)
      integer PGSd_IO_Gen_AppendUpdate
      parameter (PGSd_IO_Gen_AppendUpdate = 6)


!                           Rd/Wr/
!            PGS            Update/ FORTRAN      FORTRAN
!     FORTRAN Access Mode   Append  'access='    'form='
!     ===================   ======  ==========   ===========
!     
!     PGSd_IO_Gen_RSeqFrm   Read    Sequential   Formatted
!     PGSd_IO_Gen_RSeqUnf   Read    Sequential   Unformatted
!     PGSd_IO_Gen_RDirFrm   Read    Direct       Formatted
!     PGSd_IO_Gen_RDirUnf   Read    Direct       Unformatted
!     
!     PGSd_IO_Gen_WSeqFrm   Write   Sequential   Formatted
!     PGSd_IO_Gen_WSeqUnf   Write   Sequential   Unformatted
!     PGSd_IO_Gen_WDirFrm   Write   Direct       Formatted
!     PGSd_IO_Gen_WDirUnf   Write   Direct       Formatted
!     
!     PGSd_IO_Gen_USeqFrm   Update  Sequential   Formatted
!     PGSd_IO_Gen_USeqUnf   Update  Sequential   Unformatted
!     PGSd_IO_Gen_UDirFrm   Update  Direct       Formatted
!     PGSd_IO_Gen_UDirUnf   Update  Direct       Formatted
!     
!     PGSd_IO_Gen_ASeqFrm   Append  Sequential   Formatted
!     PGSd_IO_Gen_ASeqUnf   Append  Sequential   Unformatted

      integer PGSd_IO_Gen_RSeqFrm
      parameter (PGSd_IO_Gen_RSeqFrm = 10)
      integer PGSd_IO_Gen_RSeqUnf
      parameter (PGSd_IO_Gen_RSeqUnf = 11)
      integer PGSd_IO_Gen_RDirFrm
      parameter (PGSd_IO_Gen_RDirFrm = 12)
      integer PGSd_IO_Gen_RDirUnf
      parameter (PGSd_IO_Gen_RDirUnf = 13)

      integer PGSd_IO_Gen_WSeqFrm
      parameter (PGSd_IO_Gen_WSeqFrm = 20)
      integer PGSd_IO_Gen_WSeqUnf
      parameter (PGSd_IO_Gen_WSeqUnf = 21)
      integer PGSd_IO_Gen_WDirFrm
      parameter (PGSd_IO_Gen_WDirFrm = 22)
      integer PGSd_IO_Gen_WDirUnf
      parameter (PGSd_IO_Gen_WDirUnf = 23)

      integer PGSd_IO_Gen_USeqFrm
      parameter (PGSd_IO_Gen_USeqFrm = 30)
      integer PGSd_IO_Gen_USeqUnf
      parameter (PGSd_IO_Gen_USeqUnf = 31)
      integer PGSd_IO_Gen_UDirFrm
      parameter (PGSd_IO_Gen_UDirFrm = 32)
      integer PGSd_IO_Gen_UDirUnf
      parameter (PGSd_IO_Gen_UDirUnf = 33)

      integer PGSd_IO_Gen_ASeqFrm
      parameter (PGSd_IO_Gen_ASeqFrm = 40)
      integer PGSd_IO_Gen_ASeqUnf
      parameter (PGSd_IO_Gen_ASeqUnf = 41)

!     
! PGS Toolkit IO function declarations
!     

!     integer PGS_IO_Gen_OpenF
!     integer PGS_IO_Gen_CloseF
!     integer PGS_IO_Gen_Temp_OpenF
!     integer PGS_IO_Gen_Temp_Delete

