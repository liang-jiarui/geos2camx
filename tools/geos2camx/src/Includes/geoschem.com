!....................................................................
!  INCLUDE FILE  GEOSCHEM.COM
!
!  CONTAINS:  Parameters containing 
!
!  DEPENDENT UPON:  <files>
!
!  REVISION HISTORY:
!
!       Updated by Rokjin Park
!       September 2004
!         
!       For GEOS-CHEM  by Nankyoung Moon
!       November 2003. IMAQS, U of H
!....................................................................

      ! C Preprocessor #define statements for conditional compilation
#     include "define.h"

      !=================================================================
      ! DISIZE = size (in degrees) of a longitude grid box
      ! DJSIZE = size (in degrees) of a latitude  grid box
      !=================================================================
#if   defined( GRID4x5  ) 
      REAL*8, PARAMETER  :: DISIZE = 5.0d0
      REAL*8, PARAMETER  :: DJSIZE = 4.0d0
      INTEGER, PARAMETER :: IGLOB  = 72
      INTEGER, PARAMETER :: JGLOB  = 46
!jjung      INTEGER, PARAMETER :: LGLOB  = 30

#elif defined( GRID2x25 )
      REAL*8, PARAMETER  :: DISIZE = 2.5d0 
      REAL*8, PARAMETER  :: DJSIZE = 2.0d0
      INTEGER, PARAMETER :: IGLOB  = 144
      INTEGER, PARAMETER :: JGLOB  = 91

#elif defined( NESTED_NA )
      REAL*8, PARAMETER  :: DISIZE = 0.6666666666666667d0 
      REAL*8, PARAMETER  :: DJSIZE = 0.5d0
!jjung      INTEGER, PARAMETER :: IGLOB  = 151
      INTEGER, PARAMETER :: IGLOB  = 540
!jjung      INTEGER, PARAMETER :: JGLOB  = 121
      INTEGER, PARAMETER :: JGLOB  = 361
#elif defined( GRID1x1 )
      REAL*8, PARAMETER  :: DISIZE = 1.0d0 
      REAL*8, PARAMETER  :: DJSIZE = 1.0d0
      INTEGER, PARAMETER :: IGLOB  = 360
      INTEGER, PARAMETER :: JGLOB  = 181
!jjung      INTEGER, PARAMETER :: LGLOB  = 30

#endif

      ! Parameter for Data archived
      INTEGER, PARAMETER :: LLTROP = 20       
      REAL*4,  PARAMETER :: PTOP   = 0.01

      !======================================
      ! GRID PARAMETER
      !======================================
      INTEGER, PARAMETER :: IIPAR  = IGLOB
      INTEGER, PARAMETER :: JJPAR  = JGLOB
!jjung      INTEGER, PARAMETER :: LLPAR  = LGLOB

      INTEGER, PARAMETER :: NI    = IIPAR
      INTEGER, PARAMETER :: NJ    = JJPAR
!bk      INTEGER, PARAMETER :: NL    = LLTROP
!jjung      INTEGER, PARAMETER :: NL    = LLPAR

      !======================================
      ! CONSTANT
      !======================================
      REAL*4,  PARAMETER :: AVGN  = 6.023E+23 

      ! Re    : Radius of Earth [m] 
      REAL*4,  PARAMETER  :: Re    =   6.375e6               

      ! Rd    : Gas Constant (R) in Dry Air [287 J/K/kg] 
      REAL*4,  PARAMETER :: Rd     = 287.0

      ! g0    : Gravity at Surface of Earth [9.8 m/s^2]
      REAL*4,  PARAMETER :: g0     =  9.8

      ! Rdg0   = Rd    / g0
      REAL*4,  PARAMETER :: Rdg0   =  Rd / g0

      ! GEOS MODEL INDICATOR
      CHARACTER(LEN=20)  :: MODELNAME
      COMMON /CTLNAME/      MODELNAME
