
      SUBROUTINE AIRQNT( ibeg, jbeg, iend, jend, nlayers, PSURF, T, ALT, p_geos, 
     &                   sealevel )

      IMPLICIT NONE

#     include "geoschem.com"

      INTEGER             :: ibeg, jbeg, iend, jend, nlayers
      REAL*4, INTENT(IN)  :: PSURF( iend, jend )
      REAL*4, INTENT(IN)  :: T( iend, jend, nlayers )
      REAL*4, INTENT(OUT) :: ALT( iend, jend, nlayers )
      REAL*4, INTENT(OUT) :: p_geos( iend, jend, nlayers )

      ! Local variables
      INTEGER             :: I,  J,  L
      REAL*4              :: P1, P2, PS, GET_PEDGE

      REAL*4              :: BXHEIGHT, Z, PALT
      REAL*4              :: sealevel( iend, jend )

      !=================================================================
      ! AIRQNT begins here!
      !=================================================================

      DO J = jbeg, jend
      DO I = ibeg, iend

        PS = PSURF(I,J) + PTOP

        ! Compute surface altitude relative to sealevel
        Z  = PALT( PS ) * 1.E3  ! km => m
        sealevel(I,J) = Z

        DO L = 1, nlayers

         ! Pressure at bottom edge of grid box [hPa]

         P1 = GET_PEDGE( PS, L, nlayers+1 )

         ! Pressure at top edge of grid box [hPa]

         P2 = GET_PEDGE( PS, L+1, nlayers+1 )

         !===========================================================
         ! BXHEIGHT is the height (Delta-Z) of grid box (I,J,L)
         ! in meters.
         !
         ! The formula for BXHEIGHT is just the hydrostatic eqn.
         ! Rd = 287 J/K/kg is the value for the ideal gas constant
         ! R for air (M.W = 0.02897 kg/mol),  or
         ! Rd = 8.31 J/(mol*K) / 0.02897 kg/mol.
         !===========================================================
         BXHEIGHT = Rdg0 * T(I,J,L) * LOG( P1 / P2 )

         !dbg - beg
         if (BXHEIGHT .le. 0.0) then
            print*,'BXHEIGHT is less than or equal to zero'
            print*,'T(I,J,L)=',T(I,J,L)
            print*,'I,J,L=',I,J,L
            print*,'Rdg0,P1,P2,Z',Rdg0,P1,P2,Z
            print*,'PS=',PS
            stop
         endif !dbg - end

         Z = Z + BXHEIGHT
         ALT(I,J,L) = Z - ( 0.5 * BXHEIGHT )
         p_geos(I,J,L) = (P1 + P2)/2.

        ENDDO

      ENDDO
      ENDDO

      ! Return to calling program
      END SUBROUTINE AIRQNT

!------------------------------------------------------------------------------

      FUNCTION GET_PEDGE( PFLT, L, nlayers_plus1 ) RESULT( PEDGE )
!
!******************************************************************************
!  Function GET_PEDGE returns the pressure at the bottom edge of level L.
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) P (REAL*8 ) : P_surface - P_top (PS-PTOP)
!  (2 ) L (INTEGER) : Pressure will be returned at the bottom edge of level L
!
!  NOTES:
!******************************************************************************
!
#     include "define.h"
#     include "geoschem.com"

      ! Arguments
      REAL*4 , INTENT(IN) :: PFLT
      INTEGER, INTENT(IN) :: L
      INTEGER, INTENT(IN) :: nlayers_plus1

      ! Return value
      REAL*4              :: PEDGE
      REAL*4              :: AP_30(31), BP_30(31) ! set the maximum no. of
      REAL*4              :: AP_47(48), BP_47(48) ! set the maximum no. of
      REAL*4              :: AP_72(73), BP_72(73) ! set the maximum no. of

      !=========================================================================
      ! As of Dec. 14, 2004, GEOS-4 data are available only after Sep. 1, 2002
      ! this availability will soon be changed.
      ! Note.. This time stamp should be changed as met. data change with time
      !=========================================================================
      IF ( TRIM(MODELNAME) == 'GEOS3_30L' ) then
      !----------------------
      ! GEOS-3 30 level grid
      !----------------------

         ! AP [hPa] is just PTOP for a pure-sigma grid
         AP_30 = PTOP

         ! BP [unitless] is just SIGE for a pure-sigma grid
!         BP_30 = (/ 1.000000, 0.997095, 0.991200, 0.981500,
!     &           0.967100, 0.946800, 0.919500, 0.884000,
!     &           0.839000, 0.783000, 0.718200, 0.647600,
!     &           0.574100, 0.500000, 0.427800, 0.359500,
!     &           0.297050, 0.241950, 0.194640, 0.155000,
!     &           0.122680, 0.096900, 0.076480, 0.047610,
!     &           0.029600, 0.018380, 0.007040, 0.002530,
!     &           0.000765, 0.000155, 0.000000 /)

         ! Here Ap is just PTOP in [hPa] and Bp are the sigma edges [unitless]
         ! PFLT is the true surface pressure, so subtract PTOP from it
!         PEDGE = AP_30(L) + ( BP_30(L) * ( PFLT - PTOP ) )

      ELSE IF ( TRIM(MODELNAME) == 'GEOS4_30L' ) then
        write(*,*)'not surpporting GEOS4_30L'
        stop
      !----------------------
      ! GEOS-4 30 level grid
      !----------------------

         ! Ap [hPa] for 30 levels (31 edges)
!         AP_30 = (/  0.000000,   0.000000,  12.704939,  35.465965,
!     &           66.098427, 101.671654, 138.744400, 173.403183,
!     &          198.737839, 215.417526, 223.884689, 224.362869,
!     &          216.864929, 201.192093, 176.929993, 150.393005,
!     &          127.837006, 108.663429,  92.365662,  78.512299,
!     &           56.387939,  40.175419,  28.367815,  19.791553,
!     &            9.292943,   4.076567,   1.650792,   0.616779,
!     &            0.211349,   0.066000,   0.010000 /)

         ! Bp [unitless] for 30 levels (31 edges)
!         BP_30 = (/  1.000000,   0.985110,   0.943290,   0.867830,
!     &            0.764920,   0.642710,   0.510460,   0.378440,
!     &            0.270330,   0.183300,   0.115030,   0.063720,
!     &            0.028010,   0.006960,   0.000000,   0.000000,
!     &            0.000000,   0.000000,   0.000000,   0.000000,
!     &            0.000000,   0.000000,   0.000000,   0.000000,
!     &            0.000000,   0.000000,   0.000000,   0.000000,
!     &            0.000000,   0.000000,   0.000000 /)

         ! Here Ap is in [hPa] and Bp is [unitless].
         ! For GEOS-4, we need to have PFLT as true surface pressure,
         ! since Ap(1)=0 and Bp(1)=1.0.  This ensures that the true
         ! surface pressure will be returned for L=1. (bmy, 10/24/03)
!         PEDGE = AP(L) + ( BP(L) * PFLT )
      ELSE IF ( TRIM(MODELNAME) == 'GEOS5_47L' ) then
       !----------------------
       ! GEOS-5 47 level grid
       !----------------------
      ! Ap [hPa] for 47 levels (48 edges)
      AP_47 = (/ 0.000000d+00, 4.804826d-02, 6.593752d+00, 1.313480d+01,
     &        1.961311d+01, 2.609201d+01, 3.257081d+01, 3.898201d+01,
     &        4.533901d+01, 5.169611d+01, 5.805321d+01, 6.436264d+01,
     &        7.062198d+01, 7.883422d+01, 8.909992d+01, 9.936521d+01,
     &        1.091817d+02, 1.189586d+02, 1.286959d+02, 1.429100d+02,
     &        1.562600d+02, 1.696090d+02, 1.816190d+02, 1.930970d+02,
     &        2.032590d+02, 2.121500d+02, 2.187760d+02, 2.238980d+02,
     &        2.243630d+02, 2.168650d+02, 2.011920d+02, 1.769300d+02,
     &        1.503930d+02, 1.278370d+02, 1.086630d+02, 9.236572d+01,
     &        7.851231d+01, 5.638791d+01, 4.017541d+01, 2.836781d+01,
     &        1.979160d+01, 9.292942d+00, 4.076571d+00, 1.650790d+00,
     &        6.167791d-01, 2.113490d-01, 6.600001d-02, 1.000000d-02 /)

      ! Bp [unitless] for 47 levels (48 edges)
      BP_47 = (/ 1.000000d+00, 9.849520d-01, 9.634060d-01, 9.418650d-01,
     &        9.203870d-01, 8.989080d-01, 8.774290d-01, 8.560180d-01,
     &        8.346609d-01, 8.133039d-01, 7.919469d-01, 7.706375d-01,
     &        7.493782d-01, 7.211660d-01, 6.858999d-01, 6.506349d-01,
     &        6.158184d-01, 5.810415d-01, 5.463042d-01, 4.945902d-01,
     &        4.437402d-01, 3.928911d-01, 3.433811d-01, 2.944031d-01,
     &        2.467411d-01, 2.003501d-01, 1.562241d-01, 1.136021d-01,
     &        6.372006d-02, 2.801004d-02, 6.960025d-03, 8.175413d-09,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00 /)
 
       ! same as GEOS_4 (jjung, 11/10/10)
       PEDGE = AP_47(L) + ( BP_47(L) * PFLT )

      ELSE IF ( TRIM(MODELNAME) == 'GEOS5' ) then
       !----------------------
       ! GEOS-5 72 level grid
       !----------------------
      ! Ap [hPa] for 72 levels (73 edges)
      AP_72  =
     &     (/ 0.000000d+00, 4.804826d-02, 6.593752d+00, 1.313480d+01,
     &        1.961311d+01, 2.609201d+01, 3.257081d+01, 3.898201d+01,
     &        4.533901d+01, 5.169611d+01, 5.805321d+01, 6.436264d+01,
     &        7.062198d+01, 7.883422d+01, 8.909992d+01, 9.936521d+01,
     &        1.091817d+02, 1.189586d+02, 1.286959d+02, 1.429100d+02,
     &        1.562600d+02, 1.696090d+02, 1.816190d+02, 1.930970d+02,
     &        2.032590d+02, 2.121500d+02, 2.187760d+02, 2.238980d+02,
     &        2.243630d+02, 2.168650d+02, 2.011920d+02, 1.769300d+02,
     &        1.503930d+02, 1.278370d+02, 1.086630d+02, 9.236572d+01,
     &        7.851231d+01, 6.660341d+01, 5.638791d+01, 4.764391d+01,
     &        4.017541d+01, 3.381001d+01, 2.836781d+01, 2.373041d+01,
     &        1.979160d+01, 1.645710d+01, 1.364340d+01, 1.127690d+01,
     &        9.292942d+00, 7.619842d+00, 6.216801d+00, 5.046801d+00,
     &        4.076571d+00, 3.276431d+00, 2.620211d+00, 2.084970d+00,
     &        1.650790d+00, 1.300510d+00, 1.019440d+00, 7.951341d-01,
     &        6.167791d-01, 4.758061d-01, 3.650411d-01, 2.785261d-01,
     &        2.113490d-01, 1.594950d-01, 1.197030d-01, 8.934502d-02,
     &        6.600001d-02, 4.758501d-02, 3.270000d-02, 2.000000d-02,
     &        1.000000d-02 /)

      ! Bp [unitless] for 72 levels (73 edges)
      BP_72 =
     &     (/ 1.000000d+00, 9.849520d-01, 9.634060d-01, 9.418650d-01,
     &        9.203870d-01, 8.989080d-01, 8.774290d-01, 8.560180d-01,
     &        8.346609d-01, 8.133039d-01, 7.919469d-01, 7.706375d-01,
     &        7.493782d-01, 7.211660d-01, 6.858999d-01, 6.506349d-01,
     &        6.158184d-01, 5.810415d-01, 5.463042d-01, 4.945902d-01,
     &        4.437402d-01, 3.928911d-01, 3.433811d-01, 2.944031d-01,
     &        2.467411d-01, 2.003501d-01, 1.562241d-01, 1.136021d-01,
     &        6.372006d-02, 2.801004d-02, 6.960025d-03, 8.175413d-09,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00, 0.000000d+00, 0.000000d+00, 0.000000d+00,
     &        0.000000d+00 /)
 
       ! same to GEOS_4 (jjung, 11/10/10)
       PEDGE = AP_72(L) + ( BP_72(L) * PFLT )

      ELSE

         write(6,*) 'NEED PROPER GEOS MODEL NAME', TRIM(MODELNAME)
         STOP

      endif

      ! Return to calling program
      END FUNCTION GET_PEDGE

!------------------------------------------------------------------------------

      FUNCTION PALT( PRESS )

!
!******************************************************************************
!  Function PALT computes p-altitude following THE 1976 STANDARD ATMOSPHERE TO 86 KM.
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) PressAL*4 ) : P_surface - P_top (PS-PTOP)
!  (2 ) L (INTEGER) : Pressure will be returned at the bottom edge of level L
!
!  NOTE - IF ALT > 86, THE VALUES RETURNED WILL NOT BE CORRECT, BUT THEY WILL
!   NOT BE TOO FAR REMOVED FROM THE CORRECT VALUES FOR DENSITY.
!   THE REFERENCE DOCUMENT DOES NOT USE THE TERMS PRESSURE AND TEMPERATURE
!   ABOVE 86 KM.
!==============================================================================
!
      IMPLICIT NONE

      REAL*4, INTENT(IN) :: PRESS
      REAL*4, PARAMETER  :: REARTH = 6369.0     ! RADIUS OF THE EARTH (KM)
      REAL*4, PARAMETER  :: GMR = 34.163195     ! GAS CONSTANT
      INTEGER, PARAMETER :: NTAB=8             ! NUMBER OF ENTRIES IN THE DEFINING TABLES

      REAL*4 :: HTAB(NTAB), TTAB(NTAB), PTAB(NTAB), GTAB(NTAB)
      INTEGER :: I, J, K
      REAL*4 :: DELTA, PALT, DELTAH
      REAL*4 :: TGRAD, TBASE, SIGMA, H, THETA, TLOCAL

      HTAB = (/ 0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852 /)
      TTAB = (/ 288.15, 216.65, 216.65, 228.65, 270.65, 270.65,
     &          214.65, 186.946 /)
      PTAB = (/ 1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3,
     &          1.0945601E-3, 6.6063531E-4, 3.9046834E-5, 3.68501E-6 /)
      GTAB = (/ -6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0 /)

      DELTA = PRESS/1013.25
      IF (DELTA > 1.) DELTA = 1.

      I = 0
      J = NTAB-1

      DO WHILE (J > I+1)
        K = (I+J)/2
        IF ( -DELTA < -PTAB(K) ) then
           J = K
        ELSE
           I = K
        endif
      ENDDO

      TGRAD = GTAB(I)
      TBASE = TTAB(I)

      IF (TGRAD == 0.0) then
        DELTAH = TBASE*ALOG(DELTA/PTAB(I))/(-GMR)
      ELSE
        DELTAH = TBASE*((DELTA/PTAB(I))**(-TGRAD/GMR)-1.)/TGRAD
      endif

      H = DELTAH+HTAB(I)
      TLOCAL = TBASE + TGRAD*DELTAH
      THETA = TLOCAL /TTAB(1)
      SIGMA = DELTA/THETA

      SIGMA = SIGMA*1.225*6.022169E+20/28.9644   ! NUMBER DENSITY OF AIR (#/CM^3)
      THETA = TLOCAL                             ! TEMPERATURE (K)
      PALT  = REARTH*H/(REARTH-H)                ! CONVERT GEOPOTENTIAL TO GEOMETRIC ALTITUDE

      END FUNCTION PALT
