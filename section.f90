module section_m
#ifdef dnad
    use dnadmod
#define real type(dual)
#endif

    use airfoil_m
    implicit none

    type section_t
        integer :: iSec

        real :: P1(3)
        real :: P2(3)
        real :: PC(3)
        real :: dl(3) !vector from P1 to P2
        real :: temp_P1(3),temp_P2(3) !temporary values used for testing
        real :: chord_c,chord_1,chord_2
        real :: percent_c,percent_1,percent_2
        real :: percent_af, percent_af_1, percent_af_2
        real :: ds
        real :: ua(3)
        real :: un(3)
        real :: us(3)
        real :: uf(3) !flap deflection unit vector. For viewing purposes only
        real :: Rroot(3)
        real :: zeta(3)
        real :: cf_c          !flap percentage of chord
        real :: ef            !epsilon_f (See Phillips Eq. 1.7.12)
        real :: Cmdelta       !Cm,delta  (See Phillips Eq. 1.7.14)
        real :: control_deflection    !set in plane.f90

        real :: alpha
        real :: v(3) !local velocity
        real :: viom !dynamic pressure scaling value used by Phillips. Don't fully trust. Fix this someday.

        type(airfoil_t),pointer :: af1
        type(airfoil_t),pointer :: af2
        
        real :: Gamma
        real :: F(3),M(3) !force and moment about the local quarter-chord divided by dynamic pressure
        real :: Load_F(3),Load_M(3) !aerodynamic force and moment distributed loads
        
        !For Geometry Purposes
        real :: twist,twist1,twist2
        real :: dihedral,dihedral1,dihedral2
        real :: sweep

        ! For propeller purposes
        real :: r1,r2,rc !radius from hub center
        real :: aL0
        real :: beta
        real :: einf,ei

    end type section_t

contains

!-----------------------------------------------------------------------------------------------------------
real function sec_CLa(t)
    type(section_t),pointer :: t
    sec_CLa = sec_af_weight(t, af_CLa(t%af1,t%alpha), af_CLa(t%af2,t%alpha))
end function sec_CLa

!-----------------------------------------------------------------------------------------------------------
real function sec_CL(t)
    type(section_t),pointer :: t
    real :: etad
    if(abs(t%control_deflection)*180.0/pi.lt.11.0) then
        etad = 1.0
    else
        !etad = m*x + b
        etad = -8.71794871794872E-03*abs(t%control_deflection)*180.0/pi + 1.09589743589744
    end if
    sec_CL = sec_af_weight(t, af_CL(t%af1,t%alpha), af_CL(t%af2,t%alpha)) + sec_CLa(t)*t%ef*etad*t%control_deflection !includes flaps
end function sec_CL

!-----------------------------------------------------------------------------------------------------------
real function sec_CLmax(t)
    type(section_t),pointer :: t
    sec_CLmax = sec_af_weight(t, t%af1%CLmax, t%af2%CLmax)
end function sec_CLmax

!-----------------------------------------------------------------------------------------------------------
real function sec_CD(t)
    type(section_t),pointer :: t
    sec_CD = sec_af_weight(t, af_CD(t%af1,t%alpha), af_CD(t%af2,t%alpha)) + 0.002*180.0/pi*abs(t%control_deflection) !rough estimate for flaps
end function sec_CD

!-----------------------------------------------------------------------------------------------------------
real function sec_Cm(t)
    type(section_t),pointer :: t
    sec_Cm = sec_af_weight(t, af_Cm(t%af1,t%alpha), af_Cm(t%af2,t%alpha)) + t%Cmdelta*t%control_deflection !includes flaps
end function sec_Cm

!-----------------------------------------------------------------------------------------------------------
real function sec_stalled(t)
    type(section_t),pointer :: t
    real :: sCLmax
    sec_stalled = 0
    if(sec_CL(t) > sec_CLmax(t)) sec_stalled = sec_CL(t) - sec_CLmax(t)
end function sec_stalled

!-----------------------------------------------------------------------------------------------------------
real function sec_af_weight(t,root,tip)
    type(section_t),pointer :: t
    real :: root,tip
    sec_af_weight = root*(1.0 - t%percent_af) + tip*t%percent_af
end function sec_af_weight

!-----------------------------------------------------------------------------------------------------------
end module section_m
