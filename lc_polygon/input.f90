! input.f90
! Input parameters.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module input_module

use const_module

implicit none
character(len=255) :: f_node1, f_node2, f_face1, f_face2

double precision :: unit1 = 1.d0  ! m
double precision :: unit2 = 1.d0  ! m

double precision, dimension(2) :: pole_l_ = (/0.d0, 0.d0/)         ! rad
double precision, dimension(2) :: pole_b_ = 0.5*pi*(/1.d0, 1.d0/)  ! rad
double precision, dimension(2) :: phi0_ = (/0.d0, 0.d0/)           ! rad
double precision, dimension(2) :: P_rot_ = (/1.d0, 1.d0/)          ! d
double precision, dimension(2) :: Tmin = (/0.d0, 0.d0/)            ! d
double precision, dimension(2) :: R_body = (/1.d0, 1.d0/)          ! m
double precision, dimension(2) :: A_w = (/1.d0, 1.d0/)             ! 1

double precision :: T_star = 5770.d0  ! K
double precision :: T_eq = 300.d0  ! K
character(len=16) :: law = 'Lambert'

! cf. hapke.f90
double precision :: B0 = 0.0d0       ! opposition effect amplitude; 1
double precision :: minh = 0.0d0     ! opposition effect width; 1
double precision :: ming = 0.0d0      ! asymmetry of scattering; 1
double precision :: bartheta = 0.0d0  ! average slope, macroscopic roughness; deg

logical :: use_shadowing = .true.
logical :: use_scattering = .false.
logical :: use_thermal = .false.
logical :: debug_polygon = .false.

! common
double precision :: Phi_lambda, Phi_V_cal, Phi_nu_cal
double precision, dimension(3) :: o_, o__, s_, s__

namelist /input/ &
  f_node1, &
  f_face1, &
  f_node2, &
  f_face2, &
  unit1, &
  unit2, &
  pole_l_, &
  pole_b_, &
  phi0_, &
  P_rot_, &
  Tmin, &
  R_body, &
  A_w, &
  T_star, &
  T_eq, &
  law, &
  B0, &
  minh, &
  ming, &
  bartheta, &
  use_shadowing, &
  use_scattering, &
  use_thermal, &
  debug_polygon

abstract interface
  double precision function f_func(f_L, mu_i, mu_e, alpha)
    double precision, intent(in) :: f_L, mu_i, mu_e, alpha
  end function f_func
end interface

procedure(f_func), pointer :: f_ptr => null()

contains

end module input_module


