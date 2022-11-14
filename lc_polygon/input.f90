! input.f90
! Input parameters.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

module input_module

use const_module

implicit none
character(len=255) :: f_node1, f_node2, f_face1, f_face2

double precision :: unit1 = 1.d0  ! m
double precision :: unit2 = 1.d0  ! m

integer, parameter :: NBOD = 2
double precision, dimension(NBOD) :: pole_l = (/0.d0, 0.d0/)         ! rad
double precision, dimension(NBOD) :: pole_b = 0.5*pi*(/1.d0, 1.d0/)  ! rad
double precision, dimension(NBOD) :: Prot = (/1.d0, 1.d0/)           ! d
double precision, dimension(NBOD) :: Tmin = (/0.d0, 0.d0/)           ! d
double precision, dimension(NBOD) :: phi0 = (/0.d0, 0.d0/)           ! rad

double precision :: T_star = 5770.d0  ! K
double precision :: T_eq = 300.d0  ! K
double precision :: A_w = 1.d0
character(len=16) :: law = 'Lambert'

! cf. hapke.f90
double precision :: B0 = 0.0d0       ! opposition effect amplitude; 1
double precision :: minh = 0.0d0     ! opposition effect width; 1
double precision :: ming = 0.0d0      ! asymmetry of scattering; 1
double precision :: bartheta = 0.0d0  ! average slope, macroscopic roughness; rad <- NOT USED!

logical :: use_shadowing = .true.
logical :: use_scattering = .false.
logical :: use_thermal = .false.
logical :: debug = .false.

! common
double precision :: Phi_lambda, Phi_V_cal, Phi_nu_cal, f_L
double precision, dimension(3) :: o_, o__, s_, s__

namelist /input/ &
  f_node1, &
  f_face1, &
  f_node2, &
  f_face2, &
  unit1, &
  unit2, &
  pole_l, &
  pole_b, &
  Prot, &
  Tmin, &
  phi0, &
  T_star, &
  T_eq, &
  A_w, &
  law, &
  B0, &
  minh, &
  ming, &
  bartheta, &
  use_shadowing, &
  use_scattering, &
  use_thermal, &
  debug

abstract interface
  double precision function f_func(f_L, mu_i, mu_e, alpha)
    double precision, intent(in) :: f_L, mu_i, mu_e, alpha
  end function f_func
end interface

procedure(f_func), pointer :: f_ptr => null()

contains

end module input_module


