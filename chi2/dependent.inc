! chi2.inc
! Include file for chi2 subroutines.
! Miroslav Broz (miroslav.broz@email.cz), Jan 21st 2025

      integer ndim, nparam
      integer                                                           &
     &  n_SKY,                                                          &
     &  n_TTV,                                                          &
     &  n_RV,                                                           &
     &  n_ECL,                                                          &
     &  n_VIS,                                                          &
     &  n_CLO,                                                          &
     &  n_T3,                                                           &
     &  n_LC,                                                           &
     &  n_SYN,                                                          &
     &  n_SED,                                                          &
     &  n_AO,                                                           &
     &  n_SKY2,                                                         &
     &  n_SKY3,                                                         &
     &  n_OCC,                                                          &
     &  n_AO2,                                                          &
     &  n_SED2,                                                         &
     &  n_fit
      real*8                                                            &
     &  chi2_SKY,                                                       &
     &  chi2_TTV,                                                       &
     &  chi2_RV,                                                        &
     &  chi2_ECL,                                                       &
     &  chi2_VIS,                                                       &
     &  chi2_CLO,                                                       &
     &  chi2_T3,                                                        &
     &  chi2_LC,                                                        &
     &  chi2_SYN,                                                       &
     &  chi2_SED,                                                       &
     &  chi2_AO,                                                        &
     &  chi2_SKY2,                                                      &
     &  chi2_SKY3,                                                      &
     &  chi2_OCC,                                                       &
     &  chi2_AO2,                                                       &
     &  chi2_SED2,                                                      &
     &  chi2_MASS
      real*8                                                            &
     &  w_SKY,                                                          &
     &  w_TTV,                                                          &
     &  w_RV,                                                           &
     &  w_ECL,                                                          &
     &  w_VIS,                                                          &
     &  w_CLO,                                                          &
     &  w_T3,                                                           &
     &  w_LC,                                                           &
     &  w_SYN,                                                          &
     &  w_SED,                                                          &
     &  w_AO,                                                           &
     &  w_SKY2,                                                         &
     &  w_SKY3,                                                         &
     &  w_OCC,                                                          &
     &  w_AO2,                                                          &
     &  w_SED2

      real*8 x_param(NDIMMAX)
      logical variable(NDIMMAX)
      real*8 lns

      integer nbod, nband
      integer iband_LC(BANDMAX)
      integer geometry
      integer nshp, nsub
      real*8 T0
      real*8 R_star(NBODMAX), T_eff(NBODMAX), Lum(NBODMAX)
      real*8 log_g(NBODMAX), v_rot(NBODMAX), P_rot(NBODMAX)
      real*8 metal(NBODMAX)
      real*8 Delta_t(NBODMAX)
      real*8 C20(NBODMAX)
      real*8 pole_l(NBODMAX), pole_b(NBODMAX)
      real*8 phi0(NBODMAX)
      real*8 albedo(NBODMAX)
      real*8 scattering(4)
      real*8 psf_param(2)
      real*8 d_pc

      real*8 zero(BANDMAX)
      real*8 m_min(NBODMAX), m_max(NBODMAX)
      real*8 lightcurve_timestep, approx_eclipse_duration
      real*8 lambda1, lambda2, lambda3, lambda4
      real*8 pyterpol_Delta(4)
      real*8 silh_factor
      real*8 spectral_slope
      real*8 eps_BS

      character*80                                                      &
     &  file_SKY(NBODMAX),                                              &
     &  file_TTV,                                                       &
     &  file_RV(NBODMAX),                                               &
     &  file_ECL,                                                       &
     &  file_VIS,                                                       &
     &  file_CLO,                                                       &
     &  file_LC(BANDMAX),                                               &
     &  file_SYN,                                                       &
     &  file_synth(NBODMAX),                                            &
     &  file_SED,                                                       &
     &  file_absol(NBODMAX),                                            &
     &  file_AO,                                                        &
     &  file_SKY2,                                                      &
     &  file_SKY3,                                                      &
     &  file_OCC,                                                       &
     &  file_AO2,                                                       &
     &  file_SED2,                                                      &
     &  tmpdir

      logical                                                           &
     &  use_planck,                                                     &
     &  use_filters,                                                    &
     &  use_limbdark,                                                   &
     &  use_pyterpol,                                                   &
     &  use_vardist,                                                    &
     &  use_varpole,                                                    &
     &  use_multipole,                                                  &
     &  use_bruteforce,                                                 &
     &  use_oblat,                                                      &
     &  use_tides,                                                      &
     &  use_tides2,                                                     &
     &  use_ppn,                                                        &
     &  use_polygon,                                                    &
     &  use_zero,                                                       &
     &  use_adam,                                                       &
     &  use_stellar,                                                    &
     &  use_cliptrace

      logical use_hec88(NBODMAX)
      logical is_forward
      logical debug, debug_swift

      double precision, dimension(:,:), pointer :: nodesforchi
      integer, dimension(:,:), pointer :: facesforchi

      common /dependent/                                                &
     &  chi2_SKY,                                                       &
     &  chi2_TTV,                                                       &
     &  chi2_RV,                                                        &
     &  chi2_ECL,                                                       &
     &  chi2_VIS,                                                       &
     &  chi2_CLO,                                                       &
     &  chi2_T3,                                                        &
     &  chi2_LC,                                                        &
     &  chi2_SYN,                                                       &
     &  chi2_SED,                                                       &
     &  chi2_AO,                                                        &
     &  chi2_SKY2,                                                      &
     &  chi2_SKY3,                                                      &
     &  chi2_OCC,                                                       &
     &  chi2_AO2,                                                       &
     &  chi2_SED2,                                                      &
     &  chi2_MASS,                                                      &
     &  w_SKY,                                                          &
     &  w_TTV,                                                          &
     &  w_RV,                                                           &
     &  w_ECL,                                                          &
     &  w_VIS,                                                          &
     &  w_CLO,                                                          &
     &  w_T3,                                                           &
     &  w_LC,                                                           &
     &  w_SYN,                                                          &
     &  w_SED,                                                          &
     &  w_AO,                                                           &
     &  w_SKY2,                                                         &
     &  w_SKY3,                                                         &
     &  w_OCC,                                                          &
     &  w_AO2,                                                          &
     &  w_SED2,                                                         &
     &  x_param, variable, lns,                                         &
     &  T0,                                                             &
     &  R_star, T_eff, Lum,                                             &
     &  log_g, v_rot, P_rot,                                            &
     &  metal,                                                          &
     &  Delta_t,                                                        &
     &  C20,                                                            &
     &  pole_l, pole_b,                                                 &
     &  phi0,                                                           &
     &  albedo,                                                         &
     &  scattering,                                                     &
     &  zero,                                                           &
     &  psf_param,                                                      &
     &  d_pc,                                                           &
     &  m_min, m_max,                                                   &
     &  lightcurve_timestep, approx_eclipse_duration,                   &
     &  lambda1, lambda2, lambda3, lambda4,                             &
     &  pyterpol_Delta,                                                 &
     &  silh_factor,                                                    &
     &  spectral_slope,                                                 &
     &  eps_BS,                                                         &
     &  nodesforchi, facesforchi,                                       &
     &  file_SKY,                                                       &
     &  file_TTV,                                                       &
     &  file_RV,                                                        &
     &  file_ECL,                                                       &
     &  file_VIS,                                                       &
     &  file_CLO,                                                       &
     &  file_LC,                                                        &
     &  file_SYN,                                                       &
     &  file_synth,                                                     &
     &  file_SED,                                                       &
     &  file_absol,                                                     &
     &  file_AO,                                                        &
     &  file_SKY2,                                                      &
     &  file_SKY3,                                                      &
     &  file_OCC,                                                       &
     &  file_AO2,                                                       &
     &  file_SED2,                                                      &
     &  tmpdir,                                                         &
     &  ndim, nparam,                                                   &
     &  nbod, nband,                                                    &
     &  iband_LC,                                                       &
     &  geometry,                                                       &
     &  nshp, nsub,                                                     &
     &  n_SKY,                                                          &
     &  n_TTV,                                                          &
     &  n_RV,                                                           &
     &  n_ECL,                                                          &
     &  n_VIS,                                                          &
     &  n_CLO,                                                          &
     &  n_T3,                                                           &
     &  n_LC,                                                           &
     &  n_SYN,                                                          &
     &  n_SED,                                                          &
     &  n_AO,                                                           &
     &  n_SKY2,                                                         &
     &  n_SKY3,                                                         &
     &  n_OCC,                                                          &
     &  n_AO2,                                                          &
     &  n_SED2,                                                         &
     &  n_fit,                                                          &
     &  use_planck,                                                     &
     &  use_filters,                                                    &
     &  use_limbdark,                                                   &
     &  use_pyterpol,                                                   &
     &  use_vardist,                                                    &
     &  use_varpole,                                                    &
     &  use_multipole,                                                  &
     &  use_bruteforce,                                                 &
     &  use_oblat,                                                      &
     &  use_tides,                                                      &
     &  use_tides2,                                                     &
     &  use_ppn,                                                        &
     &  use_polygon,                                                    &
     &  use_zero,                                                       &
     &  use_adam,                                                       &
     &  use_stellar,                                                    &
     &  use_cliptrace,                                                  &
     &  use_hec88,                                                      &
     &  is_forward,                                                     &
     &  debug, debug_swift


