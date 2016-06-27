c chi2.inc
c Include file for chi2 subroutines.
c Miroslav Broz (miroslav.broz@email.cz), Jun 23rd 2016

      integer ndim, nparam
      integer n_SKY, n_TTV, n_RV, n_ECL, n_VIS, n_CLO,
     :  n_T3, n_LC, n_SYN, n_SED, n_fit
      real*8 chi2_SKY, chi2_TTV, chi2_RV, chi2_ECL, chi2_VIS, chi2_CLO,
     :  chi2_T3, chi2_LC, chi2_SYN, chi2_SED, chi2_MASS
      real*8 w_SKY, w_TTV, w_RV, w_ECL, w_VIS, w_CLO,
     :  w_T3, w_LC, w_SYN, w_SED

      real*8 x_param(NDIMMAX)
      logical variable(NDIMMAX)

      integer nbod, nband
      integer iband_LC(BANDMAX)
      integer geometry
      real*8 T0, d_pc
      real*8 R_star(NBODMAX), T_eff(NBODMAX), Lum(NBODMAX)
      real*8 m_min(NBODMAX), m_max(NBODMAX)
      real*8 lightcurve_timestep, approx_eclipse_duration
      real*8 lambda1, lambda2, lambda3, lambda4
      real*8 zero(BANDMAX)
      real*8 log_g(NBODMAX), v_rot(NBODMAX), metal(NBODMAX)
      real*8 eps_BS

      character*80 file_SKY(NBODMAX), file_TTV, file_RV(NBODMAX),
     :  file_ECL, file_VIS, file_CLO, file_LC(BANDMAX),
     :  file_SYN, file_synth(NBODMAX), file_SED, file_absol(NBODMAX)

      logical use_planck, use_filters, use_limbdark, use_pyterpol,
     :  debug, debug_swift

      common /dependent/
     :  chi2_SKY, chi2_TTV, chi2_RV, chi2_ECL, chi2_VIS, chi2_CLO,
     :  chi2_T3, chi2_LC, chi2_SYN, chi2_SED, chi2_MASS,
     :  w_SKY, w_TTV, w_RV, w_ECL, w_VIS, w_CLO,
     :  w_T3, w_LC, w_SYN, w_SED,
     :  x_param, variable,
     :  T0, d_pc, R_star, T_eff, Lum, m_min, m_max,
     :  lightcurve_timestep, approx_eclipse_duration,
     :  lambda1, lambda2, lambda3, lambda4,
     :  zero,
     :  log_g, v_rot, metal,
     :  eps_BS,
     :  file_SKY, file_TTV, file_RV, file_ECL, file_VIS, file_CLO,
     :  file_LC, file_SYN, file_synth, file_SED, file_absol,
     :  ndim, nparam, nbod, nband, iband_LC, geometry,
     :  n_SKY, n_TTV, n_RV, n_ECL, n_VIS, n_CLO,
     :  n_T3, n_LC, n_SYN, n_SED, n_fit,
     :  use_planck, use_filters, use_limbdark, use_pyterpol,
     :  debug, debug_swift


