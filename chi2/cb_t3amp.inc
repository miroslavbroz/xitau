c cb_t3amp.inc
c Triple product amplitude data.
c Miroslav Broz (miroslav.broz@email.cz), Apr 5th 2016

c observational data
      integer m_OBS
      real*8 t_OBS(OBSMAX),
     :  u1_OBS(OBSMAX), v1_OBS(OBSMAX),
     :  u2_OBS(OBSMAX), v2_OBS(OBSMAX),
     :  lambda_eff_OBS(OBSMAX), band_eff_OBS(OBSMAX),
     :  t3amp_OBS(OBSMAX), sigma_t3amp_OBS(OBSMAX),
     :  t3phi_OBS(OBSMAX), sigma_t3phi_OBS(OBSMAX)
      integer dataset_OBS(OBSMAX)

c synthetic data
      real*8 t3phi(OBSMAX), t3amp(OBSMAX)

      common /cb_t3amp/ t_OBS, u1_OBS, v1_OBS, u2_OBS, v2_OBS,
     :  lambda_eff_OBS, band_eff_OBS, t3amp_OBS, sigma_t3amp_OBS,
     :  t3phi_OBS, sigma_t3phi_OBS, dataset_OBS, t3phi, t3amp, m_OBS

