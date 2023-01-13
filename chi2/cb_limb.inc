c cb_limb.inc
c Interpolated limb-darkening coefficients u_limb(lambda) vs wavelength
c Miroslav Broz (miroslav.broz@email.cz), Jun 7th 2016

      integer m_limb
      real*8 lambda_limb_(LAMBDAMAX), u_limb_(LAMBDAMAX, NBODMAX)

      common /cb_limb/ lambda_limb_, u_limb_, m_limb

