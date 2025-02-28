c chi2_func_SED2.f
c Calculate chi^2 for spectral energy distribution, individual ones.
c Mirolav Broz (miroslav.broz@email.cz), Jan 21st 2025

c Note: Doesn't work with chi2_func_SED.f due to 2nd call of luminosity_synthetic_filters.f!

      subroutine chi2_func_SED2(chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'chi2.inc'
      include 'dependent.inc'

c input (only in dependent.inc)
c output
      real*8 chi2
      integer n

c observational data
      integer m_OBS
      integer one(SEDMAX)
      real*8 lambda_eff_OBS(SEDMAX), band_eff_OBS(SEDMAX),
     :  mag_OBS(SEDMAX), sigma_mag_OBS(SEDMAX), calibration(SEDMAX)
      character*80 file_filter(SEDMAX)

c internal variables
      integer i, k, i1st, iu
      real*8 d, lambda, band, flux, fluxtot, mag, chi2_
      real*8 band_filter(SEDMAX)
      real*8 Lum_lambda(NBODMAX), Lumtot

c functions
      integer length

      data i1st /0/
      data iu /15/

      save i1st,
     :  m_OBS, lambda_eff_OBS, band_eff_OBS, mag_OBS, sigma_mag_OBS,
     :  one, calibration, file_filter, band_filter
c
c read SED observations (only 1st time!)
c
      if (i1st.eq.0) then

        call read_SED2(file_SED2, m_OBS, lambda_eff_OBS, band_eff_OBS,
     :    mag_OBS, sigma_mag_OBS, one, calibration, file_filter)

        if (debug) then
          write(*,*) "# m_SED2 = ", m_OBS
        endif

        i1st = 1
      endif  ! i1st
c
c chi^2 for SED data
c
      if (debug) then
        open(unit=iu,file="chi2_SED2.dat",status="unknown")
        write(iu,*) "# lambda [m] & band [m] & mag [mag] & sigma [mag]",
     :    " & calibration flux F_lambda [J s^-1 m^-2 m^-1] & filter",
     :    " & chi^2"
      endif

      d = d_pc*pc  ! [m]

      chi2 = 0.d0
      n = 0

      do i = 1, m_OBS

        lambda = lambda_eff_OBS(i)
        band = band_eff_OBS(i)
c
c decide how to compute the luminosity
c
        if ((use_planck).and.(.not.use_filters)) then
          call luminosity_planck_bandpass(T_eff, R_star, nbod,
     :      lambda, band, Lum_lambda, Lumtot)

        else if ((.not.use_planck).and.((.not.use_filters)
     :    .or.(file_filter(i)(1:1).eq.'-'))) then
          call luminosity_synthetic_bandpass(T_eff, R_star, nbod,
     :      lambda, band, Lum_lambda, Lumtot)

        else if ((.not.use_planck).and.(use_filters)) then
          call luminosity_synthetic_filters2(m_OBS, file_filter, i,
     :      band_filter, Lum_lambda, Lumtot)

          band = band_filter(i)
        else
          write(*,*) "chi2_func_SED.f: Error: use_planck = ",
     :      use_planck, " and use_filters = ", use_filters,
     :      " is NOT supported."
        endif

        flux = Lum_lambda(one(i))/(4.d0*pi_*d**2)
        fluxtot = Lumtot/(4.d0*pi_*d**2)
        mag = -2.5d0*log10(flux/(fluxtot-flux))

        chi2_ = ((mag-mag_OBS(i))/sigma_mag_OBS(i))**2
        lns = lns + log(sigma_mag_OBS(i))
        chi2 = chi2 + chi2_
        n = n+1

        if (debug) then
          write(iu,*) lambda, band, mag, sigma_mag_OBS(i),
     :      calibration(i), trim(file_filter(i)), chi2_
          write(iu,*) lambda, band, mag_OBS(i), sigma_mag_OBS(i),
     :      calibration(i), trim(file_filter(i)), chi2_
          write(iu,*)
        endif

      enddo

      if (debug) then
        close(iu)
      endif

      return
      end


