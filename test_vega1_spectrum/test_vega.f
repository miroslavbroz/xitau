c test_vega.f
c Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2016

      program test_vega

      implicit none
      include 'simplex.inc'

c observational data
      integer m_OBS
      real*8 lambda_eff_OBS(OBSMAX), band_eff_OBS(OBSMAX),
     :  mag_OBS(OBSMAX), sigma_mag_OBS(OBSMAX), calibration(OBSMAX)
      character*80 file_filter(OBSMAX)

c internal
      integer i, iu, n
      real*8 lambda, band, lambda1, lambda2, flux, mag, chi2, chi2_
      integer m_SPE
      real*8 lambda_SPE(OBSMAX), Int_SPE(OBSMAX)
      logical debug

c functions
      real*8 integrate

c read SED data and spectrum

      call read_SED("Sed.dat", m_OBS, lambda_eff_OBS, band_eff_OBS,
     :  mag_OBS, sigma_mag_OBS, calibration, file_filter)

      call read_synth("alpha_lyr_stis_002.dat", m_SPE, lambda_SPE,
     :  Int_SPE)

      debug = .true.
      iu = 10

      if (debug) then
        open(unit=iu,file="chi2_SED.dat",status="unknown")
        write(iu,*) "# lambda [m] & band [m] & mag [mag]",
     :    " & sigma [mag] & chi^2"
      endif

      do i = 1, m_OBS

        lambda = lambda_eff_OBS(i)
        band = band_eff_OBS(i)
        lambda1 = lambda - band/2.d0
        lambda2 = lambda + band/2.d0

        flux = integrate(m_SPE, lambda_SPE, Int_SPE, lambda1, lambda2)

        mag = -2.5d0*log10(flux/(calibration(i)*band))

        chi2_ = ((mag-mag_OBS(i))/sigma_mag_OBS(i))**2
        chi2 = chi2 + chi2_
        n = n+1

        if (debug) then
          write(iu,*) lambda, band, mag, sigma_mag_OBS(i), chi2_
          write(iu,*) lambda, band, mag_OBS(i), sigma_mag_OBS(i), chi2_
          write(iu,*)
        endif

      enddo

      if (debug) then
        close(iu)
      endif

      stop
      end

