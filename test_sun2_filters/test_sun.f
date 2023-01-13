c test_sun.f
c Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2016

      program test_sun

      implicit none
      include 'chi2.inc'

c observational data
      integer m_OBS
      real*8 lambda_eff_OBS(OBSMAX), band_eff_OBS(OBSMAX),
     :  mag_OBS(OBSMAX), sigma_mag_OBS(OBSMAX), calibration(OBSMAX)
      character*80 file_filter(OBSMAX)

c internal
      integer i, j, iu, n, m
      real*8 lambda, band, flux, flux_calibration, mag, chi2, chi2_
      integer m1
      real*8 lambda1(OBSMAX), Int1(OBSMAX)
      integer m2
      real*8 lambda2(OBSMAX), transmit2(OBSMAX)
      logical debug
      real*8 lambda_(OBSMAX), Int_(OBSMAX)

c functions
      real*8 integrate

c read SED data and spectrum

      call read_SED("Sed.dat", m_OBS, lambda_eff_OBS, band_eff_OBS,
     :  mag_OBS, sigma_mag_OBS, calibration, file_filter)

      call read_synth("NewGuey2003.dat", m1, lambda1, Int1)

      debug = .true.
      iu = 15

      if (debug) then
        open(unit=iu,file="chi2_SED.dat",status="unknown")
        write(iu,*) "# lambda [m] & band [m] & mag [mag]",
     :    " & sigma [mag] & chi^2"
      endif

      do i = 1, m_OBS

        lambda = lambda_eff_OBS(i)

        call read_synth(file_filter(i), m2, lambda2, transmit2)

        call filter(m1, lambda1, Int1, m2, lambda2, transmit2,
     :    m, lambda_, Int_)

        flux = integrate(m, lambda_, Int_, lambda_(1), lambda_(m))

        band = integrate(m2, lambda2, transmit2, lambda2(1),
     :    lambda2(m2))

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


