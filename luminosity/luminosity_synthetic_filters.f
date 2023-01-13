c luminosity_synthetic_filters.f
c Compute luminosities from temperatures and radii,
c   using absolute synthetic spectra and filter transmissions.
c Miroslav Broz (miroslav.broz@email.cz), Jun 23rd 2016

      subroutine luminosity_synthetic_filters(m_filter, file_filter,
     :  iband, band_filter, Lum_, Lumtot)

      implicit none
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'
      include '../misc/const.inc'
      include 'cb_absol.inc'
c input
c a number of other quantities are passed in /dependent/ common block!
      integer m_filter
      character*(*) file_filter(m_filter)
      integer iband
c output
      real*8 band_filter(m_filter)
      real*8 Lum_(nbod), Lumtot
c filter transmissions
      integer n_filter(SEDMAX)
      real*8 lambda_filter(FLTMAX,SEDMAX), transmit(FLTMAX,SEDMAX)
c internal
      integer j, i1st, nbod2, n_
      real*8 R, T, flux
      real*8 lambda_(OBSMAX), flux_(OBSMAX)
      character*80 str
c functions
      real*8 integrate

      data i1st /0/

      save i1st,
     :  n_filter, lambda_filter, transmit
c
c read ALL filter transmissions (only 1st time!)
c
      if (i1st.eq.0) then

        do j = 1, m_filter

          call read_filter(file_filter(j), n_filter(j),
     :      lambda_filter(1,j), transmit(1,j))

c compute bandpass
          band_filter(j) = integrate(n_filter(j), lambda_filter(1,j),
     :      transmit(1,j), lambda_filter(1,j),
     :      lambda_filter(n_filter(j),j))

          if (debug) then
            write(*,*) "# file_filter(", j, ") = ", trim(file_filter(j))
            write(*,*) "# n_filter(", j, ") = ", n_filter(j)
            write(*,*) "# band_filter(", j, ") = ", band_filter(j), " m"
          endif
        enddo
c
c read synthetic spectra, or...
c
        if (.not.use_pyterpol) then
          do j = 1, nbod
         
            call read_synth(file_absol(j), n_absol(j),
     :        lambda_absol(1,j), flux_absol(1,j))
         
            if (debug) then
              write(*,*) "# n_absol(", j, ") = ", n_absol(j)
            endif
         
          enddo
        endif

        i1st = 1
      endif  ! i1st
c
c generate synthetic spectra using Pyterpol (Nemravova et al. 2016)
c
      if (use_pyterpol) then

        call pyterpol_(nbod, T_eff, log_g, v_rot, metal,
     :    pyterpol_Delta, lambda3, lambda4, .true., nbod2)

c read them back
        if ((nbod2.gt.0).or.(n_absol(1).eq.0)) then
          do j = 1, nbod
            write(str,10) j
10          format(i1, ".abs")
         
            call read_synth(str, n_absol(j), lambda_absol(1,j),
     :        flux_absol(1,j))
         
            if (debug) then
              write(*,*) "# n_absol(", j, ") = ", n_absol(j)
            endif
          enddo
        endif

      endif  ! use_pyterpol
c
c compute the luminosities (for i-th band)
c
      Lumtot = 0.d0

      do j = 1, nbod
        R = R_star(j)*R_S
        T = T_eff(j)

        if (T.gt.0.d0) then

          call filter(n_absol(j), lambda_absol(1,j), flux_absol(1,j),
     :      n_filter(iband), lambda_filter(1,iband), transmit(1,iband),
     :      n_, lambda_, flux_)

          flux = integrate(n_, lambda_, flux_, lambda_filter(1,iband),
     :      lambda_filter(n_filter(iband),iband))

          flux = flux * 1.d-7 * 1.d4 * 1.d10  ! erg s^-1 cm^-2 A^-1 -> J s^-1 m^-2 m^-1
          Lum_(j) = flux * 4.0d0*pi_*R**2
        else
          Lum_(j) = 0.d0
        endif

        Lumtot = Lumtot + Lum_(j)

c        write(*,*) "# Lum_(", j, ") = ", Lum_(j)  ! dbg

      enddo

c      write(*,*) "# Lumtot = ", Lumtot  ! dbg

      return
      end


