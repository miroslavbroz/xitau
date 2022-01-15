c luminosity_synthetic_bandpass.f
c Compute luminosities from temperatures and radii,
c   using absolute synthetic spectra and effective bandpass.
c Miroslav Broz (miroslav.broz@email.cz), Jun 23rd 2016

      subroutine luminosity_synthetic_bandpass(T_eff_, R_star_, nbod_,
     :  lambda, band, Lum_, Lumtot)

      implicit none
      include 'simplex.inc'
      include 'dependent.inc'
      include '../misc/const.inc'
      include 'cb_absol.inc'
c input
c a number of other quantities are passed in /dependent/ common block!
c note these variables are dashed_, because they are also in the c.b.
      integer nbod_
      real*8 T_eff_(nbod), R_star_(nbod), lambda, band
c output
      real*8 Lum_(nbod), Lumtot
c internal
      integer j, i1st, nbod2
      real*8 R, T, flux
      character*80 str
c functions
      real*8 integrate

      data i1st /0/
      data n_absol /NBODMAX*0/

      save i1st
c
c read synthetic spectra (only 1st time!)
c
      if (i1st.eq.0) then

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
c compute the luminosities
c
      Lumtot = 0.d0

      do j = 1, nbod_
        R = R_star_(j)*R_S
        T = T_eff_(j)

        if (T.gt.0.d0) then

          flux = integrate(n_absol(j), lambda_absol(1,j),
     :      flux_absol(1,j), lambda-band/2.d0, lambda+band/2.d0)

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


