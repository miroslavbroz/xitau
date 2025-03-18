c limcof_interp.f
c A trilinear interpolation of u_limb(lambda) relation,
c   i.e. linear limb-darkening coefficient vs wavelength,
c   which can be used for the final linear interpolation.
c Miroslav Broz (miroslav.broz@email.cz), Jun 2nd 2016

      subroutine limcof_interp(Teff, logg, Z, lambda, u, m)

      implicit none
      include 'limcof.inc'
c input
      real*8 Teff, logg, Z
c output
      integer m
      real*8 lambda(LAMBDAMAX), u(LAMBDAMAX)
c internal
      integer i, j, k, l
      real*8 Z_(2), logg_(2,2), Teff_(2,2,2)
      real*8 u8(LAMBDAMAX,2,2,2)
      real*8 u4(LAMBDAMAX,2,2)
      real*8 u2(LAMBDAMAX,2)
c functions
      real*8 interp

c find closest Z
      j = 1
      do while ((Z_avail(j).lt.Z).and.(j.lt.n_avail))
        j = j+1
      enddo
      if ((j.lt.2).or.(Z_avail(j).lt.Z)) then
        write(*,*) "limcof_interp.f: Extrapolation for Z = ", Z,
     :    " not allowed!"
        stop
      endif
      Z_(1) = Z_avail(j-1)
      Z_(2) = Z_avail(j)

c      write(*,*) "# Z1 = ", Z_(1)
c      write(*,*) "# Z2 = ", Z_(2)

c find closest log g
c (assuming a complete grid in Z vs log g)
      do while ((logg_avail(j).lt.logg).and.(j.lt.n_avail))
        j = j+1
      enddo
      if ((j.lt.2).or.(logg_avail(j).lt.logg)) then
        write(*,*) "limcof_interp.f: Extrapolation for log(g) = ", logg,
     :    " not allowed!"
        stop
      endif
      logg_(1,1) = logg_avail(j-1)
      logg_(1,2) = logg_avail(j)
      logg_(2,1) = logg_avail(j-1)
      logg_(2,2) = logg_avail(j)

c      write(*,*) "# logg1 = ", logg_(1,1)
c      write(*,*) "# logg2 = ", logg_(1,2)
c      write(*,*) "# logg1 = ", logg_(2,1)
c      write(*,*) "# logg2 = ", logg_(2,2)

c find closest T_eff
c (NOT assuming a complete grid, only sorted one)
      k = 1
      do i = 1,2
        do j = 1,2
          do while (Z_avail(k).ne.Z_(i))
            k = k+1
          enddo
          do while (logg_avail(k).ne.logg_(i,j))
            k = k+1
          enddo
          do while ((Teff_avail(k).lt.Teff).and.(k.lt.n_avail))
            k = k+1
          enddo
          if ((k.lt.2).or.(Teff_avail(k).lt.Teff_avail(k-1))) then
            write(*,*) "limcof_interp.f: Extrapolation for Teff = ",
     :        Teff, " not allowed!"
            stop
          endif

          Teff_(i,j,1) = Teff_avail(k-1)
          Teff_(i,j,2) = Teff_avail(k)

c          write(*,*) "# Teff1 = ", Teff_(i,j,1)
c          write(*,*) "# Teff2 = ", Teff_(i,j,2)
        enddo
      enddo

c get all relevant u_limb(lambda) data
      l = 1
      do i = 1,2
        do j = 1,2
          do k = 1,2
            do while (Z_limb(l).lt.Z_(i))
              l = l+1
            enddo
            do while (logg_limb(l).lt.logg_(i,j))
              l = l+1
            enddo
            do while (Teff_limb(l).lt.Teff_(i,j,k))
              l = l+1
            enddo

            m = 0
            do while ((Z_limb(l).eq.Z_(i))
     :        .and.(logg_limb(l).eq.logg_(i,j))
     :        .and.(Teff_limb(l).eq.Teff_(i,j,k)))
              m = m+1
              lambda(m) = lambda_limb(l)  ! wavelengths are always the same
              u8(m,i,j,k) = u_limb(l)
              l = l+1
            enddo

          enddo
        enddo
      enddo

c first linear interpolations
      do l = 1,m
        do i = 1,2
          do j = 1,2
            u4(l,i,j) = interp(Teff_(i,j,1), Teff_(i,j,2),
     :        u8(l,i,j,1), u8(l,i,j,2), Teff)
          enddo
        enddo
      enddo

c second
      do l = 1,m
        do i = 1,2
          u2(l,i) = interp(logg_(i,1), logg_(i,2), u4(l,i,1), u4(l,i,2),
     :      logg)
        enddo
      enddo

c third
      do l = 1,m
        u(l) = interp(Z_(1), Z_(2), u2(l,1), u2(l,2), Z)
      enddo

      return
      end


