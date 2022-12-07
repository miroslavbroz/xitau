c uvw1.f
c Compute sky-plane base vectors.
c Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

c Reference: Broz & Wolf (2017), Astronomicka mereni, p. 168

      subroutine uvw1(t, l, b, hatu, hatv, hatw)

      use rotate_module

      implicit none
      include 'const.inc'
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'

      real*8 t, l, b
      real*8 hatu(3), hatv(3), hatw(3)

      real*8 c_, s_, eps, zeta
      integer iu,i1st
      data iu /15/
      data i1st /0/
      save i1st

c functions
      real*8 eps_earth

c away from observer
      hatw(1) = cos(l)*cos(b)
      hatw(2) = sin(l)*cos(b)
      hatw(3) = sin(b)

c in (x,y) plane
      hatu(1) = sin(l)
      hatu(2) = -cos(l)
      hatu(3) = 0.d0

c perpendicular, left-handed?!
      call vproduct(hatu, hatw, hatv)

c ecliptic J2000 -> equatorial J2000
      eps = eps_earth(j2000)
      zeta = atan2(sin(eps)*cos(l),
     :  (cos(b)*cos(eps) - sin(b)*sin(eps)*sin(l)))
      hatu = vaxis_rotate(hatu, hatw, -zeta)
      hatv = vaxis_rotate(hatv, hatw, -zeta)

      if (debug_swift) then
        if (i1st.eq.0) then
          open(unit=iu,file="uvw.dat",status="unknown")
          write(iu,*) '# hatu(1) hatu(2) hatu(3) id'
          write(iu,*) '# hatv(1) hatv(2) hatv(3) id'
          write(iu,*) '# hatw(1) hatw(2) hatw(3) id'
          write(iu,*) '# au au au -'
        else
          open(unit=iu,file="uvw.dat",access="append")
        endif
        write(iu,*) 0.d0,0.d0,0.d0,0
        write(iu,*) hatu, 1
        write(iu,*) hatv, 2
        write(iu,*) hatw, 3
        write(iu,*)
        write(iu,*)
        close(iu)

!        if (i1st.eq.0) then
!          open(unit=iu,file="ecliptic.tmp",status="unknown")
!          write(iu,*) '# t eps zeta'
!          write(iu,*) '# JD deg deg'
!        else
!          open(unit=iu,file="ecliptic.dat",access="append")
!        endif
!        write(iu,*) t, eps/deg, zeta/deg
!        close(iu)

        i1st = 1
      endif

c 2DO: during simplex, rewrite uvw.dat for each iteration!

      return
      end


