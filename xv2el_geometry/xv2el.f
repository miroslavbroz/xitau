c xv2el.f
c Convert positions and velocities to orbital elements.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      program xv2el

      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'

c input parameters
      integer id(NBODMAX)
      real*8 t
      real*8 m(NBODMAX)
      real*8 elmts(NBODMAX,6)

c temporary variables
      integer i
      real*8 rb(NBODMAX,3), vb(NBODMAX,3)

c read chi2.in file
      call read_chi2(m)

c write header
      write(*,30)
30    format("# t [JD] & id & P [d] & loge [1] & i [deg]"
     :  " & Omega [deg] & varpi [deg] & lambda [deg]")

c read integration output
5     continue
        do i = 1, nbod
          read(*,*,end=990,err=990) t,id(i),
     :      rb(i,1),rb(i,2),rb(i,3),
     :      vb(i,1),vb(i,2),vb(i,3)
        enddo

        call geometries(nbod, m, rb, vb, elmts, geometry)

c write elements
        do i = 2, nbod
          write(*,*) t, id(i),
     :      elmts(i,1),elmts(i,2),elmts(i,3),
     :      elmts(i,4),elmts(i,5),elmts(i,6)
        enddo
      goto 5

990   continue
      stop
      end


