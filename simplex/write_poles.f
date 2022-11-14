c write_poles.f
c Write pole orientations.
c Miroslav Broz (miroslav.broz@email.cz), Apr 20th 2022

      subroutine write_poles(t, ecl, ecb, l, b, vardist, dataset)

      implicit none
      include 'simplex.inc'
      include 'dependent.inc'

      integer dataset
      real*8 t, ecl, ecb, l, b, vardist

      real*8 n_cp(3)
      integer iu,i1st
      data iu /15/
      data i1st /0/
      save i1st

      if (.not.debug) return  ! dbg
!      if (.not.debug_swift) return

c centre-pole vector
      n_cp = (/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)

      call uvw(t, ecl, ecb, n_cp(1), n_cp(2), n_cp(3),
     :  n_cp(1), n_cp(2), n_cp(3))

      if (i1st.eq.0) then
        open(unit=iu,file="poles.dat",status="unknown")
        write(iu,*) '# dataset x y z vardist'
        write(iu,*) '# - 1 1 1 au'
        i1st = 1
      else
        open(unit=iu,file="poles.dat",access="append")
      endif
      write(iu,*) dataset,0.d0,0.d0,0.d0,vardist
      write(iu,*) dataset,n_cp(1),n_cp(2),n_cp(3),vardist
      write(iu,*)
      write(iu,*)
      close(iu)

      return
      end


