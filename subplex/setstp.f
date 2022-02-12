      subroutine setstp (nsubs,n,deltax,step)
c
      implicit none
      integer nsubs,n
      double precision deltax(n),step(n)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c setstp sets the stepsizes for the corresponding components
c of the solution vector.
c
c input
c
c   nsubs  - number of subspaces
c
c   n      - number of components (problem dimension)
c
c   deltax - vector of change in solution vector
c
c   step   - stepsizes for corresponding components of
c            solution vector
c
c output
c
c   step   - new stepsizes
c
c common
c
      include 'usubc.inc'
c
c local variables
c
      integer i
      double precision dasum,stpfac
c
      save
c
c subroutines and functions
c
c   blas
      external dasum,dscal
c   fortran
      intrinsic max,min,sign
c
c-----------------------------------------------------------
c
c     set new step
c
      if (nsubs .gt. 1) then
        stpfac = min(max(dasum(n,deltax,1)/dasum(n,step,1),
     *           omega),1.d0/omega)
      else
        stpfac = psi
      end if
      call dscal (n,stpfac,step,1)
c
c     reorient simplex
c
      do 10 i = 1,n
        if (deltax(i) .ne. 0.) then
          step(i) = sign(step(i),deltax(i))
        else
          step(i) = -step(i)
        end if
   10 continue
      return
      end
