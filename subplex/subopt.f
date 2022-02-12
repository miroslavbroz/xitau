c subopt.f
c User-defined subplex options.
c Miroslav Broz (miroslav.broz@email.cz), Feb 12th 2022

      subroutine subopt(n)
      implicit none
      integer n

      include '../subplex/usubc.inc' 

      alpha = 1.d0  ! refection
      beta = .5d0   ! contraction
      gamma = 2.d0  ! expansion
      delta = .5d0  ! shrinkage

      psi = .25d0   ! simplex reduction
      omega = .1d0  ! step reduction
      
      nsmin = min(2,n)  ! min. subspace dimension
      nsmax = min(5,n)  ! max. subspace dimension

      irepl = 0     ! replication 0 .. not, 1 .. subplx, 2 .. user
      ifxsw = 1     ! retain 1 .. mean, 2 .. max, 3 .. min
      bonus = 1.d0  ! bonus
      nfstop = 0    ! stopping, 0 .. not, 1 .. if f(x), 2 .. nfxe > nfstop

      minf = .true.  ! T .. minimum, F .. maximum

      return
      end

