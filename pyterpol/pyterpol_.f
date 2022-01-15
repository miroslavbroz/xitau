c pyterpol_.f
c A simple Fortran 77 interface to Pyterpol (Nemravova et al. 2016).
c Miroslav Broz (miroslav.broz@email.cz), Jun 23rd 2016

      subroutine pyterpol_(nbod, T_eff, log_g, v_rot, metal,
     :  Delta, lambda1, lambda2, absolute, nbod2)

      implicit none
      include '../simplex/simplex.inc'
c input
      integer nbod
      real*8 T_eff(nbod), log_g(nbod), v_rot(nbod), metal(nbod)
      real*8 Delta(4)
      real*8 lambda1, lambda2
      logical absolute
c output
c  spectra will be saved in 1.syn, 2.syn, ... files
c  (or 1.abs, 2.abs, ...)
      integer nbod2

c Delta(1) ... Delta_T_eff
c Delta(2) ... Delta_log_g
c Delta(3) ... Delta_v_rot
c Delta(4) ... Delta_metal

c constants
      real*8 Ang
      parameter (Ang = 1.d-10)

c internal
      integer i, j, k, iu, ierr
      character*80 cmd
      integer name2(NBODMAX)
      real*8 T_eff1(NBODMAX,2), log_g1(NBODMAX,2), v_rot1(NBODMAX,2),
     :  metal1(NBODMAX,2)
      real*8 T_eff2(NBODMAX), log_g2(NBODMAX), v_rot2(NBODMAX),
     :  metal2(NBODMAX)
c functions
      real*8 Z

      data iu /20/

      integer NBODMAX2
      parameter (NBODMAX2=2*NBODMAX)
      data T_eff1 /NBODMAX2*0.d0/
      data log_g1 /NBODMAX2*0.d0/
      data v_rot1 /NBODMAX2*0.d0/
      data metal1 /NBODMAX2*-10.d0/

      save T_eff1, log_g1, v_rot1, metal1
c
c check if new spectra are needed or NOT
c
      if (absolute) then
        k = 2
      else
        k = 1
      endif

      j = 0
      do i = 1, nbod
        if ((abs(T_eff(i)-T_eff1(i,k)).gt.Delta(1))
     :    .or.(abs(log_g(i)-log_g1(i,k)).gt.Delta(2))
     :    .or.(abs(v_rot(i)-v_rot1(i,k)).gt.Delta(3))
     :    .or.(abs(metal(i)-metal1(i,k)).gt.Delta(4))) then

          j = j+1
          name2(j) = i
          T_eff2(j) = T_eff(i)
          log_g2(j) = log_g(i)
          v_rot2(j) = v_rot(i)
          metal2(j) = metal(i)
        endif

        T_eff1(i,k) = T_eff(i)
        log_g1(i,k) = log_g(i)
        v_rot1(i,k) = v_rot(i)
        metal1(i,k) = metal(i)
      enddo
      nbod2 = j

      if (nbod2.eq.0) then
c        write(*,*) "# pyterpol_.f: No spectra are needed."  ! dbg
        return
      endif
c
c write temporary file
c
      open(unit=iu, file="pyterpol_.in", status="unknown",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "pyterpol_.f: Error opening file 'pyterpol_.in'."
        stop
      endif

      write(iu,*) nbod2
      write(iu,*) (name2(j), j = 1, nbod2)
      write(iu,*) (T_eff2(j), j = 1, nbod2)
      write(iu,*) (log_g2(j), j = 1, nbod2)
      write(iu,*) (v_rot2(j), j = 1, nbod2)
      write(iu,*) (Z(metal2(j)), j = 1, nbod2)
      write(iu,*) lambda1/Ang, lambda2/Ang
      write(iu,*) absolute

      close(iu)
c
c call Pyterpol
c
      cmd = "python3 pyterpol_.py"
      ierr = system(cmd)

      if (ierr.ne.0) then
        write(*,*) "pyterpol_.f: Error running command '",trim(cmd),"'."
        stop
      endif

      return
      end
c
c Convert [M/H] value to Z/Z_solar ratio (approximately)
c
c                      Z/X
c [M/H] = log_10 ---------------
c                Z_solar/X_solar

      real*8 function Z(metal)
      implicit none
      real*8 metal
      Z = 10.d0**metal
      return
      end


