c read_OCC.f90
c Read occultation data.
c Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2022

      subroutine read_OCC(filename, N, t, sigma, lambda, phi, alt,
     :  alpha, delta, prlx, pmra, pmde, epoch, offra, offde,
     :  contact, dataset)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'

      character(len=*) :: filename
      integer :: N
      double precision, dimension(OCCMAX) :: t, sigma, lambda, phi, alt,
     :  alpha, delta, prlx, pmra, pmde, epoch, offra, offde
      integer, dimension(OCCMAX) :: contact, dataset

      integer i,length,ierr
      character*255 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      i = 0
      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_OCC.f: Error opening file '",
     :    trim(filename), "'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OCCMAX) then
            read(str,*,err=20,end=20) t(i),sigma(i),lambda(i),phi(i),
     :        alt(i),alpha(i),delta(i),prlx(i),pmra(i),pmde(i),epoch(i),
     :        offra(i),offde(i),contact(i),dataset(i)
          else
            write(*,*) "read_OCC.f: Error number of observations .gt. ",
     :        "OCCMAX = ", OCCMAX
            stop
          endif

          lambda(i) = lambda(i)*deg
          phi(i) = phi(i)*deg
          alt(i) = alt(i)*1.d3/au
          alpha(i) = alpha(i)*deg
          delta(i) = delta(i)*deg
          prlx(i) = prlx(i)*mas
          pmra(i) = pmra(i)*mas/365.25d0
          pmde(i) = pmde(i)*mas/365.25d0
          offra(i) = offra(i)*mas
          offde(i) = offde(i)*mas
        endif
      goto 5
20    continue
      close(10)

      N = i

      return
      end


