c read_dependent_bs.f
c Read dependent parameters for swift_bs integrator.
c Miroslav Broz (miroslav.broz@email.cz), Feb 12th 2022

      subroutine read_dependent_bs()

      include "../simplex/simplex.inc"
      include "../simplex/dependent.inc"

      write(*,*) "# use_multipole : "
      read(*,*,err=990,end=990) use_multipole
      write(*,*) "# use_multipole = ", use_multipole

      write(*,*) "# use_bruteforce : "
      read(*,*,err=990,end=990) use_bruteforce
      write(*,*) "# use_bruteforce = ", use_bruteforce

      read(*,*,err=990,end=990) use_oblat
      write(*,*) "# use_oblat = ", use_oblat

      read(*,*,err=990,end=990) use_tides
      write(*,*) "# use_tides = ", use_tides

      read(*,*,err=990,end=990) use_tides2
      write(*,*) "# use_tides2 = ", use_tides2

      read(*,*,err=990,end=990) use_ppn
      write(*,*) "# use_ppn = ", use_ppn

      write(*,*) "# is_forward : "
      read(*,*,err=990,end=990) is_forward
      write(*,*) "# is_forward = ", is_forward

      return

990   continue
      write(*,*) 'read_dependent: Error reading dependent parameters.'
      stop

      end


