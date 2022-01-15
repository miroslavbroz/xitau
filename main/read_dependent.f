
      subroutine read_dependent()

      include "../simplex/simplex.inc"
      include "../simplex/dependent.inc"

      write(*,*) "# use_multipole : "
      read(*,*,err=990,end=990) use_multipole
      write(*,*) "# use_multipole = ", use_multipole

      write(*,*) "# use_bruteforce : "
      read(*,*,err=990,end=990) use_bruteforce
      write(*,*) "# use_bruteforce = ", use_bruteforce

      write(*,*) "# is_forward : "
      read(*,*,err=990,end=990) is_forward
      write(*,*) "# is_forward = ", is_forward

      return

990   continue
      write(*,*) 'read_dependent: Error reading dependent parameters.'
      stop

      end


