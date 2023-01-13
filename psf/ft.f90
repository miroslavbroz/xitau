! ft.f90
! A 2-dimensional discrete Fourier transform (a stupid algorithm).
! Miroslav Broz (miroslav.broz@email.cz), Dec 15th 2009

! see Numerical Recipes, Sec. 12-1 for 1D:

! H(f_n) = \int_-\infty^\infty h(t) exp(2 pi i f_n t  ) dt
!        =~       \Sum_k=0^N-1 h_k  exp(2 pi i f_n t_k) Delta
!        =~ Delta \Sum_k=0^N-1 h_k  exp(2 pi i n/N k  )
!        =  H_n

! n=0   .. zero frequency
! n<N/2 .. positive frequencies
! n=N/2 .. f = f_c = 1/(2 Delta) is Nyquist critical frequency
! n>N/2 .. negative frequencies!

! see NR, Sec. 12-4 for 2D:

! H(i, j) = \Sum_l=0^N_2-1 \Sum_k=0^N_1-1 exp(2 pi i l j/N_2) exp(2 pi i k i/N_1) h(k, l)
!         = \Sum_l=0^N_2-1 exp(2 pi i l j/N_2) \Sum_k=0^N_1-1 exp(2 pi i k i/N_1) h(k, l)

module ft_module

contains

subroutine ft(h, capH)

implicit none
double precision, parameter :: pi = 4.d0*atan(1.d0)
double complex, dimension(:,:) :: h, capH

integer i, j, k, l, n, m
double precision :: f, g
double complex s1, s2

n = size(h, 1)
m = size(h, 2)

! Note: f = dble(i-1)/n would be sufficient due to periodicity.

do i = 1, n

  if (i <= n/2+1) then
    f = dble(i-1)/n
  else
    f = dble(i-1-n)/n
  endif

  do j = 1, m

    if (j <= m/2+1) then
      g = dble(j-1)/m
    else
      g = dble(j-1-m)/m
    endif

!    write(*,*) 'i-1 = ', i-1, ' f = ', f, ' 1/f = ', 1.d0/f  ! dbg
!    write(*,*) 'j-1 = ', j-1, ' g = ', g, ' 1/g = ', 1.d0/g  ! dbg

    s2 = cmplx(0.d0, 0.d0)
    do l = 1, m
      s1 = cmplx(0.d0, 0.d0)
      do k = 1, n
        s1 = s1 + exp(cmplx(0.d0, 2.d0*pi*f*(k-1))) * h(k, l)
      enddo
      s2 = s2 + exp(cmplx(0.d0, 2.d0*pi*g*(l-1))) * s1
    enddo

    capH(i, j) = s2

  enddo
enddo

return
end subroutine ft

end module ft_module


