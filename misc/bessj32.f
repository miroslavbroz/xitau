c bessj32.f
c Bessel function of the first kind, half-order 3/2.
c Miroslav Broz (miroslav.broz@email.cz), Jun 5th 2016

c 
c Derived from:
c                  2
c J_1/2(x) = sqrt ---- sin x,
c                 pi x
c
c using recurrence relations:
c
c                           2s
c J_{s-1}(x) + J_{s+1}(x) = -- J_s(x),
c                           x
c and:
c           1
c J_s'(x) = - J_{s-1}(x) - J_{s+1}(x).
c           2
c
c Consequently:
c               s
c J_{s+-1}(x) = - J_s(x) -+ J_s'(x),
c               2
c and:
c                 2     sin x
c J_{3/2} = sqrt ---- { ----- - cos x }.
c                pi x     x
c

      real*8 function bessj32(x)
      implicit none
c input
      real*8 x
c internal
      real*8 pi, eps
      parameter(pi = 4.d0*atan(1.d0), eps = 1.d-16)

      if (x.gt.eps) then
        bessj32 = sqrt(2.d0/(pi*x)) * (sin(x)/x - cos(x))
      else
        bessj32 = 0.d0
      endif
      return
      end


