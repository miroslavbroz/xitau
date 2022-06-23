
module hhms_module

contains

subroutine hhms(h2, h, m, s)
double precision, intent(in) :: h2
double precision, intent(out) :: h, m, s
h = aint(h2)
m = aint((h2-h)*60.d0)
s = (h2-h)*3600.d0-m*60.d0
return
end subroutine hhms

end module hhms_module


