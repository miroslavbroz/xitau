! write_kml.f90
! Write KML file for Google maps.
! Miroslav Broz (miroslav.broz@email.cz), Jun 28th 2022

module write_kml_module

use const_module

integer, parameter :: OCCMAX2 = 1000

integer, parameter, private :: NBODMAX = 5
integer, save, private :: i, lastj
double precision, parameter, private :: NAN = -999.d0
double precision, dimension(OCCMAX2,NBODMAX), save, private :: lambda = NAN, phi = NAN

contains

subroutine to_kml(lambda_, phi_, j)

implicit none
double precision, intent(in) :: lambda_, phi_
integer, intent(in) :: j

if (j.ne.lastj) then
  i = 0
endif
i = i+1
lastj = j

lambda(i,j) = lambda_
phi(i,j) = phi_

return
end subroutine to_kml

subroutine write_kml(filename)

implicit none
character(len=*), intent(in) :: filename

integer, parameter :: iu=20
integer :: i, j
character(len=80) :: fmt

open(unit=iu, file=filename, status='unknown')
fmt='(a)'
write(iu,fmt) '<?xml version="1.0" encoding="UTF-8"?>'
write(iu,fmt) '<kml xmlns="http://www.opengis.net/kml/2.2">'
write(iu,fmt) '<Document>'
write(iu,fmt) '  <description>Occultation computed w. Xitau (http://sirrah.troja.mff.cuni.cz/~mira/xitau/)</description>'
write(iu,fmt) '  <Style id="1"><LineStyle><color>ff00ff00</color><width>1.0</width></LineStyle></Style>'
write(iu,fmt) '  <Style id="2"><LineStyle><color>ff0000ff</color><width>1.0</width></LineStyle></Style>'
write(iu,fmt) '  <Style id="3"><LineStyle><color>ff0099ff</color><width>1.0</width></LineStyle></Style>'

do j = 1, NBODMAX
  if (lambda(1,j).eq.NAN) then
    continue
  endif
  write(iu,fmt) '  <Placemark>'
  write(iu,'(a,i0,a)') '    <styleUrl>', j, '</styleUrl>'
  write(iu,fmt) '    <LineString>'
  write(iu,fmt) '      <extrude>true</extrude>'
  write(iu,fmt) '      <tessellate>true</tessellate>'
  write(iu,fmt) '      <altitudeMode>clampToGround</altitudeMode>'
  write(iu,fmt) '      <coordinates>'

  do i = 1, OCCMAX2
    if (lambda(i,j).eq.NAN) then
      exit
    endif
    write(iu,'(f0.10,a,f0.10,a,i0)') lambda(i,j)/deg, ',', phi(i,j)/deg, ',', 0
  enddo
 
  write(iu,fmt) '      </coordinates>'
  write(iu,fmt) '    </LineString>'
  write(iu,fmt) '  </Placemark>'

enddo

write(iu,fmt) '</Document>'
write(iu,fmt) '</kml>'
close(iu)

j = 0
lastj = 0
lambda = NAN
phi = NAN

return
end subroutine write_kml

end module write_kml_module

