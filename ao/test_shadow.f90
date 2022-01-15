! test_shadow.f90
! Test shadowing computation.
! Miroslav Broz (miroslav.broz@email.cz), Aug 27th 2020

program test_shadow

use read_face_module
use read_node_module
use write_face_module
use write_silh_module
use normalize_module
use normal_module
use shadowing_module
use silhouette_module

implicit none

integer, dimension(:,:), pointer :: faces
logical, dimension(:), pointer :: masks
double precision, dimension(:,:), pointer :: nodes
double precision, dimension(:,:), pointer :: normals
double precision, dimension(:,:), pointer :: silh
double precision, dimension(3) :: n_to, n_ts

call read_node("nodes001.dat", nodes)
call read_face("output.face", faces)

allocate(normals(size(faces,1),3))
allocate(masks(size(faces,1)))
allocate(silh(nsilh,2))

n_to = (/0.d0, 0.d0, 1.d0/)
n_ts = normalize((/0.d0, 0.d0, 1.d0/))
!n_ts = normalize((/0.d0, 0.5d0, 0.5d0/))

write(*,*) 'n_to = ', n_to
write(*,*) 'n_ts = ', n_ts

open(unit=10,file='nxyz.dat',status='unknown')
write(10,*) 0.d0, 0.d0, 0.d0, 1
write(10,*) n_to, 1
write(10,*)
write(10,*) 0.d0, 0.d0, 0.d0, 2
write(10,*) n_ts, 2
write(10,*)
close(10)

call normal(faces, nodes, normals)
call shadowing(normals, n_to, n_ts, masks)

call write_face("test_shadow.face", faces, masks)

call silhouette(nodes, faces, masks, silh)
call write_silh("test_shadow.silh", silh)

end program test_shadow


