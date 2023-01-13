! test_silh.f90
! Test silhouette computation.
! Miroslav Broz (miroslav.broz@email.cz), Aug 27th 2020

program test_silh

use read_face_module
use read_node_module
use read_pnm_module
use write_pnm_module
use write_silh_module
use silhouette_module

implicit none

integer, dimension(:,:), pointer :: faces
logical, dimension(:), pointer :: masks
double precision, dimension(:,:), pointer :: nodes
double precision, dimension(:,:), pointer :: silh, silh_
double precision, dimension(:,:), pointer :: pnm
double precision :: pixel_scale, factor
double precision, dimension(2) :: c_

call read_node("nodes001.dat", nodes)
call read_face("output.face", faces)

allocate(silh(nsilh,2))
allocate(silh_(nsilh,2))
allocate(masks(size(faces,1)))

masks = .true.

call silhouette(nodes, faces, masks, silh)
call write_silh("test_silh.silh1", silh)

pixel_scale = 3.6d-3  ! arcsec/pxl
pixel_scale = 1.2d-3  ! arcsec/pxl
factor = 0.20d0

call read_pnm("AO1_1.pnm", pnm)
call silhouette2(pnm, factor, silh_)

silh_ = silh_*pixel_scale
c_ = center_silh(silh) - center_silh(silh_)
silh(:,1) = silh(:,1) + c_(1)
silh(:,2) = silh(:,1) + c_(2)
write(*,*) '# c = ', c_

!pnm(256,256) = 65535.d0

call write_pnm("test_silh.pnm2", pnm)
call write_silh("test_silh.silh2", silh_)

call read_pnm("AO1_1_ADAM.pnm", pnm)
call silhouette2(pnm, factor, silh_)

silh_ = silh_*pixel_scale
c_ = center_silh(silh) - center_silh(silh_)
silh(:,1) = silh(:,1) + c_(1)
silh(:,2) = silh(:,1) + c_(2)
write(*,*) '# c = ', c_

call write_pnm("test_silh.pnm3", pnm)
call write_silh("test_silh.silh3", silh_)

end program test_silh


