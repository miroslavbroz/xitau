! write_face.f90
! Write file with list of faces.
! Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

module write_face_module

contains

subroutine write_face(f_face,faces,masks)

implicit none

character(len=*), intent(in) :: f_face
integer, dimension(:,:), pointer, intent(in) :: faces
logical, dimension(:), pointer, intent(in), optional :: masks

logical, dimension(:), pointer :: masks_

integer :: i,j,ierr,nfaces,dummy

open(unit=10, file=f_face, status='unknown', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_face 
  stop
endif

nfaces = size(faces,1)
allocate(masks_(nfaces))

if (.not.present(masks)) then
  masks_ = .true.
else
  nfaces = 0
  masks_ = masks
  do i = 1, size(masks)
    if (masks(i)) then
      nfaces = nfaces + 1
    endif
  enddo
endif

dummy = 0
write(10,*) nfaces, dummy

j = 0
do i = 1,size(faces,1)
  if (masks_(i)) then
    j = j+1
    write(10,*,err=900) j, faces(i,1), faces(i,2), faces(i,3), masks_(i)
  endif
enddo

close(unit=10)

return

! error handlers

900 write(*,'(a,a)') '# Error: writing file ', f_face 
stop

end subroutine write_face

end module write_face_module


