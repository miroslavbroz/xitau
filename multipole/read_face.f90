! read_face.f90
! Read file with list of faces (produced by tetgen) into memory.
! Miroslav Broz (miroslav.broz@email.cz), Jul 31st 2007

module read_face_module

contains

subroutine read_face(f_face,faces)

implicit none

character(len=*) :: f_face
integer, dimension(:,:), pointer :: faces

integer :: i,ierr,nfaces,dummy

open(unit=10, file=f_face, status='old', iostat=ierr)
if (ierr.gt.0) then
  write(*,'(a,a)') '# Error: opening file ', f_face 
  stop
endif

read(10,*) nfaces, dummy

allocate(faces(nfaces,3))

do i = 1, nfaces
  read(10,*,err=900,end=900) dummy, faces(i,1), faces(i,2), faces(i,3)
enddo

close(unit=10, iostat=ierr)
if (ierr.ne.0) then
  write(*,'(a,a)') '# Warning: not closing file ', f_face 
endif

return

! error handlers

900 write(*,'(a,a)') '# Error: reading file ', f_face 
stop

end subroutine read_face

end module read_face_module


