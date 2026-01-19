!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_init(filename)                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE='inp')                                                      !!!
READ(1,*) filename                                                           !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_init                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_init2(filename1,filename2)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER*100:: filename1,filename2                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE='inp')                                                      !!!
READ(1,*) filename1                                                          !!!
READ(1,*) filename2                                                          !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_init2                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_fev_size(nf,ne,nv,filename)                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads the FEV tensor dimensions                            !!!
! (nf, ne, and nv) from the filelabel.fev                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv                                                           !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Number of elements from each type                                          !!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.fev')                            !!!
READ(1,*) nf,ne,nv                                                           !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_fev_size                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_fev(nf,ne,nv,fev,filename)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads the FEV tensor from the                              !!!
! filelabel.fev file                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,fev(nf,ne,nv),i,j,l                                       !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.fev')                            !!!
READ(1,*) nf,ne,nv          ! .fev file format:                              !!!
DO i=1,nf                   ! - 1st line: nf ne nv                           !!!
  DO j=1,ne                 ! - Then one line per each tensor entry (0 or 1) !!!
    DO l=1,nv               ! -> Outer loop --> Faces                        !!!
      READ(1,*) fev(i,j,l)  ! -> Middle loop -> Edges                        !!!
    END DO                  ! -> Inner loop --> Vertices                     !!!
  END DO                    !                                                !!!
END DO                      !                                                !!!
CLOSE(UNIT=1)               !                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_fev                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_fcs_size(nf,ne,nv,nmax,filename)                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nmax                                                      !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading fev size from .fcs                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')     !!!
  READ(3) nf,ne,nv,nmax                                                      !!!
  CLOSE(UNIT=3)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')                        !!!
    READ(3,*) nf,ne,nv,nmax                                                  !!!
    CLOSE(UNIT=3)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .fcs or .b.fcs file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_fcs_size                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_fev_in_f(nf,nmax,nface,f_in_f,e_in_f,v_in_f,filename)        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,nmax,nface(nf)                                                !!!
INTEGER:: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax)                    !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading fev in f from .fcs                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')     !!!
  READ(3)                                                                    !!!
  DO i=1,nf                                                                  !!!
    READ(3) nface(i)!,uneq_face(i)                                           !!!
    READ(3) f_in_f(i,1:nface(i))                                             !!!
    READ(3) e_in_f(i,1:nface(i))                                             !!!
    READ(3) v_in_f(i,1:nface(i))                                             !!!
    READ(3) !b_in_f(i,1:nface(i))                                            !!!
    READ(3) !u_in_f(i,1:nface(i))                                            !!!
  END DO                                                                     !!!
  CLOSE(UNIT=3)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')                        !!!
    READ(3,*)                                                                !!!
    DO i=1,nf                                                                !!!
      READ(3,*) nface(i)!,uneq_face(i)                                       !!!
      READ(3,*) f_in_f(i,1:nface(i))                                         !!!
      READ(3,*) e_in_f(i,1:nface(i))                                         !!!
      READ(3,*) v_in_f(i,1:nface(i))                                         !!!
      READ(3,*) !b_in_f(i,1:nface(i))                                        !!!
      READ(3,*) !u_in_f(i,1:nface(i))                                        !!!
    END DO                                                                   !!!
    CLOSE(UNIT=3)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .fcs or .b.fcs file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_fev_in_f                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_nxy(nf,nmax,nface,x_in_f,y_in_f,filename)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,nmax,nface(nf)                                                !!!
INTEGER:: x_in_f(nf,nmax),y_in_f(nf,nmax)                                    !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading x/y neighbor info from .nxy                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.nxy')                            !!!
DO i=1,nf                                                                    !!!
  READ(3,*) nface(i)                                                         !!!
  READ(3,*) x_in_f(i,1:nface(i))                                             !!!
  READ(3,*) y_in_f(i,1:nface(i))                                             !!!
END DO                                                                       !!!
CLOSE(UNIT=3)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_nxy                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_xydata(nv,r,filename)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nv                                                               !!!
REAL(KIND=8):: r(nv,2)                                                       !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*1:: atom                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading xy structure from .xyz                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.xyz')                            !!!
READ(3,*) nv                                                                 !!!
READ(3,*)                                                                    !!!
DO i=1,nv                                                                    !!!
  READ(3,*) atom,r(i,:)                                                      !!!
END DO                                                                       !!!
CLOSE(UNIT=3)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_xydata                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
