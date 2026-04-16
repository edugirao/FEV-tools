!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_read                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_init(filename,inpfile)                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(OUT):: filename                                      !!!
CHARACTER(LEN=*),INTENT(IN):: inpfile                                        !!!
INTEGER:: u,ios                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=inpfile,STATUS='OLD',ACTION='READ',IOSTAT=ios)           !!!
IF(ios.ne.0) STOP 'Error while opening inp file'                             !!!
READ(u,*,IOSTAT=ios) filename                                                !!!
IF(ios.ne.0) STOP 'Error while reading inp file'                             !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_init                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_init2(filename1,filename2,inpfile)                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(OUT):: filename1,filename2                           !!!
CHARACTER(LEN=*),INTENT(IN):: inpfile                                        !!!
INTEGER:: u,ios                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=inpfile,STATUS='OLD',ACTION='READ',IOSTAT=ios)           !!!
IF(ios.ne.0) STOP 'Error while opening inp file'                             !!!
READ(u,*,IOSTAT=ios) filename1                                               !!!
IF(ios.ne.0) STOP 'Error while reading 1st filename in inp file'             !!!
READ(u,*,IOSTAT=ios) filename2                                               !!!
IF(ios.ne.0) STOP 'Error while reading 1st filename in inp file'             !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_init2                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_fev(nf,ne,nv,fev,filename)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads the FEV tensor from the                              !!!
! filelabel.fev file                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nf,ne,nv                                             !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: fev(:,:,:)                                 !!!
INTEGER:: i,j,l,u,ios                                                        !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.fev',STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF(ios.ne.0) STOP 'Error while opening .fev file'                            !!!
READ(u,*,IOSTAT=ios) nf,ne,nv                                                !!!
IF(ios.ne.0) STOP 'Error while reading fev size!'                            !!!
ALLOCATE(fev(nf,ne,nv))                                                      !!!
IF(ios.ne.0) STOP 'Error while allocating fev tensor!'                       !!!
DO i=1,nf                   ! - 1st line: nf ne nv                           !!!
  DO j=1,ne                 ! - Then one line per each tensor entry (0 or 1) !!!
    DO l=1,nv                          ! -> Outer loop --> Faces             !!!
      READ(u,*,IOSTAT=ios) fev(i,j,l)  ! -> Middle loop -> Edges             !!!
      IF(ios.ne.0) STOP 'Error while reading a fev element!'                 !!!
    END DO                             ! -> Inner loop --> Vertices          !!!
  END DO                                                                     !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_fev                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_flg(nf,ne,nv,nflags,nface,flag,filename)!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nf,ne,nv,nflags                                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: nface(:)                                   !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: flag(:,:)    !!!
INTEGER:: ios,i,u                                                            !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  ! Opening unformatted flg file                                             !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=ios)
  ! Reading tensor sizes                                                     !!!
  READ(u,IOSTAT=ios) nflags,nf,ne,nv                                         !!!
  IF(ios.ne.0) STOP 'Error while reading flg size in unformatted file!'      !!!
  ALLOCATE(flag(nflags,-3:3),nface(nf)) !!!  
  ! Reading flg data                                                         !!!  
  READ(u,IOSTAT=ios) nface                                                   !!!
  IF(ios.ne.0) STOP 'Error while reading nface in .b.flg file'               !!!
  READ(u,IOSTAT=ios) flag                                                    !!!
  IF(ios.ne.0) STOP 'Error while reading flag in .b.flg file'                !!!
  CLOSE(UNIT=u)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    ! Opening formatted flg file                                             !!!
    OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg',STATUS='OLD',ACTION='READ',IOSTAT=ios)
    ! Reading tensor sizes                                                   !!!    
    READ(u,*,IOSTAT=ios) nflags,nf,ne,nv                                     !!!
    IF(ios.ne.0) STOP 'Error while reading flg size in formatted file!'      !!!  
    ALLOCATE(flag(nflags,-3:3),nface(nf))!!
    ! Reading flg data                                                       !!!      
    READ(u,*,IOSTAT=ios) nface                                               !!!
    IF(ios.ne.0) STOP 'Error while reading nface in .flg file'               !!!
    DO i=1,nflags                                                            !!!
      READ(u,*,IOSTAT=ios) flag(i,:)           !!!
      IF(ios.ne.0) STOP 'Error while reading flags in .flg file'             !!!
    END DO                                                                   !!!
    CLOSE(UNIT=u)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .flg or .b.flg file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_flg                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_cfl(nflags,cflag,filename)!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nflags                                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: cflag(:,:)    !!!
INTEGER:: i,ios,u
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Opening formatted flg file                                             !!!
    OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.cfl',STATUS='OLD',ACTION='READ',IOSTAT=ios)
    ! Reading tensor sizes                                                   !!!    
    READ(u,*,IOSTAT=ios) i                                     !!!
    IF(ios.ne.0) STOP 'Error while reading flg size in .cfl file!'      !!!  
    ALLOCATE(cflag(nflags,2))!!
    DO i=1,nflags                                                            !!!
      READ(u,*,IOSTAT=ios) cflag(i,:)           !!!
      IF(ios.ne.0) STOP 'Error while reading cflags in .cfl file'             !!!
    END DO                                                                   !!!
    CLOSE(UNIT=u)                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_cfl                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_flg_bin(nf,ne,nv,nflags,nface,flag,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nf,ne,nv,nflags                                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: nface(:)                                   !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: flag(:,:)    !!!
INTEGER:: ios,u                                                              !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Opening unformatted flg file                                               !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=ios)
! Reading tensor sizes                                                       !!!
READ(u,IOSTAT=ios) nflags,nf,ne,nv                                           !!!
IF(ios.ne.0) STOP 'Error while reading flg size in unformatted file!'        !!!
ALLOCATE(flag(nflags,-3:3),nface(nf))   !!!  
! Reading flg data                                                           !!!  
READ(u,IOSTAT=ios) nface                                                                !!!
READ(u,IOSTAT=ios) flag                                                                 !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_flg_bin                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_flg_for(nf,ne,nv,nflags,nface,flag,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nf,ne,nv,nflags                                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: nface(:)                                   !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: flag(:,:)    !!!
INTEGER:: ios,i,u                                                            !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Opening formatted flg file                                                 !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg',STATUS='OLD',ACTION='READ',IOSTAT=ios)
! Reading tensor sizes                                                       !!!    
READ(u,*,IOSTAT=ios) nflags,nf,ne,nv                                         !!!
IF(ios.ne.0) STOP 'Error while reading flg size in formatted file!'          !!!  
ALLOCATE(flag(nflags,-3:3),nface(nf))   !!!
! Reading flg data                                                           !!!      
READ(u,*,IOSTAT=ios) nface                                                              !!!
DO i=1,nflags                                                                !!!
  READ(u,*,IOSTAT=ios) flag(i,:)                          !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_flg_for                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_flg_only_nf(nf,filename)                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(OUT):: nf                                                     !!!
INTEGER:: ios,u,i                                                              !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=ios)
  ! Reading tensor sizes                                                     !!!
  READ(u,IOSTAT=ios) i,nf                                                      !!!
  IF(ios.ne.0) STOP 'Error reading nf from flg in unformatted file!'         !!!
  CLOSE(UNIT=u)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg',STATUS='OLD',ACTION='READ',IOSTAT=ios)
    ! Reading tensor sizes                                                   !!!
    READ(u,*,IOSTAT=ios) i,nf                                                  !!!
    IF(ios.ne.0) STOP 'Error reading nf from flg in formatted file!'         !!!
    CLOSE(UNIT=u)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .flg or .b.flg file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_flg_only_nf                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_map(nflags,nmaps,maps,maps_nfixed,filename)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(OUT):: nmaps                                                  !!!
INTEGER,INTENT(IN):: nflags                                                  !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: maps(:,:),maps_nfixed(:)                   !!!
INTEGER:: ios,i,u                                                            !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.map',STATUS='OLD',ACTION='READ',IOSTAT=ios)
READ(u,*,IOSTAT=ios) nmaps                                                   !!!
IF(ios.ne.0) STOP 'Error while reading .map file!'                           !!!  
ALLOCATE(maps(nmaps,nflags),maps_nfixed(nmaps))                              !!!
DO i=1,nmaps                                                                 !!!
  READ(u,*,IOSTAT=ios) maps_nfixed(i)                                                   !!!
  READ(u,*,IOSTAT=ios) maps(i,:)                                                        !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_map                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_group(group,filename)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
CHARACTER(LEN=*),INTENT(OUT):: group                                         !!!
LOGICAL:: have_file_f                                                        !!!
INTEGER:: ios                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.sym',EXIST=have_file_f)              !!!
IF(have_file_f)THEN                                                          !!!
  OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.sym',STATUS='OLD',ACTION='READ',IOSTAT=ios)
  READ(1,*,IOSTAT=ios) group                                                 !!!
  CLOSE(UNIT=1)                                                              !!!
  IF(ios.ne.0)THEN                                                           !!!
    group='x'                                                                !!!
    WRITE(*,*) 'Symmetry .sym corrupted.'                                    !!!
    WRITE(*,*) 'To get symbol with correct lattice type, first run flg2sym.' !!!
  END IF                                                                     !!!
ELSE                                                                         !!!
  group='x'                                                                  !!!
  WRITE(*,*) 'Symmetry .sym file not found.'                                 !!!
  WRITE(*,*) 'To get symbol with correct lattice type, first run flg2sym.'   !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_group                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_nxy(nf,nmax,nface,x_in_f,y_in_f,filename)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf,nmax,nface(nf)                                       !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: x_in_f(:,:),y_in_f(:,:)                     !!!
INTEGER:: i,u,ios                                                            !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading x/y neighbor info from .nxy                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(ALLOCATED(x_in_f)) stop 'aaaaaaa'
IF(ALLOCATED(y_in_f)) stop 'aaaaaaa'
ALLOCATE(x_in_f(nf,nmax),y_in_f(nf,nmax))                                    !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.nxy',STATUS='OLD',ACTION='READ',IOSTAT=ios)
DO i=1,nf                                                                    !!!
  READ(u,*,IOSTAT=ios)                                                       !!!
  READ(u,*,IOSTAT=ios) x_in_f(i,1:nface(i))                                  !!!
  READ(u,*,IOSTAT=ios) y_in_f(i,1:nface(i))                                  !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_nxy                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_xydata(nv,r,filename)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(INOUT):: nv                                                   !!!
INTEGER:: i,u,ios                                                            !!!
REAL(KIND=8),ALLOCATABLE,INTENT(OUT):: r(:,:)                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
CHARACTER*1:: atom                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading xy structure from .xyz                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(ALLOCATED(r)) stop 'aaaaaaa'
ALLOCATE(r(nv,2))                                                            !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.xyz',STATUS='OLD',ACTION='READ',IOSTAT=ios)
READ(u,*,IOSTAT=ios) nv                                                      !!!
READ(u,*,IOSTAT=ios)                                                         !!!
DO i=1,nv                                                                    !!!
  READ(u,*,IOSTAT=ios) atom,r(i,:)                                           !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_xydata                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_igen(nf,igenfile)                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(OUT):: nf                                                     !!!
CHARACTER(LEN=*),INTENT(IN):: igenfile                                       !!!
INTEGER:: u,ios                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=igenfile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
READ(u,*,IOSTAT=ios) nf                                                                 !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_igen                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_gensize(nf,nsys_p)                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf                                                      !!!
INTEGER,INTENT(OUT):: nsys_p                                                 !!!
CHARACTER*100:: gensize_p                                                    !!!
INTEGER:: u,ios                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(gensize_p,'(I0)') nf                                                   !!!
OPEN(NEWUNIT=u,FILE='gensize'//TRIM(ADJUSTL(gensize_p)),STATUS='OLD',ACTION='READ',IOSTAT=ios)
READ(u,*,IOSTAT=ios) nsys_p                                                             !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_gensize                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_genfilenames(nf,nsys_p,gen_filenames,fp_gen)                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf,nsys_p                                               !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: fp_gen(:)                                  !!!
CHARACTER*100,ALLOCATABLE,INTENT(OUT):: gen_filenames(:)                     !!!
CHARACTER*100:: gensize_p                                                    !!!
INTEGER:: u,isys,i,ios                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(gensize_p,'(I0)') nf                                                   !!!
OPEN(NEWUNIT=u,FILE='gensystems'//TRIM(ADJUSTL(gensize_p)),STATUS='OLD',ACTION='READ',IOSTAT=ios)
ALLOCATE(gen_filenames(nsys_p),fp_gen(nsys_p))                               !!!
! Getting parents' filenames, ancestor # & ancestor's dimer added face       !!!
DO isys=1,nsys_p                                                             !!!
  READ(u,*,IOSTAT=ios) gen_filenames(isys),i,fp_gen(isys)                               !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_genfilenames                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_anc(filename_k,filename_p,f0,ie1,ie2)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(OUT):: f0,ie1,ie2                                             !!!
CHARACTER(LEN=*),INTENT(IN):: filename_k                                     !!!
CHARACTER(LEN=*),INTENT(OUT):: filename_p                                    !!!
INTEGER:: u,ios                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting ancestor info                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename_k))//'.anc',STATUS='OLD',ACTION='READ',IOSTAT=ios)
READ(u,*,IOSTAT=ios) filename_p                                                         !!!
READ(u,*,IOSTAT=ios) f0,ie1,ie2                                                         !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_anc                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_anc_list(nf,filename_target,filename_list,i0)                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf                                                      !!!
INTEGER,INTENT(OUT):: i0                                                     !!!
CHARACTER*100,INTENT(IN):: filename_target                                   !!!
CHARACTER*100,ALLOCATABLE,INTENT(OUT):: filename_list(:)                     !!!
INTEGER:: u,i,ios                                                            !!!
LOGICAL:: have_file                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting ancestor info                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(filename_list(nf))                                                  !!!
filename_list(nf)=filename_target                                            !!!
i0=2                                                                         !!!
DO i=nf,2,-1                                                                 !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename_list(i)))//'.anc',STATUS='OLD',ACTION='READ',IOSTAT=ios)
  READ(u,*,IOSTAT=ios) filename_list(i-1)                                    !!!
  CLOSE(UNIT=u)                                                              !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename_list(i-1)))//'.xyz',EXIST=have_file)    !!!
  IF(have_file)THEN                                                          !!!
    i0=i                                                                     !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
IF(i0.eq.2)THEN                                                              !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename_list(1)))//'.xyz',EXIST=have_file)      !!!
  IF(.not.have_file) i0=1                                                    !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE read_anc_list                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_read                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

