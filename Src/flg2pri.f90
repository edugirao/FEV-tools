!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM primitive_reduction                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nflags,ntramaps,fi,ei,vi,ne,nv,nf2,nflags2,ne2,nv2              !!!
INTEGER:: n,i,j,l,nmaps,nfixed                                               !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),m2(:,:),m1(:,:)                     !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:),mapa(:),map(:,:)         !!!
INTEGER,ALLOCATABLE:: face(:),edge(:),vertex(:),fev(:,:,:)                   !!!
INTEGER,ALLOCATABLE:: f2f(:),e2e(:),v2v(:)                                   !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL,ALLOCATABLE:: ok_f(:),ok_e(:),ok_v(:)                                !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading                                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
CALL read_init(filename)                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading flag graph and faces sizes                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')     !!!
  READ(3) nflags,nf,ne,nv                                                    !!!
  ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags),nface(nf)) !!!
  READ(3) nface                                                              !!!
  READ(3) flag                                                               !!!
  READ(3) neigh_flag                                                         !!!
  READ(3) flag_color                                                         !!!
  CLOSE(UNIT=3)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.flg')                        !!!
    READ(3,*) nflags,nf,ne,nv                                                !!!
    ALLOCATE(flag(nflags,3),neigh_flag(nflags,3))                            !!!
    ALLOCATE(flag_color(nflags),nface(nf))                                   !!!
    READ(3,*) nface                                                          !!!
    DO i=1,nflags                                                            !!!
      READ(3,*) flag(i,:),neigh_flag(i,:),flag_color(i)                      !!!
    END DO                                                                   !!!
    CLOSE(UNIT=3)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .flg or .b.flg file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Neighbors matrices (fev_flag.f90)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! First Neighbors                                                            !!!
ALLOCATE(m2(nflags,nflags))                                                  !!!
CALL first_neighbors(nflags,neigh_flag,m2)                                   !!!
! Second Neighbors                                                           !!!
ALLOCATE(m1(nflags,nflags))                                                  !!!
CALL second_neighbors(nflags,flag,m2,m1,flag_color)                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating maps (fev_maps.f90)                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Searching for the maps                                                     !!!
CALL creating_maps(nflags,flag,neigh_flag,m1,m2,nf,nface,filename)           !!!
! Selecting only translation maps                                            !!!
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')                            !!!
READ(8,*) nmaps                                                              !!!
ALLOCATE(mapa(nflags),map(nmaps,nflags))                                     !!!
ntramaps=0                                                                   !!!
DO n=1,nmaps                                                                 !!!
  READ(8,*) nfixed                                                           !!!
  READ(8,*) mapa                                                             !!!
  IF(nfixed.eq.0)THEN                                                        !!!
    IF(flag_color(1)*flag_color(mapa(1)).eq.1)THEN                           !!!
      ntramaps=ntramaps+1                                                    !!!
      map(ntramaps,:)=mapa                                                   !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
CLOSE(UNIT=8)                                                                !!!
! New sizes                                                                  !!!
nflags2=nflags/(ntramaps+1)                                                  !!!
nf2=nf/(ntramaps+1)                                                          !!!
ne2=ne/(ntramaps+1)                                                          !!!
nv2=nv/(ntramaps+1)                                                          !!!
                                                                             !!!
IF(ntramaps.eq.0)THEN                                                        !!!
  WRITE(*,'(A)') 'Structure in the .flg file is already a primitive cell.'   !!!
ELSE                                                                         !!!
  WRITE(*,'(A)') 'Structure in the .flg file is a conventional cell.'        !!!
END IF                                                                       !!!
                                                                             !!!
                                                                             !!!
ALLOCATE(face(nf),edge(ne),vertex(nv))                                       !!!
ALLOCATE(ok_f(nf),ok_e(ne),ok_v(nv))                                         !!!
ALLOCATE(f2f(nf),e2e(ne),v2v(nv))                                            !!!
! ok indicates if the element has been marked already                        !!!
ok_f=.false.                                                                 !!!
ok_e=.false.                                                                 !!!
ok_v=.false.                                                                 !!!
fi=0                                                                         !!!
ei=0                                                                         !!!
vi=0                                                                         !!!
f2f=0                                                                        !!!
e2e=0                                                                        !!!
v2v=0                                                                        !!!
DO i=1,nflags                                                                !!!
  ! If face not marked yet                                                   !!!
  IF(.not.ok_f(flag(i,1)))THEN                                               !!!
    fi=fi+1                                                                  !!!
    ! Get the list of equivalent faces                                       !!!
    l=1                                                                      !!!
    face(l)=flag(i,1)                                                        !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      face(l)=flag(map(j,i),1)                                               !!!
    END DO                                                                   !!!
    ! Set these faces as marked                                              !!!
    ok_f(face(1:l))=.true.                                                   !!!
    ! Map them all to fi                                                     !!!
    f2f(face(1:l))=fi                                                        !!!
  END IF                                                                     !!!
  ! If edge not marked yet                                                   !!!
  IF(.not.ok_e(flag(i,2)))THEN                                               !!!
    ei=ei+1                                                                  !!!
    ! Get the list of equivalent edges                                       !!!
    l=1                                                                      !!!
    edge(l)=flag(i,2)                                                        !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      edge(l)=flag(map(j,i),2)                                               !!!
    END DO                                                                   !!!
    ! Set these edges as marked                                              !!!
    ok_e(edge(1:l))=.true.                                                   !!!
    ! Map them all to ei                                                     !!!
    e2e(edge(1:l))=ei                                                        !!!
  END IF                                                                     !!!
  ! If vertex not marked yet                                                 !!!
  IF(.not.ok_v(flag(i,3)))THEN                                               !!!
    vi=vi+1                                                                  !!!
    ! Get the list of equivalent vertices                                    !!!
    l=1                                                                      !!!
    vertex(l)=flag(i,3)                                                      !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      vertex(l)=flag(map(j,i),3)                                             !!!
    END DO                                                                   !!!
    ! Set these vertices as marked                                           !!!
    ok_v(vertex(1:l))=.true.                                                 !!!
    ! Map them all to vi                                                     !!!
    v2v(vertex(1:l))=vi                                                      !!!
  END IF                                                                     !!!
END DO                                                                       !!!
                                                                             !!!
! Relabelling flags                                                          !!!
flag(:,1)=f2f(flag(:,1))                                                     !!!
flag(:,2)=e2e(flag(:,2))                                                     !!!
flag(:,3)=v2v(flag(:,3))                                                     !!!
                                                                             !!!
! Building primitive FEV tensor                                              !!!
ALLOCATE(fev(nf2,ne2,nv2))                                                   !!!
fev=0                                                                        !!!
DO i=1,nflags                                                                !!!
  fev(flag(i,1),flag(i,2),flag(i,3))=1                                       !!!
END DO                                                                       !!!
                                                                             !!!
                                                                             !!!
OPEN(UNIT=1,FILE='primitive.fev')                                            !!!
WRITE(1,*) nf2,ne2,nv2                                                       !!!
DO i=1,nf2                                                                   !!!
DO j=1,ne2                                                                   !!!
DO l=1,nv2                                                                   !!!
  WRITE(1,*) fev(i,j,l)                                                      !!!
END DO                                                                       !!!
END DO                                                                       !!!
END DO                                                                       !!!
CLOSE(UNIT=1)                                                                !!!
                                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocations                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(flag)                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM primitive_reduction                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
