!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg_to_symmetry                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nflags,nrotmaps,nmirmaps,nglimaps,ntramaps                      !!!
INTEGER:: mirror_count,i                                                     !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),m2(:,:),m1(:,:)                     !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:)                          !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*4:: group                                                          !!!
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
  READ(3) nflags,nf                                                          !!!
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
    READ(3,*) nflags,nf                                                      !!!
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
                                                                             !!!
                                                                             !!!
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
! Identifying map types                                                      !!!
CALL map_types(nflags,flag_color,nrotmaps,nmirmaps,nglimaps,ntramaps,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Extracting symmetries from maps (fev_symm.f90)                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL get_axes(nflags,flag,m1,filename)                                       !!!
CALL get_mirror(nflags,flag,m2,filename)                                     !!!
CALL counting_mirrors(filename,nflags,flag,m2,mirror_count)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying wallpaper group                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL symmetry_group(nrotmaps,nmirmaps,nglimaps,filename,mirror_count,group)  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocations                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(flag,neigh_flag,flag_color,nface,m1,m2)                           !!!
! WRITE(*,'(A)') group                                                       !!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.sym')                            !!!
WRITE(1,'(A)') group                                                         !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg_to_symmetry                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
