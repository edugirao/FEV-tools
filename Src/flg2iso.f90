!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2iso                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for verify the existence of an isomorphism relation between           !!!
! a pair of structures from their .flg or .b.flg files.                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
USE tools_conv                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf1,nflags1,ne1,nv1,nf2,nflags2,ne2,nv2                !!!
INTEGER,ALLOCATABLE:: flag1(:,:),nface1(:)   !!!
INTEGER,ALLOCATABLE:: flag2(:,:),nface2(:)   !!!
CHARACTER*100:: filename1,filename2                                          !!!
LOGICAL:: same,flgf                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init2(filename1,filename2,'inp')                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Where to read the first system's data                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename1))//'.flg',EXIST=flgf)                     !!!
IF(.not.flgf) INQUIRE(FILE=TRIM(ADJUSTL(filename1))//'.b.flg',EXIST=flgf)       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph or obtaining it from faces-info                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(flgf)THEN                                                                  !!!
  ! Reading the flag graph from a .flg or a .b.flg file                      !!!
  WRITE(*,*) 'Reading system 1 from a .b.flg or a .flg file.'                !!!
  CALL read_flg(nf1,ne1,nv1,nflags1,nface1,flag1,filename1)
ELSE                                                                         !!!
  STOP 'No flag-graph file found for system 1!'                !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Where to read the second system's data                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename2))//'.flg',EXIST=flgf)                     !!!
IF(.not.flgf) INQUIRE(FILE=TRIM(ADJUSTL(filename2))//'.b.flg',EXIST=flgf)       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph or obtaining it from faces-info                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(flgf)THEN                                                                  !!!
  ! Reading the flag graph from a .flg or a .b.flg file                      !!!
  WRITE(*,*) 'Reading system 2 from a .b.flg or a .flg file.'                !!!
  CALL read_flg(nf2,ne2,nv2,nflags2,nface2,flag2,filename2)
ELSE                                                                         !!!
  STOP 'No flag-graph file found for system 2!'                !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Isomorphism check                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF((nf1.eq.nf2).AND.(nflags1.eq.nflags2))THEN                                !!!
  CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1, &           !!!
                         & nflags2,nf2,flag2,nface2,same)        !!!
  IF(same)THEN                                                               !!!
    WRITE(*,'(A)') 'Isomorphic structures.'                                  !!!
    WRITE(*,'(A)') 'Structures with the same cell size.'                     !!!
  ELSE                                                                       !!!
    WRITE(*,'(A)') 'Non-isomorphic structures.'                              !!!
  END IF                                                                     !!!
ELSE                                                                         !!!
  IF(nf1.gt.nf2)THEN                                                         !!!
    IF(MOD(nf1,nf2).ne.0)THEN                                                !!!
      WRITE(*,'(A)') 'Non-isomorphic structures.'                            !!!
      WRITE(*,'(A)') 'Incomensurable cell sizes.'                            !!!
      STOP                                                                   !!!
    END IF                                                                   !!!
    CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1, &         !!!
                          &  nflags2,nf2,flag2,nface2,same)      !!!
    IF(same)THEN                                                             !!!
      WRITE(*,'(A)') 'Isomorphic structures.'                                !!!
      WRITE(*,'(A,I0,A)') TRIM(ADJUSTL(filename1))//' is a NxM=',nf1/nf2, &  !!!
        &' supercell of '//TRIM(ADJUSTL(filename2))                          !!!
    ELSE                                                                     !!!
      WRITE(*,'(A)') 'Non-isomorphic structures.'                            !!!
    END IF                                                                   !!!
  ELSE                                                                       !!!
    IF(MOD(nf2,nf1).ne.0)THEN                                                !!!
      WRITE(*,'(A)') 'Non-isomorphic structures.'                            !!!
      WRITE(*,'(A)') 'Incomensurable cell sizes.'                            !!!
      STOP                                                                   !!!
    END IF                                                                   !!!
    CALL check_equiv_fgraphs(nflags2,nf2,flag2,nface2, &         !!!
                          &  nflags1,nf1,flag1,nface1,same)      !!!
    IF(same)THEN                                                             !!!
      WRITE(*,'(A)') 'Isomorphic structures.'                                !!!
      WRITE(*,'(A,I0,A)') TRIM(ADJUSTL(filename2))//' is a NxM=',nf2/nf1, &  !!!
        &  ' supercell of '//TRIM(ADJUSTL(filename1))                        !!!
    ELSE                                                                     !!!
      WRITE(*,'(A)') 'Non-isomorphic structures.'                            !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2iso                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
