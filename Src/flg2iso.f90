!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2iso                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for verify the existence of an Isomorphism relation between a pair of !!!
! structures from their .flg or .b.flg files.                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf1,nflags1,ne1,nv1,nf2,nflags2,ne2,nv2                            !!!
INTEGER,ALLOCATABLE:: flag1(:,:),nface1(:),flag2(:,:),nface2(:)              !!!
INTEGER,ALLOCATABLE:: neigh_flag1(:,:),neigh_flag2(:,:)                      !!!
INTEGER,ALLOCATABLE:: flag_color1(:),flag_color2(:)                          !!!
CHARACTER*100:: filename1,filename2                                          !!!
LOGICAL:: same                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init2(filename1,filename2,'inp')                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph for system1 from a .flg or a .b.flg file            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_flg(nf1,ne1,nv1,nflags1,nface1,flag1,neigh_flag1,flag_color1,filename1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph for system 2 from a .flg or a .b.flg file           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_flg(nf2,ne2,nv2,nflags2,nface2,flag2,neigh_flag2,flag_color2,filename2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Isomorphism check                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF((nf1.eq.nf2).AND.(nflags1.eq.nflags2))THEN                                !!!
  CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1,neigh_flag1, &           !!!
                         & nflags2,nf2,flag2,nface2,neigh_flag2,same)        !!!
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
    CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1,neigh_flag1, &         !!!
                          &  nflags2,nf2,flag2,nface2,neigh_flag2,same)      !!!
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
    CALL check_equiv_fgraphs(nflags2,nf2,flag2,nface2,neigh_flag2, &         !!!
                          &  nflags1,nf1,flag1,nface1,neigh_flag1,same)      !!!
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
