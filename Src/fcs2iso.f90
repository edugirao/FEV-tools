!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fcs2iso                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for verify the existence of an Isomorphism relation between a pair of !!!
! structures from their .flg or .b.flg files.                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
USE tools_conv                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf1,ne1,nv1,nflags1,nmax1,nf2,ne2,nv2,nflags2,nmax2                !!!
INTEGER,ALLOCATABLE:: flag1(:,:),nface1(:),neigh_flag1(:,:),flag_color1(:)   !!!
INTEGER,ALLOCATABLE:: flag2(:,:),nface2(:),neigh_flag2(:,:),flag_color2(:)   !!!
INTEGER,ALLOCATABLE:: e_in_f1(:,:),v_in_f1(:,:),e_in_f2(:,:),v_in_f2(:,:)    !!!
CHARACTER*100:: filename1,filename2                                          !!!
LOGICAL:: same                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init2(filename1,filename2,'inp')                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading part of fcs info from an .fcs or .b.fcs file (allocations inside)  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fcs_only_ev_in_f(nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,filename1)!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Create flg from fcs (allocations inside fcs_to_flg)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL fcs_to_flg(nf1,nmax1,nface1,e_in_f1,v_in_f1,nflags1,flag1,neigh_flag1,flag_color1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading part of fcs info from an .fcs or .b.fcs file (allocations inside)  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fcs_only_ev_in_f(nf2,ne2,nv2,nmax2,nface2,e_in_f2,v_in_f2,filename2)!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Create flg from fcs (allocations inside fcs_to_flg)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL fcs_to_flg(nf2,nmax2,nface2,e_in_f2,v_in_f2,nflags2,flag2,neigh_flag2,flag_color2)
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
END PROGRAM fcs2iso                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
