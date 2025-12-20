PROGRAM isomorphism_check

IMPLICIT NONE
INTEGER:: nf1,nflags1,ne1,nv1,nf2,nflags2,ne2,nv2,i
INTEGER,ALLOCATABLE:: flag1(:,:),nface1(:),flag2(:,:),nface2(:)
INTEGER,ALLOCATABLE:: neigh_flag1(:,:),neigh_flag2(:,:)
CHARACTER*100:: filename1,filename2
LOGICAL:: same,have_file_b,have_file_f
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file
CALL read_init2(filename1,filename2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading flag graph and faces sizes for system 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename1))//'.b.flg',EXIST=have_file_b)
IF(have_file_b)THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename1))//'.b.flg',FORM='UNFORMATTED')
  READ(3) nflags1,nf1,ne1,nv1
  ALLOCATE(flag1(nflags1,3),neigh_flag1(nflags1,3),nface1(nf1))    
  READ(3) nface1
  READ(3) flag1
  READ(3) neigh_flag1
  CLOSE(UNIT=3)          
ELSE
  INQUIRE(FILE=TRIM(ADJUSTL(filename1))//'.flg',EXIST=have_file_f)
  IF(have_file_f)THEN  
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename1))//'.flg')
    READ(3,*) nflags1,nf1,ne1,nv1
    ALLOCATE(flag1(nflags1,3),neigh_flag1(nflags1,3),nface1(nf1))
    READ(3,*) nface1  
    DO i=1,nflags1
      READ(3,*) flag1(i,:),neigh_flag1(i,:)
    END DO  
    CLOSE(UNIT=3)
  ELSE
    STOP 'Error: no .flg or .b.flg file found'
  END IF
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading flag graph and faces sizes for system 2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename2))//'.b.flg',EXIST=have_file_b)
IF(have_file_b)THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename2))//'.b.flg',FORM='UNFORMATTED')
  READ(3) nflags2,nf2,ne2,nv2
  ALLOCATE(flag2(nflags2,3),neigh_flag2(nflags2,3),nface2(nf2))    
  READ(3) nface2
  READ(3) flag2
  READ(3) neigh_flag2
  CLOSE(UNIT=3)          
ELSE
  INQUIRE(FILE=TRIM(ADJUSTL(filename2))//'.flg',EXIST=have_file_f)
  IF(have_file_f)THEN  
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename2))//'.flg')
    READ(3,*) nflags2,nf2,ne2,nv2
    ALLOCATE(flag2(nflags2,3),neigh_flag2(nflags2,3),nface2(nf2))
    READ(3,*) nface2
    DO i=1,nflags2
      READ(3,*) flag2(i,:),neigh_flag2(i,:)
    END DO  
    CLOSE(UNIT=3)
  ELSE
    STOP 'Error: no .flg or .b.flg file found'
  END IF
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Isomorphism check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF((nf1.eq.nf2).AND.(nflags1.eq.nflags2))THEN
  CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1,neigh_flag1, &
                                   & flag2,nface2,neigh_flag2,same)
  IF(same)THEN
    WRITE(*,'(A)') 'Isomorphic structures.'
    WRITE(*,'(A)') 'Structures with the same cell size.'
  ELSE
    WRITE(*,'(A)') 'Non-isomorphic structures.'
  END IF
ELSE
  IF(nf1.gt.nf2)THEN
    IF(MOD(nf1,nf2).ne.0)THEN
      WRITE(*,'(A)') 'Non-isomorphic structures.'
      WRITE(*,'(A)') 'Incomensurable cell sizes.'
      STOP
    END IF
    CALL check_equiv_fgraphs_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &
                              &  nflags2,nf2,flag2,nface2,neigh_flag2,same)
    IF(same)THEN
      WRITE(*,'(A)') 'Isomorphic structures.'
      WRITE(*,'(A,I0,A)') TRIM(ADJUSTL(filename1))//' is a NxM=',nf1/nf2,' supercell of '//TRIM(ADJUSTL(filename2))
    ELSE
      WRITE(*,'(A)') 'Non-isomorphic structures.'
    END IF                              
  ELSE
    IF(MOD(nf2,nf1).ne.0)THEN
      WRITE(*,'(A)') 'Non-isomorphic structures.'
      WRITE(*,'(A)') 'Incomensurable cell sizes.'
      STOP
    END IF  
    CALL check_equiv_fgraphs_dif(nflags2,nf2,flag2,nface2,neigh_flag2, &
                              &  nflags1,nf1,flag1,nface1,neigh_flag1,same)
    IF(same)THEN
      WRITE(*,'(A)') 'Isomorphic structures.'
      WRITE(*,'(A,I0,A)') TRIM(ADJUSTL(filename2))//' is a NxM=',nf2/nf1,' supercell of '//TRIM(ADJUSTL(filename1))      
    ELSE
      WRITE(*,'(A)') 'Non-isomorphic structures.'
    END IF                              
  END IF
END IF
END PROGRAM isomorphism_check

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
