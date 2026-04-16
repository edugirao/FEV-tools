!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM for2bin                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! For the Flag-Graph data of a structure, creates a new .b.flg file   !!!
! out of the .flg file, or vice-versa.                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags                                               !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:)                                     !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! (.flg) <-> (.b.flg) conversion                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)              !!!
IF((have_file_b).AND.(.not.have_file_f))THEN                                 !!!
  CALL read_flg_bin(nf,ne,nv,nflags,nface,flag,filename)!!
  CALL write_flg_for(nf,ne,nv,nflags,nface,flag,filename)!
ELSE IF((.not.have_file_b).AND.(have_file_f))THEN                            !!!
  CALL read_flg_for(nf,ne,nv,nflags,nface,flag,filename)!!
  CALL write_flg_bin(nf,ne,nv,nflags,nface,flag,filename)!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM for2bin                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
