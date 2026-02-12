!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM for2bin                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! For Flag-Graph and Faces-Info data of a structure, creates a new .b.flg or !!! 
! .b.fcs file out of the .flg or .fcs files, or vice-versa.                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags,nmax                                               !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:)                                     !!!
INTEGER,ALLOCATABLE:: neigh_flag(:,:),flag_color(:)                          !!!
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)                    !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
LOGICAL,ALLOCATABLE:: uneq_face(:)                                           !!!
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:)                                !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! (.fcs) <-> (.b.fcs) conversion                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)            !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)              !!!
IF((have_file_b).AND.(.not.have_file_f))THEN                                 !!!
  CALL read_fcs_bin(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &    !!!
                                                  b_in_f,u_in_f,filename)    !!!
  CALL write_fcs_for(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &   !!!
                                                 & b_in_f,u_in_f,filename)   !!!
  DEALLOCATE(nface,uneq_face,f_in_f,e_in_f,v_in_f,b_in_f,u_in_f)             !!!  
ELSE IF((.not.have_file_b).AND.(have_file_f))THEN                            !!!
  CALL read_fcs_for(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &    !!!
                                                  b_in_f,u_in_f,filename)    !!!
  CALL write_fcs_bin(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &   !!!
                                                 & b_in_f,u_in_f,filename)   !!!
  DEALLOCATE(nface,uneq_face,f_in_f,e_in_f,v_in_f,b_in_f,u_in_f)             !!!    
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! (.flg) <-> (.b.flg) conversion                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)              !!!
IF((have_file_b).AND.(.not.have_file_f))THEN                                 !!!
  CALL read_flg_bin(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)!!
  CALL write_flg_for(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)!
ELSE IF((.not.have_file_b).AND.(have_file_f))THEN                            !!!
  CALL read_flg_for(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)!!
  CALL write_flg_bin(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM for2bin                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
