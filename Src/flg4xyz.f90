!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg4xyz                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
USE tools_conv                                                               !!!
USE tools_maps                                                               !!!
USE tools_make                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,k,nf,ne,nv,nflags,nmax,nmaps                                   !!!
INTEGER,ALLOCATABLE:: neigh(:,:),edge(:,:),cell(:,:,:),nedge(:,:)            !!!
INTEGER,ALLOCATABLE:: face_edges(:,:),face_verts(:,:),nface(:),fev(:,:,:)    !!!
INTEGER,ALLOCATABLE:: flag(:,:),neigh_flag(:,:),flag_color(:)                !!!
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)                    !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:)                               !!!
LOGICAL,ALLOCATABLE:: uneq_face(:)                                           !!!
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:)                                !!!
REAL(KIND=8):: acc,tol,a1(3),a2(3)                                           !!!
REAL(KIND=8),ALLOCATABLE:: r(:,:)                                            !!!
CHARACTER*100:: filename,string                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading Structural Data                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL reading(filename,nv,r,a1,a2)                                            !!!
acc=1.42                           ! Carbon-carbon distance                  !!!
tol=0.35                           ! Tolerance for bond distance             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating neighbors                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL neighbors(nv,r,a1,a2,acc,tol,neigh,cell)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating edges                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL make_edges(nv,neigh,cell,ne,edge,nedge)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating faces                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL faces_maker(nv,ne,nf,neigh,cell,nedge,nface,face_edges,face_verts)      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating the flag graph                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL flg_maker(nf,nv,nface,face_edges,face_verts,nflags,flag,neigh_flag,flag_color)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing the flag graph                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL write_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting faces/edges/vertices/bridges in faces                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nmax=MAXVAL(nface)                                                           !!!
CALL fev_in_faces(nflags,flag,neigh_flag,nf,nface,f_in_f,e_in_f,v_in_f,b_in_f,nmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating symmetry maps (allocations inside)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
CALL flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,nmaps,maps,maps_nfixed)!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting unequivalent faces and edges                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(uneq_face(nf),u_in_f(nf,nmax))                                      !!!
CALL neq_fe(nflags,flag,nf,ne,nface,nmax,e_in_f,uneq_face,u_in_f,nmaps,maps) !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing faces/edges info on the .fcs file                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
CALL write_fcs(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &         !!!
                                                 & b_in_f,u_in_f,filename)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating FEV Tensor                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL flg_to_fev(nflags,flag,nf,ne,nv,fev)                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing the fev tensor                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
CALL write_fev(nf,ne,nv,fev,filename)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Faces data                                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(*,'(A40)') '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'                  !!!
WRITE(*,'(A13)') 'List of Faces'                                             !!!
WRITE(*,'(A40)') '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'                  !!!
DO i=1,nf                                                                    !!!
  WRITE(string,'(I3)') nface(i)                                              !!!
  string='(I2,A5,2X,A9,'//TRIM(ADJUSTL(string))//'I3)'                       !!!
  WRITE(*,TRIM(ADJUSTL(string))) nface(i),'-ring','Edges -> ',(face_edges(i,j),j=1,nface(i))
END DO                                                                       !!!
DO i=1,nf                                                                    !!!
  WRITE(string,'(I3)') nface(i)                                              !!!
  string='(I2,A5,2X,A9,'//TRIM(ADJUSTL(string))//'I3)'                       !!!
  WRITE(*,TRIM(ADJUSTL(string))) nface(i),'-ring','Verts -> ',(face_verts(i,j),j=1,nface(i))
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! FEV Rules                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Face-Vertex rule                                                           !!!
WRITE(*,*) 'Face-Vertex rule test'                                           !!!
DO j=1,ne                                                                    !!!
  WRITE(*,*) j,SUM(fev(:,j,:))                                               !!!
END DO                                                                       !!!
! Face-Edge rule                                                             !!!
WRITE(*,*) 'Face-Edge rule test'                                             !!!
DO k=1,nv                                                                    !!!
  WRITE(*,*) k,SUM(fev(:,:,k))                                               !!!
END DO                                                                       !!!
! Edge-Vertex rule                                                           !!!
WRITE(*,*) 'Edge-Vertex rule test'                                           !!!
DO i=1,nf                                                                    !!!
  WRITE(*,*) i,SUM(fev(i,:,:))                                               !!!
END DO                                                                       !!!
! Euler Characteristic                                                       !!!
WRITE(*,*) 'Euler Characteristic'                                            !!!
WRITE(*,*) nf-ne+nv                                                          !!!
! Number of nonzero tensor elements                                          !!!
WRITE(*,*) 'Number of nonzero FEV tensor elements'                           !!!
WRITE(*,*) SUM(fev)                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg4xyz                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

