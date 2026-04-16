!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2svg                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_make                                                               !!!
USE tools_msvg                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf,ne,nv,nflags,ii,jj,f                                        !!!
INTEGER,ALLOCATABLE:: neigh(:,:),edge(:,:),cell(:,:,:),nedge(:,:)            !!!
INTEGER,ALLOCATABLE:: face_edges(:,:),face_verts(:,:),nface(:),fcell(:,:,:)  !!!
INTEGER,ALLOCATABLE:: flag(:,:)                !!!
REAL(KIND=8):: acc,tol,a1(3),a2(3),v(3),u(3),c(4,3),drf,dff,ls,s             !!!
REAL(KIND=8):: x1,x2,y1,y2,rad,rs                                            !!!
REAL(KIND=8),ALLOCATABLE:: r(:,:),rflag(:,:)                                 !!!
CHARACTER*100:: filename                                                     !!!
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
CALL flg_maker(nf,nv,nface,face_edges,face_verts,nflags,flag)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Ccomputing flag position vectors                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL rflg_maker(nv,nflags,flag,neigh,cell,nedge,a1,a2,r,rflag,fcell)         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing the flag graph                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                
! Plot parameters                                                            !!!
CALL svg_params(a1,a2,c,ls,s,x1,x2,y1,y2)                                    !!!
! Distance parameters                                                        !!!
CALL svg_dist(nv,r,nflags,flag,rflag,drf,dff)                     !!!
! Opening input file                                                         !!!
OPEN(NEWUNIT=f,FILE=TRIM(ADJUSTL(filename))//'.svg')                         !!!
! SVG header                                                                 !!!
CALL svg_header(x1,y1,ls,s,f)                                                !!!
! Making atomic structure edges                                              !!!
DO ii=-3,3                                                                   !!!
  DO jj=-3,3                                                                 !!!
    DO i=1,nv                                                                !!!
      v=(r(i,:)+ii*a1+jj*a2)*s                                               !!!
      DO j=1,3                                                               !!!
        u=(r(neigh(i,j),:)+(ii+cell(i,j,1))*a1+(jj+cell(i,j,2))*a2)*s        !!!
        rad=0.025*s                                                          !!!
        CALL make_svg_line(v,u,x1,x2,y1,y2,rad,'black',f)                    !!!
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Making atomic structure vertices                                           !!!
DO ii=-3,3                                                                   !!!
  DO jj=-3,3                                                                 !!!
    DO i=1,nv                                                                !!!
      v=(r(i,:)+ii*a1+jj*a2)*s                                               !!!
      rad=drf*0.5D0*s                                                        !!!
      rs=rad*0.1D0                                                           !!!
      CALL make_svg_circle(v,x1,x2,y1,y2,rad,rs,'gray','black',f)            !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Making flag-graph edges                                                    !!!
DO ii=-3,3                                                                   !!!
  DO jj=-3,3                                                                 !!!
    DO i=1,nflags                                                            !!!
      v=(rflag(i,:)+ii*a1+jj*a2)*s                                           !!!
      DO j=1,3                                                               !!!
        u=(rflag(flag(i,-j),:)+(ii+fcell(i,j,1))*a1+(jj+fcell(i,j,2))*a2)*s
        rad=0.1*s                                                            !!!
        IF(j.eq.1)THEN                                                       !!!
          CALL make_svg_line(v,u,x1,x2,y1,y2,rad,'lime',f)                   !!!
        ELSE IF(j.eq.2)THEN                                                  !!!
          CALL make_svg_line(v,u,x1,x2,y1,y2,rad,'red',f)                    !!!
        ELSE IF(j.eq.3)THEN                                                  !!!
          CALL make_svg_line(v,u,x1,x2,y1,y2,rad,'blue',f)                   !!!
        END IF                                                               !!!
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Making flag-graph vertices                                                 !!!
DO ii=-3,3                                                                   !!!
  DO jj=-3,3                                                                 !!!
    DO i=1,nflags                                                            !!!
      v=(rflag(i,:)+ii*a1+jj*a2)*s                                           !!!
      rad=drf*0.3*s                                                          !!!
      rs=rad*0.1D0                                                           !!!
      IF(flag(i,0).eq.1)THEN                                             !!!
        CALL make_svg_circle(v,x1,x2,y1,y2,rad,rs,'black','black',f)         !!!
      ELSE IF(flag(i,0).eq.-1)THEN                                       !!!
        CALL make_svg_circle(v,x1,x2,y1,y2,rad,rs,'white','black',f)         !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Unit cell                                                                  !!!
write(f,*) '<polygon'                                                        !!!
write(f,*) 'points="'                                                        !!!
write(f,*) c(1,1)*s,',',c(1,2)*s                                             !!!
write(f,*) c(2,1)*s,',',c(2,2)*s                                             !!!
write(f,*) c(4,1)*s,',',c(4,2)*s                                             !!!
write(f,*) c(3,1)*s,',',c(3,2)*s                                             !!!
write(f,*) '"'                                                               !!!
write(f,*) 'style="fill:none;stroke:purple;stroke-width:10"'                 !!!
write(f,*) '/>'                                                              !!!
! Frame                                                                      !!!
write(f,*) '<rect'                                                           !!!
write(f,*) 'x="',x1+5,'"'                                                    !!!
write(f,*) 'y="',y1+5,'"'                                                    !!!
write(f,*) 'width="',ls*s-10,'"'                                             !!!
write(f,*) 'height="',ls*s-10,'"'                                            !!!
write(f,*) 'fill="none"'                                                     !!!
write(f,*) 'style="stroke:black;stroke-width:',10,'"'                        !!!
write(f,*) '/>'                                                              !!!
! Closing svg                                                                !!!
write(f,*) ' </svg>'                                                         !!!
CLOSE(UNIT=f)                                                                !!!                
! Making pdf                                                                 !!!
CALL SYSTEM('inkscape --export-type=pdf '//TRIM(ADJUSTL(filename))//'.svg -o '//TRIM(ADJUSTL(filename))//'.pdf')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2svg                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

