!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE sum_rules(nf,ne,nv,fev,srule_fe,srule_fv,srule_ev)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER:: i,j,k,nf,ne,nv,fev(nf,ne,nv)
INTEGER:: srule_fe(nv),srule_fv(ne),srule_ev(nf)
! Edge-Vertex rule
DO i=1,nf
  srule_ev(i)=SUM(fev(i,:,:))
END DO
! Face-Vertex rule
DO j=1,ne
  srule_fv(j)=SUM(fev(:,j,:))
END DO
! Face-Edge rule
DO k=1,nv
  srule_fe(k)=SUM(fev(:,:,k))
END DO
END SUBROUTINE sum_rules

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE faces_sizes(nf,nflags,flag,nface) ! b**
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.
! Also gets number of sides of each face.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER:: i,nf,nflags,flag(nflags,3),nface(nf)
! Counting flags for each face
nface=0
DO i=1,nflags
  nface(flag(i,1))=nface(flag(i,1))+1
END DO
! Removing double counting
nface=nface/2
END SUBROUTINE faces_sizes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE matrix_reduction_ev(nf,ne,nv,fev,reduc_ev)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the EV matrix reduction.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER:: j,k,nf,ne,nv,fev(nf,ne,nv),reduc_ev(ne,nv)
! Edge-Vertex reduction
reduc_ev=0
DO j=1,ne
DO k=1,nv
  reduc_ev(j,k)=SUM(fev(:,j,k))
END DO
END DO
END SUBROUTINE matrix_reduction_ev

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

