!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_symb                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE pre_symbol(nf,nmax,nface,f_in_f,npol,string_s)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.                             !!!
! Also gets number of sides of each face.                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,npol(3:100)                                                   !!!
INTEGER:: j,l,n,m,l1,l2,nmax                                                 !!!
INTEGER:: f_in_f(nf,nmax),nface(nf),av_n(nf),tmp(nmax)                       !!!
LOGICAL:: first                                                              !!!
CHARACTER*4:: aux                                                            !!!
CHARACTER*200,ALLOCATABLE:: string_s(:)                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating face neighbor size matrix and auxiliary symbols                   !!!
DO i=1,nf                                                                    !!!
  f_in_f(i,1:nface(i))=nface(f_in_f(i,1:nface(i)))                           !!!
END DO                                                                       !!!
! Computing average neighbor size                                            !!!
av_n=0                                                                       !!!
DO j=1,nf                                                                    !!!
  DO i=1,nface(j)                                                            !!!
    av_n(j)=av_n(j)+f_in_f(j,i)                                              !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Reordering level 1 - face size                                             !!!
DO i=1,nf-1                                                                  !!!
  DO j=1,nf-i                                                                !!!
    IF(nface(j).gt.nface(j+1))THEN                                           !!!
      m=nface(j)                                                             !!!
      nface(j)=nface(j+1)                                                    !!!
      nface(j+1)=m                                                           !!!
      m=av_n(j)                                                              !!!
      av_n(j)=av_n(j+1)                                                      !!!
      av_n(j+1)=m                                                            !!!
      tmp=f_in_f(j,:)                                                        !!!
      f_in_f(j,:)=f_in_f(j+1,:)                                              !!!
      f_in_f(j+1,:)=tmp                                                      !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Reordering level 2 - neighbor faces sizes                                  !!!
DO n=1,nf                                                                    !!!
  DO i=1,nface(n)-1                                                          !!!
    DO j=1,nface(n)-i                                                        !!!
      IF(f_in_f(n,j).gt.f_in_f(n,j+1))THEN                                   !!!
        m=f_in_f(n,j)                                                        !!!
        f_in_f(n,j)=f_in_f(n,j+1)                                            !!!
        f_in_f(n,j+1)=m                                                      !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Reordering level 3 - average neigh size                                    !!!
l1=1                                                                         !!!
l2=1                                                                         !!!
DO l=2,nf                                                                    !!!
  IF(nface(l).eq.nface(l-1))THEN                                             !!!
    l2=l2+1                                                                  !!!
  END IF                                                                     !!!
  IF((nface(l).ne.nface(l-1)).OR.(l.eq.nf))THEN                              !!!
    ! Reorder the faces from l1 to l2                                        !!!
    IF(l2-l1.gt.0)THEN                                                       !!!
      DO i=l1,l2-1                                                           !!!
        DO j=l1,l2-(i-l1+1)                                                  !!!
          IF(av_n(j).gt.av_n(j+1))THEN                                       !!!
            m=av_n(j)                                                        !!!
            av_n(j)=av_n(j+1)                                                !!!
            av_n(j+1)=m                                                      !!!
            tmp=f_in_f(j,:)                                                  !!!
            f_in_f(j,:)=f_in_f(j+1,:)                                        !!!
            f_in_f(j+1,:)=tmp                                                !!!
          END IF                                                             !!!
        END DO                                                               !!!
      END DO                                                                 !!!
    END IF                                                                   !!!
    l1=l                                                                     !!!
    l2=l                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Reordering level 4 - smallest neigh size                                   !!!
n=1                                                                          !!!
l1=1                                                                         !!!
l2=1                                                                         !!!
DO l=2,nf                                                                    !!!
  IF((nface(l).eq.nface(l-1)).AND.(av_n(l).eq.av_n(l-1)))THEN                !!!
    l2=l2+1                                                                  !!!
  END IF                                                                     !!!
  IF(((nface(l).ne.nface(l-1)).OR.(av_n(l).ne.av_n(l-1))).OR.(l.eq.nf))THEN  !!!
    ! Reorder the faces from l1 to l2                                        !!!
    IF(l2-l1.gt.0)THEN                                                       !!!
      DO i=l1,l2-1                                                           !!!
        DO j=l1,l2-(i-l1+1)                                                  !!!
          IF(f_in_f(j,n).ge.f_in_f(j+1,n))THEN                               !!!
            tmp=f_in_f(j,:)                                                  !!!
            f_in_f(j,:)=f_in_f(j+1,:)                                        !!!
            f_in_f(j+1,:)=tmp                                                !!!
          END IF                                                             !!!
        END DO                                                               !!!
      END DO                                                                 !!!
    END IF                                                                   !!!
    l1=l                                                                     !!!
    l2=l                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! S aux symbol for each face                                                 !!!
ALLOCATE(string_s(nf))                                                       !!!
DO j=1,nf                                                                    !!!
  npol=0                                                                     !!!
  DO i=1,nface(j)                                                            !!!
    npol(f_in_f(j,i))=npol(f_in_f(j,i))+1                                    !!!
  END DO                                                                     !!!
  first=.true.                                                               !!!
  string_s(j)=''                                                             !!!
  DO i=3,100                                                                 !!!
    IF(npol(i).gt.0)THEN                                                     !!!
      WRITE(aux,'(I4)') i                                                    !!!
      string_s(j)=TRIM(ADJUSTL(string_s(j)))//TRIM(ADJUSTL(aux))//'^'        !!!
      WRITE(aux,'(I4)') npol(i)                                              !!!
      string_s(j)=TRIM(ADJUSTL(string_s(j)))//'{'//TRIM(ADJUSTL(aux))//'}'   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Counting polygons for the whole structure                                  !!!
npol=0                                                                       !!!
DO i=1,nf                                                                    !!!
  npol(nface(i))=npol(nface(i))+1                                            !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE pre_symbol                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_lattice_type(group,syst)                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.                             !!!
! Also gets number of sides of each face.                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER*1:: syst                                                           !!!
CHARACTER*4:: group                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting the system                                                         !!!
syst='x'                                                                     !!!
IF(TRIM(ADJUSTL(group)).eq.'p6m' ) syst='h'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p6'  ) syst='h'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p31m') syst='h'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p3m1') syst='h'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p3'  ) syst='h'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p4m' ) syst='t'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p4g' ) syst='t'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p4'  ) syst='t'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p2'  ) syst='o'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'cmm' ) syst='c'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'pmm' ) syst='r'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'pgg' ) syst='r'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'pmg' ) syst='r'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'pg'  ) syst='r'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'cm'  ) syst='c'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'pm'  ) syst='r'                                  !!!
IF(TRIM(ADJUSTL(group)).eq.'p1'  ) syst='o'                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_lattice_type                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE symbol1(syst,npol,filename,string)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.                             !!!
! Also gets number of sides of each face.                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,npol(3:100)                                                      !!!
CHARACTER*1:: syst                                                           !!!
CHARACTER*4::       aux                                                      !!!
CHARACTER*4000:: string                                                      !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: first                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Mounting the symbol                                                        !!!
string=TRIM(ADJUSTL(syst))                                                   !!!
first=.true.                                                                 !!!
DO i=3,100                                                                   !!!
  IF(npol(i).gt.0)THEN                                                       !!!
    WRITE(aux,'(I4)') i                                                      !!!
    string=TRIM(ADJUSTL(string))//TRIM(ADJUSTL(aux))//'^'                    !!!
    WRITE(aux,'(I4)') npol(i)                                                !!!
    string=TRIM(ADJUSTL(string))//'{'//TRIM(ADJUSTL(aux))//'}'               !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Creating a latex-based pdf                                                 !!!
OPEN(UNIT=4,FILE='tex.tex')                                                  !!!
WRITE(4,'(A)') '\documentclass[crop]{standalone}'                            !!!
WRITE(4,'(A)') '\begin{document}'                                            !!!
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string))//'$'                               !!!
WRITE(4,'(A)') '\end{document}'                                              !!!
CLOSE(UNIT=4)                                                                !!!
CALL SYSTEM('pdflatex tex.tex > z.txt')                                      !!!
CALL SYSTEM('rm z.txt')                                                      !!!
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-1.pdf')                !!!
CALL SYSTEM('rm tex.*')                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE symbol1                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE symbol2(syst,npol,nf,filename,string_s,string)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.                             !!!
! Also gets number of sides of each face.                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf,is(nf),m,n,l1,l2,npol(3:100)                                !!!
CHARACTER*1:: syst                                                           !!!
CHARACTER*4:: aux,aux2,aux3                                                  !!!
CHARACTER*4000:: string                                                      !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*200::string_s(nf)                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! System letter                                                              !!!
string=TRIM(ADJUSTL(syst))                                                   !!!
m=0  ! aux symbol counter                                                    !!!
n=0  ! face counter                                                          !!!
is=0                                                                         !!!
DO i=3,100                                                                   !!!
  IF(npol(i).gt.0)THEN                                                       !!!
    WRITE(aux,'(I4)') i                                                      !!!
    l1=1                                                                     !!!
    l2=1                                                                     !!!
    DO j=1,npol(i)                                                           !!!
      n=n+1                                                                  !!!
      IF(j.eq.1) CYCLE                                                       !!!
      IF(string_s(n).eq.string_s(n-1))THEN                                   !!!
        l2=l2+1                                                              !!!
      END IF                                                                 !!!
      IF(string_s(n).ne.string_s(n-1))THEN                                   !!!
        m=m+1                                                                !!!
        is(m)=n-1                                                            !!!
        WRITE(aux2,'(I4)') m                                                 !!!
        WRITE(aux3,'(I4)') l2-l1+1                                           !!!
        string=TRIM(ADJUSTL(string))//TRIM(ADJUSTL(aux))//'^{'//TRIM(ADJUSTL(aux3))
        string=TRIM(ADJUSTL(string))//'}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'
        l1=j                                                                 !!!
        l2=j                                                                 !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
    m=m+1                                                                    !!!
    is(m)=n                                                                  !!!
    WRITE(aux2,'(I4)') m                                                     !!!
    WRITE(aux3,'(I4)') l2-l1+1                                               !!!
    string=TRIM(ADJUSTL(string))//TRIM(ADJUSTL(aux))//'^{'//TRIM(ADJUSTL(aux3))
    string=TRIM(ADJUSTL(string))//'}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'
  END IF                                                                     !!!
END DO                                                                       !!!
! aux symbol                                                                 !!!
DO i=1,m                                                                     !!!
  WRITE(aux,'(I4)') i                                                        !!!
  string=TRIM(ADJUSTL(string))//'-\{\mathcal{S}_{'//TRIM(ADJUSTL(aux))// &   !!!
    & '}='//TRIM(ADJUSTL(string_s(is(i))))//'\}'                             !!!
END DO                                                                       !!!
! Creating a latex-based pdf                                                 !!!
OPEN(UNIT=4,FILE='tex.tex')                                                  !!!
WRITE(4,'(A)') '\documentclass[crop]{standalone}'                            !!!
WRITE(4,'(A)') '\begin{document}'                                            !!!
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string))//'$'                               !!!
WRITE(4,'(A)') '\end{document}'                                              !!!
CLOSE(UNIT=4)                                                                !!!
CALL SYSTEM('pdflatex tex.tex > z.txt')                                      !!!
CALL SYSTEM('rm z.txt')                                                      !!!
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-2.pdf')                !!!
CALL SYSTEM('rm tex.*')                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE symbol2                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE symbol3(syst,npol,nf,filename,string_s,string)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Routine to apply the FE, FV, and EV sum rules.                             !!!
! Also gets number of sides of each face.                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf,m,npol(3:100)                                               !!!
CHARACTER*1:: syst                                                           !!!
CHARACTER*4:: aux,aux2                                                       !!!
CHARACTER*4000:: string                                                      !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*200::string_s(nf)                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! System letter                                                              !!!
string=TRIM(ADJUSTL(syst))                                                   !!!
m=0  ! aux symbol counter                                                    !!!
DO i=3,100                                                                   !!!
  IF(npol(i).gt.0)THEN                                                       !!!
    WRITE(aux,'(I4)') i                                                      !!!
    DO j=1,npol(i)                                                           !!!
      m=m+1                                                                  !!!
      WRITE(aux2,'(I4)') m                                                   !!!
      string=TRIM(ADJUSTL(string))//TRIM(ADJUSTL(aux))// &                   !!!
        & '^{1}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'                   !!!
    END DO                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Aux symbol                                                                 !!!
m=0  ! aux symbol counter                                                    !!!
DO i=3,100                                                                   !!!
  IF(npol(i).gt.0)THEN                                                       !!!
    DO j=1,npol(i)                                                           !!!
      m=m+1                                                                  !!!
      WRITE(aux,'(I4)') m                                                    !!!
      string=TRIM(ADJUSTL(string))//'-\{\mathcal{S}_{'//TRIM(ADJUSTL(aux))&  !!!
        & //'}='//TRIM(ADJUSTL(string_s(m)))//'\}'                           !!!
    END DO                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Creating a latex-based pdf                                                 !!!
OPEN(UNIT=4,FILE='tex.tex')                                                  !!!
WRITE(4,'(A)') '\documentclass[crop]{standalone}'                            !!!
WRITE(4,'(A)') '\begin{document}'                                            !!!
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string))//'$'                               !!!
WRITE(4,'(A)') '\end{document}'                                              !!!
CLOSE(UNIT=4)                                                                !!!
CALL SYSTEM('pdflatex tex.tex > z.txt')                                      !!!
CALL SYSTEM('rm z.txt')                                                      !!!
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-3.pdf')                !!!
CALL SYSTEM('rm tex.*')                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE symbol3                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_symb                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
