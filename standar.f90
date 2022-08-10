PROGRAM power_method_standar

!***********************************************************************
!    This program compute the lowest eigenvalue by the power method
!***********************************************************************
!        with love,
!        	VCastor 2022
!***********************************************************************

IMPLICIT NONE
INTEGER                                   :: i, j, k, l, dm
REAL(KIND=8)                              :: aux1, aux2, ei, tmps
REAL(KIND=8), DIMENSION(:), ALLOCATABLE   :: x0, xn, xp
REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: mat_a
CHARACTER(LEN=100)                        :: archivo

!-----------------------------------------------------------------------
!                        Reading the input file
!-----------------------------------------------------------------------
!READ(*,*) archivo
archivo = "mat100.dat"
OPEN(11,FILE=archivo)
  READ(11,*) dm
  ALLOCATE(mat_a(dm,dm), x0(dm), xn(dm), xp(dm))
  DO k = 1, dm*dm
    READ(11,*) i, j, mat_a(i,j)
  ENDDO
CLOSE(11)

!-----------------------------------------------------------------------
! Power method eigenvector compute:
!                                    x_1 = Ax_0
!               x_2 = Ax_1 = A(Ax_0)     = A^{2}x_0
!               x_3 = Ax_2 = A(A^{2}x_0) = A^{3}x_0
!                                       ...
!         x_k = Ax_{k-1} = A(A^{k-1}x_0) = A^{k}x_0
!-----------------------------------------------------------------------

x0(:) = 1.d0                     !First approximation, SOULD NOT BE ZERO

DO i = 1, 10                     !take k = 30
  x0(:)  = MMULT(dm,mat_a,x0)
ENDDO

aux2   = MINVAL(ABS(x0))         !put of the scalar which multiply the 
x0(:)  = ABS(x0)/aux2            !vector: y = c*x

!-----------------------------------------------------------------------
! Computing the eigenvalue:
!                                      Ax * x
!                          \lambda = ----------
!                                      x * x
!-----------------------------------------------------------------------

xn(:)  = MMULT(dm,mat_a,x0)
ei     = DOT_PRODUCT(xn,x0)/DOT_PRODUCT(x0,x0)

WRITE(*,*) "Eigenvalue: "
WRITE(*,*) ei

                                  CONTAINS

!***********************************************************************
! Here is the matrix-vector SUBROUTINE, not use the intrisic fortran
! function, to paralelizate exactly the same code.
!***********************************************************************

  FUNCTION MMULT(dm,mat_a,x0) RESULT (x1)

  IMPLICIT NONE
  INTEGER                        :: i, j, dm
  REAL(KIND=8), DIMENSION(dm)    :: x0, x1
  REAL(KIND=8), DIMENSION(dm,dm) :: mat_a

  DO i = 1, dm
    x1(i) = 0.d0
    DO j = 1, dm
      x1(i) = x1(i) + mat_a(i,j)*x0(j)
    ENDDO
  ENDDO

  ENDFUNCTION MMULT

ENDPROGRAM power_method_standar
