PROGRAM big_matrices

IMPLICIT NONE
INTEGER :: i, j, m, n

m = 4 !to get 8000 X 8000 matrix
n = 1000*2**(m-1)

OPEN(11,FILE='mat100.dat')

DO i = 1, n
  DO j = 1, n
    WRITE(11,*) j, i, REAL(i-j)
  ENDDO
ENDDO

ENDPROGRAM big_matrices
