PROGRAM Taylor
IMPLICIT NONE

 REAL(kind=8):: x, exp_true, y
 REAL(kind=8),external:: exptaylor
 INTEGER:: n

  n=20   ! number of terms to use
  x=1.0

   exp_true=exp(x)
   y=exptaylor(x,n)   ! uses function below

    PRINT*, '    x    =',x
    PRINT*, 'exp_true =',exp_true
    PRINT*, 'exptaylor=',y
    PRINT*, '  error  =',y - exp_true

END PROGRAM Taylor

!==========================
FUNCTION ExpTaylor(x,n)
!==========================
IMPLICIT NONE

 !function arguments:
 REAL(kind=8),intent(in):: x
 INTEGER,intent(in):: n
 REAL(kind=8):: exptaylor
 !local variables:
 REAL(kind=8):: term, partial_sum
 INTEGER:: j

  term = 1.
  partial_sum = term

   DO j=1,n
     ! j'th term is  x**j / j!  which is the previous term times x/j:
     term = term*x/j   
     ! add this term to the partial sum:
     partial_sum = partial_sum + term   
   END DO

    exptaylor = partial_sum  ! this is the value returned

end function exptaylor
