PROGRAM Taylor
IMPLICIT NONE

 !Declaracion de variables necesarias
 REAL(kind=8),external:: LnTaylor  !Esta es la variable que viene desde fuera
 REAL(kind=8)::y,x
 INTEGER:: n,i,a

  a=0 !Variable integer para darle color a las curvas

   !Abrimos un documento para graficar las aproximaciones de taylor de SIN
   OPEN(1,FILE='Ln.dat')
    !Primer DO para el grado del polinomio, desde 4 hasta 16
    DO n=4,16,1
       IF (n>4 .AND. n<7) CYCLE
       IF (n>7 .AND. n<11) CYCLE
       IF (n>11 .AND. n<16) CYCLE

      a=a+1 !Cambiar color de la curva
       !Segundo DO para el valor de la x, yendo desde -10 hasta 10
       DO i=-150,150,1
         x=0.01*i         !Aqui convertimos a real el valor y lo reducimos
         y=LnTaylor(x,n)  !Asigna a y la suma de Taylor de grado n y valor x
          WRITE(1,*) x,y,a !Se escribe en el documento los valores y el color
       END DO
        WRITE(1,*) ' '    !Escribimos un espacio para separar las curvas
    END DO
   CLOSE (1)

END PROGRAM Taylor

!==========================
FUNCTION LnTaylor(x,n)   
!==========================
IMPLICIT NONE

 !Se definen los valores externos para usar
 REAL(kind=8),intent(in):: x !Valor a valuar
 INTEGER,intent(in):: n      !Grado del polinomio
 REAL(kind=8):: LnTaylor    !Nombre con el que saldra el valor de la funcion
 !Variables que se usaran aqui y ya
 REAL(kind=8):: term,suma,a,b,c
 INTEGER:: j

  suma=0 !Comenzamos dando valor 0 a la suma

   !Primer DO para comenzar la suma desde grado 0 hasta n
   DO j=1,n,1 
     a=(-1.0)**(j+1) !Habiendo separado la sumatoria en tres partes queda:
     b=x**j     !a=multiplicador, b=numerador, c=denominador, de una misma
     c=j        !operacion

      term=a*(b/c)      !Se realiza la operacion de la sumatoria
       suma=suma+term   !Se suma al valor anterior
   END DO

    LnTaylor=suma !Se le asigna el valor de la sumatoria a la variable externa

END FUNCTION LnTaylor
