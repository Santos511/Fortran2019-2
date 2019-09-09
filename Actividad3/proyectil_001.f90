PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.80665, pi=3.1415927
 !Definimos variables
 REAL::a,t,u,x,y,ta
 INTEGER::n,i,b

  !Explicamos el programa y pedimos los valores requeridos para el calculo
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula la trayectoria de un proyectil partiendo de&
             & unas condiciones iniciales'
  WRITE(*,*) 'Para ello debes ingresar los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'Tiempo total de vuelo'
  READ*, t
  WRITE(*,*) ' '
  WRITE(*,*) 'Velocidad inicial dada en m/s'
  READ*, u
  WRITE(*,*) ' '

   !Esto servirá para el color de las curvas más adelante
   b=0
   !Calcular el angulo usando la velocidad inicial y el tiempo de vuelo
   a=asin((g*t)/(2*u))
  
    !Abrir archivo para guardar los datos que vendrán del DO
    OPEN(1,FILE='salida.dat')

     !Se abre un DO para calcular las distintas posiciones
     primer:DO i=0,99999,1
            !Usamos variable real para hacer el contador de 0.1 en 0.1
            ta=i*0.1

            !Calculamos las posiciones
            x=u*ta*cos(a)
            y=u*ta*sin(a)-0.5*g*ta*ta

             !Condición de altura para salir del DO 
             IF (y<0) EXIT

            !Escribimos las posiciones en el documento y el contador b para el
            !color de la curva
            WRITE(1,*) x,y,b
     END DO primer

     !Imprimimos un espacio para separar los datos anteriores de los nuevos
     !dentro del documento
     WRITE(1,*) ' '
     
     !Se abre un segundo DO para calcular para distintos angulos, desde 15 hasta
     !90 yendo de 15 en 15
     segundo:DO n=15,90,15
             !Usamos variable real para pasar los grados a radianes
             a=(n*pi)/180.0
             !Sumamos 1 a b para cambiar de color la siguiente curva
             b=b+1
             
             !Imprimimos en el documento un comentario del angulo
             WRITE(1,*)'#',n

             !Tercer DO igual al primero, pero dentro del segundo DO para usar
             !los distintos angulos
             tercer:DO i=0,99999,1
                    !Variable real para ir de 0.1 en 0.1
                    ta=i*0.1

                    !Calcular posiciones
                    x=u*cos(a)*ta
                    y=(u*sin(a)*ta)-(0.5*g*ta*ta)

                     !Condición de altura para salir del DO
                     IF (y<0) EXIT
                   
                    !Imprimir las posiciones en el documento y el color
                    WRITE(1,*) x,y,b 
             END DO tercer

             !Imprimimos un espacio para separar las distintas curvas
             WRITE(1,*) ' '
     END DO segundo             
    ClOSE (1)

END PROGRAM proyectil
