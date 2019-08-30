PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.8, pi=3.1415927
 !Definimos las variables
 REAL:: a, t, u, y

  !Leer valores para el ángulo a y la velocidad inicial u desde la terminal
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula la altura maxima de un proyectil usando par&
             &a los calculos unas condiciones iniciales'
  WRITE(*,*) 'Para ello necesito que ingreses los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'El angulo incial'
  READ*, a
  WRITE(*,*) ' '
  WRITE(*,*) 'La velocidad inicial dada en metros sobre segundos'
  READ*, u
  WRITE(*,*) ' '

   !Convirtiendo angulo a radianes
   a=a*pi/180.0
  
   !Calculamos el tiempo y lo dividimos entre 2 porque es en ese momento que la
   !altura está en su valor más alto
   t=2*u*sin(a)/g
   t=t/2

   !Calculamos la altura  maxima del proyectil con las ecuaciones de posición
   !y el tiempo antes calculado
   y=u*sin(a)*t-0.5*g*t*t

   !Regresamos las distancia a angular a grados para mostrarlo en pantalla
   a=a*180.0/pi
   
    !Escribiendo el resultado en la pantalla
    WRITE(*,*) 'Para un tiro parabolico de angulo',a
    WRITE(*,*) 'Y una velocidad inicial',u
    WRITE(*,*) 'La altuira maxima es',y
    WRITE(*,*) ' '

END PROGRAM proyectil
