PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.8, pi=3.1415927
 !Definimos las variables
 REAL:: a, t, u, x, y, theta, v, vx, vy

  !Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde
  !la terminal
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula la posición, velocidad y angulo de un objet&
             &o de un tiro parabolico en un cierto momento'
  WRITE(*,*) 'Para ello necesito que ingreses los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'El angulo incial'
  READ*, a
  WRITE(*,*) ' '
  WRITE(*,*) 'La velocidad inicial dada en metros sobre segundos'
  READ*, u
  WRITE(*,*) ' '
  WRITE(*,*) 'El tiempo del que quieres saber la información en segundos'
  READ*, t
  WRITE(*,*) ' '

   !Convirtiendo angulo a radianes
   a=a*pi/180.0
  
   !Las ecuaciones de la posición en x y y
   x=u*cos(a)*t
   y=u*sin(a)*t-0.5*g*t*t

   !La velocidad al tiempo t
   vx = u * cos(a)
   vy = u * sin(a) - g * t
   v = sqrt(vx * vx + vy * vy)
   theta = atan(vy / vx) * 180.0 / pi
 
    !Escribiendo el resultado en la pantalla
    WRITE(*,*) 'Para un tiempo',t
    WRITE(*,*) 'Estos son los datos'
    WRITE(*,*) 'x:',x,'  y:',y
    WRITE(*,*) 'v:',v,'  theta:',theta
    WRITE(*,*) 'vx:',vx,'  vy:',vy

END PROGRAM proyectil
