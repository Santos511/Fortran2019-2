PROGRAM proyectil_con_resistencia_al_aire
IMPLICIT NONE

 !Constantes necesarias
 REAL,PARAMETER:: g=-9.81, pi=3.1415927
 !Variables necesarias
 REAL::v0x,v0y,k,x,y,ta,ax,ay,fx,fy,vx,vy
 REAL::m,v0,o,vt
 !Contadores
 INTEGER::i,b

  !Presentamos el programa y pedimos los datos
  WRITE(*,*) ' '
  WRITE(*,*) '     Proyectil con resistencia al aire'
  WRITE(*,*) 'Este programa grafica la trayectoria de una pelota de baseball co&
             &nsiderando la resistencia al aire'
  WRITE(*,*) 'Para ello es necesario que introduzcas las siguientes variables'
  WRITE(*,*) ' '
  WRITE(*,*) 'La masa de la pelota en kg'
  READ(*,*) m
  WRITE(*,*) ' '
  WRITE(*,*) 'La velocidad inicial en m/s'
  READ(*,*) v0
  WRITE(*,*) ' '
  WRITE(*,*) 'El angulo de tiro'
  READ(*,*) o
  WRITE(*,*) ' '
  WRITE(*,*) 'Y finalmente la velocidad terminal'
  READ(*,*)  vt
  WRITE(*,*) ' '

   !Convertimos el angulo a radianes
   o=(o*pi)/180.0

   !Calculamos las velocidades de v0x y v0y
   v0x=v0*cos(o)
   v0y=v0*sin(o)

   !Calculamos k
   k=(m*g)/vt

    !Abrimos un OPEN para escribir los datos que conseguiremos
    OPEN(1,FILE='datos.dat')
     !Abrimos un DO para calcular las distintas posiciones del proyectil
     DO i=0,99999,1
       !Utilizamos una variable real para hacer el contador de 0.1 en 0.1
       ta=i*0.01

       vx=v0x*exp((-k*ta)/m)
       IF (vt+vx>0) THEN
          vx=vt*(-1)

       END IF
       vy=(v0y-((m*g)/k))*exp((-k*ta)/m)+(m*g)/k

       IF (vt+vy>0 .AND. vy>0) THEN
          vy=vt*(-1)

        ELSE IF (vy<vt .AND. vy<0)THEN   
          vy=vt
       END IF

       !Calculamos las posiciones
       x=(m/k)*vx*(1-exp((-k*ta)/m))*3
       y=((m/k)*(vy-((m*g)/k))*(1-(exp((-k/m)*ta)))+((m*g)/k)*ta)*3

        !Condición de altura para salir del DO
        IF (y<0) EXIT

       !Escribimos las posiciones en el documento
       WRITE(1,*) x,y,'2'
     END DO

     WRITE(1,*) ' '
     
     !Se abre un DO para calcular las distintas posiciones
     DO i=0,99999,1
       !Usamos variable real para hacer el contador de 0.1 en 0.1
       ta=i*0.01

       !Calculamos las posiciones
       x=v0x*ta
       y=v0y*ta+0.5*g*ta*ta

        !Condición de altura para salir del DO 
        IF (y<0) EXIT

       !Escribimos las posiciones en el documento y el contador b para el
       !color de la curva
       WRITE(1,*) x,y,'4'
     END DO

    CLOSE (1)
END PROGRAM proyectil_con_resistencia_al_aire
