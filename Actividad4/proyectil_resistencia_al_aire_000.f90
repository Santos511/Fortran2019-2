PROGRAM proyectil_con_resistencia_al_aire
IMPLICIT NONE

 !Constantes necesarias
 REAL,PARAMETER:: g=-9.81, pi=3.1415927
 !Variables necesarias
 REAL::v0x,v0y,k,x,y,ta,vx,vy,ymax,tymax,xmax,txmax,vf,v,vymax,vxmax
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

   o=(o*pi)/180.0 !Convertimos el angulo a radianes
  
   v0x=v0*cos(o)  !Calculamos la velocidad inicial en x

   v0y=v0*sin(o)  !Calculamos la velocidad inicial en y

   k=(m*g)/vt     !Calculamos la constante k

    !Abrimos un OPEN para escribir los datos que conseguiremos
    OPEN(1,FILE='datos.dat')
     !Abrimos un DO para calcular las distintas posiciones del proyectil
     DO i=0,99999,1
       !Utilizamos una variable real para hacer el contador de 0.1 en 0.1
       ta=i*0.01

        !Calculamos la velocidad en el eje x
        vx=v0x*exp((-k*ta)/m)

         !Condición de velocidad horizontal por si supera la velocidad terminal
         IF (vt+vx>0) THEN
           vx=vt*(-1) !La velocidad es igual a la velocidad temrinal positiva
         END IF

        !Calculamos la velocidad en el eje y
        vy=(v0y-((m*g)/k))*exp((-k*ta)/m)+(m*g)/k

         !Condición de velocidad vertical por si supera la velocidad terminal
         IF (vt+vy>0 .AND. vy>0) THEN
           vy=vt*(-1) !Mientras la pelota va subiendo y llega a superar la
                      !velocidad terminal se iguala a esta pero en positivo
         ELSE IF (vy<vt .AND. vy<0)THEN   
           vy=vt !Lo mismo pero en negativo ya que la pelota va bajando
         END IF

         IF (vy==0) THEN
            tymax=ta
            vymax=vy
         END IF

        !Calculamos las posiciones
        x=(m/k)*vx*(1-exp((-k*ta)/m))*3
        y=((m/k)*(vy-((m*g)/k))*(1-(exp((-k/m)*ta)))+((m*g)/k)*ta)*3

         !Condición de altura para salir del DO
         IF (y<0) THEN
            txmax=ta
            EXIT
         END IF

       !Escribimos las posiciones en el documento
       WRITE(1,*) x,y,'2'!El número es para el color
     END DO

     !Escribimos un espacio para seprar los datos que vienen
     WRITE(1,*) ' '
     
     !Se abre un DO para calcular la trayectoria sin resistencia al aire
     DO i=0,99999,1
       !Usamos variable real para hacer el contador de 0.1 en 0.1
       ta=i*0.01

        !Calculamos las posiciones
        x=v0x*ta
        y=v0y*ta+0.5*g*ta*ta

         !Condición de altura para salir del DO 
         IF (y<0) EXIT

        !Escribimos las posiciones en el documento
        WRITE(1,*) x,y,'4'
     END DO
    CLOSE (1)

     !Calculamos la posición en x cuando llega al suelo
     xmax=(m/k)*vx*(1-exp((-k*txmax)/m))*3

     !Calculamos las velocidades en y y x al momento del alcance maximo 
     vxmax=v0x*exp((-k*txmax)/m)

      !Condición de velocidad horizontal por si supera la velocidad terminal
      IF (vt+vx>0) THEN
         vx=vt*(-1) !La velocidad es igual a la velocidad temrinal positiva
      END IF
 
     vy=(v0y-((m*g)/k))*exp((-k*txmax)/m)+(m*g)/k

      IF (vy<vt .AND. vy<0)THEN   
         vy=vt
      END IF

      v=sqrt((vx*vx)+(vy*vy))

      WRITE(*,*) txmax,xmax,v
      
END PROGRAM proyectil_con_resistencia_al_aire
