      PROGRAM PENDULO
      implicit real*8 (a-h, o-z)
      PARAMETER(g=9.8)
      PARAMETER(l=9.8)
      PARAMETER(m=1)
      PARAMETER(n=5000)
      OPEN(unit=10,file='oscilar.dat',status='unknown')
      OPEN(unit=20,file='energia.dat',status='unknown')
      OPEN(unit=30,file='oscilar2.dat',status='unknown')
      OPEN(unit=40,file='energia2.dat',status='unknown')
      pi=4*atan(1.0)
      dt=1e-2
      wi=0.0d0
      thetai=0.14
      ei=0.0d0
C     Método de Euler 
      DO i=1,n,1
         w=wi-(g/l)*thetai*dt
         theta=mod(thetai+wi*dt, 2.0d0*pi)
         t=i*dt
         energia=ei+(0.5)*m*(w*l)**2+m*g*l*(1-cos(theta))
         WRITE(10,*) t, theta
         WRITE(20,*) t, energia
         wi=w
         thetai=theta
         ei=energia
      END DO
C     Método de Euler-Cromer
      w = 0.0d0
      wi = 0.0d0
      theta2 = 0.0d0
      thetai2 = 0.14
      ei2 = 0
      DO j=1,n,1
        w=wi-(g/l)*thetai2*dt
        theta2= mod(thetai2+w*dt, 2.0d0*pi)
        t=j*dt
        energia2=ei2+(0.5)*m*(w*l)**2+m*g*l*(1-cos(theta2))
        WRITE(30,*) t, theta2
        WRITE(40,*) t, energia2
        wi=w
        thetai2=theta2
        ei2=energia2
      END DO
      CLOSE(10)
      CLOSE(20)
      CLOSE(30)
      CLOSE(40)
      END PROGRAM
