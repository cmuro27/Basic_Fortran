program conicas2
implicit none
real :: a,b,c,d,e,f,g,aux1,aux2,aux3,aux4,aux5,aux6,h,k,i,f1,f2,direct,lr,p,df,v1,v2,ex
print*,"la ecuaci¢n c¢nica general tiene la forma Ax^2+By^2+Cx+Dy+E=0"
print*,"introducir constantes A,B,C,D,E para determinar c¢nica y sus caracter¡sticas"
read*, a,b,c,d,e
i=(-1)*4*a*b
h=-c/(2*a)
k=-d/(2*b)
f=(-e/a)+((c**2)/(4*(a**2)))+((d**2)/(4*a*b))
g=(-e/b)+((c**2)/(4*(a*b)))+((d**2)/(4*(b**2)))
!
cond1:if(f<0.and.g<0)then
print*, "ecuaci¢n no tiene soluci¢n real"
end if cond1
!
!
!circunferencia
cond2:if(a==b.and.f>0.and.g>0)then
print*, "es una circunferencia"
write(*,1)h,k
1 format("con centro en ",f6.2,",",f6.2)
aux1=sqrt((-e/b)+((c**2)/(4*(a**2)))+((d**2)/(4*(b**2))))
write(*,2)aux1
2 format("con radio ",f6.2)
end if cond2
!
!
!par bola
cond3:if(i==0)then
print*, "es una par bola"
!
cond3a:if(a==0)then
print*, "con eje focal paralelo al eje x"
aux1=(((d**2)/(4*c*b))-(e/c))
write(*,3)aux1,k
3 format("con v‚rtice en: ",f6.2,"",f6.2)
p=((-1)*c)/(4*b)
f1=aux1+p
write(*,4)f1,k
4 format("con foco en: ",f6.2,f6.2)
if(p>0)then
print*, "abre hacia la derecha"
else
print*, "abre hacia la izquierda"
end if
lr=ABS(4*p)
write(*,44)lr
44 format("con longitud de lado recto ",f6.2)
direct=aux1-p
write(*,6)direct
6 format("con ecuaci¢n de la directriz x=",f6.2)
end if cond3a
!
cond3b:if(b==0)then
print*,"con eje focal paralelo al eje y"
aux1=((c**2)/(4*a*d))-(e/d)
write(*,7)h,aux1
7 format("con v‚rtice en: ",f6.2,",",f6.2)
p=((-1)*d)/(4*a)
f1=aux1+p
write(*,8)h,f1
8 format("con foco en: ",f6.2,",",f6.2)
if(p>0)then
print*, "abre hacia arriba"
else
print*, "abre hacia abajo"
end if
lr=ABS(4*p)
write(*,5)lr
5 format("con longitud de lado recto ",f6.2)
direct=aux1-p
write(*,9)direct
9 format("con ecuaci¢n de la directriz y=",f6.2)
 end if cond3b
 end if cond3
 !
 !
!elipse
cond4:if(i<0)then
print*," es una elipse"
write(*,213)h,k
213 format("con centro en: ",f6.2,",",f6.2)
!
cond4a:if(f>g)then
print*, "con su eje focal paralelo al eje x"
aux1=sqrt(f)
aux2=sqrt(g)
v1=h+aux1
v2=h-aux1
write(*,10)v1,k,v2,k
10 format("con v‚rtices en: ",f6.2,",",f6.2,"  y  ",f6.2,",",f6.2)
df=sqrt(f-g)
f1=h+df
f2=h-df
write(*,11)f1,k,f2,k
11 format("con focos en: ",f6.2,",",f6.2," y  ",f6.2,",",f6.2)
ex=df/aux1
write(*,12)ex
12 format("con excentricidad de:",f6.2)
lr=((2*(g))/(aux1))
write(*,13)lr
13 format("con lado recto de: ",f6.2)
end if cond4a
 !
cond4b:if(f<g)then
print*,"con su eje focal paralelo al eje y"
aux1=sqrt(f)
aux2=sqrt(g)
v1=k+aux2
v2=k-aux2
write(*,111)h,v1,h,v2
111 format("con v‚rtices en: ",f6.2,",",f6.2,"  y  ",f6.2,",",f6.2)
df=sqrt(g-f)
f1=k+df
f2=k-df
write(*,113)h,f1,h,f2
113 format("con focos en: ",f6.2,",",f6.2," y  ",f6.2,",",f6.2)
ex=df/aux2
write(*,124)ex
124 format("con excentricidad de:",f6.2)
lr=(2*(f))/((aux2))
write(*,137)lr
137 format("con lado recto de: ",f6.2)
end if cond4b
end if cond4
!
!
!hip‚rbola
cond5:if(i>0)then
print*,"es una hip‚rbola"
df=sqrt(f+g)
!
cond5a:if(f>0.and.g<0)then
print*, "que su eje focal es paralelo al eje x"
write(*,141)h,k
141 format("con centro en: ",f6.2,",",f6.2)
aux1=sqrt(f)
v1=h+aux1
v2=h-aux1
write(*,144)v1,k,v2,k
144 format("con v‚rtices en: ",f6.2,",",f6.2,"  y  ",f6.2,",",f6.2)
f1=h+df
f2=h-df
write(*,142)f1,k,f2,k
142 format("con focos en: ",f6.2,",",f6.2," y  ",f6.2,",",f6.2)
ex=df/aux1
write(*,147)ex
147 format("con excentricidad de: ",f6.2)
lr=(2*g)/aux1
write(*,50)lr
50 format("con lado recto de: ",f6.2)
write(*,775)h,sqrt(f),k,sqrt(g)
775 format("con as¡ntotas: ","((x-",f6.2,")/(",f6.2,"))  +  ((y-",f6.2,")/(",f6.2,"))  =0")
write(*,779)h,sqrt(f),k,sqrt(g)
779 format("               ","((x-",f6.2,")/(",f6.2,"))  -  ((y-",f6.2,")/(",f6.2,"))  =0")
end if cond5a
!
cond5b:if(g>0.and.f<0)then
print*, "que su eje focal es paralelo al eje y"
write(*,149)h,k
149 format("con centro en: ",f6.2,",",f6.2)
aux1=sqrt(g)
v1=k+aux1
v2=k-aux1
write(*,122)h,v1,h,v2
122 format("con v‚rtices en: ",f6.2,",",f6.2,"  y  ",f6.2,",",f6.2)
f1=k+df
f2=k-df
write(*,1456)h,f1,h,f2
1456 format("con focos en: ",f6.2,",",f6.2," y  ",f6.2,",",f6.2)
ex=df/aux1
write(*,1477)ex
1477 format("con excentricidad de: ",f6.2)
lr=(2*f)/aux1
write(*,501)ABS(lr)
501 format("con lado recto de: ",f6.2)
write(*,773)k,sqrt(g),h,sqrt(f)
773 format("con as¡ntotas: ","((y-",f6.2,")/(",f6.2,")) +  ((x-",f6.2,")/(",f6.2,"))  =0")
write(*,90)k,sqrt(g),h,sqrt(f)
90 format("               ","((y-",f6.2,")/(",f6.2,")) -  ((x-",f6.2,")/(",f6.2,"))  =0")
end if cond5b
!
end if cond5
!
!
read(*,*)
end program conicas2

