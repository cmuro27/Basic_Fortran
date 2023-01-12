program happy7
implicit none
integer :: x,suma,i
integer, allocatable :: num(:)
character(len=4) :: aux,b
print*, "Leer n£mero para ver si es feliz"
read*, aux
33 allocate(num(len_trim(aux)))
ciclo1:do i=1,len_trim(aux)
read(aux(i:i),"(I3)")x
num(i)=x*x
end do ciclo1
suma=sum(num)
cond1:if(suma==1)then
write(*,32)
32 format("este n£mero y los dem s que aparecen son felices")
else
write(b,"(I3)")suma
aux=b
print*, aux
deallocate(num)
suma=0
go to 33
end if cond1
write(*,*)
end program happy7
