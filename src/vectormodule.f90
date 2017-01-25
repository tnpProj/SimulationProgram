module vectormod1
	implicit none
	!integer,parameter :: rk=selected_real_kind(4,20)
	integer,parameter :: rk=selected_real_kind(10,40)
	real(kind=rk),parameter :: pi=4*atan(1.0) !define PI
	complex(kind=rk),parameter :: ic=(0.0,1.0) !define imaginary unit
	
	type :: vector
		real(kind=rk) :: x,y,z
	end type

	!operator overloading for ease of use
	interface operator(+)
		module procedure vadd
	end interface operator(+)

	interface operator(-)
		module procedure vsub
	end interface operator(-)

	interface operator(*)
		module procedure vmult2
	end interface operator(*)

	interface operator(*)
		module procedure vmult1
	end interface operator(*)

	interface operator(/)
		module procedure vdiv
	end interface operator(/)

contains


	type (vector) function vadd(v,u)
		implicit none
		type (vector),intent(in) :: v,u
		vadd%x=v%x+u%x
		vadd%y=v%y+u%y
		vadd%z=v%z+u%z
	end function vadd

	type (vector) function vsub(v,u)
		implicit none
		type (vector),intent(in) :: v,u
		vsub%x=v%x-u%x
		vsub%y=v%y-u%y
		vsub%z=v%z-u%z
	end function vsub

	type (vector) function vmult1(v,a)
		implicit none
		type (vector),intent(in) :: v
		real(kind=rk),intent(in) :: a
		vmult1%x=v%x*a
		vmult1%y=v%y*a
		vmult1%z=v%z*a
	end function vmult1

	type (vector) function vmult2(a,v)
		implicit none
		type (vector),intent(in) :: v
		real(kind=rk),intent(in) :: a
		vmult2%x=v%x*a
		vmult2%y=v%y*a
		vmult2%z=v%z*a
	end function vmult2

	type (vector) function vdiv(v,a)
		implicit none
		type (vector),intent(in) :: v
		real(kind=rk),intent(in) :: a
		vdiv%x=v%x/a
		vdiv%y=v%y/a
		vdiv%z=v%z/a
	end function vdiv

	real(kind=rk) function dist(v1,v2)
		type(vector)::v1,v2
		dist=sqrt( (v1%x - v2%x)**2 + (v1%y - v2%y)**2 + (v1%z - v2%z)**2 )
	end function

	subroutine rotateXYplane1(v,deg) !rotate vector 'v' in relation to origin by degrees 'deg' using complex numbers
		type(vector),intent(inout) :: v
		real(kind=rk),intent(in) :: deg
		complex(kind=rk) :: z,w
		z=cmplx(v%x,v%y)
		w=z*exp(ic*(deg*pi/180))
		v=vector(realpart(w),imagpart(w),0)
	end subroutine rotateXYplane1

	type(vector) function rotateXYplane2(v,deg) !same thing but this returns a value
		type(vector),intent(inout) :: v
		real(kind=rk),intent(in) :: deg
		complex(kind=rk) :: z,w
		z=cmplx(v%x,v%y)
		w=z*exp(ic*(deg*pi/180))
		v=vector(realpart(w),imagpart(w),0)
		rotateXYplane2=vector(realpart(w),imagpart(w),0)
	end function rotateXYplane2



end module vectormod1









