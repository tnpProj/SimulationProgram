module simumod
	use vectormod1
	implicit none
	integer :: i,j,k,n !do loop variables
	integer :: ios !for file opening procedure
	logical :: exist1,exist2 !for file opening procedure

	integer,parameter :: ik=selected_int_kind(10)

	integer,parameter :: yr=365*24*60*60, day=24*60*60
	real(kind=rk),parameter :: G=6.67408E-11 !gravitational constant
	real(kind=rk),parameter :: AU=149.597870700E9 !astronomical unit

	real(kind=rk) :: dt !timestep of the simulation
	integer(kind=ik):: simtime,writestep,printstep !simulation time (seconds), planet position writing step,planet position printing step
	integer :: numberofobjects !amount of objects in the simulation
	integer :: filewritecount !counter how many times has been written in the output file

	type :: planet !Define a new type called 'planet' which has name, mass, position and velocity
		character(len=80) :: name
		real(kind=rk) :: mass
		type(vector):: pos
		type(vector):: vel	
	end type

	!these are used for completing galilean tranformation (change of reference frames)
	logical :: galileanOn !This is used to decide whether the simulation makes galilean transformation
	integer :: galileanPlanet !this is the index of the planet from the variable 'list' (which is the list of all of the objects in the system)
	type(vector)::galileitransV=vector(0,0,0)

	character(len=80),allocatable:: plname(:) !list of planet (or object) names
	real(kind=rk),allocatable :: plmass(:),plvel(:),plaph(:),plper(:) ! !list of masses, initial velocities, aphelions and perihelions
	type(planet),allocatable :: list(:) !list of all of the planets
	type(vector),allocatable :: Fi(:),Ff(:) !GravForce at current (initial) positions and GravForce at next (final) position

contains

	type(vector) function GravForce(p1,p2) !Gravitational force that planet1 (p1) inflicts to planet2 (p2)
		type(planet)::p1,p2
		real(kind=rk) :: magnitude
		type(vector) :: r
		magnitude=-G*(p1%mass*p2%mass)/((dist(p1%pos,p2%pos))**3)
		r=vector(p2%pos%x - p1%pos%x, p2%pos%y - p1%pos%y, p2%pos%z - p1%pos%z)
		GravForce=vector(magnitude*r%x,magnitude*r%y,magnitude*r%z)
	end function

	subroutine initplanets !Initialize the planets using the different lists we have created by reading the 'inputfile' using subroutine 'readplanetdata'
		implicit none		
		allocate(list(numberofobjects))
		allocate(Fi(numberofobjects))
		allocate(Ff(numberofobjects))		

		do i=1,numberofobjects
			list(i)%name=plname(i)
			list(i)%mass=plmass(i)
			list(i)%vel=vector(0,-plvel(i),0)
			list(i)%pos=vector(-(plaph(i)+plper(i))/2,0,0) !Set the initial distance of a planet to be average of aphelion+perihelion
		end do
	end subroutine initplanets

	subroutine setplanetinitpos ! Optional subroutine for the simulation. This is used to rotate the initial planet positions by user specified degrees. For example from the website www.theplanetstoday.com we can check how much each planet position is rotated on the XY-plane and write those values here. This way we get more realistic starting positions. 
		if (numberofobjects == 9) then
		
			do i=1,numberofobjects
				list(i)%pos%x=-list(i)%pos%x
				list(i)%pos%y=-list(i)%pos%y
				list(i)%pos%z=-list(i)%pos%z
				list(i)%vel%x=-list(i)%vel%x
				list(i)%vel%y=-list(i)%vel%y
				list(i)%vel%z=-list(i)%vel%z
			end do			

			call rotateXYplane1(list(2)%pos,20.0_rk)
			call rotateXYplane1(list(3)%pos,20.0_rk)	
			call rotateXYplane1(list(4)%pos,85.0_rk) !earth
			call rotateXYplane1(list(5)%pos,10.0_rk)
			call rotateXYplane1(list(6)%pos,190.0_rk)
			call rotateXYplane1(list(7)%pos,260.0_rk)
			call rotateXYplane1(list(8)%pos,20.0_rk)
			call rotateXYplane1(list(9)%pos,340.0_rk)

			call rotateXYplane1(list(2)%vel,20.0_rk)
			call rotateXYplane1(list(3)%vel,20.0_rk)	
			call rotateXYplane1(list(4)%vel,85.0_rk) !earth
			call rotateXYplane1(list(5)%vel,10.0_rk)
			call rotateXYplane1(list(6)%vel,190.0_rk)
			call rotateXYplane1(list(7)%vel,260.0_rk)
			call rotateXYplane1(list(8)%vel,20.0_rk)
			call rotateXYplane1(list(9)%vel,340.0_rk)
		else
			print*,'Error: you need to have 9 objects!'
		end if
		
	end subroutine setplanetinitpos

	subroutine writecurpos(unit1,k,i,writestep,maxim) !Write data to the unit 'unit1' defined by the 'writestep' argument. Also data is written during the first and last iterations.
		integer,intent(in) :: unit1,k,i
		integer(kind=ik),intent(in)::writestep,maxim

		if(k==1 .and. i==1) then !In the first two lines of the file we write this information for the animation program
			write(unit1,*) numberofobjects
			write(unit1,*) dt*writestep
			filewritecount=1
		end if
		if(mod(k,writestep)==0 .or. k==1 .or. k==maxim) then
			write(unit1,*) list(i)%pos%x/AU, list(i)%pos%y/AU
			if (i==1) then
				filewritecount=filewritecount+1
			end if
		end if

	end subroutine writecurpos
	
	subroutine initposGravForce(i) !Calculate all the initial forces that affect i:th planet
		integer,intent(in) :: i
		Fi(i)=vector(0,0,0)
		do j=1,numberofobjects
			if (i /= j) then
				Fi(i)=Fi(i)+GravForce(list(j),list(i))
			end if
		end do	
	end subroutine initposGravForce

	subroutine finalposGravForce(i) !Calculate all the final forces (after updating planet positions) that affect i:th planet
		integer,intent(in) :: i
		Ff(i)=vector(0,0,0)
		do j=1,numberofobjects
			if (i /= j) then
				Ff(i)=Ff(i)+GravForce(list(j),list(i))
			end if
		end do	
	end subroutine finalposGravForce
	
	subroutine updatepos(i,dt) !Update the i:th planet's position
		integer,intent(in) :: i
		real(kind=rk),intent(in) :: dt
		list(i)%pos=list(i)%pos + list(i)%vel*dt + 0.5_rk*(Fi(i)/list(i)%mass)*(dt**2)
	end subroutine updatepos

	subroutine updatevel(i,dt) !Update the i:th planet's velocity
		integer,intent(in) :: i
		real(kind=rk),intent(in) :: dt
		list(i)%vel=list(i)%vel + 0.5_rk*( (Fi(i)+Ff(i))/list(i)%mass )*dt
	end subroutine updatevel

	subroutine simulate(unit1,simtime,dt,writestep) !The simulation and file writing algorithm
		integer,intent(in):: unit1
		integer(kind=ik),intent(in) ::simtime,writestep
		integer(kind=ik) :: iterations
		real(kind=rk) :: dt
		type(vector)::b
		iterations=simtime/dt

		print *,'Started simulation...'
		print '(a,f17.5)','Simulation time (days):', simtime/(24*60*60.0)
		print '(a,f10.5)','Simulation timestep(days):',dt/day 
		print '(a,i6)','File writing step:', writestep

		do k=1,iterations

			!Printing planet positions depending on the printstep given in the input file
			if (printstep==0) then
				if (k==1 .or. k==iterations) then
					call printplanpos(k,dt)	
				end if
			else if (mod(k,printstep)==0 .or. k==1 .or. k==iterations) then
				call printplanpos(k,dt)
			end if

			!Writing current positions to 'unit1' (the file that is open in the main program)
			do i=1,numberofobjects
				call writecurpos(unit1,k,i,writestep,iterations)
			end do

			!Completing galilean coordinate transform if the subroutine 'movetoplanetscoordsys' has been called from the main program
			if (galileanOn .eqv. .true.) then 
				galileitransV=list(galileanPlanet)%vel
				do i=1,numberofobjects
					list(i)%pos=list(i)%pos-galileitransV*dt
				end do
			end if
			
			!Calculating inital forces Fi at current position
			do i=1,numberofobjects
				call initposGravForce(i)
			end do

			!Updating the positions of all planets using forces from Fi
			do i=1,numberofobjects
				call updatepos(i,dt)
			end do

			!Calculating final forces Ff at the updated position
			do i=1,numberofobjects
				call finalposGravForce(i)
			end do

			!Updating the velocities of each planet using the average force (1/2)*(Fi + Ff)
			do i=1,numberofobjects
				call updatevel(i,dt)
			end do

		end do
		print *,'Finished simulation!'		
	
	end subroutine simulate
	
	subroutine printplanpos(k,dt) !Print all of the current planet positions
		real(kind=rk),intent(in)::dt
		integer,intent(in) :: k
		print *,'Printing planet positions...'
		print *,'Number of objects:', numberofobjects
		print *,'Current iteration:', k
		print *,'Current time (days):', dt*k/day
		print *,'Number of times written in the file:', filewritecount
		do i=1,numberofobjects
			print *,list(i)%name, list(i)%pos
		end do	
	end subroutine printplanpos

	subroutine allocateplanetdata(fname) !Calculate how many objects there is in the simulation and allocate the lists 'plname', 'plmass' etc. according to that number
		character(len=80),intent(in)::fname
		character(len=80) :: strdummy

		inquire(file=fname, exist=exist1)
		if (exist1) then
			open(unit=1,file=fname,iostat=ios,status='old',action='read',access='sequential', &
				form='formatted')	
		else
			print *,'file does not exist!'
		end if	

		do i=1,17 !Skip the first 17 lines of the inputfile
			read(1,*)
		end do

		i=1
		countloop: do !Count the number of objects in the simulation
				read(1,*,iostat=ios)
				if (ios<0) exit		
				i=i+1	
			end do countloop	

		numberofobjects=i-1

		allocate(plname(numberofobjects)) !Allocate lists to be of the right size
		allocate(plmass(numberofobjects))
		allocate(plvel(numberofobjects))
		allocate(plaph(numberofobjects))
		allocate(plper(numberofobjects))

		close(unit=1,status='keep')				
	end subroutine allocateplanetdata

	subroutine readplanetdata(fname) !read all the information from the inputfile
		implicit none
		character(len=80),intent(in)::fname
		character(len=80) :: strdummy
		real(kind=rk) :: t
		real(kind=rk) :: simtimes(5),steptimes(5)
		integer(kind=rk) :: m

		call allocateplanetdata(fname)

		inquire(file=fname, exist=exist1)

		if (exist1) then
			open(unit=1,file=fname,iostat=ios,status='old',action='read',access='sequential', &
				form='formatted')	
		else
			print *,'file does not exist!'
		end if	

		do i=1,17

			if (i==1 .or. i==7 .or. i==13 .or. i==15 .or. i==17) then !Do nothing with the input file's header's
				read(1,*) strdummy
			else if(i>1 .and. i<7) then !Read seconds, minutes, hours, days and years for the simulation time 
				read(1,*) strdummy,t
				simtimes(i-1)=t
			else if(i>7 .and. i<13) then !Read seconds, minutes, hours, days and years for the simulation time step
				read(1,*)strdummy,t
				steptimes(i-7)=t
			else if (i==14) then !Read the writestep from line 14
				read(1,*) strdummy,t 
				writestep=t
			else if (i==16) then !Read printstep from line 16
				read(1,*) strdummy,t
				printstep=t
			end if 
		end do

		simtime=simtimes(1)+60*simtimes(2)+60*60*simtimes(3)+day*simtimes(4)+yr*simtimes(5) !simtime in seconds
		dt=steptimes(1)+60*steptimes(2)+60*60*steptimes(3)+day*steptimes(4)+yr*steptimes(5) !timestep in seconds

		!At this point we are reading data from the file starting from line 18

		do i=1,numberofobjects !fill the lists 'plname', 'plmass', etc. with the information from the file
			read(1,*) plname(i),plmass(i),plvel(i),plaph(i),plper(i)
			plaph(i)=AU*plaph(i)
			plper(i)=AU*plper(i)
		end do

		close(unit=1,status='keep')		
	end subroutine readplanetdata

	subroutine movetoplanetscoordsys(j) !Galilean transformation between two reference frames. This does not work perfectly (the chosen planet makes small circular motion, even though it should not).
		integer,intent(in)::j
		type(vector)::galileitransX
		galileitransX=list(j)%pos
		galileanOn=.true.
		galileanPlanet=j
		do i=1,numberofobjects !Move all the planets so that the chosen planet is in the origin
			list(i)%pos=list(i)%pos - galileitransX
		end do
	end subroutine movetoplanetscoordsys

	function linspace(start1,end1,n) result(linsp) !Optional subroutine for testing purposes. Create evenly spaced list from 'start1' to 'end1' with 'n' amount of elements
		implicit none
		integer,intent(in) :: n
		real(kind=rk) :: start1,end1, step 
		real(kind=rk),allocatable::linsp(:)
		allocate(linsp(n))
		step=(end1-start1)/n
		linsp=0
		do j=1,n
			linsp(j)=linsp(j)+(j)*step
		end do
	end function

end module simumod








