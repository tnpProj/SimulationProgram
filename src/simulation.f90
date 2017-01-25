program solarsystemsimulator
	use simumod
	implicit none
	character(len=80) :: inputfile,outputfile

	inputfile='simulationinput1.txt'
	outputfile='simulationoutput.txt'

	call readplanetdata(inputfile)
	call initplanets

	!Use this call to rotate the planet positions by predefined degrees in the module 'simumodule.f90' (you must have exactly 9 objects for this to work)
	!call setplanetinitpos

	!Use this call below to move the 4th objects (earth by default) reference frame by using galilean transformation (does not work perfectly)
	!call movetoplanetscoordsys(4)

	!open the output file
	inquire(file=outputfile, exist=exist2)

	if (exist2) then
		open(unit=1,file=outputfile,iostat=ios,status='replace',action='write',access='sequential', &
			form='formatted')	
	else
		open(unit=1,file=outputfile,iostat=ios,status='new',action='write',access='sequential', &
			form='formatted')
	end if	
		
	call simulate(1,simtime,dt,writestep) !run the simulation while writing to unit=1, simulation time 'simtime', timestep 'dt', file writing step 'writestep' and save the data to unit '1'

	close(unit=1,status='keep')

end program solarsystemsimulator






