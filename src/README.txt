Quick instructions (use these commands in the linux command prompt):

bash openall1 (opens all of the project files in Gedit)
bash openall2 (opens all of the project files in Emacs)
bash makeall (compiles the program and runs the simulation, outputting file 'simulationoutput.txt')
python animation.py (visualizes the simulation using the data from file 'simulationoutput.txt')

Purpose of the program:

Planetary motion simulator. This program aims to simulate the motion of N celestial objects (point particles), when the only force in effect is Newton's gravitational force that these objects inflicts to each other. The algorithm used is the Velocity Verlet algorithm. The program can be used to simulate the Solar System for example.

Explanations for each file:

#simulationinputX.txt
This is the data that is used to run the simulation. For #SimulationTime, #SimulationTimeStep, #FileWritingStep and #PositionPrintingStep the user needs to input integers. The user cannot leave these sections empty or have extra spaces in them. There has to be number in every slot for the program to work. The user cannot make any additional rows in the above sections. #FileWritingStep needs to positive and bigger than 0.

Below these, the objects (or planets) of the simulation are defined. The names are strings. Masses, velocities, aphelions and perihelions are floating point numbers. The user can add/remove planets to the system, as long as no extra spaces or extra lines are left in the file.

The predefined input files contain the following:
X=1: The Solar System (sun+8 planets)
X=2: The Solar System + Moon
X=3: Sun + Jupiter
X=4: Sun + Earth
X=5: Earth + Moon

#simulation.f90
This is the main program. Parameters the user can change are:
- 'inputfile' on line 6
- use 'call setplanetinitpos' to rotate the starting positions of the Solar System's planets to be more realistic
- use 'call movetoplanetscoordsys(X)' to move in to the reference frame of the planet X (does not work perfectly)

#animation.py
This is the animation program. Parameters the user can change are: 
- 'scale' in line 15 which sets the zoom level of the plot
- 'interval' in line 114 to change the speed of the animation

#simumodule.f90
This is the simulation module. No changes should be made here. [Though if the user wishes to use smaller distances instead, the parameter AU (astronomical unit) can be changed in this file and also in animation.py to the value of 1 for example]

#vectormodule.f90
This the module that defines 3-component vector and some calculus operations for this type. No changes should be made here.

Made by tnpProj
