import itertools
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

AU=149.597870700E9 #astronomical unit

#initialize the plot and its settings
fig = plt.figure()
ax = fig.add_subplot(111,aspect='equal')
ax.grid()
plt.title('Solar system simulation')
plt.xlabel('AU')
plt.ylabel('AU')
scale=1.2 #this scalar is applied to the limits of the plot, with it you can choose how 'zoomed in' towards the origin you are in the animation

#initialize the required variables
positionsx=[] #x-components of positions of all planets (this will contain 'numberofobjects' amount of lists inside of it)
positionsy=[] #y-components of positions of all planets (this will contain 'numberofobjects' amount of lists inside of it)
dots=[] #scatter dot objects on the plot (these represent the planets) (this will contain 'numberofobjects' amount of 'plt.scatter' objects)
traces=[] #line traces behind the scatters dots  (this will contain 'numberofobjects' amount of 'plt.plot' objects)
result=[] #the result from the animation 'update(i)' function : scatter dot objects + line traces + text objects 
numberofobjects=0 #initialize numberofobjects and later read it from the input files second row
limits=0 #global variable for axis limits
timestep=0 #initialize timestep and later read it from the input files second row (this is used for displaying the 'current' day on the simulation plot)

fname='simulationoutput.txt'

#read the amount of planets and timestep
with open(fname) as f:
	numberofobjects=f.readline()
	numberofobjects=int(numberofobjects)
	timestep=float(f.readline())/(24*60*60)
	

for i in range(1,numberofobjects+1): 
	positionsx.append([]) #add to positionsx lists
	positionsy.append([]) #add to positionsy lists
	dots.append(plt.scatter([],[],s=30,marker='o')) #add to dots objects 'plt.scatter'
	traces.append(plt.plot([],[],lw=2,color='grey')) #add to traces objects 'plt.plot'
	with open(fname) as f: #read the planet positions from the file, positionsx[2][7] is the 3rd planet's positions x-component on the 8th frame
		f.readline() #skip first line
		f.readline() #skip second line
		lines=itertools.islice(f, i-1, None, numberofobjects)
		for l in lines: 
			a,b=l.split()
			positionsx[i-1].append(float(a))
			positionsy[i-1].append(float(b))

#set these colors for the dots and traces if we have 9 objects in our system
if numberofobjects == 9:
	dots[0].set_color('orange')
	dots[3].set_color('green')
	dots[4].set_color('red')
	dots[5].set_color('tan')
	dots[7].set_color('lightblue')
	dots[8].set_color('purple')
	traces[0][0].set_color('orange')
	traces[1][0].set_color('blue')
	traces[2][0].set_color('blue')
	traces[3][0].set_color('green')
	traces[4][0].set_color('red')
	traces[5][0].set_color('tan')
	traces[7][0].set_color('lightblue')
	traces[8][0].set_color('purple')

#count how many frames we have in animation (or how many timesteps in the file)
count=0
for x in positionsx[0]:
	count=count+1

#the animation initialization function
def init():
	global limits
	for j in range(1,numberofobjects+1):
		if(count < numberofobjects+1):
			for i in range(1,count):
				if 2*abs(positionsx[j-1][i-1])>limits:
					limits=2*abs(positionsx[j-1][i-1])
				if 2*abs(positionsy[j-1][i-1])>limits:
					limits=2*abs(positionsy[j-1][i-1])
		else:
			for i in range(1,numberofobjects+1):
				if 2*abs(positionsx[j-1][i-1])>limits:
					limits=2*abs(positionsx[j-1][i-1])
				if 2*abs(positionsy[j-1][i-1])>limits:
					limits=2*abs(positionsy[j-1][i-1])
	
	tex01=[plt.text((1.0/5)*(1.0/scale)*limits,(4.0/5)*(1.0/scale)*limits, 'last frame:')]
	tex02=[plt.text((3.5/5)*(1.0/scale)*limits,(4.0/5)*(1.0/scale)*limits, count-1)]				
	ax.set_xlim(-(1.0/scale)*limits,(1.0/scale)*limits)
	ax.set_ylim(-(1.0/scale)*limits,(1.0/scale)*limits)
	ax.grid()
	return []
	
#the animation update algorithm
def update(i):
	tex1=[plt.text((2.0/5)*(1.0/scale)*limits,-(4.0/5)*(1.0/scale)*limits, 'frame:')]
	tex2=[plt.text((3.5/5)*(1.0/scale)*limits,-(4.0/5)*(1.0/scale)*limits, i)]
	tex3=[plt.text((2.0/5)*(1.0/scale)*limits,-(4.5/5)*(1.0/scale)*limits, 'day:')]
	tex4=[plt.text((3.5/5)*(1.0/scale)*limits,-(4.5/5)*(1.0/scale)*limits, (i)*timestep)]	

	for j in range(1,numberofobjects+1):
		traces[j-1][0].set_xdata(positionsx[j-1][0:i+1])
		traces[j-1][0].set_ydata(positionsy[j-1][0:i+1])
		dots[j-1].set_offsets((positionsx[j-1][i],positionsy[j-1][i]))	
		if j==1:
			result=traces[0]
		else:
			result=result+traces[j-1]
	result=result+dots+tex1+tex2+tex3+tex4
	return result

ani = animation.FuncAnimation(fig,update,frames=count,interval=50,blit=True,init_func=init)
plt.show()












