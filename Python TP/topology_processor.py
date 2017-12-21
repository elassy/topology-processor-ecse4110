'''
Author: Youssef Elasser
		elassy@rpi.edu

ECSE 4110 Project

'''

## ------ Imports --------------------------------------------------------------

import numpy as np


## ------ Functions ------------------------------------------------------------

# Changes Investigator function, comparing the current switch matrix to the old
# switch matrix to see what breakers have had changes in status
def changes_investigator(cur, old, config):
	a = 0
	b = 0
	out = np.zeros([len(cur),2])

	num_stations = int(np.amax(old, axis=0)[1]) # The number of substations is given
										        # by max(col_2) of switch matrix
	num_circuits = int(np.amax(config, axis=0)[0]) # max(col_1) of config mtx is
												   # number of circuits present

	# Check and save changed braker statuses
	for i in range(len(cur)):
		if cur[i][4] != old[i][4]:
			print "Switch %d has changed."%(i+1)
			if old[i][3] == 3: 		# If the breaker is of Type 3...
				out[a][0] = old[i][7]
				out[a][1] = old[i][0]
				a += 1


	# For the substations that have a status change in their breakers, we create
	# a matrix of them, called 'sub'
	sub = []
	for i in range(num_stations):
		for j in range(a):
			if int(out[j][0]) == i+1:
				sub.append(i+1)
				b += 1
				break

	# For the substations in 'sub', we find them in the config matrix, and make 
	# them positive if they're negative
	for i in range(num_circuits):
		quit = False;
		for j in range(b):
			if np.absolute(config[i][1]) in sub or config[i][2] in sub:
				quit = True
		if config[i][1] < 0 and quit:
			config[i][1] = -config[i][1]

	# We return the 'sub' matrix, the first of the outputs, which reports
	# substations in which switching has occurred. We also return 'b', which 
	# tells us how many breakers have changed in status, as well as 'out', 'a'
	return (config, sub, b, out, a)


# Susbstation Splitting/Merging function, for the type 3 switches that have changed
def substation_split_merge(b, sub, out, a, st, config):
	num_stations = int(np.amax(st, axis=0)[1]) # The number of substations is given
										       # by max(col_2) of switch matrix
	num_circuits = int(np.amax(config, axis=0)[0]) # max(col_1) of config mtx is
												   # number of circuits present

	for i in range(b):
		for j in range(num_stations):
			if st[j][7] == sub[0][i] and int(st[j][3]) == 3: # Check for type 3
				st[j][1] = st[j][2] = sub[0][i]

		for j in range(num_circuits): # Make appropriate updates in config mtx
			if config[j][10] == sub[0][i]:
				config[j][1] = config[j][10]
			if config[j][11] == sub[0][i]:
				config[j][2] = config[j][11]

	for i in range(b):
		for j in range(num_stations):
			if int(st[j][4]) == 0 and st[j][7] == sub[0][i] and int(st[j][4]) == 3:
				a += 1
				out[a][0] = sub[0][i]
				out[a][1] = j

	return (st, config, out)

# Circuit Connectivity Analysis for each substation in which switching has occurred
def circuit_conn_analysis(a, b, sub, out, switch_table, configuration):
	num_stations = int(np.amax(st, axis=0)[1]) # The number of substations is given
										       # by max(col_2) of switch matrix

	flag = False

	for i in range(a):
		q = c = d = l = 0
		p1 = q1 = p2 = q2 = f = 1
		q3 = 2
		for t in range(q3):
			for y in range(q1):				
				for u in range(q2):
					if int(out[i][1]) == 1000 or int(switch_table[out[i][1]][4]) != 0:
						q = 1
						break
					if int(switch_table[out[i][1]][5]) == 0 or switch_table[out[i][1]][7] == sub[b]:
						flag = True
					if q == 1: break

		q1 = 0
		p3 = b
		for m in range(p3):
			for j in range(p1):
				for i in range(p2):
					if int(switch_table[out[i][1]][5]) == 0 and m == (b - 1):
						q1 = 1
					if int(switch_table[out[i][1]][6]) == 0 and m == (b - 1):
						c = 1

		if q == 0:
			if int(switch_table[out[i][1]][7]) == int(sub[b]):
				if q1 == 0: d += 1
				x = 0
				while x < f:
					if int(switch_table[out[i][1]][5]) < 0:
						for num in range(num_stations):
							if int(switch_table[num][5]) == 0:
								if int(switch_table[num][3]) == 3 and int(switch_table[num][7] == sub[b]) and int(switch_table[num][4]) == 1:								
									for w in range(f):
										if int(switch_table[num][6]) == 0: l = 1
										if l == 0: f += 1
							if int(switch_table[num][6]) == 0:
								if int(switch_table[num][3]) == 3 and int(switch_table[num][7] == sub[b]) and int(switch_table[num][4]) == 1:								
									for w in range(f):
										if int(switch_table[num][6]) == 0: l = 1
										if l == 0: f += 1

					x += 1

	gx = 0
	List = [[],[]]
	if flag == False:
		gx = 1
		for zx in range(num_stations):
			if int(switch_table[zx][7]) == sub[b] and int(switch_table[zx][3]) == 3:
				E = 1
				l1 = l2 = 0
				for w1 in range(E-1):
					if int(switch_table[zx][5]) == 0:
						l1 = 1
					if int(switch_table[zx][6]) == 0:
						l2 = 1

				if l1 != 1:
					List[0].append(int(switch_table[zx][5]))
					gx += 1
				if l2 != 1:
					List[1].append(int(switch_table[zx][6]))
					gx += 1

	return List





## ------ Main Code ------------------------------------------------------------

if __name__ == '__main__':
	# First, we must read in the two input matrices into NumPy arrays:
	#	1. The Switch Table Matrix (N x 8 where N = # of switches)
	#	2. The Configuration Matrix (M x 12 where M = # of lines + buses)
	# As well, we read in (1) from the previous iteration, to be able to detect
	# changes between iterations

	switch_table_file = 'switch_table_IEEE.txt' 	# File Names are user
	configuration_file = 'configuration_IEEE.txt'	# specified values
	prev_switch_file = 'prev_switch_table_IEEE.txt'

	switch_table_file = 'func_test_new_st.txt' 	# File Names are user
	configuration_file = 'func_test_config.txt'	# specified values
	prev_switch_file = 'func_test_old_st.txt'

	switch_table = np.loadtxt(switch_table_file)
	configuration = np.loadtxt(configuration_file)
	prev_switch = np.loadtxt(prev_switch_file)

	# The topology engine first searches for those switches and disconnectors 
	# that have experienced a change in their status. If it has changed, we save
	# the breaker and corresponding substation number.
	# This 'sub' matrix is one of the outputs, as defined in the paper
	(configuration, sub, b, out, a) = changes_investigator(switch_table, prev_switch, configuration)
	
	print "Sub Matrix contents: ",
	print sub

	
	# Generate the 'max' list
	max = [x+1 for x in range(40)]

	# Substation Splitting/Merging Analysis, this is only if there are type 3
	# switches that have a status change. This is denoted by the variable 'b'.
	# The variables are updated in the function and appropriately returned
	(switch_table, configuration, out) = substation_split_merge(b, sub, out, a, switch_table, configuration)

	# Perform the connectivity/islanding analysis of the substations in which switching
	# has occurred
	List = circuit_conn_analysis(a, b, sub, out, switch_table, configuration)