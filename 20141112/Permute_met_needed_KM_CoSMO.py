import os 
from subprocess import call


f=open('params.txt','r')
ll=f.readlines()
for line in ll:
	line=line.rstrip('\n')
	args=line.split(',')
	dir=args[1]+'_'+args[0]
	dir_exists=(os.path.exists(dir)) # checks to make sure directory exists, returns bool
	print(dir_exists)
	if dir_exists==True:
		print ('Dir Exists. Deleting....')
		call(['rm','-rf',dir])
		print('Making new dir....')
		call(['mkdir',dir])
	if dir_exists==False:
		print('Dir does not Exist. Creating....')
		call(['mkdir',dir])
	call(['Rscript','CoSMO_Euler_KM_permute_metinit.R',args[0],args[1],dir])
