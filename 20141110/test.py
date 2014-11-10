import os 
from subprocess import call
#call(["ls"])
#os.path.isfile(fname)

# f=open('dirs.txt','r') # open directory file
# 
# ll=f.readlines()
# for line in ll:
# 	#print line
# 	dir_exists=(os.path.exists(line.rstrip('\n'))) # checks to make sure directory exists, returns bool
# 	print(dir_exists)
# 	if dir_exists==True:
# 		print ('Dir Exists. Deleting....')
# 		call(['rm','-rf',line.rstrip('\n')])
# 	if dir_exists==False:
# 		print('Dir does not Exist. Creating....')
# 		call(['mkdir',line.rstrip('\n')])
# 		
# f.close()

f=open('params.txt','r')
ll=f.readlines()
for line in ll:
	args=line.split(',')
	dir=args[2]+'_'+args[0]+'_'+args[1]
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
	call(['Rscript','foo.R',args[0],args[1],args[2],dir])