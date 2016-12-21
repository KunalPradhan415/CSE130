//CSE130.py

def funny_fun (s):
	for x in s:
		print(x)
		s.append(x)

def logged(f):
	def g(*args):
		print ("Call" + f._name_ + args "args: " + str(args))
		rv = f(*args)
		print ("Returns" + str(rv))
		return rv






		