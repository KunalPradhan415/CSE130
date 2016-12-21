#Winter 2012 Final
def square_img(img):
	return [ [  w *w for w in l ] for l in img ]


img1=[[ 11, 0, 12],
[ 0, 0, 0],
[ 13, 0, 14],
[ 15, 16, 17]]

#print square_img(img1)


def crop_img(img,x1,y1,x2,y2):
	return [ row[x1:x2] for row in img[y1::y2]]

def zip(l1,l2):
	ret = []
	maxLength = min(len(l1), len(l2) )
	for indx in range(maxLength):
		ret = ret + [(l1[indx] , l2[indx] ) ]
	return ret

#print zip([1,2,3], [4,5,6]) 
#print zip([1,2,3], [4,5])


def add_imgs(img1, img2):
	return [ [ j+k for (j,k) in zip (l1,l2) ] for (l1,l2) in zip(img1,img2)]


def derivative(delta):
	def decorator(f):
		def decorated(x):
			numerator = f(x + delta) - f(x)
			fraction = numerator/delta
			retVal = round(fraction,2)
			return retVal
		return decorated
	return decorator

def derivative(delta):
	class decorator(f):
		def __init__(self, f):
			self.f = f
		def __call__(self, arg):
			numerator = self.f(arg + delta) - self.f(arg)
			fraction = numerator/delta
			retVal = round (fraction, 2)
			return retVal
	return decorator 












# Winter 2013 final
def transpose(m):
	height = len(m)
	width = len(m[0])
	return [ [ m[i][j] for i in range(height) ] for j in range(width) ]

A=[[ 1, 2, 3],[ 4, 5, 6]]

#print transpose(A)

def access(g, x, y):
	try: return g[y][x]
	except: return 0

def count_live_neighbours(g, x, y):
	live = 0
	for x_delta in [ x-1 , x , x+1 ]:
		for y_delta in [ y -1 , y  , y + 1 ]:
			if access(g, x_delta, y_delta) == 1:
				live = live + 1 
	return live

def new_val(g, x, y):
	count = count_live_neighbours(g,x,y)
	point = access(g,x,y)
	if point == 1 and count < 2:
		return 0
	elif point == 1 and count == 2:
		return 1
	elif point == 1 and count == 3:
		return 1
	elif point ==1 and count > 3:
		return 0
	elif point == 0 and count == 3 :
		return 1
	else:
		return 0

def step(g):
	height = len(g)
	width = len(g[0])
	return [ [new_val(g,x,y) for x in range (width) ] for  y in range ( height ) ]

def lift_1(f):
	def decorated(x):
		return [ f(i) for i in x  ]
	return decorated

def lift_2(f):
	def decorated(x,y):
		return [ f(x[i],y[i]) for i  in range (len(x) ) ]
	return decorated




# Fall 2013 final


def lookup(d,k):
	return [ value for (key,value) in d if k == key ]

d = [ ("a", 10), ("b", 20), ("c", 30), ("a", 40) ]

#print lookup(d,"a")

def cond(b, t, f):
	if b: return t
	else: return f

"""def update(d,k,v):
	#temp
	return [ cond(k==key,(key,v),(key,value)) for (key,value) in d ]
"""

#print update(d, "a", "CSE130")

#print update(d, "b", "CSE130")

#print update(d, "d", "CSE130")

def delete(d,k):
	return [ (key,value) for (key,value) in d if key != k ]

#print delete(d, 'a')

def add(d,k,v):
	return [(x,y) for (x,y) in d] + [(k,v)]

#print add(d, 'z', 'War')

def update(d, k, v):
	newdic = []
	for (a,b) in d:
		if a == k:
			newdic.append((k,v))
		else:
			newdic.append((a,b))
	return newdic

#print update(d, "a", "CSE130")
"""
def in_range(i, range):
	def decorator(f):
		def decorated(*args):
			retVal = f(*args)
			if i == -1:
				if retVal < range[0]:
					raise Exception (" Return value " + str(retVal) + " too small" )
				elif retVal > range[1]:
					raise Exception (" Return value " + str(retVal) + " too big" )
			elif i >= 0 and i < len(args) :
				if args[i] < range[0]:
					raise Exception (str(i) + "th arg " + str(args[i]) + " too small")
				elif args[i] > range[1]:
					raise Exception (str(i) + "th arg " + str(args[i]) + " too big")
			return retVal
		return decorated
	return decorator
"""
def in_range(i, range):
	class decorator(f):
		def __init__(self,f):
			self.f = f
		def __call__(self, *args):
			retVal = self.f(*args)
			if i == -1:
				if retVal < range[0]:
					raise Exception (" Return value " + str(retVal) + " too small" )
				elif retVal > range[1]:
					raise Exception (" Return value " + str(retVal) + " too big" )
			elif i >= 0 and i < len(args) :
				if args[i] < range[0]:
					raise Exception (str(i) + "th arg " + str(args[i]) + " too small")
				elif args[i] > range[1]:
					raise Exception (str(i) + "th arg " + str(args[i]) + " too big")
			return retVal
	return decorator




# Spring 2013 final
#def rev(l):
#	return [l[-i] for i in range(1,len(l)+1)]
#def rev2(l):
#	return l[:: -1]

def rev(l):
	def fold_fn(acc,elm): return [elm] + acc  
	return reduce(fold_fn, l, [])

#print rev([1,5,3,4])













