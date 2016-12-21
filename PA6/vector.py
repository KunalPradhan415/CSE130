from misc import Failure

#Told to use by the TA
""" This function returns True if the paramater is a sequence"""
def isSequence(obj):
    return type(obj) in [str, unicode, list, tuple, buffer, xrange]
#My own helper function
""" This helper function is intended to help me iterate through the list"""
def helpIter(obj):
	tempLen = obj.vecLen
	num = 0
	while num < tempLen:
		yield (obj.vec)[num]
		num+= 1

class Vector(object):
	def __init__ (self, value):
		if isinstance(value, long) or isinstance(value, int):
			self.vecLen = value
			if self.vecLen < 0:
				raise ValueError("Vector length cannot be negative")
			else:
				self.vec = [0.0] * self.vecLen
		elif isSequence(value):
			self.vecLen = len(value)
			self.vec = list(value)
		else:
			raise TypeError("Wrong type was inputted")

	def __repr__(self):
		return "Vector(" + str (self.vec) + ")"

	def __len__(self):
		return self.vecLen

	def __iter__(self):
		return helpIter(self)

	""" Implement binary operators """

	""" Allows you to add vectors and sequences """
	def __add__(self, other):
		tempLen = self.vecLen
		if len(other) != tempLen:
			raise ValueError("length must be the same")
		elif isSequence(other) or isinstance(other, Vector):
			tempList = []
			count = 0
			for i in other:
				tempList.append(self.vec[count] + i)
				count += 1
			return Vector(tempList)
		else:
			raise ValueError("second arg must be sequence")

 	""" Allows you to add vectors and sequences in a right
 		precedence
 	"""
	def __radd__(self, other):
		tempLen = self.vecLen
		if len(other) != tempLen:
			raise ValueError("length must be the same")
		elif isSequence(other) or isinstance(other, Vector):
			tempList = []
			count = 0
			for i in other:
				tempList.append(self.vec[count] + i)
				count += 1
			return Vector(tempList)
		else:
			raise TypeError("second arg must be sequence")

 	""" Allows you to use add in place operator """
	def __iadd__(self, other):
		tempLen = self.vecLen
		if len(other) != tempLen:
			raise ValueError("length must be the same")
		elif isSequence(other) or isinstance(other, Vector):
			tempList = []
			count = 0
			for i in other:
				tempList.append(self.vec[count] + i)
				count += 1
			return Vector(tempList)
		else:
			raise TypeError("not sequence or a vector")

	""" Implement dot product - sum of the components
		of the list
	"""
	def dot(self, other):
		tempLen = self.vecLen
		if len(other) != tempLen:
			raise ValueError("length must be the same")
		elif isSequence(other) or isinstance(other, Vector):
			sum = 0
			count = 0
			for i in other:
				if isinstance(i,int) or isinstance(i,long):
					sum = sum + (self.vec[count] * i)
					count += 1
				else:
					raise TypeError ("Not numeric - undefined")
			return sum
		else:
			raise TypeError("not sequence or a vector")

	""" Implement get and set item functionality """

 	""" Allows you to get the value at the specified index
 		and supports slicing
 	"""
 	def __getitem__(self, arg):
 		if isinstance(arg, slice) or isinstance(arg, int):
 			return self.vec[arg]


  	""" Allows you to set the value at the specified index
 		and supports slicing
 	"""
 	def __setitem__(self, arg1, arg2):
 		#get initial length
 		tempLen= self.vecLen
 		#copy list to use
		tempList = list(self.vec)
 		if isinstance(arg1, slice) or isinstance(arg1,int):
 			tempList[arg1] = arg2
 		if len(tempList) == tempLen:
 			self.vec = tempList
 		else:
 			raise ValueError("Changed the length- error")


 	""" Implement comparison operators"""

 	""" Return true if every element in self is
 		equal to its respective counterpart in a
 		Vector
 	"""
 	def __eq__(self, a):
 		if isinstance(a,Vector):
 			holder = True
 			for i,j in zip(self.vec,a.vec):
 				if i != j:
 					holder = False
 			return holder
 		elif isSequence(a):
 			return False
 		else:
 			return False

 	""" Return true if every element in self is not
 		equal to its respective counterpart in a Vector
 		or if the other object is not a vector
 	"""
 	def __ne__(self, a):
 		if isinstance(a,Vector):
 			holder = False
 			for i,j in zip(self.vec,a.vec):
 				if i != j:
 					holder = True
 			return holder
 		elif isSequence(a):
			return True
		else:
			return True

 	""" Return true if every element in self is
 		greater than its respective counterpart in a
 		Vector when both are sorted
 	"""
 	def __gt__(self, a):
   		x = list(self.vec)
 		y = list(a.vec)
 		x.sort()
 		y.sort() 
 		for i,j in zip(x,y):
 			if i > j :
 				return True
 			elif i == j:
 				pass
 			else:
 				return False
 		return False

 	""" Return true if every element in self is
 		greater than or equal to its respective counterpart 
 		in a Vector when both are sorted
 	"""
 	def __ge__(self, a):
  		x = list(self.vec)
 		y = list(a.vec)
 		x.sort()
 		y.sort() 
 		for i,j in zip(x,y):
 			if i >= j :
 				return True
 			else:
 				return False

 	""" Return true if every element in self is less
 		than its respective counterpart 
 		in a Vector when both are sorted
 	"""
 	def __lt__(self, a):
 		x = list(self.vec)
 		y = list(a.vec)
 		x.sort()
 		y.sort()
 		for i,j in zip(x,y):
 			if i < j :
 				return True
 			elif i == j:
 				pass
 			else:
 				return False
 		return False

 	""" Return true if every element in self is less
 		than or  equal to its respective counterpart 
 		in a Vector when both are sorted
 	"""
 	def __le__(self, a):
 		x = list(self.vec)
 		y = list(a.vec)
 		x.sort()
 		y.sort() 
 		for i,j in zip(x,y):
 			if i <= j :
 				return True
 			else:
 				return False





 		




    	 


