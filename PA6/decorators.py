from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

#test with decorators.run_examples()

""" This decorator will print out the function calls and
    return values so you can trace the function.
"""
class traced(object):
    #maintain the nesting level
    __nest = 0
    def __init__(self,f):
        #initalize the value
        self.__name__= f.__name__
        self.__f = f
    def __call__(self, *args,**dargs):
        #build the string as the TA said to do
        # print the correct number of pipes and ",="
        x = "| " * traced.__nest + ",- "
        x = x + str(self.__name__) + "("
        temp1 = []
        tempComma = ", "
        for i in args:
            temp1.append(repr(i))
        x = x + tempComma.join(temp1)
        temp2 = []
        #key words
        for key,val in dargs.items():
            tempValue = str(key)+ "=" + repr(val)
            temp2.append(tempValue)
        x = x + tempComma.join(temp2) + ")"
        print x
        #increment nest 
        traced.__nest = traced.__nest + 1
        #call function/catch exception
        try:
            result = self.__f(*args,**dargs)
            traced.__nest = traced.__nest -1
            x =  "| " * traced.__nest + "`- " + repr(result)
            print x
            return result
        except Exception as excep:
            traced.__nest = traced.__nest -1
            raise excep

""" This decorator will store function arguments and
    return values so you that when you call the function 
    it is quicker to return
"""
class memoized(object):
    def __init__(self,f):
        self.__name__= f.__name__
        self.__f = f
        self.__store = {}
    def __call__(self, *args,**dargs):
        #build the possible key
        storeArg = ''
        storeDarg = ''
        #loop through args
        for x in args:
            storeArg = storeArg + "@@!@@" + repr(x)
        #loop though dargs
        for d in dargs:
            storeDarg = storeDarg + str(d)+ "@@!@@" + repr(dargs[d])
        #check if it already has been called and if so return prior value
        for (i,j) in self.__store:
            if i == storeArg and j == storeDarg:
                storeValue = self.__store[(i,j)]
                if isinstance(storeValue, Exception):
                    raise storeValue
                else:
                    return storeValue
        #else call the function
        try:
            result = self.__f(*args,**dargs)
            self.__store[(storeArg,storeDarg)] = result 
            return result
        except Exception as excep:
            self.__store[(storeArg,storeDarg)] = excep
            raise excep


# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)