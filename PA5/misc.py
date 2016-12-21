#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    #get length   
    length = len(l)
    #check length for viability
    if length == 0:
      return None
    # loop through the list
    close = l[0]
    for i in l:
      if abs(v-i) < abs(v-close):
          close = i
    return close




def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    # make blank dic
    dic = {}
    # get pair of keys,values and add to dic
    for i,j in zip(keys, values):
      dic[i] = j
    return dic  
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    #open the file
    myFile = open(fn, 'r')
    x = []
    dic = {}
    #read through the file
    for line in myFile:
      x = re.findall('\w+', line, flags = re.IGNORECASE) 
      for i in x:
        #check if value has already been seen
        if dic.has_key(i.lower()) == True:
          dic[(i.lower())]+=1
        else:
          #first time value was seen
          temp = i.lower()
          dic[temp] = 1 
    return dic



