
from misc import *
import crypt
import re
from time import time


def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    #open the file 
    myFile = open(filename, 'r')
    #declare return list
    listReturn = []
    #loop through the file
    for line in myFile:
      for word in line.split():
        if re.match(regexp, word):
          listReturn.append(word)    
    return listReturn

def transform_reverse(str):
    x = str
    #reverse list
    y = str [::-1]
    return [x, y]

def transform_capitalize(str):
    #make sure the value is all lowercase to get all possible caps
    value = str.lower()
    #store it in a list
    myList  = [value]
    #loop through chars
    for indx in range (0, len(value)):
      tempList = []
      #loop through words, and change character
      for x in myList:
        up = x[indx].upper()
        holder = x[0:indx] + up + x[indx+1::]
        tempList.append(holder)
      myList = myList + tempList
    #return list without duplicates
    listReturn = []
    for i in myList:
      if i not in listReturn:
        listReturn.append(i)
    return listReturn


def transform_digits(str):
    value = str
    myList  = [value]
    #same principle as transform_capitalize
    #loop through characters
    for indx in range (0, len(value)):
      tempList = []
      #match
      for x in myList:
        check = x[indx]
        if check == 'o' or check == 'O':
          check = '0'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 'b' or  check == 'B' :
          check = '6'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
          check = '8'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder) 
        if check == 'i' or check == 'I' or check == 'L' or check =='l':
          check = '1'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check ==  'z' or check == 'Z': 
          check = '2'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 'e' or check == 'E' :
          check = '3'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 'a' or check == 'A' :
          check = '4'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 's' or check == 'S' :
          check = '5'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 't' or check == 'T' :
          check  = '7'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        if check == 'g' or check == 'G' or check == 'q' or check == 'Q':
          check = '9'
          holder = x[0:indx] + check + x[indx+1::]
          tempList.append(holder)
        
      myList = myList + tempList
    return myList

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    if crypt.crypt(plain, enc[0:2]) == enc:
      return True
    else:
      return False

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    #open file
    myFile = open(filename, 'r')
    listReturn = []
    #split each line and add to dic
    for x in myFile:
      dic = {}
      tempList = re.split (':', x)
      dic['account'] = tempList[0]
      dic['shell'] = tempList[6].rstrip('\n')
      dic['UID'] = int(tempList[2])
      dic['GID'] = int(tempList[3])
      dic['GECOS'] = tempList[4]
      dic['directory'] = tempList[5]
      dic['password'] = tempList[1]
      #append to list
      listReturn.append(dic)
    return listReturn


def crack_pass_file(pass_filename,words_filename,out_filename):
  """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
  #load the necessary words, etc
  start = time()   
  wordList = load_words(words_filename, r"^.{6,8}$")
  passwordList = load_passwd(pass_filename)
  #get total number of passwords
  passwordLen = len(passwordList)
  #store found accounts
  final = []
  #open the file
  outFile = open(out_filename, 'w')

  #for untransformed and reverse
  # combined for efficiency
  # avoid use of all variables when possible
  if passwordLen == len(final):
    return
  for word in wordList:
    for transform in transform_reverse(word):
      for user in passwordList:
        if check_pass(transform, user['password']):
          outFile.write(user['account'] + "=" + transform + "\n")
          outFile.flush()
          final.append(user)
          passwordList.remove(user)
  #for digit
  if passwordLen == len(final):
    return
  for word in wordList:
    for digit in transform_digits(word):
      for user in passwordList:
        if check_pass(digit, user['password']):
          outFile.write(user['account'] + "=" + digit + "\n")
          outFile.flush()
          final.append(user)
          passwordList.remove(user)
  #reverse and capitalize
  if passwordLen == len(final):
    return
  for word in wordList:
    for reverse in transform_reverse(word):
      for cap in transform_capitalize(reverse):
        for user in passwordList:
          if check_pass(cap, user['password']):
            outFile.write(user['account'] + "=" + cap + "\n")
            outFile.flush()
            final.append(user)
            passwordList.remove(user)
  #reverse and digits
  if passwordLen == len(final):
    print('FIN @ %0.3fs' % (time() - start))
    return
  for word in wordList:
    for reverse in transform_reverse(word):
      for digit in transform_digits(reverse):
        for user in  passwordList:
          if check_pass(digit, user['password']):
            outFile.write(user['account'] + "=" + digit + "\n")
            outFile.flush()
            final.append(user)
            passwordList.remove(user)
  #digits + capitalize
  if passwordLen == len(final):
    return
  for word in wordList:
    for digit in transform_digits(word):
      for cap in transform_capitalize(digit):
        for user in passwordList:
          if check_pass(cap, user['password']):
            outFile.write(user['account'] + "=" + cap + "\n")
            outFile.flush()
            final.append(user)
            passwordList.remove(user)
  #go through all 3- brute force
  if passwordLen == len(final):
    return
  for word in wordList:
    for digit in transform_digits(word):
      for cap in transform_capitalize(digit):
        for reverse in transform_reverse(cap):
          for user in passwordList:
            if check_pass(reverse, user['password']):
              outFile.write(user['account'] + "=" + reverse + "\n")
              outFile.flush()
              final.append(user)
              passwordList.remove(user)
              if len(final) == passwordLen:
                return
  #close the file
  outFile.close()






