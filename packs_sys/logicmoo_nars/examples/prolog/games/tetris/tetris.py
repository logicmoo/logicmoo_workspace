# -*- coding: utf-8 -*-
from pyswip import Prolog
from random import randint
import time

class ansi:
    END 	= '\033[0m'
    BOLD 	= '\033[1m'
    RED 	= '\033[31m'
    GREEN 	= '\033[32m'
    YELLOW 	= '\033[33m'
    BLUE 	= '\033[34m'
    MAGENTA	= '\033[35m'
    GRAY 	= '\033[30;1m'
    CLEAR 	= '\033[2J'
    @staticmethod
    def POS(_x=0,_y=0):
	   return '\033['+str(_x)+';'+str(_y)+'H'

prolog = Prolog()
prolog.consult("tetris");

def show(x,y,stone,field):
	newfield = []
	out = ""
	#print x,y
	a = 0
	i=0
	n="   "
	for b in range(0,11):
		n+= ("%01d " % b)
	out += n+"\n"
	for row in field:
		out+= ("%02d" % a)
		newrow = []
		printrow = []
		if i<len(stone) and y==0:
			#print x,y,row[0:x],stone[i],row[x+len(stone[i]):]
			l = row[0:x]+stone[i]+row[x+len(stone[i]):]
			#print l
			#print row
			i+=1
			for p,q in zip(row,l):
				if p==2: 
					newrow.append(2)
					printrow.append(2)
				elif p==1: 
					newrow.append(1)
					printrow.append(1)
				elif q==1: 
					newrow.append(1)
					printrow.append(3)
				else: 
					newrow.append(0)
					printrow.append(0)
		else:
			y-=1
			for p in row:
				if p==2: 
					newrow.append(2)
					printrow.append(2)
				elif p==1: 
					newrow.append(1)
					printrow.append(1)
				elif p==0: 
					newrow.append(0)
					printrow.append(0)

		if ([2]+[1]*(len(row)-2)+[2]) == newrow:
			newfield.reverse()
			newfield.append([2]+[0]*(len(row)-2)+[2])
			newfield.reverse()
			for p in printrow:
				if p==2: 
					out+= ansi.GRAY+'▉ '+ansi.END
				elif p==3: 
					out+= ansi.MAGENTA+'▉ '+ansi.END
				else:
					out+= ansi.YELLOW+'▉ '+ansi.END
		else:
			newfield.append(newrow)
			for p in printrow:
				if p==3: 
					out+= ansi.BLUE+'▉ '+ansi.END
				if p==2: 
					out+= ansi.GRAY+'▉ '+ansi.END
				elif p==1: 
					out+= ansi.RED+'▉ '+ansi.END
				elif p==0: 
					out+= '▉ '
		
		out +="\n"
		a+=1
	return newfield,out
		

field =[[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,0,0,0,0,0,0,0,0,0,0,2],
		[2,2,2,2,2,2,2,2,2,2,2,2]]

stones =[[[0,0,0,0],
		[0,1,1,0],
		[0,1,0,0],
		[0,1,0,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,1,1,0],
		[0,0,1,0],
		[0,0,1,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,1,0,0],
		[0,1,0,0],
		[0,1,1,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,0,1,0],
		[0,0,1,0],
		[0,1,1,0],
		[0,0,0,0]],

		[[0,0,0,0,0],
		[0,0,0,1,0],
		[0,1,1,1,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,1,0,0,0],
		[0,1,1,1,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,1,1,1,0],
		[0,0,0,1,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,1,1,1,0],
		[0,1,0,0,0],
		[0,0,0,0,0]],

		[[0,0,0],
		[0,1,0],
		[0,1,0],
		[0,1,0],
		[0,1,0],
		[0,0,0]],

		[[0,0,0,0,0,0],
		[0,1,1,1,1,0],
		[0,0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,0,1,1,0],
		[0,1,1,0,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,1,1,0,0],
		[0,0,1,1,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,0,1,0,0],
		[0,1,1,1,0],
		[0,0,0,0,0]],

		[[0,0,0,0,0],
		[0,1,1,1,0],
		[0,0,1,0,0],
		[0,0,0,0,0]],

		[[0,0,0,0],
		[0,0,1,0],
		[0,1,1,0],
		[0,1,0,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,1,0,0],
		[0,1,1,0],
		[0,0,1,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,0,1,0],
		[0,1,1,0],
		[0,0,1,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,1,0,0],
		[0,1,1,0],
		[0,1,0,0],
		[0,0,0,0]],

		[[0,0,0,0],
		[0,1,1,0],
		[0,1,1,0],
		[0,0,0,0]]]

#field = [[2,0,0,0,2],
#		[2,0,0,0,2],
#		[2,1,0,0,2],
#		[2,0,1,0,2],
#		[2,0,0,0,2],
#		[2,0,0,0,2],
#		[2,2,2,2,2]]

#stones = [[[0,0,0,0],
#		[0,0,1,0],
#		[0,1,1,0],
#		[0,0,1,0],[0,0,0,0]]]


s = None
count=1
startTime = time.time()
while(s!='exit'):
	
	stone = stones[randint(0,len(stones)-1)]
	q="my_best_position(%s,%s,XPos,YPos,Stone)." % (str(field),str(stone))
	#print q
	pos = list(prolog.query(q))[0]
#print pos
	
	count=count+1
	timePerStone=int(round((time.time()-startTime)/count*1000))	
	
	out = ansi.CLEAR
	# draw old stone
	out += show(0,0,stone,[[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]])[1]
	out += "\n"
	# draw rotated stone
	out += show(0,0,pos['Stone'],[[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]])[1]
	out += "\n"
	#draw game field
	field,asd = show(pos['XPos'],pos['YPos'],pos['Stone'],field)
	out += asd
	out += "\n"
	print out
	print "stone no. %d - %d ms/stone" % (count, timePerStone)
    #    print "current stone:"
	#s = raw_input('$ ')
	#print "placed at: "+str(pos['XPos'])+"/"+str(pos['YPos'])
	if pos['XPos']==-1 and pos['YPos']==-1:
		s = 'exit'
print "stone no. %d" % count
#print list(prolog.query("""allpositions(%s,%s,Z).""" % 
#			(str(field),str(stone))))

#print list(prolog.query("""positions(XPos,YPos,%s,%s,P)"""% 
#			(str(field),str(stone))))
