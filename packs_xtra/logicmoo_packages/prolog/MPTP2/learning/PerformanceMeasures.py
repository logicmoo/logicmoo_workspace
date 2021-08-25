
import math
import numpy
def ERROR(true,pred):
	error=0.0
	assert len(true)==len(pred)
	for t,p in zip(true,pred):
		if numpy.sign(t)!=numpy.sign(p):
			error=error+1
	totalerror=float(error)/float(len(true))
	return totalerror


def computeAUC(deci, label):
	#count of postive and negative labels
	db = []
	pos, neg = 0, 0
	for i in range(len(label)):
		if label[i]>0:
			pos+=1
		else:	
			neg+=1
		db.append([deci[i], label[i]])
	
	#sorting by decision value
	def cmp (x,y):
		if x[0] < y[0]:
			return 1
		else:
			return -1;
	
	db.sort(cmp)
	
	#calculate ROC 
	xy_arr = []
	tp, fp = 0., 0.			#assure float division
	for i in range(len(db)):
		if db[i][1]>0:		#positive
			tp+=1
		else:
			fp+=1
		xy_arr.append([fp/neg,tp/pos])
	
	
	#area under curve
	auc = 0.
	prev_x = 0
	for x,y in xy_arr:
		if x != prev_x:
			auc += (x - prev_x) * y
			prev_x = x
	
	
	return auc




def computeWMW(deci, label):
	#count of postive and negative labels
	db = []
	pos, neg = 0., 0.
	posindices = []
	negindices = []
	for i in range(len(label)):
		if label[i]>0:
			pos += 1.
			posindices.append(i)
		else:	
			neg += 1.
			negindices.append(i)
		db.append([deci[i], label[i]])
	
	auc = 0.
	for i in posindices:
		for j in negindices:
			if deci[i] > deci[j]:
				auc += 1.
			elif deci[i] == deci[j]:
				auc += 0.5
	auc /= pos * neg
	
	return auc



def kendall(A, B):
	
	numerator = 0.
	
	a_squares = 0.
	b_squares = 0.
	
	for i in range(0, len(A)):
		for j in range(i + 1, len(A)):
			score_a = A[i] - A[j]
			if not score_a == 0: score_a /= abs(score_a)
			score_b = B[i] - B[j]
			if not score_b == 0: score_b /= abs(score_b)
			numerator += score_a * score_b
			a_squares += 2 * abs(score_a)
			b_squares += 2 * abs(score_b)
	kendall = 0.
	#comb = 0.5 * len(A) * (len(A) - 1)
	#alternative_a = numerator / comb
	
	denominator = 0.5 * math.sqrt(a_squares * b_squares)
	if denominator == 0:
		kendall = 0.
		#raise Exception, 'Zero denominator when calculating Kendall!'
		denominator = 1.
	else:
		kendall = numerator / denominator
	
	#print a_squares, b_squares, numerator, denominator
	return kendall
#


def precomputeIndexPairs(A):
	a_squares = 0.
	pairs = []
	for i in range(0, len(A)):
		for j in range(i + 1, len(A)):
			score_a = A[i] - A[j]
			if not score_a == 0:
				score_a /= abs(score_a)
				pairs.append((i, j, score_a))
			a_squares += 2 * abs(score_a)
	return a_squares, pairs


def kendallFast(A, B, a_squares, a_pairs, b_ranks):
	
	b_squares = 0.
	for i in range(len(b_ranks)):
		for j in range(i + 1, len(b_ranks)):
			b_squares += 2 * b_ranks[i] * b_ranks[j]
	
	numerator = 0.
	
	for i, j, score_a in a_pairs:
		if B[i] < B[j]:
			numerator -= score_a
		elif B[i] > B[j]:
			numerator += score_a
	kendall = 0.
	
	#comb = 0.5 * len(A) * (len(A) - 1)
	#alternative_a = numerator / comb
	
	denominator = 0.5 * math.sqrt(a_squares * b_squares)
	if denominator == 0:
		kendall = 0.
		#raise Exception, 'Zero denominator when calculating Kendall!'
		denominator = 1.
	else:
		kendall = numerator / denominator
	
	#print a_squares, b_squares, numerator, denominator
	return kendall


def computeAccuracy(deci, label):
	correct = 0.
	for i in range(len(label)):
		if label[i] * deci[i] > 0:
			correct += 1.
	acc = correct / len(label)
	return acc

if __name__ == '__main__':
	
	
	from numarray import *
	import numarray.linear_algebra as la
	import numarray.mlab as mlab
	
	#A = [7, 4, 3, 10, 6, 2, 9, 8, 1, 5]
	#B = [5, 7, 3, 10, 1, 9, 6, 2, 8, 4]
	#A = [1, 2.5, 2.5, 4.5, 4.5, 6.5, 6.5, 8, 9.5, 9.5,]
	#B = [1, 2, 4.5, 4.5, 4.5, 4.5, 8, 8, 8, 10]
	#A = range(0, 10)
	#B = range(0, 10)
	#A = [1, 2.5, 2.5, 4.5, 4.5, 6.5, 6.5, 8, 9.5, 9.5,]
	#B = A
	
	A = []
	for i in range(0, 609):
		A.append(0)
	for i in range(0, 14):
		A.append(1)
	for i in range(0, 10):
		A.append(2)
	for i in range(0, 22):
		A.append(3)
	B = []
	for i in range(0, 651):
		B.append(0)
	for i in range(0, 4):
		B.append(1)
	
	a_squares, a_pairs = precomputeIndexPairs(A)
	b_ranks = [651, 4, 0, 0]
	print kendallFast(A, B, a_squares, a_pairs, b_ranks)
	#print kendall(A, B)
	sys.exit()
	
	m = 10
	a = mlab.rand(m)
	b = mlab.rand(m)
	for i in range(m):
		if a[i] < 0.5: a[i] = -1
		else: a[i] = 1
		if b[i] < 0.5: b[i] = -1
		else: b[i] = 1
	a = array(a, type = Int32)
	b = array(b, type = Int32)
	
	print a
	print b
	
	print computeWMW(a, b), computeWMW(b, a)
