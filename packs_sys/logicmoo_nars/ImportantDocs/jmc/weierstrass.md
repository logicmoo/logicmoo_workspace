Reprinted from the AMERICAN MATHEMATICAL MONTHLY,

Vol. LX, No. 10, December 1953

AN EVERYWHERE CONTINUOUS NOWHERE DIFFRENTIABLE

FUNCTION

John McCarthy, Princeton University

The following is an especially simple example. It is

f (x) =

2−ng(22n

x)

∞

X

n=1

where g(x) = 1 + x for −2 ≤ x ≤ 0, g(x) = 1 − x for 0 ≤ x ≤ 2 and g(x) has

period 4.

The function f (x) is continuous because it is the uniform limit of con-

tinuous functions. To show that it is not diﬀerentiable, take ∆x = ±2−2k

,

choosing whichever sign makes x and x + ∆x be on the same linear segment

of g(22k

x). We have

x) = 0 for n > k, since g(22n

x)| = 1.

1. ∆g(22n

2. |∆g(22k

3. |∆ Pk−1

Hence |∆f /∆x| ≥ 2−k22k

The proof that the present example has the required property is simpler

which goes to inﬁnity with k.

x)| ≤ (k−1)max|∆g(22n

x) has period 4 · 2−2n

x)| ≤ (k−1)22k−1

< 2k2−2k−1

− 2k22k−1

n=1 g(22n

2−2k

.

.

than that for any other example the author has seen.

Weierstrass gave the example F (x) = P∞

n=0 bn cos(anπx) for b < 1 and

ab > 1 + 3π/2 which is discussed in Goursat-Hedrick Mathematical Analysis.

A complete discussion of functions with various singular properties is

given in Hobson, Functions of a Real Variable, volume II, Cambridge, 1926.

2006 January note: I was tempted to dig up this 1953 note of mine and put it

on my web page by reading The Calculus Gallery by William Dunham. This

excellent book includes the ﬁrst proofs of a number of important theorems,

including Weierstrass’s proof that his function has the required properties.

Dunham’s version of Weierstrass’s proof is six pages of what Dunham de-

scribes as diﬃcult mathematics. Since my proof is 13 lines of what I consider

easy math, I decided to copy my old note and discuss it. Dunham recounts

that the famous mathematicians Hermite, Poincare and Picard all expressed

themselves as repelled by Weierstrass’s “pathological example”.

I’m sure

that by the time I was born in 1927, such functions were no longer regarded

as repellent. My own opinion is that most everywhere continuous functions,

in some suitable sense of most, are nowhere diﬀerentiable.

Remarks:

1. To prove a function f diﬀerentiable at x, one must show that no matter

how ∆x goes to zero, f (x + ∆x)/∆x approaches a limit. To prove f non-

diﬀerentiable at x, one need only ﬁnd a sequence of values of ∆x for which

the limit doesn’t exist.

2. If f (x) is to be represented as the sum of a series of continuous func-

tions, it suﬃces to bound the terms by a suitable positive terms, 2−k in our

case. Then f is sure to be everywhere continuous. It doesn’t matter how

fast the successive terms wiggle.

3. In our case, the terms are 2−kg(22k

x). The 22k

x grows fast enough to

overcome the 2−k damping.

g(x).

4. I would expect P∞

i=1(x) to be nowhere diﬀerentiable for most any initial

5. However, the particular g(x) makes the proof easy, because the peri-

odicity kills the higher terms of the series for ∆g(x), and using 22k

x for the

argument allows the kth term to go to inﬁnity and dominate the earlier terms

of the series. It seems lucky in whatever sense luck exists in mathematics.

