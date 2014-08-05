plascalding
===========
<pre>
Perceptron Learning Algorithm using Scalding Typed API

Single Layer Perceptron Learning Algorithm: Supervised Classification
aka W = W + X

PLA Algo:
Given a (guess) vector W,
For all vectors X, we want sign(W dot X) == sign(X)
For any X, if sign does not agree,
Apply Update rule:
W = W + X
Start all over again with this new W.

More Details in pla.scala

To run: 
$ scald.rb --hdfs-local pla.scala --n 10 --a 2 --b 3 --c 4
where 
n = number of points to classify
a, b, c = coefficients of the line ax + by + c = 0
