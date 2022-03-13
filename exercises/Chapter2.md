# Chapter 2 - Exercises

## Exercise 2.1

State machine rules:

- Rule 1: <!-- $(n, m, d, t) \rightarrow (n, m, d - 1, t + 1),\,d > 0$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(n%2C%20m%2C%20d%2C%20t)%20%5Crightarrow%20(n%2C%20m%2C%20d%20-%201%2C%20t%20%2B%201)%2C%5C%2Cd%20%3E%200">
- Rule 2: <!-- $(n, m, 0, t) \rightarrow (n, m - 1, n, t),\,m > 0$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(n%2C%20m%2C%200%2C%20t)%20%5Crightarrow%20(n%2C%20m%20-%201%2C%20n%2C%20t)%2C%5C%2Cm%20%3E%200">
- Initial state: <!-- $(2, 3, 0, 0)$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(2%2C%203%2C%200%2C%200)">

```
(2, 3, 0, 0)
-> {Rule 2}
(2, 2, 2, 0)
-> {Rule 1}
(2, 2, 1, 1)
-> {Rule 1}
(2, 2, 0, 2)
-> {Rule 2}
(2, 1, 2, 2)
-> {Rule 1}
(2, 1, 1, 3)
-> {Rule 1}
(2, 1, 0, 4)
-> {Rule 2}
(2, 0, 2, 4)
-> {Rule 1}
(2, 0, 1, 5)
-> {Rule 1}
(2, 0, 0, 6)
```
## Exercise 2.2

Let the initial state be <!-- $(N, M, 0, 0)$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(N%2C%20M%2C%200%2C%200)">.

**Invariant:** <!-- $0 \leq m \leq M$, $0 \leq d \leq N$, $N*M = n*m + d + t$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=0%20%5Cleq%20m%20%5Cleq%20M%24%2C%20%240%20%5Cleq%20d%20%5Cleq%20N%24%2C%20%24N*M%20%3D%20n*m%20%2B%20d%20%2B%20t">

**Base case:** invariant is true since <!-- $N*M = N*M + 0 + 0$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=N*M%20%3D%20N*M%20%2B%200%20%2B%200">

**Inductive case:** if invariant is true for <!-- $(n, m, d, t)$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(n%2C%20m%2C%20d%2C%20t)">, then:

**Case** <!-- $(n, m, d, t) \rightarrow (n'=n, m'=m, d'=d-1, t'=t + 1)$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(n%2C%20m%2C%20d%2C%20t)%20%5Crightarrow%20(n'%3Dn%2C%20m'%3Dm%2C%20d'%3Dd-1%2C%20t'%3Dt%20%2B%201)">, then:


<!-- $$
\begin{aligned}
  n'*m' + d' + t' &= n*m + d - 1 + t + 1 \\
  &= n*m + d + t \\
  &= N*M
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0A%20%20n'*m'%20%2B%20d'%20%2B%20t'%20%26%3D%20n*m%20%2B%20d%20-%201%20%2B%20t%20%2B%201%20%5C%5C%0A%20%20%26%3D%20n*m%20%2B%20d%20%2B%20t%20%5C%5C%0A%20%20%26%3D%20N*M%0A%5Cend%7Baligned%7D"></div>

**Case** <!-- $(n, m, d=0, t) \rightarrow (n'=n, m'=m-1, d'=n, t'=t)$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=(n%2C%20m%2C%20d%3D0%2C%20t)%20%5Crightarrow%20(n'%3Dn%2C%20m'%3Dm-1%2C%20d'%3Dn%2C%20t'%3Dt)">, then:

<!-- $$
\begin{aligned}
n'*m' + d' + t' &= n*(m-1) + n + t \\
&= n*m + n - n + t \\
&= n*m + t \\
&= N*M
\end{aligned}
$$ --> 

<div align="center"><img style="background: white;" src="https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0An'*m'%20%2B%20d'%20%2B%20t'%20%26%3D%20n*(m-1)%20%2B%20n%20%2B%20t%20%5C%5C%0A%26%3D%20n*m%20%2B%20n%20-%20n%20%2B%20t%20%5C%5C%0A%26%3D%20n*m%20%2B%20t%20%5C%5C%0A%26%3D%20N*M%0A%5Cend%7Baligned%7D"></div>

**Termination condition:** <!-- $m = d = 0$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=m%20%3D%20d%20%3D%200">, then by the invariant, <!-- $N*M = n*0 + 0 + t = t$ --> <img style="transform: translateY(0.1em); background: white;" src="https://render.githubusercontent.com/render/math?math=N*M%20%3D%20n*0%20%2B%200%20%2B%20t%20%3D%20t">

**Termination:** from both transitions we can see that $m$ is monotonically decreasing and $d$ ranges between $0$ and $N$. Since $m$ decreases only when $d = 0$, that is, when $d$ decreases from $N$ to $0$, then eventually both $m$ and $n$ will be $0$, and the machine terminates.

## Exercise 2.3

```haskell
type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

multFinal :: MultState -> Bool
multFinal (_, 0, 0, _)  = True
multFinal _             = False
```