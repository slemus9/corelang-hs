# Chapter 2 - Exercises

## Exercise 2.1

State machine rules:

- Rule 1: $(n, m, d, t) \rightarrow (n, m, d - 1, t + 1),\,d > 0$
- Rule 2: $(n, m, 0, t) \rightarrow (n, m - 1, n, t),\,m > 0$
- Initial state: $(2, 3, 0, 0)$

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

Let the initial state be $(N, M, 0, 0)$.

**Invariant:** $0 \leq m \leq M$, $0 \leq d \leq N$, $N*M = n*m + d + t$

**Base case:** invariant is true since $N*M = N*M + 0 + 0$

**Inductive case:** if invariant is true for $(n, m, d, t)$, then:

Case $(n, m, d, t) \rightarrow (n'=n, m'=m, d'=d-1, t'=t + 1)$, then:
$$n'*m' + d' + t'$$
$$= n*m + d - 1 + t + 1$$
$$= n*m + d + t$$
$$= N*M$$

Case $(n, m, d=0, t) \rightarrow (n'=n, m'=m-1, d'=n, t'=t)$, then:
$$n'*m' + d' + t'$$
$$= n*(m-1) + n + t$$
$$= n*m + n - n + t$$
$$= n*m + t = N*M$$

**Termination condition:** $m = d = 0$, then by the invariant, $N*M = n*0 + 0 + t = t$

**Termination:** from both transitions we can see that $m$ is monotonically decreasing and $d$ ranges between $0$ and $N$. Since $m$ decreases only when $d = 0$, that is, when $d$ decreases from $N$ to $0$, then eventually both $m$ and $n$ will be $0$, and the machine terminates.

## Exercise 2.3

```haskell
type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

multFinal :: MultState -> Bool
multFinal (_, 0, 0, _)  = True
multFinal _             = False
```