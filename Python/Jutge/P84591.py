def absValue(x):
    return x if x >= 0 else x*-1


def power(x, p):
    return x ** p if p < 10 else power(x * x, p - 1)


def isPrime(x):
    return (x > 1) and not [isPrime(p) for p in range(2, x+1)
                            if p * p <= x and x % p == 0]


def slowFib(n):
    return n if n <= 1 else slowFib(n - 1) + slowFib(n - 2)


def quickFib(n):
    lookup = [0, 1]
    for i in range(2, n+1):
        lookup.append(lookup[i-1] + lookup[i-2])
    return lookup[n]
