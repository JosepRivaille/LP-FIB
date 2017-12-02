from functools import reduce


def myLength(L):
    return sum(map(lambda x: 1, L))


def myMaximum(L):
    return reduce(lambda x, y: x if x >= y else y, L)


def average(L):
    return reduce(lambda x, y: x + y, L) / myLength(L)


def buildPalindrome(L):
    return L[::-1] + L


def remove(L1, L2):
    return list(filter(lambda x: x not in L2, L1))


def flatten(L):
    return [x for e in L for x in flatten(e)] if isinstance(L, list) else [L]


def oddsNevens(L):
    evens = list(filter(lambda x: x % 2 != 0, L))
    odds = list(filter(lambda x: x % 2 == 0, L))
    return evens, odds


def isPrime(x):
    return (x > 1) and not [isPrime(p) for p in range(2, x+1)
                            if p * p <= x and x % p == 0]


def primeDivisors(n):
    return list(filter(isPrime, [i for i in range(1, n + 1) if n % i == 0]))
