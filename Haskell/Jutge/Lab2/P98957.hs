ones :: [Integer]
ones = iterate (+0) 1

nats :: [Integer]
nats = iterate (+1) 0

negs :: [Integer]
negs = iterate (+(-1)) $ -1

ints :: [Integer]
ints = zipWith (++) negs nats
