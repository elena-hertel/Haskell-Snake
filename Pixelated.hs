module Pixelated where

import Data.Char

type Character = [String]

blk :: String
blk = "\x1b[48;5;0m  \x1b[0m"

wht :: String
wht = "  "

a :: Character
a = map concat [[wht,blk,blk], [blk,wht,blk], [blk,blk,blk], [blk,wht,blk]]

b :: Character
b = map concat [[blk,wht,wht], [blk,blk,blk], [blk,wht,blk], [blk,blk,blk]]

c :: Character
c = map concat [[blk,blk,blk], [blk,wht,wht], [blk,wht,wht], [blk,blk,blk]]

d :: Character
d = map concat [[blk,blk,wht], [blk,wht,blk], [blk,wht,blk], [blk,blk,wht]]

e :: Character
e = map concat [[blk,blk,blk], [blk,wht,wht], [blk,blk,wht], [blk,blk,blk]]

f :: Character
f = map concat [[blk,blk,blk], [blk,wht,wht], [blk,blk,wht], [blk,wht,wht]]

g :: Character
g = map concat [[blk,blk,blk], [blk,wht,wht], [blk,wht,blk], [blk,blk,blk]]

h :: Character
h = map concat [[blk,wht,blk], [blk,wht,blk], [blk,blk,blk], [blk,wht,blk]]

i :: Character
i = map concat [[blk], [blk], [blk], [blk]]

j :: Character
j = map concat [[wht,wht,blk], [wht,wht,blk], [blk,wht,blk], [blk,blk,blk]]

k :: Character
k = map concat [[blk,wht,blk], [blk,blk,wht], [blk,wht,blk], [blk,wht,blk]]

l :: Character
l = map concat [[blk,wht,wht], [blk,wht,wht], [blk,wht,wht], [blk,blk,blk]]

m :: Character
m = map concat [[blk,blk,wht,blk,blk], [blk,wht,blk,wht,blk], [blk,wht,blk,wht,blk], [blk,wht,wht,wht,blk]]

n :: Character
n = map concat [[blk,wht,wht,blk], [blk,blk,wht,blk], [blk,wht,blk,blk], [blk,wht,wht,blk]]

o :: Character
o = map concat [[blk,blk,blk], [blk,wht,blk], [blk,wht,blk], [blk,blk,blk]]

p :: Character
p = map concat [[blk,blk,blk], [blk,wht,blk], [blk,blk,blk], [blk,wht,wht]]

q :: Character
q = map concat [[blk,blk,blk], [blk,wht,blk], [blk,blk,blk], [wht,wht,blk]]

r :: Character
r = map concat [[blk,blk,blk], [blk,wht,blk], [blk,blk,wht], [blk,wht,blk]]

s :: Character
s = map concat [[blk,blk], [blk,wht], [wht,blk], [blk,blk]]

t :: Character
t = map concat [[blk,blk,blk], [wht,blk,wht], [wht,blk,wht], [wht,blk,wht]]

u :: Character
u = map concat [[blk,wht,blk], [blk,wht,blk], [blk,wht,blk], [blk,blk,blk]]

v :: Character
v = map concat [[blk,wht,blk], [blk,wht,blk], [blk,wht,blk], [wht,blk,wht]]

w :: Character
w = map concat [[blk,wht,blk,wht,blk], [blk,wht,blk,wht,blk], [blk,wht,blk,wht,blk], [wht,blk,wht,blk,wht]]

x :: Character
x = map concat [[blk,wht,blk], [wht,blk,wht], [blk,wht,blk], [blk,wht,blk]]

y :: Character
y = map concat [[blk,wht,blk], [blk,blk,blk], [wht,blk,wht], [wht,blk,wht]]

z :: Character
z = map concat [[blk,blk], [wht,blk], [blk,wht], [blk,blk]]

space :: Character
space = [wht, wht, wht, wht]

zero :: Character
zero = map concat [[blk,blk,blk], [blk,wht,blk], [blk,wht,blk], [blk,blk,blk]]

one :: Character
one = map concat [[wht,blk], [blk,blk], [wht,blk], [wht,blk]]

two :: Character
two = map concat [[blk,blk], [wht,blk], [blk,wht], [blk,blk]]

three :: Character
three = map concat [[blk,blk,blk], [wht,blk,blk], [wht,wht,blk], [blk,blk,blk]]

four :: Character
four = map concat [[blk,wht,blk], [blk,wht,blk], [blk,blk,blk], [wht,wht,blk]]

five :: Character
five = map concat [[blk,blk], [blk,wht], [wht,blk], [blk,blk]]

six :: Character
six = map concat [[blk,wht,wht], [blk,blk,blk], [blk,wht,blk], [blk,blk,blk]]

seven :: Character
seven = map concat [[blk,blk,blk], [wht,wht,blk], [wht,wht,blk], [wht,wht,blk]]

eight :: Character
eight = map concat [[wht,blk,blk,blk], [wht,blk,wht,blk], [blk,blk,blk,blk], [blk,blk,blk,blk]]

nine :: Character
nine = map concat [[blk,blk,blk], [blk,wht,blk], [blk,blk,blk], [wht,wht,blk]]

exclamation :: Character
exclamation = [blk,wht,wht,blk]

colon :: Character
colon = [blk, wht, wht, blk]

charToCharacter :: Char -> Character
charToCharacter ch =
    case (toLower ch) of
        'a' -> a
        'b' -> b
        'c' -> c
        'd' -> d
        'e' -> e
        'f' -> f
        'g' -> g
        'h' -> h
        'i' -> i
        'j' -> j
        'k' -> k
        'l' -> l
        'm' -> m
        'n' -> n
        'o' -> o
        'p' -> p
        'q' -> q
        'r' -> r
        's' -> s
        't' -> t
        'u' -> u
        'v' -> v
        'w' -> w
        'x' -> x
        'y' -> y
        'z' -> z
        '0' -> zero
        '1' -> one
        '2' -> two
        '3' -> three
        '4' -> four
        '5' -> five
        '6' -> six
        '7' -> seven
        '8' -> eight
        '9' -> nine
        '!' -> exclamation
        ':' -> colon
        ' ' -> space
        _   -> error "Unsupported character"

toChars :: String -> [Character]
toChars str =
    map charToCharacter str


printWord :: [Character] -> IO ()
printWord chars = do
  mapM_ (\i -> mapM_ (\c -> putStr ((c !! i) ++ wht)) chars >> putStrLn "") [0..3]
