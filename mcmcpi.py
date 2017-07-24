import random
import math
 
def withinCircle(x,y):
    if(x**2+y**2<1):
        return True
    else:
        return False
 
def main():
    circleArea = 0
    squareArea = 0
    pi = 0
    for i in range(0,100000):
        x = random.random()
        y = random.random()
        if(withinCircle(x,y)==1):
                   circleArea=circleArea+1
        squareArea=squareArea+1
    pi = 4.0*circleArea/squareArea
    print "Approximate value for pi: ", pi
    print "Difference to exact value of pi: ", pi-math.pi
    print "Error: (approx-exact)/exact=", (pi-math.pi)/math.pi*100, "%"
