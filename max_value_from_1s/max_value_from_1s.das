//example, max value built from 1s of binary representation of number

def popcount(x)
	var temp = x
	var count = 0
	while temp != 0
		temp &= temp - 1
		count++
	return count

def maxFrom1s(x)
	let count1s = popcount(x)
	var res = 0
	for i in range(count1s)
		res++
		res<<=1
	for i in range(7 - count1s) //assume 8 bits
		res<<=1
	return res 
[export]
def main
	print("{maxFrom1s(2-1)}\n")    //'0b10000000'
	print("{maxFrom1s(4-1)}\n")    //'0b11000000'
	print("{maxFrom1s(8-1)}\n")    //'0b11100000'
	print("{maxFrom1s(16-1)}\n")   //'0b11110000'
	print("{maxFrom1s(32-1)}\n")   //'0b11111000'
	print("{maxFrom1s(64-1)}\n")   //'0b11111100'
	print("{maxFrom1s(128-1)}\n")  //'0b11111110'
	print("{maxFrom1s(256-1)}\n")  //'0b11111111'

