//example, max value built from 1s of binary representation of 32-bit number

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
	for i in range(31 - count1s) //assume 32 bits
		res<<=1
	return res 
[export]
def main
	print("{uint(maxFrom1s(2-1))}\n")    //'0x80000000'
	print("{uint(maxFrom1s(4-1))}\n")    //'0xc0000000'
	print("{uint(maxFrom1s(8-1))}\n")    //'0xe0000000'
	print("{uint(maxFrom1s(16-1))}\n")   //'0b11110000'
	print("{uint(maxFrom1s(32-1))}\n")   //'0xf0000000'
	print("{uint(maxFrom1s(64-1))}\n")   //'0xfc000000'
	print("{uint(maxFrom1s(128-1))}\n")  //'0xfe000000'
	print("{uint(maxFrom1s(256-1))}\n")  //'0xff000000'



