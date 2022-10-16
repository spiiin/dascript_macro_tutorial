//example, delete every 5th element of list

struct ListItem
	value: int
	[[do_not_delete]] next: ListItem?

def makeDemoList
	var head =  new [[ListItem value=1]]
	var current = head
	for i in range(2, 21)
		current.next = new [[ListItem value=i]]
		current = current.next
	return head

[sideeffects]
def deleteEveryFifth(lst: ListItem?)
	var counter = 1
	var current = lst

	while current != null
		if counter++ % 4 == 0
			var toDelete = current.next
			current.next = current.next?.next
			unsafe { delete toDelete; }
		current = current.next

def printList(lst: ListItem?)
	if lst != null
		print("{lst.value} ")
		printList(lst.next)
	else
		print("\n")

[export]
def main
	var list = makeDemoList()
	printList(list)
	deleteEveryFifth(list)
	printList(list)
	//freeList //or just kill context

//Output:
// 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
// 1 2 3 4 6 7 8 9 11 12 13 14 16 17 18 19