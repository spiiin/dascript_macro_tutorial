//example, sort words stat by word length

require fio
require strings
require daslib/strings_boost

[export]
def main()
	var strings : array<string>
	fopen("eng_to_rus.txt","rt") <| $(f)
		if f != null
			while !feof(f)
				strings |> push <| fgets(f)

	var counter : table<int; int>
	for str in strings
		var words <- str |> split_by_chars(" .,:-\n\t()%\"'")
		for word in words
			if word != ""
				counter[length(word)] += 1   //word length
				//counter[word] += 1         //word
	
	var freq_pairs : array<tuple<int;int>>
	for k, v in keys(counter), values(counter)
		freq_pairs |> push <| [[ tuple<int;int> k, v]]
	freq_pairs |> sort($(a,b) => !(a._1 < b._1))

	for pair in freq_pairs
		print("{pair._0} : {pair._1}\n")
