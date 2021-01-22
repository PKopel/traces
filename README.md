# traces

Small program computing dependency relation, trace, FNF and Hasse diagram (in Dot format) of a given word.

## Usage

Building: `stack build`
Running: `stack exec traces-exe -- <input file name>`

### Input:

Input file must contain an alphabeth, an independence relation and a word to process.
Example:
```
abcd
{(a, d), (d, a), (b, c), (c, b)}
baadcb
```

### Output

Program produces output in a file named "results.txt".
Example:
```
[(d,d),(d,c),(d,b),(c,d),(c,c),(c,a),(b,d),(b,b),(b,a),(a,c),(a,b),(a,a)]
["baadcb","bdaacb","badacb","baadbc","bdaabc","badabc"]
(b)(ad)(a)(bc)
digraph g{
	4 -> 5
	4 -> 6
	3 -> 6
	3 -> 5
	2 -> 3
	1 -> 2
	1 -> 4
	1[label=b]
	2[label=a]
	3[label=a]
	4[label=d]
	5[label=c]
	6[label=b]
}
(b)(ad)(a)(bc)
```