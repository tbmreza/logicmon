StringCopy(s, n)
for i := 1 #{sorry we start at 1}# to n
	t[i] := s[i]
return t

@output StringCopy("Poster", 255)  -- outputs "Poster"

InsertionSort(a)
for j := 2 to a.length
  key := a[j]
  -- Insert a[j] into the sorted ...
  i := j - 1
  while i > 0 and a[i] > key
    ....

MergeSort(a, p, r)
if p < r
  q := ⌊(p + r) / 2⌋
  MergeSort(a, p, q)
  ..

FindMaxCrossingSubarray(a, low, mid, high)
for i := mid downto low
  sum := sum + a[i]
  if ...
right-sum := -∞
sum := 0
...
