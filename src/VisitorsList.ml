let rec interval i j =
  if i < j then
    i :: interval (i + 1) j
  else
    []
