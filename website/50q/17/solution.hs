fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t