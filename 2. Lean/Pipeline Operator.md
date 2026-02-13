Passes the result of one expression as the **last argument** to the next function
```
xs |> f |> g |> h
-- the same as:
h (g (f xs))
```
```
[1, 2, 3] 
	|> List.map (fun x => x + 1) -- [2, 3, 4] 
	|> List.filter (fun x => x > 2) -- [3, 4] 
	|> List.sum -- 7
	
List.sum (List.filter (fun x => x > 2) (List.map (fun x => x + 1) [1, 2, 3]))
```
