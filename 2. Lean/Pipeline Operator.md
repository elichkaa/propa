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

### **Chaining method calls with `.` after `|>`

After using `|>`, you can chain method/field access with `.` on the result

The first `.` after `|>` operates on the piped result, and subsequent `.` calls continue chaining without needing another `|>`.

```
s.drop n |>.reverse.toString 
-- the same as: 
(s.drop n).reverse.toString
```

