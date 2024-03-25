// Print a table of a given function f, computed by taylor series

// function to compute
let f x = exp (x**2.0)
let rec fact x =
  if x = 1.0 then 1.0
  else x * (fact (x-1.0))

let a = 0.0
let b = 1.0
let n = 10
let series = 15.0

// Define a function to compute f using naive taylor series method
let rec taylor_naive x n = 
  if n = 0.0 then 1.0
  else (x**(2.0*n))/(fact n) + taylor_naive x (n-1.0)


// Define a function to do the same in a more efficient way
let taylor x n =
  if n > 0.0 then
    let memberr = taylor_naive x (n-1.0)
    memberr + (memberr * x * x) / n
  else 1.0

let main =
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     printfn "%5.2f  %10.6f  %10.6f   %10.6f" x (f x) (taylor_naive x series) (taylor x series)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main

