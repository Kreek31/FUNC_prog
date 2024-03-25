// Определите функции для решение алгебраических уравнений

let rec dichotomy f a b =
  let eps = 0.0001
  let c = (a+b)/2.0
  if abs(f c)<eps then c
  else
    if (f a)*(f c)<0.0 then dichotomy f a c
    else dichotomy f b c

let rec iterations f x0 h =
  let eps = 0.0001
  let F n = (f n) + n
  if h < 90 then
    if abs(x0 - (F x0))<eps then x0
    else
        //printfn"%i %f" h x0
        iterations f (F x0) (h+1)
  else x0

let rec newthon f f' x0 = 
    let eps = 0.0001
    if abs(f x0)<eps then x0
    else
        let x1 = x0-((f x0)/(f' x0))
        newthon f f' x1


// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = cos(2.0/x)-2.0*sin(1.0/x)+(1.0/x)
let f2 x = sqrt(1.0-(0.4*(x**2.0)))-asin(x)
let f3 x = exp(x)-exp(-1.0*x)-2.0

let f1' x = (2.0/(x**2.0))*(sin(2.0/x)+cos(1.0/x))-(1.0/(x**2.0))
let f2' x = -1.0*(2.0*x/(5.0*sqrt(1.0-(2.0*(x**2.0)/5.0))))-(1.0/sqrt(1.0-(x**2.0)))
let f3' x = exp(x)+exp(-1.0*x)

//let phi1 = ...
//let phi2 = ...
//let phi3 = ...

let main = 
    printfn"У 1-й и 3-й функций в методе итераций не выполняется условие сходимости последовательности корней 0<F'(X)<1"
    printfn "%10.5f %10.5f %10.5f" (dichotomy f1 1. 2.) (dichotomy f2 0. 1.) (dichotomy f3 0. 1.)
    printfn "%10.5f %10.5f %10.5f" (iterations f1 1.5 0) (iterations f2 0.5 0) (iterations f3 0.5 0)
    printfn "%10.5f %10.5f %10.5f" (newthon f1 f1' 1.5) (newthon f2 f2' 0.5) (newthon f3 f3' 0.5)

 