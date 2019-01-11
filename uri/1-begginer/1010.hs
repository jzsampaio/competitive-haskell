import Text.Printf

t line =
    let i = words line
        p = read ( i !! 1 ) :: Float
        k = read ( i !! 2 ) :: Float
    in p * k

solution text =
    let i = lines text
        a = t (i !! 0)
        b = t (i !! 1)
        total = a + b
    in printf "VALOR A PAGAR: R$ %.2f\n" total

main = interact solution
