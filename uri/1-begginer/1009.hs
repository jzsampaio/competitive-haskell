import Text.Printf

solution text =
    let i = lines text
        nome = i !! 0
        salario = read ( i !! 1 ) :: Float
        vendas = read ( i !! 2 ) :: Float
        total = salario + vendas * 0.15
    in printf "TOTAL = R$ %.2f\n" total

main = interact solution
