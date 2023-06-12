--Lista 1 - Ana Elisa Ghanem Zanon

--Exercicio 1
ehTriangulo a b c = a + b > c && b + c > a && c + a > b

--Exercicio 2
tipoTriangulo d e f = if d == e && e == f then "equilatero"
                        else if d == e || e == f then "isoceles"
                            else "escaleno"

--Exercicio 3
triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c 
                else "nao eh um triangulo"

--Exercicio 4
somaPares' 0 = 0
somaPares' n = n + somaPares(n-2)
somaPares n = if mod n 2 == 0 then somaPares' n 
            else somaPares' (n - 1)

--Exercicio 5
somaPot2m m 0 = m
somaPot2m m n = 2^n * m + somaPot2m m (n-1)

--Exercicio 6 
primo n = primo' n (n-1)
primo' n d |d==1 = True
           |rem n d == 0 = False
           |otherwise = primo' n (d-1)

--Exercicio 7
seriePI n = seriePI' n 1 1
seriePI' n d s |d<n = 4/d*s + seriePI' n (d+2) (s*(-1))
               |otherwise = 0