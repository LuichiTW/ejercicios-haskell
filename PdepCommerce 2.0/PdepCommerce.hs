precioTotal :: Fractional a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento envio = (aplicarDescuento precioUnitario descuento * cantidad) + envio

esProductoDeElite :: [Char] -> Bool
esProductoDeElite nombre = esProductoDeLujo nombre && not (esProductoCorriente nombre)

aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento = precio - precio*descuento/100

entregaSencilla :: [Char] -> Bool
entregaSencilla = even.length

esProductoDeLujo :: [Char] -> Bool
esProductoDeLujo nombre = elem 'a' nombre||elem 'z' nombre

aplicarCostoDeEnvio :: Fractional a => a -> a -> a
aplicarCostoDeEnvio precio envio = precio + envio

esProductoCodiciado :: [Char] -> Bool
esProductoCodiciado nombre = length nombre > 10

esProductoCorriente :: [Char] -> Bool
esProductoCorriente nombre = head nombre == 'a'||head nombre == 'e'||head nombre == 'i'||head nombre == 'o'||head nombre == 'u'

productoXL :: [Char] -> [Char]
productoXL producto = producto ++ "XL"