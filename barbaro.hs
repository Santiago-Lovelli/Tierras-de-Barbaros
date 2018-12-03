import Text.Show.Functions

type Habilidad = String

type Objeto = Barbaro -> Barbaro

type Aventura = [Evento]

type Evento = Barbaro -> Bool

type Barbaros = [Barbaro]

data Barbaro = Barbaro{
	nombre :: String,
	fuerza :: Int,
	habilidades :: [Habilidad],
	objetos :: [Objeto]
}deriving(Show)

dave = Barbaro{
	nombre = "Dave", 
	fuerza = 100,
	habilidades = ["tejer","escribirPoesia"], 
	objetos = [ardilla]--como no se define que hace el libroPedKing no se agrega
}

faffy = Barbaro{
	nombre = "Faffy", 
	fuerza = 90,
	habilidades = ["robar"], 
	objetos = [cuerda megafono ardilla]
}

astro = Barbaro{
	nombre = "Astro", 
	fuerza = 70,
	habilidades = ["Pasear", "Levantar una piramide", "Escribir poesia atroz"], 
	objetos = [megafonoBarbarico, varitaDefectuosa]
}

espada :: Int -> Barbaro -> Barbaro

espada peso barbaro = barbaro{fuerza = (2*peso) + fuerza barbaro}

amuletoMistico :: Habilidad -> Barbaro -> Barbaro

amuletoMistico habilidad barbaro = barbaro{habilidades = [habilidad] ++ habilidades barbaro}

varitaDefectuosa :: Barbaro -> Barbaro

varitaDefectuosa barbaro = barbaro{habilidades = ["HacerMagia"]}

ardilla :: Barbaro -> Barbaro

ardilla = id

cuerda :: Objeto -> Objeto -> Objeto

cuerda objeto otroObjeto = objeto.otroObjeto

megafono :: Barbaro -> Barbaro

megafono barbaro = barbaro {habilidades = [(todoMayuscula.concat.habilidades) barbaro]}

todoMayuscula :: String -> String

todoMayuscula palabra = map ponerEnMayuscula palabra

ponerEnMayuscula :: Char -> Char

ponerEnMayuscula unCaracter
	|elem unCaracter ['a'..'z'] = toEnum (fromEnum unCaracter + (fromEnum 'A'- fromEnum 'a'))::Char
	|otherwise = unCaracter

megafonoBarbarico :: Barbaro -> Barbaro

megafonoBarbarico barbaro = (cuerda megafono ardilla) barbaro

invasionDeSuciosDuendes :: Barbaro -> Bool

invasionDeSuciosDuendes barbaro = elem ("Escribir poesia atroz") (habilidades barbaro)

cremalleraDelTiempo :: Barbaro -> Bool 

cremalleraDelTiempo (Barbaro nombre _ _ _) = nombre == "Faffy" || nombre == "Astro"

saqueo :: Barbaro -> Bool

saqueo barbaro = elem("Robar")(habilidades barbaro) && (fuerza barbaro) > 80

gritoDeGuerra :: Barbaro -> Bool

gritoDeGuerra barbaro = length(objetos barbaro) * 4 <= (poderDeGrito barbaro)

poderDeGrito :: Barbaro -> Int

poderDeGrito barbaro = (length.concat.habilidades) barbaro

caligrafia :: Barbaro -> Bool

caligrafia barbaro = all condicionCaligrafica (habilidades barbaro)

condicionCaligrafica :: String -> Bool

condicionCaligrafica habilidad = elem (head habilidad) ['A'..'Z'] && tieneVocalesSuficientes habilidad

tieneVocalesSuficientes :: String -> Bool

tieneVocalesSuficientes palabra = length(filter esVocal palabra) >= 3

esVocal :: Char -> Bool

esVocal unCaracter = elem unCaracter vocales

vocales = "aeiouAEIOU"

listaDePruebas = [saqueo, gritoDeGuerra, caligrafia]

ritualDeFechorias :: Barbaro -> Bool

ritualDeFechorias barbaro = any($ barbaro) listaDePruebas

sobrevivientes :: Aventura -> Barbaros -> Barbaros

sobrevivientes aventura barbaros = filter (siPasa aventura) barbaros

siPasa :: Aventura -> Barbaro->Bool

siPasa aventura barbaro = all ($ barbaro) aventura

sinRepetidos :: Eq a => [a]->[a]

sinRepetidos [] = []

sinRepetidos lista
	|not(elem (head lista) (tail lista))= [head lista] ++ (sinRepetidos (tail lista))
	|otherwise = sinRepetidos (tail lista)

descendiente :: Barbaro -> Barbaro

descendiente (Barbaro unNombre unaFuerza unasHabilidades unosObjetos) = foldr ($) (Barbaro {nombre=(unNombre++"*"), fuerza=unaFuerza, habilidades=sinRepetidos(unasHabilidades), objetos=unosObjetos}) unosObjetos

descendientes :: Barbaro -> Barbaros

descendientes barbaro = iterate descendiente barbaro