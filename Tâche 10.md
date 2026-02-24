HC6T10 : Récupérer les chiffres d'un nombre (récursif)

### Code Haskell

```haskell
-- Définition de la fonction chiffres récursive
chiffres :: Integer -> [Int]
chiffres 0 = []  -- Cas de base : 0 n'a pas de chiffres
chiffres n = chiffres (n `div` 10) ++ [fromInteger (n `mod` 10)]  -- Cas récursif : reste + dernier chiffre

-- Fonction main pour tester la fonction chiffres
main :: IO ()
main = do
    let nombre = 123  -- Nombre à tester (chiffres = [1,2,3])
    putStrLn $ "Les chiffres de " ++ show nombre ++ " sont : " ++ show (chiffres nombre)
    
    let zero = 0
    putStrLn $ "Les chiffres de " ++ show zero ++ " sont : " ++ show (chiffres zero)
```

### Explications détaillées

#### 1. **Signature de la fonction `chiffres`**
```haskell
chiffres :: Integer -> [Int]
```
- **Type** : La fonction prend un `Integer` (entier potentiellement grand et non négatif) en entrée et retourne une liste d'`Int` (les chiffres, de 0 à 9). `Integer` est choisi pour supporter de grands nombres sans débordement (ex. : 10^100), tandis que les chiffres sont des `Int` petits.
- **Pourquoi non polymorphique ?** Les chiffres sont spécifiques aux entiers en base 10. On assume n ≥ 0 ; pour les négatifs, on pourrait ajouter `abs n`, mais ici on simplifie (erreur implicite si n < 0, car `div` et `mod` gèrent les négatifs différemment).
- **Ordre des chiffres** : De gauche à droite (MSB à LSB), grâce à la récursion sur `div 10` suivie de `++ [mod 10]`.

#### 2. **Cas de base**
```haskell
chiffres 0 = []
```
- **Rôle** : Pour 0, il n'y a pas de chiffres significatifs, donc retourne une liste vide. C'est le **cas de base** qui arrête la récursion.
- Sans ce cas, l'appel sur 0 mènerait à une boucle infinie (div 10 de 0 reste 0).

#### 3. **Cas récursif**
```haskell
chiffres n = chiffres (n `div` 10) ++ [fromInteger (n `mod` 10)]
```
- **Logique** : Pour n > 0, calcule récursivement les chiffres de la partie "haut" (n sans le dernier chiffre, via `div 10`), puis concatène le dernier chiffre (n `mod` 10) à la fin via `++`.
- **Opérateurs** :
  - `div` : Division entière (ex. : 123 `div` 10 = 12).
  - `mod` : Reste (ex. : 123 `mod` 10 = 3).
  - `fromInteger` : Convertit le résultat de `mod` (qui est `Integer`) en `Int` pour la liste.
- **Pourquoi `++` ?** Cela ajoute le chiffre à la fin, préservant l'ordre naturel (premiers chiffres en tête).
- **Exemple de déroulement pour `123`** :
  - `chiffres 123 = chiffres 12 ++ [3]`
  - `chiffres 12 = chiffres 1 ++ [2]`
  - `chiffres 1 = chiffres 0 ++ [1] = [] ++ [1] = [1]`
  - Remontant : `chiffres 12 = [1] ++ [2] = [1,2]`
  - `chiffres 123 = [1,2] ++ [3] = [1,2,3]`
- **Gestion des cas extrêmes** :
  - n = 0 : `[]`.
  - n = 1 : `[1]`.
  - n = 10 : `[1,0]`.
  - Grands n : Fonctionne grâce à `Integer`.
  - n < 0 : Non géré (pour le gérer : `chiffres (abs n)`).

#### 4. **Efficacité et améliorations potentielles**
- **Complexité** : O(log n) en temps (nombre d'appels = nombre de chiffres) et O(log n) en espace (stack récursif). Efficace même pour de très grands nombres.
- **Alternative** : Pour un ordre inverse (LSB à MSB), utiliser `:` au lieu de `++` : `chiffres n = if n == 0 then [] else (n `mod` 10) : chiffres (n `div` 10)`, puis `reverse` à la fin. Mais ici, on vise l'ordre naturel.
