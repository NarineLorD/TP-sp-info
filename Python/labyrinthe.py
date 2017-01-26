#! /usr/bin/python3

from random import randrange

# Fonctions auxiliaires
def swap(t, i, j):
    """Échange t[i] et t[j]"""
    t[i], t[j] = t[j], t[i]

def melange(t):
    """Prend en argument un tableau t de taille n et permute
    aléatoirement ses éléments. Toutes les permutations sont
    équiprobables."""
    # On utilise le mélange de Fisher-Yates
    # ( https://fr.wikipedia.org/wiki/M%C3%A9lange_de_Fisher-Yates )
    n = len(t)
    for i in range(0, n):
        j = randrange(i, n)
        swap(t, i, j)

# Représentation d'un labyrinthe
# ------------------------------
#
# Notre labyrinthe est une grille rectangulaire comportant des pièces,
# séparée par des murs. Ces pièces sont repérées par des coordonnées
# entières (i, j) avec i dans range(p) et j dans range(q) pour des
# valeurs p et q dépendant du labyrinthe. Chaque mur est repéré comme
# un couple de couple ((i1, j1), (i2, j2)) donnant les coordonnées
# (i1, j1) et (i2, j2) des pièces qu'il sépare.
# Le labyrinthe est représenté comme une matrice t telle que pour tout
# (i, j), t[i, j] est un tableau donnant les pièces voisines de (i,
# j) qui ne sont pas séparées de (i, j) par un mur. Ces pièces peuvent
# être données dans un ordre quelconque.

# Création du labyrinthe
# ----------------------
#
# Pour créer un labyrinthe, on va utiliser l'algorithme union-find
# pour trouver quelles cases sont accessibles à partir de quelles
# cases ( https://fr.wikipedia.org/wiki/Union-Find ).

def find(i, j, parent):
        """Retourne le représentant canonique de (i, j) dans la
        structure d'union-find parent. Effectue la compression de
        chemin."""
        a, b = i, j
        # on cherche le représentant
        while parent[a][b] != (a, b):
            a, b = parent[a][b]
        # on compresse les chemins :
        while parent[i][j] != (a, b):
            parent[i][j] = (a, b)
            i, j = parent[i][j]
        return (a, b)

def fusionne(a1, b1, a2, b2, parent, rang):
        """Fusionne les composantes de (a1, b1) et (a2, b2), supposés
        être des racines dans la structure parent. Rang est supposé
        être une matrice donnant les rangs des différentes racines."""
        # on fait pointer l'élément de plus petit rang vers celui de
        # plus grand rang, avec mise à jour du rang si les deux sont
        # égaux.
        r1 = rang[a1][b1]
        r2 = rang[a2][b2]
        if r1 < r2:
            parent[a1][b1] = (a2, b2)
        elif r1 > r2:
            parent[a2][b2] = (a1, b1)
        else: # r1 == r2
            parent[a1][b1] = (a2, b2)
            rang[a2][b2] += 1

def construit_labyrinthe(p, q):
    """Construit et retourne un labyrinthe de taille p*q de telle
    façon que toutes les cases soient reliées et qu'il n'y ait qu'un
    seul chemin d'une case à une autre."""
    # initialement, les pièces sont toutes séparées
    t = [[[] for j in range(q)] for i in range(p)]
    # on fait alors la liste des murs qu'on peut potentiellement
    # casser :
    murs = [((i, j), (i+1, j)) for j in range(q) for i in range(p-1)]
    murs += [((i, j), (i, j+1)) for j in range(q-1) for i in range(p)]
    # On supprime maintenant des murs au hasard jusqu'à obtenir une
    # unique composante connexe.
    n = p * q # nombre initial de composantes connexes.
    # tableau des parents
    parent = [[(i, j) for j in range(q)] for i in range(p)]
    # tableau des rangs des composantes connexes :
    rang = [[0 for j in range(q)] for i in range(p)]
    # on considère les différents murs, dans un ordre aléatoire
    # (toutes les permutations étant équiprobables)
    melange(murs)
    for ((i1, j1), (i2, j2)) in murs:
        # s'il n'y a plus qu'une composante connexe, on a terminé
        if n == 1: break
        # sinon, on cherche les représentants canoniques de leurs
        # composantes connexes :
        a1, b1 = find(i1, j1, parent)
        a2, b2 = find(i2, j2, parent)
        if (a1, b1) != (a2, b2):
            fusionne(a1, b1, a2, b2, parent, rang)
            # on crée un passage entre (i1, j1) et (i2, j2) :
            t[i1][j1].append((i2, j2))
            t[i2][j2].append((i1, j1))
            # on diminue de 1 le nombre de composantes connexes
            n -= 1
    return t

import pylab as pl
def mur(x, y, dx, dy, ouvert ):
    """Trace un mur, centré en (x, y), d'extrémités (x+dx,
       y+dy) et (x-dx, y-dy). Si ouvert vaut True, une porte est
       ouverte au milieu de ce mur."""
    if ouvert:
        pl.plot([x+0.5*dx, x+dx], [y+0.5*dy, y+dy], color='black')
        pl.plot([x-0.5*dx, x-dx], [y-0.5*dy, y-dy], color='black')
    else:
        pl.plot([x-dx, x+dx], [y-dy, y+dy], color='black')
        
    
def dessine_labyrinthe(t):
    """Dessine le labyrinthe représenté par le tableau t.
    La pièce (i, j) a un centre situé au point de coordonnées (i, j).
    """
    p = len(t)
    q = len(t[0])
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for i in range(p):
        for j in range(q):
            for (a, b) in directions:
                i2 = i+a
                j2 = j+b
                o = (i2, j2) in t[i][j] or (i2, j2) == (-1, 0) \
                    or (i2, j2) == (p, q-1)
                mur(i+0.5*a, j+0.5*b, 0.5*b, -0.5*a, o)

