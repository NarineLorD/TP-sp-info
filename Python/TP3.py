from labyrinthe import *
def trouve_chemin(t):
    p=len(t)
    q=len(t[0])
    M=[[1]*q for y in range(p)]
    V=[[(0,0)]*q for y in range(p)]
    P=[(0,0)]
    M[0][0]=0
    n=0
    while P[-1] != (p-1,q-1) and n<p*q: 
        a = P[-1]
        n+=1
        for b in t[a[0]][a[1]]:
            if M[b[0]][b[1]]:
                P.append(b)
                M[b[0]][b[1]]=0
                V[b[0]][b[1]]=a

    x=(p-1,q-1)
    C=[]
    while x != (0,0):
        C.append(x)
        x=V[x[0]][x[1]]
    return C


        

p=10
q=20
t=construit_labyrinthe(p,q)
a=trouve_chemin(t)
