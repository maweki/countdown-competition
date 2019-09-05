Ich benutze non-empty und utility-ht. Das Programm braucht mit ghc-8.6.5 
-O2 etwa 2,5s für jedes Beispiel und muss dafür 1,3 Mio Gleichungen 
durchprobieren. Wenn du das Modul als eigenständiges Programm übersetzen 
willst, musst du die module-Zeile auskommentieren.

Meine Idee ist, alle Ausdrücke bzgl. Kommutativität und Assoziativität zu 
normalisieren. D.h. ich benutze für Summen die Form:
    a+b+c-d-e-f

für Produkte:
    a*b*c/d/e/f

wobei a,b,c und d,e,f jeweils nach aufsteigendem "kleinsten" Element 
sortiert sind. Neben der verringerten Anzahl zu probierender Ausdrücke 
bekomme ich auch direkt eine schöne Ausgabe mit wenig Klammern.

