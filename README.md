# Electronic Invoicing Kit for EU (EIKE)

Tools for processing, conversion and validation of EN 16931 E-Invoices.

## Description [German]

Mit diesen Werkzeugen soll die Verarbeitung von E-Rechnungen, die auf der
EN 16931 Norm basieren, erleichtert werden. Für die geplanten Werkzeuge (EIKE)
sind drei Merkmale wichtig:

- Eigene Syntax
- Werkzeuge für die Kommandozeile
- Graphische Unterstützung (GUI)

### Eigene Syntax

Die von der EN 16931 erlaubten Syntaxen basieren alle auf XML. Weder das Lesen
noch Schreiben von E-Rechnungen in diesen Formaten ist praktikabel. Aus diesem
Grund soll es in EIKE eine eigene Syntax geben, die leicht lesbar ist und 
stärker das semantische Modell der EN 16931 Rechnung wiederspiegelt. Es soll
dann möglich sein, elektronische Rechnungen als Textdatei zu schreiben. Dann
können diese mit einem Werkzeug geprüft und in die XML Syntaxen konvertiert
werden.

### Werkzeuge für die Kommandozeile

Die EIKE Werkzeuge sollen wie ein Compiler vom Terminal aus aufgerufen werden
können, um E-Rechnungen einzulesen, zu prüfen und in verschiedene Formate 
umzuwandeln. So soll beispielsweise eine Rechnung in der eigenen Syntax in eine
Rechnung in der UBL Syntax überführt werden können:

    eike rechnung.txt --syntax=ubl -o invoice.xml

Weiterhin sollen die Werkzeuge auch eine Validierung der Rechnung ermöglichen
(beispielsweise nach dem XRechnung Standard):

    eike rechnung.txt --cius=xrechnung

### Graphische Unterstützung (GUI)

Es soll auch ein graphisches Werkzeug geben, das E-Rechnungen einlesen und
visualisieren kann. Intern wird das GUI Werkzeug die Werkezeuge für die 
Kommandozeile nutzen. Etwaige Fehlermeldungen, die sonst auf dem Terminal
ausgegeben werden, zeigt das GUI Werkzeug dann in einem Fenster an.