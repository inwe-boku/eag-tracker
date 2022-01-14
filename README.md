# eag-tracker
 Vergleicht den Ausbau an erneuerbaren Energien in Österreich mit den EAG Zielen. Dazu lädt es die aktuellen Erzeugungsdaten von e-control.at und plottet diese im Vergleich zu einem linearen Ausbaupfad, welcher erlaubt, das EGA in 2030 zu erreichen. Alternativ können auch entso-e Daten verwendet werden. Diese scheinen für Biomasse und PV aber ungenau zu sein. Der Update der e-control Daten für Biomasse und PV erfolgt jährlich, der Update für Wind- und Wasserkraft monatlich.
 
## Dependencies
library(tidyverse)
library(lubridate)
library(tidyquant)
library(zoo)
library(feather)

Falls die Ergebnisse direkt getwittert wer den sollen, ist ein App-Account bei Twitter notwendig, außerdem 
library(twitteR)

## Verwendung
Verwendung: das Skript eag-tracker.R laufen lassen.

Sollen auch entso-e Daten verwendet werden (Achtung! Langer Download!), kann das Skript ega-tracker-entso-e.R ausgeführt werden.







