# eag-tracker
 Vergleicht den Ausbau an erneuerbaren Energien in Österreich mit den EAG Zielen. Dazu lädt es die aktuellen Erzeugungsdaten von e-control.at und plottet diese im Vergleich zu einem linearen Ausbaupfad, welcher erlaubt, das EGA in 2030 zu erreichen. Alternativ können auch entso-e Daten verwendet werden. Diese scheinen für Biomasse und PV aber ungenau zu sein. Der Update der e-control Daten für Biomasse und PV erfolgt jährlich, der Update für Wind- und Wasserkraft monatlich.
 
## Dependencies
```tidyverse

lubridate

tidyquant

zoo```

Falls die Ergebnisse direkt getweetet werden sollen, ist ein App-Account bei Twitter notwendig, außerdem die folgenden libraries:
```twitteR

feather```

## Verwendung
Verwendung: das Skript ```eag-tracker.R``` laufen lassen.

Sollen auch entso-e Daten verwendet werden (Achtung! Langer Download!), kann das Skript ega-tracker-entso-e.R ausgeführt werden. Hierzu ist ein Account auf der [entso-e transparency platform notwendig](https://keycloak-transparency.entsoe.eu/auth/realms/tp/login-actions/registration?client_id=tp-web&tab_id=9udiCmkuvB8).







