# Réplication : Angrist & Krueger (1991) - Rendements de l'éducation

Ce projet reproduit et étend les résultats de l'article célèbre d'Angrist et Krueger (1991) *"Does Compulsory School Attendance Affect Schooling and Earnings?"*.

##  Auteurs
* **Théo COSTANZO**
* **Damien LEVACHER**
* **Zeinabou AMADOU HAINIKOYE**

##  Résumé
L'objectif est d'estimer l'effet causal de l'éducation sur le salaire. Pour corriger le biais d'endogénéité (aptitudes non observées), nous utilisons le **trimestre de naissance** (Quarter of Birth) comme variable instrumentale (IV), exploitant les lois sur l'obligation scolaire.

**Méthodes utilisées :**
* MCO (Moindres Carrés Ordinaires / OLS)
* TSLS (Double Moindres Carrés / 2SLS)
* Estimateur de Wald

##  Données
Les données proviennent de l'**IPUMS** (Integrated Public Use Microdata Series), similaires aux données de recensement utilisées dans l'article original.

##  Outils
* **Langage :** R (R Markdown)
* **Librairies principales :** `tidyverse`, `AER` (pour ivreg), `stargazer` (pour les tableaux).

## Structure du dépôt
* Le rapport complet généré avec code et rédaction.
