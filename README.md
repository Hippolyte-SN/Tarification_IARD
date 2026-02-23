# Tarification en assurance IARD – Automobile

## 🧭 Description
Ce projet explore dans son ensemble la tarification en assurance automobile.
Il vise à estimer la prime pure à partir de plusieurs méthodes notamment la méthode du maximum de vraissemblance, la modélisation GLM standards en tarification IARD.

---

## 🎯 Objectifs
- Analyser un portefeuille automobile
- Modéliser la fréquence et le cout des sinistres
- Construire une prime pure individuelle

---

## 📂 Données
Jeu de données représentant un portefeuille automobile.

### Variables principales
- `age`
- `Sexe`
- `Type.vehicule`
- `Date.sous`
- `anc.permis`
- `Zone.geo`
- `valeur`
- `exposition`
- `charge.sin`
- `nb_sinistres`

---

## ⚙️ Méthodologie

### Fréquence
- Moyenne empirique
- Maximum de vraissemblance
- GLM Poisson
- offset = log(exposition)

### Sévérité
- Moyenne empirique
- Maximum de vraissemblance
- GLM Log.normal
- lien logarithmique

### Prime pure
\[
\text{Prime pure} = \mathbb{E}[N] \times \mathbb{E}[C]
\]

---

## 📈 Résultats
Analyses univariées / bivariées, visualisation de données, modélisation de variables aléatoires, modélisation économétrique et tarification.

---

## 🛠️ Technologies
- R version 4.4.3
- fonctions R
- fonctions personnalisées

---
